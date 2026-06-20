;;; agent-shell-links.el --- Bookmarks and Org links for agent-shell sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Paul D. Nelson

;; Author: Paul D. Nelson <ultrono@gmail.com>
;; Version: 0.0.1
;; URL: https://github.com/ultronozm/agent-shell-links.el
;; Package-Requires: ((emacs "29.1") (agent-shell "0.56.1"))
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Resume `agent-shell' sessions (or jump to existing ones) via bookmarks
;; and Org links.
;;
;; For bookmark support:
;;
;;  (agent-shell-links-bookmark-setup)
;;
;; For Org link support:
;;
;;   (org-link-set-parameters
;;    "agent-shell"
;;    :follow #'agent-shell-links-org-follow
;;    :store #'agent-shell-links-org-store)
;;
;; You can then link to `agent-shell' sessions via the usual commands,
;; such as `bookmark-set', `bookmark-jump', `org-store-link' and
;; `org-insert-link'.

;;; Code:

(require 'agent-shell)
(require 'map)
(require 'seq)
(require 'subr-x)

(defvar bookmark-make-record-function)

(declare-function bookmark-get-filename "bookmark" (bookmark-name-or-record))
(declare-function bookmark-prop-get "bookmark" (bookmark-name-or-record prop))
(declare-function org-link-decode "ol" (text))
(declare-function org-link-encode "ol" (text table))
(declare-function org-link-store-props "ol" (&rest plist))

(defvar agent-shell-links--strict-resume-advice-installed nil
  "Non-nil when fallback prevention advice has been installed.")

(defvar-local agent-shell-links--strict-resume-session-id nil
  "Session id this buffer must resume without falling back to a new session.")

(defvar agent-shell-links--encode-chars
  '(?\s ?\t ?\n ?% ?& ?? ?= ?#)
  "Characters percent-encoded in link query values.
These are the characters that would otherwise confuse query
parsing or Org's own link reader.")

(defun agent-shell-links--encode (string)
  "Return STRING encoded for an `agent-shell' Org link path."
  (require 'ol)
  (org-link-encode string agent-shell-links--encode-chars))

(defun agent-shell-links--decode (string)
  "Return STRING decoded from an `agent-shell' Org link path."
  (require 'ol)
  (org-link-decode string))

(defun agent-shell-links--build (session-id identifier dir)
  "Build an agent-shell link path from SESSION-ID, IDENTIFIER and DIR.
IDENTIFIER and DIR are optional and omitted from the result when nil."
  (let* ((agent-param
          (when identifier
            (format "agent=%s"
                    (agent-shell-links--encode (symbol-name identifier)))))
         (dir-param
          (when (and dir (not (string-empty-p dir)))
            (format "dir=%s"
                    (agent-shell-links--encode (expand-file-name dir)))))
         (params (delq nil (list agent-param dir-param))))
    (concat (agent-shell-links--encode session-id)
            (when params
              (concat "?" (string-join params "&"))))))

(defun agent-shell-links--parse (path)
  "Parse link PATH into a list of session id, agent id, and directory."
  (let* ((qpos (string-search "?" path))
         (session-id (agent-shell-links--decode
                      (if qpos (substring path 0 qpos) path)))
         (query (and qpos (substring path (1+ qpos))))
         agent
         dir)
    (dolist (pair (and query (split-string query "&" t)))
      (let* ((eq (string-search "=" pair))
             (key (if eq (substring pair 0 eq) pair))
             (val (agent-shell-links--decode
                   (if eq (substring pair (1+ eq)) ""))))
        (pcase key
          ("agent" (setq agent val))
          ("dir" (setq dir val)))))
    (list session-id agent dir)))

(defun agent-shell-links--config-for-identifier (identifier)
  "Return the agent config whose :identifier matches IDENTIFIER (a symbol).
Return nil when none matches."
  (when identifier
    (seq-find (lambda (config)
                (eq (map-elt config :identifier) identifier))
              agent-shell-agent-configs)))

(defun agent-shell-links--buffer-for-session (session-id &optional identifier)
  "Return a live `agent-shell' buffer for SESSION-ID.
When IDENTIFIER is non-nil, require the buffer's agent config
identifier to match it."
  (seq-find (lambda (buffer)
              (with-current-buffer buffer
                (and (equal (map-nested-elt agent-shell--state '(:session :id))
                            session-id)
                     (or (null identifier)
                         (eq (map-nested-elt agent-shell--state
                                             '(:agent-config :identifier))
                             identifier)))))
            (agent-shell-buffers)))

(defun agent-shell-links--display-buffer (buffer)
  "Display agent-shell BUFFER, respecting viewport preference."
  (if agent-shell-prefer-viewport-interaction
      (agent-shell-viewport--show-buffer :shell-buffer buffer)
    (agent-shell--display-buffer buffer)))

(defun agent-shell-links--current-session ()
  "Return a plist describing the current `agent-shell' session, or nil."
  (when (derived-mode-p 'agent-shell-mode)
    (when-let* ((state agent-shell--state)
                (session-id (map-nested-elt state '(:session :id)))
                ((not (string-empty-p session-id))))
      (list :session-id session-id
            :identifier (map-elt (map-elt state :agent-config) :identifier)
            :dir (ignore-errors (agent-shell-cwd))
            :title (map-nested-elt state '(:session :title))))))

(defun agent-shell-links--description (session)
  "Return a display description for SESSION."
  (let ((title (plist-get session :title)))
    (if (and title (not (string-empty-p title)))
        title
      (format "agent-shell session %s" (plist-get session :session-id)))))

(defun agent-shell-links--prevent-new-session-fallback (orig &rest args)
  "Call ORIG with ARGS unless this is a failed strict resume fallback."
  (let* ((shell-buffer (plist-get args :shell-buffer))
         (session-id (and (buffer-live-p shell-buffer)
                          (buffer-local-value
                           'agent-shell-links--strict-resume-session-id
                           shell-buffer))))
    (if (and session-id (not (string-empty-p session-id)))
        (progn
          (display-warning
           'agent-shell-links
           (format "Could not resume agent-shell session %s; not starting a new session"
                   session-id))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer shell-buffer)))
      (apply orig args))))

(defun agent-shell-links--ensure-strict-resume-advice ()
  "Install advice that prevents linked session resumes from falling back."
  (unless agent-shell-links--strict-resume-advice-installed
    (advice-add 'agent-shell--initiate-new-session
                :around #'agent-shell-links--prevent-new-session-fallback)
    (setq agent-shell-links--strict-resume-advice-installed t)))

(defun agent-shell-links--mark-strict-resume (buffer session-id)
  "Mark BUFFER as requiring a strict resume of SESSION-ID."
  (agent-shell-links--ensure-strict-resume-advice)
  (with-current-buffer buffer
    (setq-local agent-shell-links--strict-resume-session-id session-id)
    (agent-shell-subscribe-to
     :shell-buffer buffer
     :event 'init-finished
     :on-event
     (lambda (_event)
       (setq-local agent-shell-links--strict-resume-session-id nil)))))

;;;###autoload
(defun agent-shell-links-open-session (session-id &optional agent dir)
  "Open SESSION-ID in `agent-shell'.
AGENT is an optional agent identifier, as a symbol or string.  DIR is an
optional working directory.  If a live buffer already has the same
session id and agent identifier, display that buffer instead of
starting another shell."
  (let* ((identifier (cond ((symbolp agent) agent)
                           ((and (stringp agent)
                                 (not (string-empty-p agent)))
                            (intern-soft agent))))
         (dir (and (stringp dir) (not (string-empty-p dir)) dir))
         (buffer (and (or (null agent) identifier)
                      (agent-shell-links--buffer-for-session
                       session-id identifier))))
    (when (or (null session-id) (string-empty-p session-id))
      (user-error "Agent-shell link has no session id"))
    (if buffer
        (agent-shell-links--display-buffer buffer)
      (when (and dir (not (file-directory-p dir)))
        (user-error "Agent-shell session directory no longer exists: %s" dir))
      (let* ((config (or (agent-shell-links--config-for-identifier identifier)
                         (unless agent
                           (agent-shell--resolve-preferred-config))
                         (agent-shell-select-config :prompt "Resume with agent: ")
                         (error "No agent config found")))
             (default-directory (or dir default-directory)))
        (let ((buffer (agent-shell-start :config config :session-id session-id)))
          (agent-shell-links--mark-strict-resume buffer session-id)
          buffer)))))

;;; Bookmarks

;;;###autoload
(defun agent-shell-links-bookmark-enable ()
  "Make `bookmark-set' store the current `agent-shell' session in this buffer."
  (setq-local bookmark-make-record-function
              #'agent-shell-links-bookmark-make-record))

;;;###autoload
(defun agent-shell-links-bookmark-setup ()
  "Enable `bookmark-set' support in `agent-shell' buffers.
This adds `agent-shell-links-bookmark-enable' to
`agent-shell-mode-hook' and enables it in live `agent-shell' buffers."
  (interactive)
  (add-hook 'agent-shell-mode-hook #'agent-shell-links-bookmark-enable)
  (dolist (buffer (agent-shell-buffers))
    (with-current-buffer buffer
      (agent-shell-links-bookmark-enable))))

(defun agent-shell-links-bookmark-make-record ()
  "Return a bookmark record for the current `agent-shell' session."
  (let* ((session (or (agent-shell-links--current-session)
                      (user-error "No active agent-shell session")))
         (session-id (plist-get session :session-id))
         (identifier (plist-get session :identifier))
         (dir (plist-get session :dir))
         (description (agent-shell-links--description session)))
    (list description
          (cons 'handler #'agent-shell-links-bookmark-jump)
          (cons 'session-id session-id)
          (cons 'agent identifier)
          (cons 'filename dir)
          (cons 'location description))))

;;;###autoload
(defun agent-shell-links-bookmark-jump (bookmark)
  "Jump to an agent-shell BOOKMARK."
  (require 'bookmark)
  (let* ((filename (bookmark-get-filename bookmark))
         (dir (or filename (bookmark-prop-get bookmark 'dir))))
    (when (and dir (not (string-empty-p dir))
               (not (file-directory-p dir)))
      (user-error "Agent-shell bookmark directory no longer exists: %s" dir))
    (agent-shell-links-open-session
     (bookmark-prop-get bookmark 'session-id)
     (bookmark-prop-get bookmark 'agent)
     dir)))

;;; Org links

;;;###autoload
(defun agent-shell-links-org-store ()
  "Store an Org link to the current `agent-shell' session.
Returns nil when not in an `agent-shell' buffer with an active session,
so other store functions can still run."
  (when-let* ((session (agent-shell-links--current-session)))
    (let ((link (agent-shell-links--build
                 (plist-get session :session-id)
                 (plist-get session :identifier)
                 (plist-get session :dir))))
      (require 'ol)
      (org-link-store-props
       :type "agent-shell"
       :link (concat "agent-shell:" link)
       :description (agent-shell-links--description session))
      link)))

(defun agent-shell-links-org-follow (path &optional _arg)
  "Follow an `agent-shell' Org link described by PATH.
Resumes the stored session, resolving the agent by identifier and
binding `default-directory' to the stored directory."
  (pcase-let ((`(,session-id ,agent ,dir) (agent-shell-links--parse path)))
    (agent-shell-links-open-session session-id agent dir)))

(provide 'agent-shell-links)

;;; agent-shell-links.el ends here
