;;; agent-shell-org-link.el --- Org links that resume agent-shell sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Paul Nelson

;; Author: Paul Nelson
;; URL: https://github.com/xenodium/agent-shell
;; Package-Requires: ((emacs "29.1") (agent-shell "0.56.1"))

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; An Org link type that points into an agent's session store.  Run
;; `org-store-link' from inside an `agent-shell' buffer to capture a
;; link to the current session, then `org-insert-link' to drop it
;; wherever you like.  Following the link resumes that session with the
;; right agent in the right directory.
;;
;; The link is a small, stable pointer -- session id, agent identifier,
;; and working directory -- not a serialized snapshot.  If the session
;; has since been pruned agent-side, following it degrades to a fresh
;; session (handled by `agent-shell' itself) rather than erroring.
;;
;; Stored links look like:
;;
;;   [[agent-shell:SESSION-ID?agent=claude-code&dir=/path/to/project][Title]]
;;
;; Enable simply by loading this file; it registers the link type on
;; load.

;;; Code:

(require 'agent-shell)
(require 'ol)
(require 'map)
(require 'seq)
(require 'subr-x)

(defvar agent-shell-org-link--encode-chars
  '(?\s ?\t ?\n ?% ?& ?? ?= ?#)
  "Characters percent-encoded in link query values.
These are the characters that would otherwise confuse query
parsing or Org's own link reader.")

(defun agent-shell-org-link--build (session-id identifier dir)
  "Build an agent-shell link path from SESSION-ID, IDENTIFIER and DIR.
IDENTIFIER and DIR are optional and omitted from the result when nil."
  (let ((chars agent-shell-org-link--encode-chars)
        (params '()))
    (when identifier
      (push (format "agent=%s"
                    (org-link-encode (symbol-name identifier) chars))
            params))
    (when (and dir (not (string-empty-p dir)))
      (push (format "dir=%s"
                    (org-link-encode (expand-file-name dir) chars))
            params))
    (concat (org-link-encode session-id chars)
            (when params
              (concat "?" (string-join (nreverse params) "&"))))))

(defun agent-shell-org-link--parse (path)
  "Parse link PATH into a list of session id, agent id, and directory."
  (let* ((qpos (string-search "?" path))
         (session-id (org-link-decode (if qpos (substring path 0 qpos) path)))
         (query (and qpos (substring path (1+ qpos))))
         agent
         dir)
    (dolist (pair (and query (split-string query "&" t)))
      (let* ((eq (string-search "=" pair))
             (key (if eq (substring pair 0 eq) pair))
             (val (org-link-decode
                   (if eq (substring pair (1+ eq)) ""))))
        (pcase key
          ("agent" (setq agent val))
          ("dir" (setq dir val)))))
    (list session-id agent dir)))

(defun agent-shell-org-link--config-for-identifier (identifier)
  "Return the agent config whose :identifier matches IDENTIFIER (a symbol).
Return nil when none matches."
  (when identifier
    (seq-find (lambda (config)
                (eq (map-elt config :identifier) identifier))
              agent-shell-agent-configs)))

(defun agent-shell-org-link--buffer-for-session (session-id &optional identifier)
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

(defun agent-shell-org-link--display-buffer (buffer)
  "Display agent-shell BUFFER, respecting viewport preference."
  (if agent-shell-prefer-viewport-interaction
      (agent-shell-viewport--show-buffer :shell-buffer buffer)
    (agent-shell--display-buffer buffer)))

;;;###autoload
(defun agent-shell-org-link-store ()
  "Store an Org link to the current `agent-shell' session.
Intended for `org-store-link' via `org-link-set-parameters'.
Returns nil when not in an `agent-shell' buffer with an active
session, so other store functions can still run."
  (when (derived-mode-p 'agent-shell-mode)
    (when-let* ((state agent-shell--state)
                (session-id (map-nested-elt state '(:session :id)))
                ((not (string-empty-p session-id))))
      (let* ((identifier (map-elt (map-elt state :agent-config) :identifier))
             (dir (ignore-errors (agent-shell-cwd)))
             (title (map-nested-elt state '(:session :title)))
             (link (agent-shell-org-link--build
                    session-id identifier dir)))
        (org-link-store-props
         :type "agent-shell"
         :link (concat "agent-shell:" link)
         :description (if (and title (not (string-empty-p title)))
                          title
                        (format "agent-shell session %s" session-id)))
        link))))

(defun agent-shell-org-link-follow (path &optional _arg)
  "Follow an `agent-shell' Org link described by PATH.
Resumes the stored session, resolving the agent by identifier and
binding `default-directory' to the stored directory when it still
exists."
  (pcase-let* ((`(,session-id ,agent ,dir) (agent-shell-org-link--parse path))
               (identifier (and agent (intern-soft agent)))
               (buffer (and (or (null agent) identifier)
                            (agent-shell-org-link--buffer-for-session
                             session-id identifier))))
    (when (or (null session-id) (string-empty-p session-id))
      (user-error "Agent-shell link has no session id"))
    (if buffer
        (agent-shell-org-link--display-buffer buffer)
      (let* ((config (or (agent-shell-org-link--config-for-identifier identifier)
                         (unless agent
                           (agent-shell--resolve-preferred-config))
                         (agent-shell-select-config :prompt "Resume with agent: ")
                         (error "No agent config found")))
             (default-directory (if (and dir (file-directory-p dir))
                                    dir
                                  default-directory)))
        (agent-shell-start :config config :session-id session-id)))))

(org-link-set-parameters "agent-shell"
                         :follow #'agent-shell-org-link-follow
                         :store #'agent-shell-org-link-store)

(provide 'agent-shell-org-link)

;;; agent-shell-org-link.el ends here
