;;; sie.el --- Scuttle Interface for Emacs

;; Copyright (C) 2012 Tom Willemsen <tom@ryuslash.org>

;; Author: Tom Willemsen <tom@ryuslash.org>
;; Created: Aug 07, 2012
;; Version: 0
;; Keywords: net

;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the
;; above copyright notice and this permission notice appear in all
;; copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
;; CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
;; OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
;; NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;; CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:

;; Coming soon...

;;; Code:
(require 'auth-source)
(require 'xml)

(defvar sie-scuttle-api-url "http://pegas/scuttle/api/"
  "Location of the Scuttle API.")

(defvar sie-user nil
  "Username to authenticate scuttle with.")

(defvar sie-password nil
  "Password to authenticate scuttle with.")

(defgroup sie nil
  "Scuttle Interface for Emacs."
  :group 'applications)

(defface sie-description
  '((t (:inherit shadow)))
  "Face for bookmark descriptions."
  :group 'sie)

(defmacro sie-get-prop (prop post)
  `(cdr (assq ,prop (cadr ,post))))

(defmacro sie-getset (var prompt &optional passwd)
  "Ask the user for, and then save, VAR with PROMPT.  Use
`read-passwd' if PASSWD is non-nil and `read-string' otherwise."
  `(or ,var (setq ,var (,(if passwd 'read-passwd 'read-string)
                        ,prompt))))

(defun sie-get-credentials ()
  "Search in `auth-sources' or ask the user for a username and
password to log in with."
  (let ((credentials (auth-source-search :max 1 :host sie-scuttle-api-url
                                         :type 'netrc
                                         :require '(:user :secret)
                                         :user sie-user)))
    (if credentials
        (setq sie-user (plist-get (car credentials) :user)
              sie-password (plist-get (car credentials) :secret))
      (sie-getset sie-user "Username: ")
      (sie-getset sie-password "Password: " t))))

(defun sie--password ()
  "Return the stored password.  If `sie-password' is a
function, return the result of that function, otherwise return it
as-is."
  (if (functionp sie-password)
      (funcall sie-password)
    sie-password))

(defun sie-credential-header ()
  "Generate the Authorization header to send along to Scuttle."
  (concat "Basic " (base64-encode-string
                    (concat sie-user ":" (sie--password)))))

(defun sie-send-command (command)
  "Send a request to sie for COMMAND."
  (let* ((url-request-extra-headers
          `(("Authorization" . ,(sie-credential-header))))
         (buffer (url-retrieve-synchronously
                  (url-encode-url
                   (concat sie-scuttle-api-url command))))
         response)
    (with-current-buffer buffer
      (goto-char (point-min))
      (search-forward "")
      (forward-line)                    ; Skip <xml declaration
      (setq response (xml-parse-region (point))))
    (kill-buffer buffer)
    response))

(define-derived-mode sie-list-mode special-mode "Sie"
  "Major mode for viewing bookmarks from scuttle.

\\{avandu-overview-map}
\\<avandu-overview-map>")

;;;###autoload
(defun sie-bookmark-list (tag)
  "Show a list of all the bookmarks collected."
  (interactive "MTag: ")
  (unless (and sie-user sie-password)
    (sie-get-credentials))

  (let ((buffer (get-buffer-create "*sie-bookmarks*"))
        (result (sie-send-command (concat "posts_all.php?tag=" tag))))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min))
      (mapc #'(lambda (elt)
                (unless (stringp elt)
                  (when (eq 'post (car elt))
                    (insert-button
                     (sie-get-prop 'description elt)
                     'link (sie-get-prop 'href elt)
                     'action #'(lambda (button)
                                 (browse-url (button-get button 'link))))
                    (insert-char ?\n)

                    (let ((pos (point)))
                      (insert-char ?\t)
                      (insert (propertize (sie-get-prop 'description elt)
                                          'face 'sie-description))
                      (fill-region pos (point)))

                    (insert-char ?\n)

                    (let ((pos (point)))
                      (insert-char ?\t)
                      (mapc #'(lambda (tag)
                                (insert-button
                                 tag
                                 'action #'(lambda (button)
                                             (sie-bookmark-list (button-label button))))
                                (insert-char ?\ ))
                            (split-string (sie-get-prop 'tag elt)))
                      (fill-region pos (point)))

                    (insert-char ?\n 2))))
            (cdr (assq 'posts result)))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (sie-list-mode))
    (switch-to-buffer buffer)))

(provide 'sie)

;;; sie.el ends here
;; (posts-update)
;; (posts-add url description &keyword extended tags date replace shared)
;; (posts-delete url)
;; (posts-get &keyword tags date url hashes meta)
;; (posts-recent &keyword tag count)
;; (posts-dates &optional tag)
;; (posts-all &keyword tags start results from-date to-date meta)
;; (tags-get)
;; (tags-rename old new)
