;;; sewyg.el --- Scuttle in Emacs is What You Get

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

(defvar sewyg-scuttle-api-url "http://pegas/scuttle/api/"
  "Location of the Scuttle API.")

(defvar sewyg-user nil
  "Username to authenticate scuttle with.")

(defvar sewyg-password nil
  "Password to authenticate scuttle with.")

(defgroup sewyg nil
  "Scuttle Interface for Emacs."
  :group 'applications)

(defface sewyg-description
  '((t (:inherit shadow)))
  "Face for bookmark descriptions."
  :group 'sewyg)

(defmacro sewyg-get-prop (prop post)
  `(cdr (assq ,prop (cadr ,post))))

(defmacro sewyg-getset (var prompt &optional passwd)
  "Ask the user for, and then save, VAR with PROMPT.  Use
`read-passwd' if PASSWD is non-nil and `read-string' otherwise."
  `(or ,var (setq ,var (,(if passwd 'read-passwd 'read-string)
                        ,prompt))))

(defun sewyg-get-credentials ()
  "Search in `auth-sources' or ask the user for a username and
password to log in with."
  (let ((credentials (auth-source-search :max 1 :host sewyg-scuttle-api-url
                                         :type 'netrc
                                         :require '(:user :secret)
                                         :user sewyg-user)))
    (if credentials
        (setq sewyg-user (plist-get (car credentials) :user)
              sewyg-password (plist-get (car credentials) :secret))
      (sewyg-getset sewyg-user "Username: ")
      (sewyg-getset sewyg-password "Password: " t))))

(defun sewyg--password ()
  "Return the stored password.  If `sewyg-password' is a
function, return the result of that function, otherwise return it
as-is."
  (if (functionp sewyg-password)
      (funcall sewyg-password)
    sewyg-password))

(defun sewyg-credential-header ()
  "Generate the Authorization header to send along to Scuttle."
  (concat "Basic " (base64-encode-string
                    (concat sewyg-user ":" (sewyg--password)))))

(defun sewyg-send-command (command)
  "Send a request to sewyg for COMMAND."
  (let* ((url-request-extra-headers
          `(("Authorization" . ,(sewyg-credential-header))))
         (buffer (url-retrieve-synchronously
                  (url-encode-url
                   (concat sewyg-scuttle-api-url command))))
         response)
    (with-current-buffer buffer
      (goto-char (point-min))
      (search-forward "")
      (forward-line)                    ; Skip <xml declaration
      (setq response (xml-parse-region (point))))
    (kill-buffer buffer)
    response))

(define-derived-mode sewyg-list-mode special-mode "Sewyg"
  "Major mode for viewing bookmarks from scuttle.

\\{avandu-overview-map}
\\<avandu-overview-map>")

;;;###autoload
(defun sewyg-bookmark-list (tag)
  "Show a list of all the bookmarks collected."
  (interactive "MTag: ")
  (unless (and sewyg-user sewyg-password)
    (sewyg-get-credentials))

  (let ((buffer (get-buffer-create "*sewyg-bookmarks*"))
        (result (sewyg-send-command (concat "posts_all.php?tag=" tag))))
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer)
      (goto-char (point-min))
      (mapc #'(lambda (elt)
                (unless (stringp elt)
                  (when (eq 'post (car elt))
                    (insert-button
                     (sewyg-get-prop 'description elt)
                     'link (sewyg-get-prop 'href elt)
                     'action #'(lambda (button)
                                 (browse-url (button-get button 'link))))
                    (insert-char ?\n)

                    (let ((pos (point)))
                      (insert-char ?\t)
                      (insert (propertize (sewyg-get-prop 'description elt)
                                          'face 'sewyg-description))
                      (fill-region pos (point)))

                    (insert-char ?\n)

                    (let ((pos (point)))
                      (insert-char ?\t)
                      (mapc #'(lambda (tag)
                                (insert-button
                                 tag
                                 'action #'(lambda (button)
                                             (sewyg-bookmark-list (button-label button))))
                                (insert-char ?\ ))
                            (split-string (sewyg-get-prop 'tag elt)))
                      (fill-region pos (point)))

                    (insert-char ?\n 2))))
            (cdr (assq 'posts result)))
      (setq buffer-read-only t)
      (goto-char (point-min))
      (sewyg-list-mode))
    (switch-to-buffer buffer)))

(provide 'sewyg)

;;; sewyg.el ends here
;; (posts-update)
;; (posts-add url description &keyword extended tags date replace shared)
;; (posts-delete url)
;; (posts-get &keyword tags date url hashes meta)
;; (posts-recent &keyword tag count)
;; (posts-dates &optional tag)
;; (posts-all &keyword tags start results from-date to-date meta)
;; (tags-get)
;; (tags-rename old new)
