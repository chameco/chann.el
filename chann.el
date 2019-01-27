;;; chann.el --- Magic: The Gathering links in org-mode -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'org)
(require 'json)
(require 'seq)

(defgroup chann-faces nil
  "Faces for `chann'."
  :group 'faces)

(defface chann-face
  '((t :inherit org-link :foreground "green"))
  "Face used to highlight the current selection.")

(defvar chann-directory
  (file-name-as-directory (expand-file-name ".chann" user-emacs-directory)))

(defvar chann-current-help-echo-query nil)

(defvar chann-current-help-echo-card nil)

(org-link-set-parameters
 "mtg"
 :follow 'chann-scryfall-search
 :face 'chann-face
 :help-echo 'chann-help-echo)

(defun chann-link-path ()
  "Get the path of the link under point."
  (org-element-property :path (org-element-context)))

(defun chann-scryfall-search (query)
  "Search for QUERY on Scryfall."
  (browse-url (concat "https://scryfall.com/search?q=" query)))

(defun chann-scryfall-api-search (query)
  "Search for QUERY on the Scryfall API."
  (let* ((buf (url-retrieve-synchronously
              (concat "https://api.scryfall.com/cards/named?fuzzy=" query)
              t t))
         (obj (with-current-buffer buf (json-read))))
    (kill-buffer buf)
    obj))

(defun chann-read-card (query)
  "Read QUERY from Scryfall or local cache."
  (mkdir chann-directory t)
  (let* ((file (concat (md5 query) ".el"))
         (path (expand-file-name file chann-directory)))
    (if (file-readable-p path)
        (with-temp-buffer
          (insert-file-contents path)
          (let ((inhibit-message t)) (message "reading"))
          (read (current-buffer)))
      (let ((card (chann-scryfall-api-search query)))
        (with-temp-file path
          (let ((print-length nil))
            (print card (current-buffer))))
        card))))

(defun chann-help-echo (window object position)
  "Help echo for `chann' links.
WINDOW, OBJECT, and POSITION as described in `org-link-parameters'"
  (ignore window)
  (save-excursion
    (with-current-buffer object
      (goto-char position)
      (let* ((query (chann-link-path))
             (card (if (string= query chann-current-help-echo-query)
                       chann-current-help-echo-card
                     (setq chann-current-help-echo-query query
                           chann-current-help-echo-card (chann-read-card query)))))
        (display-message-or-buffer (alist-get 'oracle_text card) " *chann*")
        nil))))

(print (chann-scryfall-api-search "Ponder"))
(alist-get 'oracle_text (chann-read-card "Ponder"))

(provide 'chann)
;;; chann.el ends here
