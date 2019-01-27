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

(defvar chann-current-query nil)

(defvar chann-current-card nil)

(defvar chann-message-timer nil)

(org-link-set-parameters
 "mtg"
 :follow #'chann-scryfall-search
 :face 'chann-face
 :help-echo #'chann-help-echo)

(defun chann-link-path ()
  "Get the path of the link under point."
  (let ((elem (org-element-context)))
    (when (and (eq 'link (org-element-type elem))
               (string= "mtg" (org-element-property :type elem)))
      (org-element-property :path elem))))

(defun chann-scryfall-search (query)
  "Search for QUERY on Scryfall."
  (let ((card (chann-read-card query)))
    (browse-url (alist-get 'scryfall_uri card))))

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

(defun chann-describe-card (card)
  "Build a human-readable description of CARD."
  (concat (alist-get 'name card) " " (alist-get 'mana_cost card) "\n"
          (alist-get 'type_line card) "\n"
          (alist-get 'oracle_text card)))

(defun chann-link-message ()
  "Display card info for the `chann-mode' link under point."
  (interactive)
  (let ((query (chann-link-path)))
    (when query
      (let ((card (if (string= query chann-current-query)
                      chann-current-card
                    (setq chann-current-query query
                          chann-current-card (chann-read-card query)))))
        (display-message-or-buffer (chann-describe-card card) " *chann*")
        nil))))

(defun chann-help-echo (window object position)
  "Help echo for `chann-mode' links.
WINDOW, OBJECT, and POSITION as described in `org-link-parameters'"
  (ignore window)
  (save-excursion
    (with-current-buffer object
      (goto-char position)
      (chann-link-message))))

(defun chann-auto-link-messages ()
  "Automatically display card info for `chann-mode' links under point."
  (interactive)
  (unless chann-message-timer
    (setq chann-message-timer (run-with-idle-timer 0.5 t 'chann-link-message))))

(defun chann-stop-auto-link-messages ()
  "Stop automatically display card info for `chann-mode' links under point."
  (interactive)
  (cancel-timer chann-message-timer)
  (setq chann-message-timer nil))

;;;###autoload
(define-minor-mode chann-mode
  "Toggle org-mode Magic: The Gathering link support."
  :global t
  :lighter " MTG")

(defun chann-toggle-link-hook ()
  "Hook to manage automatically displaying card info for `chann-mode' links under point."
  (if chann-mode
      (chann-auto-link-messages)
    (chann-stop-auto-link-messages)))

(add-hook 'chann-mode-hook #'chann-toggle-link-hook)

(provide 'chann)
;;; chann.el ends here
