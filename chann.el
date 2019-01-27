;;; chann.el --- Magic: The Gathering links in org-mode -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:
(require 'org)

(defgroup chann-faces nil
  "Faces for `chann'."
  :group 'faces)

(defface chann-face
  '((t :inherit org-link :foreground "green"))
  "Face used to highlight the current selection.")

(org-link-set-parameters "mtg" :follow 'chann-scryfall-search :face 'chann-face)

(defun chann-scryfall-search (query)
  "Search for QUERY on Scryfall."
  (browse-url (concat "https://scryfall.com/search?q=" query)))

(provide 'chann)
;;; chann.el ends here
