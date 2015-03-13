;;; interleave.el --- Interleaving text books since 2015

;; Author: Sebastian Christ <rudolfo.christ@gmail.com>
;; URL: https://github.com/rudolfochrist/interleave
;; Version: 0.1.1

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; In the past, textbooks were sometimes published as /interleaved/ editions. That meant, each page
;; was followed by a blank page and the ambitious student/scholar had the ability to take his notes directly
;; in her copy of the textbook. Newton and Kant were prominent representatives of this technique.

;; Nowadays textbooks (or lecture material) come in PDF format. Although almost every PDF Reader has the ability to add some notes to the PDF itself, it is not as powerful as it could be.
;; This is what this minor mode tries to accomplish. It presents your PDF side by side to an [[http://orgmode.org][Org Mode]] buffer with you notes.
;; Narrowing down to just those passages that are relevant to this particular page in the document viewer.

;;; Usage:
;;
;; Create a Org file thath will keep your notes. In the Org headers section add
;; #+INTERLEAVE_PDF: /the/path/to/your/pdf.pdf
;;
;; Then start 'interleave' with
;; M-x interleave
;;
;; To insert a note for a page, type i.
;; Navigation is the same as in `doc-view-mode'.

;;; Code:

(require 'org)
(require 'doc-view)

(defvar *interleave--org-buf* nil "The Org Buffer")

(make-variable-buffer-local
 (defvar *interleave-page-marker* 0
   "Caches the current page while scrolling"))

(defun interleave-find-pdf-path (buffer)
  "Searches for the 'interleave_pdf' property in BUFFER and extracts it when found"
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^#\\+interleave_pdf: \\(.*\\)")
      (when (match-string 0)
        (match-string 1)))))

(defun interleave-open-file (split-window)
  "Opens the interleave pdf file in doc-view besides the notes buffer.

SPLIT-WINDOW is a function that actually splits the window, so it must be either
`split-window-right' or `split-window-below'."
  (let ((buf (current-buffer)))
    (condition-case nil
        (progn
          (funcall split-window)
          (find-file (expand-file-name (interleave-find-pdf-path buf)))
          (interleave-docview-mode 1))
      ('error (message "Please specify PDF file with #+INTERLEAVE_PDF document property.")
              (interleave-quit)))))

(defun interleave-go-to-page-note (page)
  "Searches the notes buffer for an headline with the 'interleave_page_note' property set
to PAGE. It narrows the subtree when found."
  (with-current-buffer *interleave--org-buf*
      (save-excursion
        (widen)
        (goto-char (point-min))
        (when (re-search-forward (format "^\[ \t\r\]*\:interleave_page_note\: %s$" page) nil t)
          (org-narrow-to-subtree)
          (org-show-entry)
          t))))

(defun interleave-create-new-note (page)
  "Creates a new headline for the page PAGE."
  (with-current-buffer *interleave--org-buf*
    (save-excursion
      (widen)
      (goto-char (point-max))
      (org-insert-heading-respect-content)
      (insert (format "Notes for page %d" page))
      (org-set-property "interleave_page_note" (number-to-string page))
      (org-narrow-to-subtree)
      (other-window 1))))

(defun interleave-go-to-next-page ()
  "Go to the next page in PDF. Look up for available notes."
  (interactive)
  (doc-view-next-page)
  (interleave-go-to-page-note (doc-view-current-page)))

(defun interleave-go-to-previous-page ()
  "Go to the previous page in PDF. Look up for available notes."
  (interactive)
  (doc-view-previous-page)
  (interleave-go-to-page-note (doc-view-current-page)))

(defun interleave-scroll-up ()
  "Scroll up the PDF. Look up for available notes."
  (interactive)
  (setq *interleave-page-marker* (doc-view-current-page))
  (doc-view-scroll-up-or-next-page)
  (unless (= *interleave-page-marker* (doc-view-current-page))
    (interleave-go-to-page-note (doc-view-current-page))))

(defun interleave-scroll-down ()
  "Scroll down the PDF. Look up for available notes."
  (interactive)
  (setq *interleave-page-marker* (doc-view-current-page))
  (doc-view-scroll-down-or-previous-page)
  (unless (= *interleave-page-marker* (doc-view-current-page))
    (interleave-go-to-page-note (doc-view-current-page))))

(defun interleave-add-note ()
  "Add note for the current page. If there are already notes for this page,
jump to the notes buffer."
  (interactive)
  (let ((page (doc-view-current-page)))
    (with-current-buffer *interleave--org-buf*
      (save-excursion
        (if (interleave-go-to-page-note page)
            (other-window 1)
          (interleave-create-new-note page))))))

(when (featurep 'pdf-view) ; if `pdf-tools' is installed

  (defun interleave-pdf-view-after-page ()
    "Go to the page's notes after the page has been changend with `pdf-tools'."
    (interleave-go-to-page-note (pdf-view-current-page)))

  (add-hook 'pdf-view-change-page-hook 'interleave-pdf-view-after-page)

  (defun interleave-add-note-pdf-view ()
    "Add note for the current page. If there are already notes for this page,
jump to the notes buffer. Applies if `pdf-tools' are installed."
    (interactive)
    (let ((page (pdf-view-current-page)))
      (with-current-buffer *interleave--org-buf*
        (save-excursion
          (if (interleave-go-to-page-note page)
              (other-window 1)
            (interleave-create-new-note page)))))))

(defun interleave-quit ()
  "Quit interleave mode."
  (interactive)
  (with-current-buffer *interleave--org-buf*
    (widen)
    (interleave 0))
  (doc-view-kill-proc-and-buffer)
  (delete-window))

;;;###autoload
(define-minor-mode interleave
  "Interleaving your text books since 2015.

In the past, textbooks were sometimes published as /interleaved/ editions. That meant, each page
was followed by a blank page and the ambitious student/scholar had the ability to take his notes directly
in her copy of the textbook. Newton and Kant were prominent representatives of this technique.

Nowadays textbooks (or lecture material) come in PDF format. Although almost every PDF Reader has the ability to add some notes to the PDF itself, it is not as powerful as it could be.
This is what this minor mode tries to accomplish. It presents your PDF side by side to an [[http://orgmode.org][Org Mode]] buffer with you notes.
Narrowing down to just those passages that are relevant to this particular page in the document viewer.

Usage:

Create a Org file thath will keep your notes. In the Org headers section add
#+INTERLEAVE_PDF: /the/path/to/your/pdf.pdf

Then start 'interleave' with
M-x interleave

To insert a note for a page, type 'i'.
Navigation is the same as in `doc-view-mode'."
  :lighter " Interleave"
  (when interleave
    (setq *interleave--org-buf* (current-buffer))
    (interleave-open-file (or (and current-prefix-arg 'split-window-below)
                              'split-window-right))))
;;;###autoload
(define-minor-mode interleave-docview-mode
  "Interleave view for doc-view"
  :lighter " Interleave-DocView"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") 'interleave-quit)
            (if (featurep 'pdf-view)
                (progn
                  (define-key map (kbd "i") 'interleave-add-note-pdf-view))
              (progn
                  (define-key map (kbd "n") 'interleave-go-to-next-page)
                  (define-key map (kbd "p") 'interleave-go-to-previous-page)
                  (define-key map (kbd "SPC") 'interleave-scroll-up)
                  (define-key map (kbd "S-SPC") 'interleave-scroll-down)
                  (define-key map (kbd "DEL") 'interleave-scroll-down)
                  (define-key map (kbd "i") 'interleave-add-note)))
            map))


(provide 'interleave)

;;; interleave.el ends here
