;;; interleave-mode.el --- Interleaving text books since 2015

;; Author: Sebastian Christ <rudolfo.christ@gmail.com>
;; URL: https://github.com/rudolfochrist/interleave-mode
;; Version: 0.1

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
;; After setting the #+INTERLEAVE_PDF property in the header section of you org file to the PDF file
;; you'd like to use you can run M-x interleave-mode RET to start up interleave mode. This will
;; display your PDF and notes side by side.

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
        (when (re-search-forward (format "^:interleave_page_note: %d$" page) nil t)
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

(defun interleave-quit ()
  "Quit interleave mode."
  (interactive)
  (with-current-buffer *interleave--org-buf*
    (widen)
    (interleave-mode 0))
  (doc-view-kill-proc-and-buffer)
  (delete-window))

;;;###autoload
(define-minor-mode interleave-mode
  "Interleaving your text books since 2015."
  :lighter " Interleave"
  (when interleave-mode
    (setq *interleave--org-buf* (current-buffer))
    (interleave-open-file (or (and current-prefix-arg 'split-window-below)
                              'split-window-right))))
;;;###autoload
(define-minor-mode interleave-docview-mode
  "Interleave view for doc-view"
  :lighter " Interleave-DocView"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "n") 'interleave-go-to-next-page)
            (define-key map (kbd "p") 'interleave-go-to-previous-page)
            (define-key map (kbd "q") 'interleave-quit)
            (define-key map (kbd "i") 'interleave-add-note)
            (define-key map (kbd "SPC") 'interleave-scroll-up)
            (define-key map (kbd "S-SPC") 'interleave-scroll-down)
            (define-key map (kbd "DEL") 'interleave-scroll-down)
            map))


(provide 'interleave-mode)

;;; interleave-mode.el ends here
