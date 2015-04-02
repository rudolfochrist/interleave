;;; interleave.el --- Interleaving text books since 2015

;; Author: Sebastian Christ <rudolfo.christ@gmail.com>
;; URL: https://github.com/rudolfochrist/interleave
;; Package-Version: 20150402.206
;; Version: 0.2.2

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
;; Navigation is the same as in `doc-view-mode'/`pdf-view-mode'.

;;; Code:

(require 'org)

(require 'doc-view)
;; Redefining `doc-view-kill-proc-and-buffer' as `interleave--pdf-kill-proc-and-buffer'
;; because this function is obsolete in emacs 25.1 onwards.
(defun interleave--pdf-kill-proc-and-buffer ()
  "Kill the current converter process and buffer."
  (interactive)
  (when (derived-mode-p 'doc-view-mode)
    (doc-view-kill-proc))
  (when (or (derived-mode-p 'doc-view-mode)
            (derived-mode-p 'pdf-view-mode))
    (kill-buffer (current-buffer))))

(defvar *interleave--org-buffer* nil "Org notes buffer")
(defvar *interleave--pdf-buffer* nil "PDF buffer associated with the notes buffer")

(defvar interleave--window-configuration nil
  "Variable to store the window configuration before interleave mode was
enabled.")

(if (featurep 'pdf-view) ; if `pdf-tools' is installed
    (progn
      ;; Function wrapper for the macro `pdf-view-current-page'
      (defconst interleave--pdf-current-page-fn                 (lambda () (pdf-view-current-page)))
      (defconst interleave--pdf-next-page-fn                    #'pdf-view-next-page)
      (defconst interleave--pdf-previous-page-fn                #'pdf-view-previous-page)
      (defconst interleave--pdf-goto-page-fn                    #'pdf-view-goto-page)
      (defconst interleave--pdf-scroll-up-or-next-page-fn       #'pdf-view-scroll-up-or-next-page)
      (defconst interleave--pdf-scroll-down-or-previous-page-fn #'pdf-view-scroll-down-or-previous-page))
  (progn
    (defconst interleave--pdf-current-page-fn                 #'doc-view-current-page)
    (defconst interleave--pdf-next-page-fn                    #'doc-view-next-page)
    (defconst interleave--pdf-previous-page-fn                #'doc-view-previous-page)
    (defconst interleave--pdf-goto-page-fn                    #'doc-view-goto-page)
    (defconst interleave--pdf-scroll-up-or-next-page-fn       #'doc-view-scroll-up-or-next-page)
    (defconst interleave--pdf-scroll-down-or-previous-page-fn #'doc-view-scroll-down-or-previous-page)))

(make-variable-buffer-local
 (defvar *interleave--page-marker* 0
   "Caches the current page while scrolling"))

(defun interleave--find-pdf-path (buffer)
  "Searches for the 'interleave_pdf' property in BUFFER and extracts it when found"
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^#\\+interleave_pdf: \\(.*\\)")
      (when (match-string 0)
        (match-string 1)))))

(defun interleave--open-file (split-window)
  "Opens the interleave pdf file in `doc-view-mode'/`pdf-view-mode'  besides the
notes buffer.

SPLIT-WINDOW is a function that actually splits the window, so it must be either
`split-window-right' or `split-window-below'."
  (let ((buf (current-buffer)))
    (condition-case nil
        (progn
          (delete-other-windows)
          (funcall split-window)
          (find-file (expand-file-name (interleave--find-pdf-path buf)))
          (interleave-pdf-mode 1))
      ('error (message "Please specify PDF file with #+INTERLEAVE_PDF document property.")
              (interleave--quit)))))

(defun interleave--go-to-page-note (page)
  "Searches the notes buffer for an headline with the 'interleave_page_note' property set
to PAGE. It narrows the subtree when found."
  (with-current-buffer *interleave--org-buffer*
    (save-excursion
      (widen)
      (goto-char (point-min))
      (when (re-search-forward (format "^\[ \t\r\]*\:interleave_page_note\: %s$" page) nil t)
        (org-narrow-to-subtree)
        (org-show-entry)
        t))))

(defun interleave--go-to-next-page ()
  "Go to the next page in PDF. Look up for available notes."
  (interactive)
  (funcall interleave--pdf-next-page-fn)
  (interleave--go-to-page-note (funcall interleave--pdf-current-page-fn)))

(defun interleave--go-to-previous-page ()
  "Go to the previous page in PDF. Look up for available notes."
  (interactive)
  (funcall interleave--pdf-previous-page-fn)
  (interleave--go-to-page-note (funcall interleave--pdf-current-page-fn)))

(defun interleave--scroll-up ()
  "Scroll up the PDF. Look up for available notes."
  (interactive)
  (setq *interleave--page-marker* (funcall interleave--pdf-current-page-fn))
  (funcall interleave--pdf-scroll-up-or-next-page-fn)
  (unless (= *interleave--page-marker* (funcall interleave--pdf-current-page-fn))
    (interleave--go-to-page-note (funcall interleave--pdf-current-page-fn))))

(defun interleave--scroll-down ()
  "Scroll down the PDF. Look up for available notes."
  (interactive)
  (setq *interleave--page-marker* (funcall interleave--pdf-current-page-fn))
  (funcall interleave--pdf-scroll-down-or-previous-page-fn)
  (unless (= *interleave--page-marker* (funcall interleave--pdf-current-page-fn))
    (interleave--go-to-page-note (funcall interleave--pdf-current-page-fn))))

(defun interleave--switch-to-org-buffer (&optional insert-newline-maybe)
  (if (or (derived-mode-p 'doc-view-mode)
          (derived-mode-p 'pdf-view-mode))
      (switch-to-buffer-other-window *interleave--org-buffer*)
    (switch-to-buffer *interleave--org-buffer*))
  (when insert-newline-maybe
    (goto-char (point-max))
    (redisplay)
    ;; Insert a new line if not already on a new line
    (when (not (looking-back "^ *"))
      (org-return))))

(defun interleave--switch-to-pdf-buffer ()
  (if (derived-mode-p 'org-mode)
      (switch-to-buffer-other-window *interleave--pdf-buffer*)
    (switch-to-buffer *interleave--pdf-buffer*)))

(defun interleave--create-new-note (page)
  "Creates a new headline for the page PAGE."
  (with-current-buffer *interleave--org-buffer*
    (save-excursion
      (widen)
      (goto-char (point-max))
      (org-insert-heading-respect-content)
      (insert (format "Notes for page %d" page))
      (org-set-property "interleave_page_note" (number-to-string page))
      (org-narrow-to-subtree)))
  (interleave--switch-to-org-buffer t))

(defun interleave--add-note ()
  "Add note for the current page. If there are already notes for this page,
jump to the notes buffer."
  (interactive)
  (let ((page (funcall interleave--pdf-current-page-fn)))
    (if (interleave--go-to-page-note page)
        (interleave--switch-to-org-buffer t)
      (interleave--create-new-note page))))

(defun interleave--sync-pdf-page-current ()
  "Synchronize the page in the pdf buffer to be the same as the page in
the current narrowed down notes view."
  (interactive)
  (interleave--switch-to-org-buffer)
  (let ((pdf-page))
    (save-excursion
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (search-forward-regexp "Notes for page \\(.*\\)")
      (setq pdf-page (string-to-number (match-string 1))))
    (interleave--switch-to-pdf-buffer)
    (funcall interleave--pdf-goto-page-fn pdf-page)))

(defun interleave--sync-pdf-page-previous ()
  "Synchronize the page in the pdf buffer to be the same as the page in the
previous set of notes."
  (interactive)
  (interleave--switch-to-org-buffer)
  (let ((pdf-page))
    (save-excursion
      (widen)
      (org-backward-heading-same-level 1)
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (search-forward-regexp "Notes for page \\(.*\\)")
      (setq pdf-page (string-to-number (match-string 1))))
    (interleave--switch-to-pdf-buffer)
    (funcall interleave--pdf-goto-page-fn pdf-page)))

(defun interleave--sync-pdf-page-next ()
  "Synchronize the page in the pdf buffer to be the same as the page in the
next set of notes."
  (interactive)
  (interleave--switch-to-org-buffer)
  (let ((pdf-page))
    (save-excursion
      (widen)
      (org-forward-heading-same-level 1)
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (search-forward-regexp "Notes for page \\(.*\\)")
      (setq pdf-page (string-to-number (match-string 1))))
    (interleave--switch-to-pdf-buffer)
    (funcall interleave--pdf-goto-page-fn pdf-page)))

(defun interleave--quit ()
  "Quit interleave mode."
  (interactive)
  (with-current-buffer *interleave--org-buffer*
    (widen)
    (interleave 0))
  (interleave--pdf-kill-proc-and-buffer))

;;; Interleave
;; Minor mode for the org file buffer containing notes

(defvar interleave-map (make-sparse-keymap)
  "Keymap while `interleave' is active in the org file buffer.")

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
Navigation is the same as in `doc-view-mode'/`pdf-view-mode'."
  :lighter " ≡"
  :keymap  interleave-map
  (if interleave
      (progn
        (message "Interleave enabled")
        (setq interleave--window-configuration (current-window-configuration))
        (setq *interleave--org-buffer* (current-buffer))
        (interleave--open-file (or (and current-prefix-arg #'split-window-below)
                                   #'split-window-right)))
    (progn
      (message "Interleave disabled")
      (set-window-configuration interleave--window-configuration))))

;;; Interleave PDF Mode
;; Minor mode for the pdf file buffer associated with the notes

(defvar interleave-pdf-mode-map (make-sparse-keymap)
  "Keymap while `interleave-pdf-mode' is active in the pdf file buffer.")

;;;###autoload
(define-minor-mode interleave-pdf-mode
  "Interleave view for the pdf."
  :lighter " ≡"
  :keymap  interleave-pdf-mode-map
  (when interleave-pdf-mode
    (progn
      (setq *interleave--pdf-buffer* (current-buffer)))))

;;; Key-bindings

(define-key interleave-map (kbd "M-.") #'interleave--sync-pdf-page-current)
(define-key interleave-map (kbd "M-p") #'interleave--sync-pdf-page-previous)
(define-key interleave-map (kbd "M-n") #'interleave--sync-pdf-page-next)

(define-key interleave-pdf-mode-map (kbd "n")     #'interleave--go-to-next-page)
(define-key interleave-pdf-mode-map (kbd "p")     #'interleave--go-to-previous-page)
(define-key interleave-pdf-mode-map (kbd "SPC")   #'interleave--scroll-up)
(define-key interleave-pdf-mode-map (kbd "S-SPC") #'interleave--scroll-down)
(define-key interleave-pdf-mode-map (kbd "DEL")   #'interleave--scroll-down)
(define-key interleave-pdf-mode-map (kbd "i")     #'interleave--add-note)
(define-key interleave-pdf-mode-map (kbd "q")     #'interleave--quit)
(define-key interleave-pdf-mode-map (kbd "M-.")   #'interleave--sync-pdf-page-current)
(define-key interleave-pdf-mode-map (kbd "M-p")   #'interleave--sync-pdf-page-previous)
(define-key interleave-pdf-mode-map (kbd "M-n")   #'interleave--sync-pdf-page-next)


(provide 'interleave)

;;; interleave.el ends here
