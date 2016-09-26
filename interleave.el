;;; interleave.el --- Interleaving text books since 2015

;; Author: Sebastian Christ <rudolfo.christ@gmail.com>
;; URL: https://github.com/rudolfochrist/interleave
;; Version: 1.2.0

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

;; In the past, textbooks were sometimes published as 'interleaved' editions.
;; That meant, each page was followed by a blank page and the ambitious student/
;; scholar had the ability to take their notes directly in their copy of the
;; textbook. Newton and Kant were prominent representatives of this technique.

;; Nowadays textbooks (or lecture material) come in PDF format. Although almost
;; every PDF Reader has the ability to add some notes to the PDF itself, it is
;; not as powerful as it could be.

;; This is what this minor mode tries to accomplish. It presents your PDF side by
;; side to an [[http://orgmode.org][Org Mode]] buffer with your notes, narrowing
;; down to just those passages that are relevant to the particular page in the
;; document viewer.

;;; Usage:

;; - Create a Org file that will keep your notes. In the Org headers section, add
;; #+INTERLEAVE_PDF: /the/path/to/your/pdf.pdf
;; - Start `interleave' with `M-x interleave'.
;; - To insert a note for a page, type `i'.
;; - Navigation is the same as in `doc-view-mode'/`pdf-view-mode'."

;;; Code:

(require 'org)
(require 'org-element)

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

(defcustom interleave-org-notes-dir-list '("~/org/interleave_notes" ".")
  "List of directories to look into when opening interleave notes org from a
pdf file. The notes file is assumed to have the exact same base name as the pdf
file (just that the file extension is .org instead of .pdf).

If the notes org file is not found, it is created in the directory returned on
doing `car' of this list (first element of the list).

The notes file is searched in order from the first list element till the last;
the search is aborted once the file is found.

If a list element is \".\" or begins with \"./\", that portion is replaced with
the pdf directory name. e.g. \".\" is interpreted as \"/pdf/file/dir/\",
\"./notes\" is interpreted as \"/pdf/file/dir/notes/\"."
  :type '(repeat directory)
  :group 'interleave)

(defvar interleave-org-buffer nil
  "Org notes buffer name.")

(defvar interleave-pdf-buffer nil
  "Name of PDF buffer associated with `interleave-org-buffer'.")

(defvar interleave--window-configuration nil
  "Variable to store the window configuration before interleave mode was enabled.")

(defvar interleave--pdf-current-page-fn (lambda () (doc-view-current-page))
  "Function to call to display the current PDF page.")
(defvar interleave--pdf-next-page-fn #'doc-view-next-page
  "Function to call to display the next PDF page.")
(defvar interleave--pdf-previous-page-fn #'doc-view-previous-page
  "Function to call to display the previous PDF page.")
(defvar interleave--pdf-goto-page-fn #'doc-view-goto-page
  "Function to call to jump to a given PDF page.")
(defvar interleave--pdf-scroll-up-or-next-page-fn #'doc-view-scroll-up-or-next-page
  "Function to call for line/page scrolling in upward direction." )
(defvar interleave--pdf-scroll-down-or-previous-page-fn #'doc-view-scroll-down-or-previous-page
  "Function to call for line/page scrolling in downward direction.")

(defcustom interleave-sort-order 'asc
  "Specifiy the notes' sort order in the notes buffer.

The possible values are 'asc for ascending and 'desc for descending."
  :type '(choice (const  asc)
                 (const  desc))
  :group 'interleave)

(eval-after-load 'pdf-view ; if/when `pdf-tools' is loaded
  '(progn
     ;; Function wrapper for the macro `pdf-view-current-page'
     (setq interleave--pdf-current-page-fn (lambda () (pdf-view-current-page)))
     (setq interleave--pdf-next-page-fn #'pdf-view-next-page)
     (setq interleave--pdf-previous-page-fn #'pdf-view-previous-page)
     (setq interleave--pdf-goto-page-fn #'pdf-view-goto-page)
     (setq interleave--pdf-scroll-up-or-next-page-fn #'pdf-view-scroll-up-or-next-page)
     (setq interleave--pdf-scroll-down-or-previous-page-fn #'pdf-view-scroll-down-or-previous-page)))

(make-variable-buffer-local
 (defvar *interleave--page-marker* 0
   "Caches the current page while scrolling"))

(make-variable-buffer-local
 (defvar *interleave--multi-pdf-notes-file* nil
   "Indicates if the current Org notes file is a multi-pdf notes file."))

(defun interleave--find-pdf-path (buffer)
  "Searches for the `interleave_pdf' property in BUFFER and extracts it when found."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^#\\+interleave_pdf: \\(.*\\)" nil :noerror)
          (match-string 1))))))

(defun interleave--headline-pdf-path (buffer)
  (with-current-buffer buffer
    (save-excursion
      (let ((headline (org-element-at-point)))
        (when (and (equal (org-element-type headline) 'headline)
                   (equal (org-element-property :level headline) 1)
                   (org-entry-get nil "interleave_pdf"))
          (setq *interleave--multi-pdf-notes-file* t)
          (org-entry-get nil "interleave_pdf"))))))

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
          (find-file (expand-file-name (or (interleave--headline-pdf-path buf)
                                           (interleave--find-pdf-path buf)))))
      ('error
       (let ((pdf-file-name
              (read-file-name "No #+INTERLEAVE_PDF property found. Please specify path: " "~/")))
         (find-file (expand-file-name pdf-file-name))
         (with-current-buffer buf
           (insert "#+INTERLEAVE_PDF: " pdf-file-name)))))
    (interleave-pdf-mode 1)))

(defun interleave--goto-parent-headline ()
  "Search the tree for the outermost parent headline."
  (let ((headline (org-element-at-point)))
    (unless (equal (org-element-type headline) 'headline)
      (outline-up-heading 1)
      (setq headline (org-element-at-point)))
    (ignore-errors
      (outline-up-heading (1- (org-element-property :level headline))))))

(defun interleave--goto-search-position ()
  "Move point to the search start position.

For multi-pdf notes this is the outermost parent headline. For everything else
this is the beginning of the buffer."
  (if *interleave--multi-pdf-notes-file*
      (interleave--goto-parent-headline)
    (goto-char (point-min))))

(defun interleave--go-to-page-note (page)
  "Searches the notes buffer for an headline with the `interleave_page_note'
property set to PAGE. It narrows the subtree when found."
  (with-current-buffer interleave-org-buffer
    (save-excursion
      (widen)
      (interleave--goto-search-position)
      (when *interleave--multi-pdf-notes-file*
        ;; only search the current subtree for notes. See. Issue #16
        (org-narrow-to-subtree))
      (when (re-search-forward (format "^\[ \t\r\]*\:interleave_page_note\: %s$"
                                       page)
                               nil t)
        (org-narrow-to-subtree)
        (org-show-entry)
        t))))

(defun interleave-go-to-next-page ()
  "Go to the next page in PDF. Look up for available notes."
  (interactive)
  (funcall interleave--pdf-next-page-fn)
  (interleave--go-to-page-note (funcall interleave--pdf-current-page-fn)))

(defun interleave-go-to-previous-page ()
  "Go to the previous page in PDF. Look up for available notes."
  (interactive)
  (funcall interleave--pdf-previous-page-fn)
  (interleave--go-to-page-note (funcall interleave--pdf-current-page-fn)))

(defun interleave-scroll-up ()
  "Scroll up the PDF. Look up for available notes."
  (interactive)
  (setq *interleave--page-marker* (funcall interleave--pdf-current-page-fn))
  (funcall interleave--pdf-scroll-up-or-next-page-fn)
  (unless (= *interleave--page-marker* (funcall interleave--pdf-current-page-fn))
    (interleave--go-to-page-note (funcall interleave--pdf-current-page-fn))))

(defun interleave-scroll-down ()
  "Scroll down the PDF. Look up for available notes."
  (interactive)
  (setq *interleave--page-marker* (funcall interleave--pdf-current-page-fn))
  (funcall interleave--pdf-scroll-down-or-previous-page-fn)
  (unless (= *interleave--page-marker* (funcall interleave--pdf-current-page-fn))
    (interleave--go-to-page-note (funcall interleave--pdf-current-page-fn))))

(defun interleave--switch-to-org-buffer (&optional insert-newline-maybe)
  (if (or (derived-mode-p 'doc-view-mode)
          (derived-mode-p 'pdf-view-mode))
      (switch-to-buffer-other-window interleave-org-buffer)
    (switch-to-buffer interleave-org-buffer))
  (when insert-newline-maybe
    (goto-char (point-max))
    (redisplay)
    ;; Insert a new line if not already on a new line
    (when (not (looking-back "^ *" (line-beginning-position)))
      (org-return))))

(defun interleave--switch-to-pdf-buffer ()
  "Switch to the pdf buffer."
  (if (derived-mode-p 'org-mode)
      (switch-to-buffer-other-window interleave-pdf-buffer)
    (switch-to-buffer interleave-pdf-buffer)))

(defun interleave--goto-insert-position ()
  "Move the point to the right insert postion.

For multi-pdf notes this is the end of the subtree. For everything else
this is the end of the buffer"
  (if (not *interleave--multi-pdf-notes-file*)
      (goto-char (point-max))
    (interleave--goto-parent-headline)
    (org-end-of-subtree)))

(defun interleave--insert-heading-respect-content ()
  (org-insert-heading-respect-content)
  (let ((new-heading (org-element-at-point)))
    (when (and *interleave--multi-pdf-notes-file*
               (< (org-element-property :level new-heading) 2))
      (org-demote))))

(defun interleave--create-new-note (page)
  "Creates a new headline for the page PAGE."
  (with-current-buffer interleave-org-buffer
    (save-excursion
      (widen)
      (interleave--goto-insert-position)
      (interleave--insert-heading-respect-content)
      (insert (format "Notes for page %d" page))
      (org-set-property "interleave_page_note" (number-to-string page))
      (org-narrow-to-subtree)))
  (interleave--switch-to-org-buffer t))

(defun interleave-add-note ()
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
  (let (pdf-page)
    (save-excursion
      (when *interleave--multi-pdf-notes-file*
        (interleave--goto-search-position))
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (re-search-forward "^ *:interleave_page_note: *\\(.*\\)")
      (setq pdf-page (string-to-number (match-string 1))))
    (when (and (integerp pdf-page)
               (> pdf-page 0)) ; The page number needs to be a positive integer
      (interleave--switch-to-pdf-buffer)
      (funcall interleave--pdf-goto-page-fn pdf-page))))

(defun interleave--sync-pdf-page-previous ()
  "Synchronize the page in the pdf buffer to be the same as the page in the
previous set of notes."
  (interactive)
  (interleave--switch-to-org-buffer)
  (let (pdf-page)
    (save-excursion
      (when *interleave--multi-pdf-notes-file*
        (interleave--goto-search-position))
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (widen)
      (when *interleave--multi-pdf-notes-file*
        (save-excursion (interleave--goto-search-position)
                        (org-narrow-to-subtree)))
      (when (re-search-backward "^ *:interleave_page_note: *\\(.*\\)" nil :noerror)
        (setq pdf-page (string-to-number (match-string 1)))))
    (if (and (integerp pdf-page)
             (> pdf-page 0)) ; The page number needs to be a positive integer
        (progn
          (interleave--go-to-page-note pdf-page)
          (interleave--switch-to-pdf-buffer)
          (funcall interleave--pdf-goto-page-fn pdf-page))
      (org-narrow-to-subtree))))

(defun interleave--sync-pdf-page-next ()
  "Synchronize the page in the pdf buffer to be the same as the page in the
next set of notes."
  (interactive)
  (interleave--switch-to-org-buffer)
  (let (pdf-page)
    (save-excursion
      (when *interleave--multi-pdf-notes-file*
        (interleave--goto-search-position))
      (org-narrow-to-subtree)
      (goto-char (point-min))
      (re-search-forward "^ *:interleave_page_note:") ; current page
      (widen)
      (when *interleave--multi-pdf-notes-file*
        (save-excursion (interleave--goto-search-position)
                        (org-narrow-to-subtree)))
      (when (re-search-forward "^ *:interleave_page_note: *\\(.*\\)" nil :noerror) ; next page
        (setq pdf-page (string-to-number (match-string 1)))))
    (if (and (integerp pdf-page)
             (> pdf-page 0)) ; The page number needs to be a positive integer
        (progn
          (interleave--go-to-page-note pdf-page)
          (interleave--switch-to-pdf-buffer)
          (funcall interleave--pdf-goto-page-fn pdf-page))
      (org-narrow-to-subtree))))

;;;###autoload
(defun interleave--open-notes-file-for-pdf ()
  "Open the notes org file for the current pdf file if it exists.
Else create it.

It is assumed that the notes org file will have the exact same base name
as the pdf file (just that the notes file will have a .org extension instead
of .pdf)."
  (interactive)
  (when (or (derived-mode-p 'doc-view-mode)
            (derived-mode-p 'pdf-view-mode))
    (let* ((pdf-file-name (buffer-file-name))
           (org-file-name-sans-directory (concat (file-name-base pdf-file-name)
                                                 ".org"))
           org-file-create-dir
           (cnt 0)
           try-org-file-name
           (org-file-name (catch 'break
                            (dolist (dir interleave-org-notes-dir-list)
                              ;; If dir is "." or begins with "./", replace
                              ;; the "." or "./" with the pdf dir name
                              (setq dir (replace-regexp-in-string
                                         "^\\(\\.$\\|\\./\\).*"
                                         (file-name-directory pdf-file-name)
                                         dir nil nil 1))
                              (when (= cnt 0)
                                ;; In the event the org file is needed to be
                                ;; created, it will be created in the directory
                                ;; listed as the first element in
                                ;; `interleave-org-notes-dir-list'
                                (setq org-file-create-dir dir))
                              (setq cnt (1+ cnt))
                              (setq try-org-file-name (locate-file
                                                       org-file-name-sans-directory
                                                       (list dir)))
                              (when try-org-file-name
                                ;; return the first match
                                (throw 'break try-org-file-name))))))
      ;; Create the notes org file if it does not exist
      (when (null org-file-name)
        (setq org-file-name (if (null interleave-org-notes-dir-list)
                                (read-file-name "Path: " "~/")
                              (progn
                                (when (null (file-exists-p org-file-create-dir))
                                  (make-directory org-file-create-dir))
                                (expand-file-name org-file-name-sans-directory
                                                  org-file-create-dir))))
        (with-temp-file org-file-name
          (insert "#+INTERLEAVE_PDF: " pdf-file-name)))
      ;; Open the notes org file and enable `interleave'
      (find-file org-file-name)
      (interleave))))

(defun interleave--quit ()
  "Quit interleave mode."
  (interactive)
  (with-current-buffer interleave-org-buffer
    (widen)
    (interleave--goto-search-position)
    (when (interleave--headlines-available-p)
      (interleave--sort-notes interleave-sort-order)
      (org-overview))
    (interleave 0))
  (interleave--pdf-kill-proc-and-buffer))

(defun interleave--headlines-available-p ()
  (save-excursion
    (re-search-forward "^\* .*" nil t)))

(defun interleave--sort-notes (sort-order)
  "Sort notes by interleave_page_property.

SORT-ORDER is either 'asc or 'desc."
  (org-sort-entries nil ?f
                    (lambda ()
                      (or (string-to-number
                           (org-entry-get nil
                                          "interleave_page_note"))
                          -1))
                    (if (eq sort-order 'asc)
                        #'<
                      #'>)))

;;; Interleave
;; Minor mode for the org file buffer containing notes

(defvar interleave-map (make-sparse-keymap)
  "Keymap while `interleave' is active in the org file buffer.")

;;;###autoload
(define-minor-mode interleave
  "Interleaving your text books since 2015.

In the past, textbooks were sometimes published as 'interleaved' editions.
That meant, each page was followed by a blank page and the ambitious student/
scholar had the ability to take their notes directly in their copy of the
textbook. Newton and Kant were prominent representatives of this technique.

Nowadays textbooks (or lecture material) come in PDF format. Although almost
every PDF Reader has the ability to add some notes to the PDF itself, it is
not as powerful as it could be.

This is what this minor mode tries to accomplish. It presents your PDF side by
side to an [[http://orgmode.org][Org Mode]] buffer with your notes, narrowing
down to just those passages that are relevant to the particular page in the
document viewer.

Usage:

- Create a Org file that will keep your notes. In the Org headers section, add
#+INTERLEAVE_PDF: /the/path/to/your/pdf.pdf
- Start `interleave' with `M-x interleave'.
- To insert a note for a page, type `i'.
- Navigation is the same as in `doc-view-mode'/`pdf-view-mode'.

Keybindings (`doc-view-mode'/`pdf-view-mode'):

\\{interleave-pdf-mode-map}

Keybindings (org-mode buffer):

\\{interleave-map}"
  :lighter " ≡"
  :keymap  interleave-map
  (if interleave
      (progn
        (message "Interleave enabled")
        (setq interleave--window-configuration (current-window-configuration))
        (setq interleave-org-buffer (buffer-name))
        (interleave--open-file (or (and current-prefix-arg #'split-window-below)
                                   #'split-window-right))
        (interleave--go-to-page-note 1))
    (progn
      ;; Disable the corresponding minor mode in the PDF file too.
      (when (get-buffer interleave-pdf-buffer)
        (interleave--switch-to-pdf-buffer)
        (interleave-pdf-mode -1))
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
      (setq interleave-pdf-buffer (buffer-name)))))

;;; Key-bindings

(define-key interleave-map (kbd "M-.") #'interleave--sync-pdf-page-current)
(define-key interleave-map (kbd "M-p") #'interleave--sync-pdf-page-previous)
(define-key interleave-map (kbd "M-n") #'interleave--sync-pdf-page-next)

(define-key interleave-pdf-mode-map (kbd "n")     #'interleave-go-to-next-page)
(define-key interleave-pdf-mode-map (kbd "p")     #'interleave-go-to-previous-page)
(define-key interleave-pdf-mode-map (kbd "SPC")   #'interleave-scroll-up)
(define-key interleave-pdf-mode-map (kbd "S-SPC") #'interleave-scroll-down)
(define-key interleave-pdf-mode-map (kbd "DEL")   #'interleave-scroll-down)
(define-key interleave-pdf-mode-map (kbd "i")     #'interleave-add-note)
(define-key interleave-pdf-mode-map (kbd "q")     #'interleave--quit)
(define-key interleave-pdf-mode-map (kbd "M-.")   #'interleave--sync-pdf-page-current)
(define-key interleave-pdf-mode-map (kbd "M-p")   #'interleave--sync-pdf-page-previous)
(define-key interleave-pdf-mode-map (kbd "M-n")   #'interleave--sync-pdf-page-next)

(define-key doc-view-mode-map (kbd "i") #'interleave--open-notes-file-for-pdf)
(when (featurep 'pdf-view)
  (define-key pdf-view-mode-map (kbd "i") #'interleave--open-notes-file-for-pdf))


(provide 'interleave)

;;; interleave.el ends here
