
(defvar *interleave--org-buf* nil "The Org Buffer")

(make-variable-buffer-local
 (defvar *interleave-page-marker* 0))

(defun interleave-find-pdf-path (buffer)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^#\\+interleave_pdf: \\(.*\\)")
      (when (match-string 0)
        (match-string 1)))))

(defun interleave-open-file (split-window)
  (let ((buf (current-buffer)))
    (condition-case nil
        (progn
          (funcall split-window)
          (find-file (expand-file-name (interleave-find-pdf-path buf)))
          (interleave-docview-mode 1))
      ('error (message "Please specify PDF file with #+INTERLEAVE_PDF document property.")
              (interleave-quit)))))

(defun interleave-go-to-page-note (page)
  (with-current-buffer *interleave--org-buf*
      (save-excursion
        (widen)
        (goto-char (point-min))
        (when (re-search-forward (format "^:interleave_page_note: %d$" page) nil t)
          (org-narrow-to-subtree)
          (org-show-entry)
          t))))

(defun interleave-create-new-note (page)
  (with-current-buffer *interleave--org-buf*
    (save-excursion
      (widen)
      (end-of-buffer)
      (org-insert-heading-respect-content)
      (insert (format "Notes for page %d" page))
      (org-set-property "interleave_page_note" (number-to-string page))
      (org-narrow-to-subtree)
      (other-window 1))))

(defun interleave-go-to-next-page ()
  (interactive)
  (doc-view-next-page)
  (interleave-go-to-page-note (doc-view-current-page)))

(defun interleave-go-to-previous-page ()
  (interactive)
  (doc-view-previous-page)
  (interleave-go-to-page-note (doc-view-current-page)))

(defun interleave-scroll-up ()
  (interactive)
  (setq *interleave-page-marker* (doc-view-current-page))
  (doc-view-scroll-up-or-next-page)
  (unless (= *interleave-page-marker* (doc-view-current-page))
    (interleave-go-to-page-note (doc-view-current-page))))

(defun interleave-scroll-down ()
  (interactive)
  (setq *interleave-page-marker* (doc-view-current-page))
  (doc-view-scroll-down-or-previous-page)
  (unless (= *interleave-page-marker* (doc-view-current-page))
    (interleave-go-to-page-note (doc-view-current-page))))

(defun interleave-add-note ()
  (interactive)
  (let ((page (doc-view-current-page)))
    (with-current-buffer *interleave--org-buf*
      (save-excursion
        (if (interleave-go-to-page-note page)
            (other-window 1)
          (interleave-create-new-note page))))))

(defun interleave-quit ()
  (interactive)
  (with-current-buffer *interleave--org-buf*
    (widen)
    (interleave-mode 0))
  (doc-view-kill-proc-and-buffer)
  (delete-window))

(define-minor-mode interleave-mode
  "Interleeaving your text books scince 2015"
  :lighter " Interleave"
  (interactive "P")
  (when interleave-mode
    (setq *interleave--org-buf* (current-buffer))
    (interleave-open-file (or (and current-prefix-arg 'split-window-below)
                              'split-window-right))))

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
