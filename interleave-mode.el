
(make-variable-buffer-local
 (defvar *interleave-org-buf* nil "The Org Buffer"))

(defun interleave-find-pdf-path (buffer)
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^#\\+interleave_pdf: \\(.*\\)")
      (when (match-string 0)
        (match-string 1)))))

(defun interleave-open-file ()
  (let ((buf (current-buffer)))
    (split-window-right)
    (find-file (interleave-find-pdf-path buf))
    (interleave-docview-mode 1)))

(defun interleave-go-to-next-page ()
  (interactive)
  (doc-view-next-page))

(defun interleave-go-to-previous-page ()
  (interactive)
  (doc-view-previous-page))

(defun interleave-quit ()
  (interactive)
  (with-current-buffer *interleave-org-buf*
    (interleave-mode 0))
  (doc-view-kill-proc-and-buffer)
  (delete-window))

(define-minor-mode interleave-mode
  "Interleeaving your text books scince 2015"
  :lighter " Interleave"
  (when interleave-mode
    (setq *interleave-org-buf* (current-buffer))
    (interleave-open-file)))

(define-minor-mode interleave-docview-mode
  "Interleave view for doc-view"
  :lighter " Interleave-DocView"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "n") 'interleave-go-to-next-page)
            (define-key map (kbd "p") 'interleave-go-to-previous-page)
            (define-key map (kbd "q") 'interleave-quit)
            map))


(provide 'interleave-mode)
