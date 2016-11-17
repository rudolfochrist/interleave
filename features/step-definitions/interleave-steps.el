;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(And "^debug$"
     (lambda ()
       (print (buffer-name))))

(Then "^sleep \"\\([^\"]+\\)\"$"
      (lambda (arg)
        (sleep-for (string-to-number arg))))

(Given "^I open the file \"\\([^\"]+\\)\"$"
       (lambda (name)
         (find-file (concat interleave-features-path "/" name))))

(Then "^I should see buffer \"\\([^\"]+\\)\"$"
      (lambda (name)
        (cl-assert (not (null (cl-remove-if-not (lambda (buffer)
                                                  (string= name (buffer-name buffer)))
                                                (buffer-list))))
                   nil
                   "Buffer %s not found" name)))
(When "^I start interleave-mode$"
      (lambda ()
        (When "I start an action chain")
        (When "I press \"M-x\"")
        (When "I type \"interleave-mode\"")
        (When "I execute the action chain")))

(When "^I quit interleave-mode$"
      (lambda ()
        (interleave--quit)))

(Then "^the current page should be \"\\([^\"]+\\)\"$"
      (lambda (arg)
        (cl-assert (= (string-to-number arg)
                      (funcall interleave-pdf-current-page-fn))
                   nil
                   "%s is not equal to %s"
                   arg
                   (funcall interleave-pdf-current-page-fn))))

(And "^I widen the buffer$"
     (lambda ()
       (widen)))
