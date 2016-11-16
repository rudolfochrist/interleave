;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(And "^debug$"
     (lambda ()
       (print (buffer-name))))

(Given "^I open the file \"\\([^\"]+\\)\"$"
       (lambda (name)
         (find-file (concat interleave-features-path "/" name))))

(Then "^I should see buffer \"\\([^\"]+\\)\"$"
      (lambda (name)
        (not (null (cl-remove-if-not (lambda (buffer)
                                       (string= name (buffer-name buffer)))
                                     (buffer-list))))))
(When "^I start interleave-mode$"
      (lambda ()
        (When "I start an action chain")
        (When "I press \"M-x\"")
        (When "I type \"interleave-mode\"")
        (When "I execute the action chain")))

(When "^print buffer name$"
      (lambda ()
        (print (buffer-file-name))))

(When "^I quit interleave-mode$"
      (lambda ()
        (interleave--quit)))
