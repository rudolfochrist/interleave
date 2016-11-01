(require 'f)

(defvar interleave-support-path
  (f-dirname load-file-name))

(defvar interleave-features-path
  (f-parent interleave-support-path))

(defvar interleave-root-path
  (f-parent interleave-features-path))

(add-to-list 'load-path interleave-root-path)

(require 'interleave)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
