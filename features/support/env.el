(require 'f)

(defvar google-translate-ng-support-path
  (f-dirname load-file-name))

(defvar google-translate-ng-features-path
  (f-parent google-translate-ng-support-path))

(defvar google-translate-ng-root-path
  (f-parent google-translate-ng-features-path))

(add-to-list 'load-path google-translate-ng-root-path)

(require 'google-translate-ng
	 (f-expand "google-translate-ng"
		   google-translate-ng-root-path))
(require 'google-translate-ng-default-ui)
(require 'google-translate-ng-smooth-ui)
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
