(ert-deftest test-google-translate-ng--insert-nulls ()
  (should (string-equal
           (google-translate-ng--insert-nulls "[,[,[,,],,],,]")
           "[null,[null,[null,null,null],null,null],null,null]")))

;; (ert-deftest test-google-translate-ng-request-words-fixtures ()
;;   (dolist (file (f-files google-translate-ng-test/word-fixture-path))
;;     (let ((fixture (th-google-translate-ng-load-fixture file)))
;;       (th-google-translate-ng-request-fixture fixture)
;;       ;; assertions are skipped. In case of no errors assume that test pass.
;;       )))

;; (ert-deftest test-google-translate-ng-request-sentences-fixtures ()
;;   (dolist (file (f-files google-translate-ng-test/sentence-fixture-path))
;;     (let ((fixture (th-google-translate-ng-load-fixture file)))
;;       (th-google-translate-ng-request-fixture fixture)
;;       ;; assertions are skipped. In case of no errors assume that test pass.
;;       )))

(ert-deftest test-google-translate-ng--strip-string-with-spaces ()
  (should (string-equal
           (google-translate-ng--strip-string "    spaces     spaces ")
           " spaces spaces ")))

(ert-deftest test-google-translate-ng--strip-string-with-carriage-return-and-line-feeds ()
  (should (string-equal
           (google-translate-ng--strip-string "\n\n\r\nspaces\r\n\n\r\r\n\nspaces\r\n\r\r\r\n")
           " spaces spaces ")))

(ert-deftest test-google-translate-ng--trim-string-with-spaces ()
  (should (string-equal
           (google-translate-ng--trim-string "    spaces   spaces     ")
           "spaces   spaces")))

(ert-deftest test-google-translate-ng-prepare-text-for-request ()
  (should (string-equal
           (google-translate-ng-prepare-text-for-request "\n\r\nspaces\r   \n\n\rspaces\n\n\r")
           "spaces spaces")))

;; (ert-deftest test-google-translate-ng-json-text-phonetic ()
;;   (dolist (file (f-files google-translate-ng-test/word-fixture-path))
;;     (let ((fixture (th-google-translate-ng-load-fixture file)))
;;       (should (string-equal
;;                (th-google-translate-ng-fixture-text-phonetic fixture)
;;                (google-translate-ng-json-text-phonetic
;;                 (th-google-translate-ng-request-fixture fixture)))))))

;; (ert-deftest test-google-translate-ng-json-translation-phonetic ()
;;   (dolist (file (f-files google-translate-ng-test/word-fixture-path))
;;     (let ((fixture (th-google-translate-ng-load-fixture file)))
;;       (should (string-equal
;;                (th-google-translate-ng-fixture-translation-phonetic fixture)
;;                (google-translate-ng-json-translation-phonetic
;;                 (th-google-translate-ng-request-fixture fixture)))))))

;; (ert-deftest test-google-translate-ng-json-translation ()
;;   (dolist (file (f-files google-translate-ng-test/word-fixture-path))
;;     (let ((fixture (th-google-translate-ng-load-fixture file)))
;;       (should (string-equal
;;                (th-google-translate-ng-fixture-translation fixture)
;;                (google-translate-ng-json-translation
;;                 (th-google-translate-ng-request-fixture fixture)))))))

;; (ert-deftest test-google-translate-ng-json-suggestion ()
;;   (dolist (file (f-files google-translate-ng-test/word-fixture-path))
;;     (let ((fixture (th-google-translate-ng-load-fixture file)))
;;       (should (string-equal
;;                (th-google-translate-ng-fixture-suggestion fixture)
;;                (google-translate-ng-json-suggestion
;;                 (th-google-translate-ng-request-fixture fixture)))))))

;; (ert-deftest test-google-translate-ng-json-detailed-translation ()
;;   (dolist (file (f-files google-translate-ng-test/word-fixture-path))
;;     (let* ((fixture (th-google-translate-ng-load-fixture file))
;;            (index 0)
;;            (detailed-translation (google-translate-ng-json-detailed-translation
;;                                   (th-google-translate-ng-request-fixture fixture)))
;;            (fixture-dt (th-google-translate-ng-fixture-detailed-translation fixture))
;;            (detailed-translation-str "")
;;            (fixture-dt-str ""))
;;       (when fixture-dt
;;         (with-temp-buffer
;;           (th-google-translate-ng-detailed-translation-to-string detailed-translation)
;;           (setq detailed-translation-str (buffer-substring-no-properties (point-min) (point-max))))
;;         (setq fixture-dt-str (mapconcat (lambda (w) w) fixture-dt "\n"))
;;         (should (string-equal
;;                  detailed-translation-str
;;                  fixture-dt-str))))))

(ert-deftest test-google-translate-ng-request-empty-text ()
  (should (null
           (google-translate-ng-request "en" "ru" ""))))

(defvar test-example-query "client=t&ie=UTF-8&oe=UTF-8&sl=en&tl=ru&sc=2&text=first")

(defvar test-example-query-params '(("client" . "t")
                                    ("ie"     . "UTF-8")
                                    ("oe"     . "UTF-8")
                                    ("sl"     . "en")
                                    ("tl"     . "ru")
                                    ("sc"     . "2")
                                    ("text"   . "first")))

(ert-deftest test-google-translate-ng--format-query-string ()
  (should (string-equal
           test-example-query
           (google-translate-ng--format-query-string
            test-example-query-params))))

(ert-deftest test-google-translate-ng--format-request-url ()
  (should (string-equal
           (concat google-translate-ng-base-url "?" test-example-query)
           (google-translate-ng--format-request-url test-example-query-params))))
