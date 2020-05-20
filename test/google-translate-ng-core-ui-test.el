;; (ert-deftest test-google-translate-ng-request-words-fixtures ()
;;   (dolist (file (f-files google-translate-ng-test/word-fixture-path))
;;     (let ((fixture (th-google-translate-ng-load-fixture file)))
;;       (th-google-translate-ng-request-fixture fixture)
;;       ;; assertions are skipped. In case of no errors assume that test pass.
;; )))

(ert-deftest test-google-translate-ng-language-abbreviation/English/en ()
  (should
   (string-equal
    (google-translate-ng-language-abbreviation "English")
    "en")))

(ert-deftest test-google-translate-ng-language-abbreviation/Detect-Language/auto ()
  (should
   (string-equal
    (google-translate-ng-language-abbreviation "Detect language")
    "auto")))

(ert-deftest test-google-translate-ng-language-display-name/auto/unspecified ()
  (should
   (string-equal
    (google-translate-ng-language-display-name "auto")
    "unspecified language")))

(ert-deftest test-google-translate-ng-language-display-name/en/English ()
  (should
   (string-equal
    (google-translate-ng-language-display-name "en")
    "English")))

(ert-deftest test-google-translate-ng--translation-title/source-auto/detected ()
  (should
   (string-equal
    "Translate from English (detected) to Russian:\n"
    (google-translate-ng--translation-title (make-gtos :source-language "auto"
                                                    :target-language "ru"
                                                    :auto-detected-language "en")
                                         "Translate from %s to %s:\n"))))

(ert-deftest test-google-translate-ng--translation-title/source-auto/detected-nil ()
  (should
   (string-equal
    "Translate from English to Russian:\n"
    (google-translate-ng--translation-title (make-gtos :source-language "en"
                                                    :target-language "ru"
                                                    :auto-detected-language nil)
                                         "Translate from %s to %s:\n"))))

(ert-deftest test-google-translate-ng--text-phonetic/do-not-show-phonetic ()
  (should
   (string-equal
    ""
    (google-translate-ng--text-phonetic
     (make-gtos :text-phonetic "phonetic") "%s"))))

(ert-deftest test-google-translate-ng--text-phonetic/show-phonetic-but-empty ()
  (setq google-translate-ng-show-phonetic t)
  (should
   (string-equal
    ""
    (google-translate-ng--text-phonetic (make-gtos :text-phonetic "") "%s")))
  (setq google-translate-ng-show-phonetic nil))

(ert-deftest test-google-translate-ng--text-phonetic/show-phonetic ()
  (setq google-translate-ng-show-phonetic t)
  (should
   (string-equal
    "phonetic"
    (google-translate-ng--text-phonetic
     (make-gtos :text-phonetic "phonetic") "%s")))
  (setq google-translate-ng-show-phonetic nil))

(ert-deftest test-google-translate-ng--translated-text ()
  (should
   (string-equal
    "translation"
    (google-translate-ng--translated-text
     (make-gtos :translation "translation") "%s"))))

(ert-deftest test-google-translate-ng--suggestion ()
  (should
   (string-equal
    "\nDid you mean: suggest\n"
    (google-translate-ng--suggestion (make-gtos
                                   :suggestion "suggest"
                                   :source-language "en"
                                   :target-language "ru")))))

(ert-deftest test-google-translate-ng--suggestion-action ()
  (with-temp-buffer
    (let* ((suggestion "suggestion")
           (source-language "en")
           (target-language "ru")
           (button (insert-text-button "Foo"
                                       'action 'test
                                       'suggestion suggestion
                                       'source-language source-language
                                       'target-language target-language)))
      (with-mock
       (mock (google-translate-ng-translate source-language
                                         target-language
                                         suggestion))
       (google-translate-ng--suggestion-action button)))))

(ert-deftest test-google-translate-ng--translation-phonetic/do-not-show-phonetic ()
  (should
   (string-equal
    ""
    (google-translate-ng--translation-phonetic
     (make-gtos :translation-phonetic "phonetic") "%s"))))

(ert-deftest test-google-translate-ng--translation-phonetic/show-phonetic-but-empty ()
  (setq google-translate-ng-show-phonetic t)
  (should
   (string-equal
    ""
    (google-translate-ng--translation-phonetic
     (make-gtos :translation-phonetic "") "%s")))
  (setq google-translate-ng-show-phonetic nil))

(ert-deftest test-google-translate-ng--translation-phonetic/show-phonetic ()
  (setq google-translate-ng-show-phonetic t)
  (should
   (string-equal
    "phonetic"
    (google-translate-ng--translation-phonetic
     (make-gtos :translation-phonetic "phonetic") "%s")))
  (setq google-translate-ng-show-phonetic nil))

(ert-deftest test-google-translate-ng-read-source-language/detect-language ()
  (with-mock
   (stub google-translate-ng-completing-read => "Detect language")
   (should
    (string-equal
     (google-translate-ng-read-source-language)
     "auto"))))

(ert-deftest test-google-translate-ng-read-source-language/english ()
  (with-mock
   (stub google-translate-ng-completing-read => "English")
   (should
    (string-equal
     (google-translate-ng-read-source-language)
     "en"))))
