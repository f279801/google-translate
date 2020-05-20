(ert-deftest test-google-translate-ng--gen-rl ()
  (should (equal
           (google-translate-ng--gen-rl 403419 "+-a^+6") 415696276.0)))

(ert-deftest test-google-translate-ng--gen-tk ()
  (let ((text "This buffer is for notes you don't want to save, and for Lisp evaluation."))
    (should
     (equal (google-translate-ng--gen-tk text '(406401 299565202.0)) "751893.871060"))))
