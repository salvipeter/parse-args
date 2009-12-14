(in-package #:parse-args)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op 'fiveam)
  (use-package 'fiveam 'parse-args))

(def-suite basic-tests)
(in-suite basic-tests)

(defparameter *test-options*
  `((ein #\o "one")
    (due #\w "two" :optional)
    (san #\3 "three" :optional "333")
    (sei nil "four" :required)
    (wu3 #\g "five" :optional
	 ,(lambda (name x) (or (and x (parse-integer x)) 555)))
    (hat #\h nil :required
	 ,(lambda (name x) (concatenate 'string "arg = " x "!")))))

(test long
  (is (equal (multiple-value-list
	      (parse-args '("--one" "--two" "--three") *test-options*))
	     '(((ein) (due) (san . "333"))
	       ()
	       ()))))

(test short
  (is (equal (multiple-value-list
	      (parse-args '("-o" "-w" "-3") *test-options*))
	     '(((ein) (due) (san . "333"))
	       ()
	       ())))
  (is (equal (multiple-value-list
	      (parse-args '("-ow3") *test-options*))
	     '(((ein) (due) (san . "333"))
	       ()
	       ()))))

(test parameter
  (is (equal (multiple-value-list
	      (parse-args '("--three=3" "--four" "4") *test-options*))
	     '(((san . "3") (sei . "4"))
	       ()
	       ())))
  (is (equal (multiple-value-list
	      (parse-args '("--five" "48" "-h" "hat") *test-options*))
	     '(((wu3 . 48) (hat . "arg = hat!"))
	       ()
	       ())))
  (is (equal (multiple-value-list
	      (parse-args '("-g") *test-options*))
	     '(((wu3 . 555))
	       ()
	       ()))))

(test problems
  (is (equal (multiple-value-list
	      (parse-args '("--four" "--one" "--foo" "bar") *test-options*))
	     '(((ein))
	       ("--foo" "bar")
	       (sei))))
  (is (equal (multiple-value-list
	      (parse-args '("-one") *test-options*))
	     '(((ein))
	       ("-n" "-e")
	       ()))))
