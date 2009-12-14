(in-package #:cl-user)

(defpackage #:parse-args
  (:use #:common-lisp)
  (:export #:parse-args))

(in-package #:parse-args)

(defvar *options*)
(defvar *parsed*)
(defvar *skipped*)
(defvar *missing*)
(defvar *args*)

(defun next-arg ()
  (setf *args* (rest *args*)))

(defun short-option-p (str)
  (and (>= (length str) 2)
       (char= (char str 0) #\-)
       (char/= (char str 1) #\-)))

(defun long-option-p (str)
  (and (>= (length str) 3)
       (char= (char str 0) #\-)
       (char= (char str 1) #\-)))

(defun handle-option (option)
  (let ((name (first option))
	(type (fourth option))
	(handler (fifth option))
	(next (second *args*)))
    (case type
      (:required (if (and next (not (or (short-option-p next)
					(long-option-p next))))
		     (progn
		       (if handler
			   (push (cons name (funcall handler name next))
				 *parsed*)
			 (push (cons name next) *parsed*))
		       (next-arg))
		   (push name *missing*)))
      (:optional (if (and next (not (or (short-option-p next)
					(long-option-p next))))
		     (progn
		       (if (functionp handler)
			   (push (cons name (funcall handler name next))
				 *parsed*)
			 (push (cons name next) *parsed*))
		       (next-arg))
		   (if (functionp handler)
		       (push (cons name (funcall handler name nil)) *parsed*)
		     (push (cons name handler) *parsed*))))
      (t (push (cons name nil) *parsed*)))))

(defun handle-short (c)
  (let ((option (member c *options* :test #'equal :key #'second)))
    (if option
	(handle-option (first option))
      (push (concatenate 'string "-" (string c)) *skipped*))))

(defun handle-long (str)
  (let ((option (member (subseq str 2) *options* :test #'equal :key #'third)))
    (if option
	(handle-option (first option))
      (push str *skipped*))))

(defun handle-shorts (str)
  (loop for c across (subseq str 1) do
	(handle-short c)))

(defun unequalize (lst)
  "For every element of the pattern --[^=]+=.* create two elements."
  (when lst
    (if (cl-ppcre:scan "^(--[^=]+)=(.*)$" (first lst))
	(destructuring-bind (switch parameter)
	    (cl-ppcre:split "=" (first lst) :limit 2)
	  (cons switch (cons parameter (unequalize (rest lst)))))
      (cons (first lst) (unequalize (rest lst))))))

(defun parse-args (args options)
  "ARGS is a list of strings, for example \(\"--some-switch\" \"arg\").
OPTIONS is a list of options. Every option is given as a list
containing \(name short long [type [handler]]).

Valid types are: :optional and :required.
As an abbreviation, instead of a handler function you can give a single value,
for switches with optional arguments, and it will behave as a default value.

There are three return values:
- the first contains a list of (name . value) pairs, where value is the
  value returned by the handler function, or NIL if ther was no handler
- the second is a list of strings that were not parsed
- the third is a list of names where the switch was present, but the
  required argument was missing

Handler functions are called with the switch name symbol and the argument.
For optional arguments this will be NIL if the user has not supplied it.

BUGS: May not work well for parameters starting with dashes.

Also see the unit tests for examples."
  (let ((*parsed* nil)
	(*skipped* nil)
	(*missing* nil)
	(*args* (unequalize args))
	(*options* options))
    (loop for arg = (first *args*) while *args* do
	  (cond ((short-option-p arg) (handle-shorts arg))
		((long-option-p arg) (handle-long arg))
		(t (push arg *skipped*)))
	  (next-arg))
    (values (nreverse *parsed*) (nreverse *skipped*) (nreverse *missing*))))
