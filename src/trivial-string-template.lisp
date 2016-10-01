;;;; trivial-string-template.lisp
;;;; Inspired by Python's string.Template
(in-package :cl-user)
(defpackage #:trivial-string-template
  (:use #:cl #:proc-parse #+allegro #:util.string)
  #+allegro (:import-from #:excl #:if*)
  (:shadow #:substitute)
  (:export #:*delimiter*
           #:*variable-pattern*
           #:*tokenizer*
           #:make-tokenizer #:tokenizer-regex #:tokenizer-separators
           #:add-separator #:add-separators #:delete-separator #:delete-separators
           #:substitute
           #:safe-substitute
           #:template
           #:source-string
           #:delimiter
           #:variable-pattern
           #:template-tokenizer
           #:safe
           #:reset-template
           #:define-template)
  (:nicknames #:string.template #:st))
(in-package #:trivial-string-template)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Global Constants and Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(alexandria:define-constant +empty-string+ ""
  :test 'string= :documentation "Just an empty string.")

(alexandria:define-constant +left-brace+ #\{
  :test 'char= :documentation "The left brace character.")

(alexandria:define-constant +right-brace+ #\}
  :test 'char= :documentation "The right brace character.")

(alexandria:define-constant
    +python-pattern+ "[_a-z][_a-z0-9]*"
  :test 'string=
  :documentation "Regular expression pattern for a valid python identifier.")

(alexandria:define-constant
    +lisp-common-pattern+ "([A-Za-z][A-Za-z0-9]*)(-[A-Za-z0-9]+)*"
  :test 'string=
  :documentation "Regular expression pattern for a regular Common Lisp identifier,
e.g. `variable-information', `define-symbol-macro', etc.")

(defvar *delimiter* #\$ "Default delimiter character.")

(defvar *variable-pattern* +lisp-common-pattern+ "Default variable pattern")

(defvar %segments%
  (make-array 0 :adjustable t :fill-pointer 0
              :element-type '(or cons symbol simple-string))
  "A vector that stores segments information for a string template,
e.g. \"$who likes $me.\" => #((:VARIABLE \"who\") \" likes \" (:VARIABLE \"me\") \".\");
It will be reused over and over again.")

(defvar %variables%
    (make-array 0 :adjustable t :fill-pointer 0
                :element-type 'simple-string)
  "A vector that stores variables names for a string template,
e.g. \"$who likes $me.\" => #(\"who\" \"me\");
It will be reused over and over again.")

(define-symbol-macro .to-symbol.
    ;; this symbol macro will be used when the current Lisp is using Modern Case, e.g. Allegro's `mlisp';
    ;; see http://franz.com/support/documentation/10.0/doc/case.htm#modern-mode-1 for details.
    (case (readtable-case *readtable*)
      (:preserve 'intern)
      (t (lambda (str) (intern (string-upcase str))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass %tokenizer% ()
  ((regex :initarg :regex :accessor tokenizer-regex :type simple-string)
   (compiled-re)
   (separators :initarg :separators :accessor tokenizer-separators :type list))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "A class used for managing the way of how to recognize separators during parsing a template string."))

(defun make-tokenizer-function (compiled-re separators)
  "Given a compiled regular expression, and a list of separators characters, produce a predicating function that
when given a character, it tells whether it's a separator."
  (lambda (char)
    (or 
     (block %separators%
       (dolist (sep separators t)
         (when (not (char= char sep))
           (return-from %separators% nil))))
     (not #+allegro (regexp:match-re compiled-re (string char))
          #-allegro (cl-ppcre:scan-to-strings compiled-re (string char))))))

(defun make-tokenizer
    (&key (regex "[a-zA-Z0-9-]") (separators (list #\Space #\Newline #\Tab)))
  "Construction function for `%tokenizer%' class."
  (make-instance '%tokenizer% :regex regex :separators separators))

(defmethod initialize-instance :after ((this %tokenizer%) &key)
  (with-slots (regex compiled-re separators) this
    (setf compiled-re
          #+allegro (regexp:compile-re regex)
          #-allegro (cl-ppcre:create-scanner regex))
    (closer-mop:set-funcallable-instance-function
     this (make-tokenizer-function compiled-re separators))))

(defvar *tokenizer* (make-tokenizer)
  "A default tokenizer will be used during parsing the template strings.")

(defmethod (setf tokenizer-regex) :before ((new string) (this %tokenizer%))
  (with-slots (regex) this
    (warn "The regular expression of the TOKENIZER will be modified from ~A to ~A"
          regex new)))

(defmethod (setf tokenizer-regex) :after ((new string) (this %tokenizer%))
  (with-slots (regex compiled-re separators) this
    (setf compiled-re
          #+allegro (regexp:compile-re regex)
          #-allegro (cl-ppcre:create-scanner regex))
    (closer-mop:set-funcallable-instance-function
     this (make-tokenizer-function compiled-re separators))))

(defmethod add-separator ((tokenizer %tokenizer%) separator)
  (with-slots (compiled-re separators) tokenizer
    (pushnew separator separators :test 'char=)
    (closer-mop:set-funcallable-instance-function
     tokenizer (make-tokenizer-function compiled-re separators))
    tokenizer))

(defmethod add-separators ((tokenizer %tokenizer%) &rest separators)
  (let ((len (length separators)))
    (cond ((zerop len)
           (progn (warn "No seprators were given.") nil))
          ((= len 1) (add-separator tokenizer (first separators)))
          (t (with-slots (compiled-re) tokenizer
               (symbol-macrolet ((%separators%
                                  (slot-value tokenizer 'separators)))
                 (dolist (sep separators)
                   (pushnew sep %separators% :test 'char=))
                 (closer-mop:set-funcallable-instance-function
                  tokenizer (make-tokenizer-function compiled-re %separators%)))
               tokenizer)))))

(defmethod delete-separator ((tokenizer %tokenizer%) separator)
  (with-slots (compiled-re separators) tokenizer
    (if (find separator separators :test 'char=)
        (progn
          (setf separators (delete separator separators :test 'char=))
          (closer-mop:set-funcallable-instance-function
           tokenizer (make-tokenizer-function compiled-re separators))
          tokenizer)
        (progn
          (let ((*print-length* 5))
            (warn "~A is not in the separators list: ~A" separator separators)
            tokenizer)))))

(defmethod delete-separators ((tokenizer %tokenizer%) &rest separators)
  (let ((len (length separators))
        (*print-length* 5))
    (cond ((zerop len)
           (progn (warn "No separators were given.") nil))
          ((= len 1) (delete-separator tokenizer (first separators)))
          (t (with-slots (compiled-re) tokenizer
               (symbol-macrolet ((%separators%
                                  (slot-value tokenizer 'separators)))
                 (dolist (sep separators)
                   (unless (find sep %separators% :test 'char=)
                     (warn "~A is not in the separators list: ~A" sep %separators%)))
                 (setf %separators%
                       (delete-if (lambda (sep)
                                    (find sep separators :test 'char=))
                                  %separators%))
                 (closer-mop:set-funcallable-instance-function
                  tokenizer (make-tokenizer-function compiled-re %separators%)))
               tokenizer)))))

(defun variable-pattern-matched-p (variable-name variable-pattern)
  "Validate whether a variable's name is matched by the pattern."
  (declare (type simple-string variable-name))
  #+allegro
  (multiple-value-bind (matched-p matched)
      (regexp:match-re variable-pattern variable-name)
    (and matched-p (string= matched variable-name)))
  #-allegro
  (let ((matched (cl-ppcre:scan-to-strings variable-pattern variable-name)))
    (and (string/= +empty-string+ matched) (string= matched variable-name))))

(define-compiler-macro variable-pattern-matched-p
    (&whole form &environment env variable-name variable-pattern)
  (cond ((constantp variable-pattern env)
         `(variable-pattern-matched-p ,variable-name
                                      #+allegro (load-time-value (regexp:compile-re ,variable-pattern))
                                      #-allegro (load-time-value (cl-ppcre:create-scanner ,variable-pattern))))
        (t form)))

#+allegro
(defun collect-string+-arguments (segments variables args &key (safe nil))
  "Prepare arguments for the `string+' function."
  (loop
     with variable-index = 0 and var = nil
     for seg across segments
     collect
       (if* (keywordp seg)
            :then
            (setf var (aref variables variable-index))
            (incf variable-index)
            (let ((val (getf args seg nil)))
              (if* val
                   :then val
                   :elseif safe
                   :then (string+ *delimiter* var)
                   :else (error "Missing variable ~A~A information." *delimiter* var)))
            :else seg)))

#-allegro
(defun collect-format-arguments (args variables &key (safe nil))
  "Prepare arguments for the `format' function."
  (declare (type list args)
           (type (array simple-string (*)) variables))
  (loop for var across variables
     collect (let ((value (getf args (intern (case (readtable-case *readtable*)
                                               (:preserve var)
                                               (t (string-upcase var)))
                                             :keyword)
                                (when safe (concatenate 'string (string *delimiter*) var)))))
               (if value value
                   (error "Missing variable ~A~A information." *delimiter* var)))))

(declaim (inline make-template-datum)
         (ftype (function ((array (or cons simple-string) (*))) simple-string)))
(defun make-template-datum (segments)
  "Given the segments vector, produce the control string for `format' function."
  (apply 'concatenate 'string
         (map 'list (lambda (s)
                      (cond ((consp s) "~A")
                            ((stringp s) s)))
              segments)))

(declaim (inline append-supplied-p))
(defun append-supplied-p (name)
  "Depending on the `(readtable-case *readtable*)', when given a `name' string,
prefix it with \"-SUPPLIED-P\" or \"-supplied-p\"."
  (concatenate 'string name
               (case (readtable-case *readtable*)
                 (:preserve "-supplied-p")
                 (t "-SUPPLIED-P"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Core Functions and APIs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declaim (ftype (function (simple-string character simple-string %tokenizer%)
                          (values (array (or cons symbol simple-string) (*)) (array simple-string (*))))
                parse-template))
(defun parse-template (template delimiter variable-pattern &optional (tokenizer *tokenizer*))
  "The essential parsing function."
  (declare (simple-string template variable-pattern)
           (character delimiter)
           (optimize speed (space 0) (safety 0) (debug 0) (compilation-speed 0)))
  (let ((segments %segments%)
        (variables %variables%))
    (declare (type (array (or simple-string cons) (*)) segments)
             (type (array simple-string (*)) variables))
    (setf (fill-pointer segments) 0
          (fill-pointer variables) 0)
    (when (string= template +empty-string+)
      (return-from parse-template
        (values (make-template-datum #()) #())))
    (labels ((tokenize (char)
               (declare (type character char))
               (funcall tokenizer char))
             (collect-variable (variable)
               (declare (type simple-string variable))
               (if (variable-pattern-matched-p variable variable-pattern)
                   (progn
                     (vector-push-extend variable variables)
                     (vector-push-extend (list :variable variable) segments))
                   (error "~A is not a valid identifier." variable))))
      (with-string-parsing (template)
        (tagbody
         meet-next-delimiter
           (bind (str (skip-until (lambda (c) (char= c delimiter))))
             (declare (type simple-string str))
             (cond ((eofp)
                    (vector-push-extend str segments)
                    (go result))
                   ((string= str +empty-string+)
                    (go parse-variable))
                   (t (vector-push-extend str segments)
                      (go parse-variable))))
         parse-variable
           (progn (advance*)
                  (let ((current (current)))
                    (declare (type character current))
                    (cond ((char= current delimiter) ;; escape
                           (vector-push-extend (string delimiter) segments)
                           (advance*)
                           (go meet-next-delimiter))
                          ((char= current +left-brace+) ;; brace wrapped placeholder
                           (advance*)
                           (bind (var (skip-until (lambda (c) (char= c +right-brace+))))
                             (declare (type simple-string var))
                             (unless (char= (current) +right-brace+)
                               (error "Mismatched braces."))
                             (collect-variable var)
                             (advance*)
                             (if (eofp)
                                 (go result)
                                 (go meet-next-delimiter))))
                          (t (bind (var (skip-until #'tokenize))
                               (declare (type simple-string var))                               
                               (collect-variable var)
                               (if (eofp)
                                   (go result)
                                   (progn
                                     (go meet-next-delimiter))))))))
         result
           #+allegro
           (return-from parse-template
             (values
              ;; segments
              (loop for i from 0 to (1- (fill-pointer segments))
                do (let ((seg (aref segments i)))
                     (when (consp seg)
                       (setf (aref segments i) (intern (case (readtable-case *readtable*)
                                                         (:preserve (second seg))
                                                         (t (string-upcase (second seg))))
                                                       :keyword))))
                 finally (return segments))
              ;; variables
              (subseq variables 0 (fill-pointer variables))))
           #-allegro
           (return-from parse-template
             (values (make-template-datum segments) ;; datum
                     (subseq variables 0 (fill-pointer variables))))))))) ;; variables

(defun substitute (template &rest args &key &allow-other-keys)
  "Given a template string and the variables(as keywords) it requires, return the formatted string;
if there's missing variable information, it will issue an error."
  #+allegro
  (multiple-value-bind (segments variables)
      (parse-template template *delimiter* *variable-pattern* *tokenizer*)
    (apply 'string+ (collect-string+-arguments segments variables args)))
  #-allegro
  (multiple-value-bind (datum variables)
      (parse-template template *delimiter* *variable-pattern* *tokenizer*)
    (apply 'format nil datum (collect-format-arguments args variables))))

(define-compiler-macro substitute
    (&whole form &environment env template &rest args &key &allow-other-keys)
  (cond ((constantp template env)
         #+allegro
         (multiple-value-bind (segments variables)
             (parse-template template *delimiter* *variable-pattern* *tokenizer*)
           `(string+ ,@(collect-string+-arguments segments variables args)))
         #-allegro
         (multiple-value-bind (datum variables)
             (parse-template template *delimiter* *variable-pattern* *tokenizer*)
           `(format nil ,datum
                    ,@(collect-format-arguments args variables))))
        (t form)))

(defun safe-substitute (template &rest args &key &allow-other-keys)
  "Given a template string and the variables(as keywords) it requires, return the formatted string;
compared to `substitute', this function won't issue an error when there's missing variable information but
uses the variable's name as a default value."
  #+allegro
  (multiple-value-bind (segments variables)
      (parse-template template *delimiter* *variable-pattern* *tokenizer*)
    (apply 'string+ (collect-string+-arguments segments variables args :safe t)))
  #-allegro
  (multiple-value-bind (datum variables)
      (parse-template template *delimiter* *variable-pattern* *tokenizer*)
    (apply 'format nil datum (collect-format-arguments args variables :safe t))))

(define-compiler-macro safe-substitute
    (&whole form &environment env template &rest args &key &allow-other-keys)
  (cond ((constantp template env)
         #+allegro
         (multiple-value-bind (segments variables)
             (parse-template template *delimiter* *variable-pattern* *tokenizer*)
           `(string+ ,@(collect-string+-arguments segments variables args :safe t)))
         #-allegro
         (multiple-value-bind (datum variables)
             (parse-template template *delimiter* *variable-pattern* *tokenizer*)
           `(format nil ,datum
                    ,@(collect-format-arguments args variables :safe t))))
        (t form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;; Class Definitions and APIs ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass template ()
  (;; "This is the literal string describing a placeholder introducing delimiter.
   ;; The default value is #\$."
   ;; -- Python Docs (https://docs.python.org/2/library/string.html?highlight=string.template#string.Template)
   (delimiter :initarg :delimiter :accessor delimiter :type character)
   ;; "This is the regular expression describing the pattern for non-braced placeholders
   ;; (the braces will be added automatically as appropriate)."
   ;; The default value is the regular expression '([A-Za-z][A-Za-z0-9]*)(-[A-Za-z][A-Za-z0-9]*)*'(+lisp-common-pattern+).
   ;; -- Python Docs (https://docs.python.org/2/library/string.html?highlight=string.template#string.Template)
   (variable-pattern :initarg :variable-pattern :accessor variable-pattern :type function)
   ;; The source string.
   (source :initarg :template-source :accessor source-string :type simple-string)
   (safe :initform nil :initarg :safe :accessor safe :type symbol)
   (tokenizer :initarg :tokenizer :accessor template-tokenizer :type %tokenizer%)
   #+allegro (segments :initarg :template-segments :type (array (or symbol simple-string) (*)))
   #-allegro (datum :initarg :template-datum  :type simple-string)
   (variables :initarg :template-variables  :type (array simple-string (*))))
  (:metaclass closer-mop:funcallable-standard-class)
  (:documentation "The definition of Template class. Unlike `substitute' and `safe-substitute', it is a high-level API."))

(defun template
    (template &key (delimiter *delimiter*) (variable-pattern *variable-pattern*) (safe nil) (tokenizer *tokenizer*))
  "Constructor method for template class."
  (declare (type simple-string template))
  (when (string= template +empty-string+)
    (warn "You are using an empty string to initialize the template.")
    (return-from template
      (make-instance 'template :safe safe
                     :template-source "" :delimiter delimiter
                     :variable-pattern
                     #+allegro (regexp:compile-re variable-pattern)
                     #-allegro (cl-ppcre:create-scanner variable-pattern))))
  #+allegro
  (multiple-value-bind (segments variables)
      (parse-template template delimiter variable-pattern tokenizer)
    (make-instance 'template :template-source template :safe safe :tokenizer tokenizer
                   :delimiter delimiter
                   :variable-pattern (regexp:compile-re variable-pattern)
                   :template-segments segments :template-variables variables))
  #-allegro
  (multiple-value-bind (datum variables)
      (parse-template template delimiter variable-pattern tokenizer)
    (make-instance 'template :template-source template :safe safe :tokenizer tokenizer
                   :delimiter delimiter
                   :variable-pattern
                   (cl-ppcre:create-scanner variable-pattern)
                   :template-datum datum :template-variables variables)))

(defmacro define-template
    (var (&key (delimiter *delimiter*) (variable-pattern *variable-pattern*) (safe nil) (tokenizer *tokenizer*)) template)
  "Given the template string and thoses parameters, set the function definition of the symbol `var'."
  (let ((obj (gensym)))
    `(let ((,obj (template ,template
                           :delimiter ,delimiter
                           :variable-pattern ,variable-pattern
                           :tokenizer ,tokenizer
                           :safe ,safe)))
       (handler-bind ((warning #'muffle-warning))
         (setf (symbol-function ',var) ,obj)))))

(defmethod initialize-instance :after ((this template) &key)
  (with-slots (delimiter #+allegro segments #-allegro datum variables safe) this
    (closer-mop:set-funcallable-instance-function
     this (make-template-function #+allegro segments #-allegro datum variables delimiter :safe safe))))

(defun reset-template
    (template source-string
     &key (delimiter (delimiter template))
       (variable-pattern (variable-pattern template))
       (tokenizer (template-tokenizer template))
       (safe (safe template)))
  "Reset several slots and re-parse the template itself."
  (declare (type template template))
  (progn
    (setf (slot-value template 'delimiter) delimiter
          (slot-value template 'variable-pattern) variable-pattern
          (slot-value template 'tokenizer) tokenizer
          (slot-value template 'safe) safe
          (slot-value template 'source) source-string)
    (reparse-template template)
    template))

(defmethod (setf source-string) :around ((new string) (this template))
  (when (string/= new (slot-value this 'source))
    (call-next-method))
  this)

(defmethod (setf source-string) :after ((new string) (this template))
  (reparse-template this))

(defmethod (setf delimiter) :around ((new character) (this template))
  (when (char/= new (slot-value this 'delimiter))
    (call-next-method))
  this)

(defmethod (setf delimiter) :after ((new character) (this template))
  (reparse-template this))

(defmethod (setf variable-pattern) :around ((new string) (this template))
  (when (string/= new (slot-value this 'variable-pattern))
    (call-next-method))
  this)

(defmethod (setf variable-pattern) :after ((new string) (this template))
  (reparse-template this))

(defmethod (setf template-tokenizer) :around ((new %tokenizer%) (this template))
  (when (or (string/= (slot-value new 'regex)
                      (slot-value (slot-value this 'tokenizer) 'regex))
            (not (equal (slot-value new 'separators)
                        (slot-value (slot-value this 'tokenizer) 'separators))))
    (call-next-method))
  this)

(defmethod (setf template-tokenizer) :after ((new %tokenizer%) (this template))
  (reparse-template this))

(defmethod add-separator ((this template) separator)
  (setf (template-tokenizer this)
        (add-separator (template-tokenizer this) separator)))

(defmethod add-separators ((this template) &rest separators)
  (setf (template-tokenizer this)
        (apply 'add-separators (template-tokenizer this) separators)))

(defmethod delete-separator ((this template) separator)
  (setf (template-tokenizer this)
        (delete-separator (template-tokenizer this) separator)))

(defmethod delete-separators ((this template) &rest separators)
  (setf (template-tokenizer this)
        (apply 'delete-separators (template-tokenizer this) separators)))

(defmethod (setf safe) :around ((new symbol) (this template))
  (when (not (eql new (slot-value this 'safe)))
    (call-next-method))
  this)

(defmethod (setf safe) :after ((new symbol) (this template))
  (with-slots (delimiter #+allegro segments #-allegro  datum variables safe) this
    (closer-mop:set-funcallable-instance-function
     this (make-template-function #+allegro segments #-allegro datum variables delimiter :safe safe))))

(defun reparse-template (template)
  "Re-parse a template with its own slots;
this function will be issued when one of slot is modified."
  (declare (type template template))
  (with-slots (source delimiter variable-pattern tokenizer #+allegro segments #-allegro datum variables safe) template
    (multiple-value-bind (#+allegro segments #-allegro datum variables)
        (parse-template source delimiter variable-pattern tokenizer)
      (setf (slot-value template #+allegro 'segments #-allegro 'datum) #+allegro segments #-allegro datum
            (slot-value template 'variables) variables)
      (closer-mop:set-funcallable-instance-function
       template (make-template-function #+allegro segments #-allegro datum variables delimiter :safe safe))
      template)))

(defun make-template-parameters-list (variables delimiter safe)
  "Helper function for `make-template-function', it helps generate the corresponding parameters list."
  (let ((vars (remove-duplicates variables :test 'string=))
        (delimiter* (string delimiter)))
    `(&key
      ,@(if safe
            (map 'list (lambda (var)
                         `(,(funcall .to-symbol. var) ,(concatenate 'string delimiter* var)))
                 vars)
            (map 'list (lambda (var)
                         `(,(funcall .to-symbol. var) nil
                            ,(funcall .to-symbol. (append-supplied-p var))))
                 vars)))))

#+allegro
(defun make-template-function (segments variables delimiter &key (safe nil))
  "Given its datum, variables, delimiter and probably the safe, make a compiled function for a template."
  (let ((vars (remove-duplicates variables :test 'string=))
        (args `(,@(loop
                     for seg across segments
                     collect (if* (keywordp seg)
                                  :then (funcall .to-symbol. seg)
                                  :else seg)))))
    (compile nil
             (if* safe
                  :then `(lambda ,(make-template-parameters-list variables delimiter t)
                           (string+ ,@args))
                  :else `(lambda ,(make-template-parameters-list variables delimiter nil)
                           (progn
                             ,@(loop for var across vars
                                  collect `(unless ,(funcall .to-symbol. (append-supplied-p var))
                                             (error "The variable ~A~A is not supplied, which must be supplied in non-safe mode."
                                                    ,delimiter ,var)))
                             (string+ ,@args)))))))

#-allegro
(defun make-template-function (datum variables delimiter &key (safe nil))
  "Given its datum, variables, delimiter and probably the safe, make a compiled function for a template." 
  (let ((vars (remove-duplicates variables :test 'string=)))
    (compile nil
             (if safe
                 `(lambda ,(make-template-parameters-list variables delimiter t)
                    (format nil ,datum
                            ,@(loop for var across variables
                                 collect (funcall .to-symbol. var))))
                 `(lambda ,(make-template-parameters-list variables delimiter nil)
                    (progn
                      ,@(loop for var across vars
                           collect `(unless ,(funcall .to-symbol. (append-supplied-p var))
                                      (error "The variable ~A~A is not supplied, which must be supplied in non-safe mode."
                                             ,delimiter ,var)))
                      (format nil ,datum 
                              ,@(loop for var across variables
                                   collect (intern (case (readtable-case *readtable*)
                                                     (:preserve var)
                                                     (t (string-upcase var))))))))))))
