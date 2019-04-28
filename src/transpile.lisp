(defpackage shot.transpile
  (:use :cl
        :trivia)
  (:import-from :shot.condition
                :failed)
  (:export :*symbol-table*
           :*bindings*
           :statements
           :statement
           :expression))
(in-package :shot.transpile)

(defvar *symbol-table*)
(defvar *bindings*)

(defun statements (ast)
  `(progv
       (mapcar #'%identifier (mapcar #'car *bindings*))
       (mapcar #'cdr *bindings*)
     ,@(mapcar #'statement ast)))

(defun statement (ast)
  (ematch ast
    ((list :definition-statement x)
     (definition-statement x))
    ((list :expression-statement x)
     (expression x))))

(defun definition-statement (ast)
  (ematch ast
    ((list :function-definition identifier block)
     (function-definition identifier block))
    ((list :constant-definition pattern expression)
     (constant-definition pattern expression))))

(defun function-definition (identifier block)
  `(let ((value (lambda (&rest args)
                  ,(%block 'args block))))
     (defparameter ,(%identifier identifier) value)
     (push (cons ',identifier value) *bindings*)))

(defun pattern-variables (pattern)
  (ematch pattern
    ;((list :apply-expression operator x y))
    ;((list :apply-expression operator x))
    ((list :at-pattern x y)
     (append (pattern-variables x) (pattern-variables y)))
    ((list :value-expression x)
     '())
    ((list :variable-expression "_")
     '())
    ((list :variable-expression identifier)
     (list identifier))
    ((list :list-expression expressions)
     (mapcon #'pattern-variables expressions))
    ((list :object-expression members)
     (labels ((f (members)
                (ematch members
                  (() '())
                  ((list (list :spread-member expression))
                   (pattern-variables expression))
                  ((cons (list :property-member key value) rest)
                   (append (pattern-variables value) (f rest))))))
       (f members)))))

(defun constant-definition (pattern expression)
  `(match ,(expression expression)
     (,(pattern pattern)
      ,@(loop
          for variable in (pattern-variables pattern)
          for identifier = (%identifier variable)
          collect `(defparameter ,identifier ,identifier)
          collect `(push (cons ',variable ,identifier) *bindings*)))
     (_
      (error 'failed :format-control "Pattern match failed"))))

(defun not-equal (x y)
  (not (equal x y)))

(defun build-object (&rest items)
  (cons :obj
        (remove-duplicates (loop
                             for item in items
                             append (if (eq (first item) :obj)
                                        (cdr item)
                                        (list item)))
                           :test #'equal
                           :key #'car)))

(defun expression (ast)
  (ematch ast
    ((list :apply-expression "|" x y)
     `(handler-case ,(expression x)
        (failed (c)
          (declare (ignore c))
          ,(expression y))))
    ((list :apply-expression "?" x y)
     `(if ,(expression x)
          ,(expression y)
          (error 'failed :format-control "Guarded")))
    ((list :apply-expression "()" x y)
     `(funcall ,(expression x) ,@(mapcar #'expression y)))
    ((list :apply-expression operator x y)
     `(,(cdr (assoc operator
                    '(("&&" . and) ("||" . or) ("==" . equal) ("!=" . not-equal)
                      (">=" . >=) ("<=" . <=) (">" . >) ("<" . <) ("+" . +) ("-" . -)
                      ("*" . *) ("/" . /) ("%" . mod))
                    :test 'equal))
       ,(expression x) ,(expression y)))
    ((list :apply-expression operator x)
     `(,(cdr (assoc operator
                    '(("-" . -))
                    :test 'equal))
       ,(expression x)))
    ((list :value-expression x)
     (value-expression x))
    ((list :variable-expression identifier)
     (%identifier identifier))
    ((list :list-expression expressions)
     `(list ,@(mapcar #'expression expressions)))
    ((list :object-expression members)
     `(build-object
       ,@(loop
           for member in members
           collect (ematch member
                     ((list :property-member key value)
                      (assert (stringp key))
                      `(cons ,key ,(expression value)))
                     ((list :spread-member expression)
                      (expression expression))
                     ((list :computed-property-member key value)
                      `(cons ,(expression key) ,(expression value)))))))
    ((list :closure-expression block)
     `(lambda (&rest args)
        ,(%block 'args block)))))

(defun value-expression (ast)
  (ematch ast
    ((list :number-value x)
     x)
    ((list :string-value x)
     x)
    ((list :bool-value x)
     x)
    ((list :null-value)
     :null)))

(defun pattern (ast)
  (ematch ast
    ;((list :apply-expression operator x y))
    ;((list :apply-expression operator x))
    ((list :at-pattern x y)
     `(and ,(pattern x) ,(pattern y)))
    ((list :value-expression x)
     (value-expression x))
    ((list :variable-expression "_")
     '_)
    ((list :variable-expression identifier)
     (%identifier identifier))
    ((list :list-expression expressions)
     `(list ,@(mapcar #'pattern expressions)))
    ((list :object-expression members)
     (labels ((f (members)
                (ematch members
                  (() '())
                  ((list (list :spread-member expression))
                   (pattern expression))
                  ((cons (list :property-member key value) rest)
                   `(assoc* (,key . ,(pattern value)) ,(f rest))))))
       `(list* :obj ,(f members))))))

(defun %block (args ast)
  `(match ,args
     ,@(loop
         for (patterns body) in ast
         collect `((list ,@(mapcar #'pattern patterns))
                   ,(expression body)))
     (_
      (error 'failed :format-control "Pattern match failed"))))

(defun %identifier (name)
  (or (gethash name *symbol-table*)
      (setf (gethash name *symbol-table*)
            (gensym name))))


(defpattern assoc* (a b)
  (alexandria:with-gensyms (it)
    `(guard1 (,it :type list) (assoc ,(car a) ,it :test #'equal)
             (cdr (assoc ,(car a) ,it :test #'equal)) ,(cdr a)
             (remove-assoc ,(car a) ,it) ,b)))

(defun remove-assoc (item alist)
  (remove (assoc item alist :test #'equal)
          alist))
