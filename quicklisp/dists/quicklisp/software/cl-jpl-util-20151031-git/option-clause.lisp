(in-package #:jpl-util)

(defmacro option-clause-bind (lambda-list expression &body body)
  "Evaluates EXPRESSION to produce a list of \"option clauses,\" then
evaluates BODY with the variables of LAMBDA-LIST bound to the values
of the option clauses.

An option clause is a cons of the form (NAME . VALUE), where each NAME
is unique.  A list of option clauses looks a lot like an association
list, with the difference being that no value is allowed to be the
NAME of more than one option clause.

Although the term \"option clause\" is not used in the Common Lisp
HyperSpec, the syntax of some Common Lisp macros call for expressing
options in this style.  DEFCLASS and DEFGENERIC are among these
macros.

LAMBDA-LIST is just like a sequence of variable specifications in the
&KEY section of a destructuring lambda list.  Each member of
LAMBDA-LIST designates one value of NAME to match from the list of
option clauses, and must take one of the following forms:

  VAR
  
    If the keyword name appears as the name of an option clause, VAR
    is bound to the value of that option clause.  Otherwise, VAR is
    bound to NIL.
    
    The keyword name is a keyword with the same name as VAR.
    
    VAR must be a symbol which is not a lambda list keyword.
  
  ({VAR | (KEYWORD-NAME VAR)} [INIT-FORM [SUPPLIED-P-PARAMETER]])
  
    If the keyword name appears as the name of an option clause, VAR
    is bound to the value of that option clause.  Otherwise, if
    INIT-FORM is supplied, VAR is bound to the result of evaluating
    INIT-FORM.  Otherwise, VAR is bound to NIL.
    
    SUPPLIED-P-PARAMETER, when supplied, is bound to a boolean value
    indicating whether a matching option clause was found.  When the
    bound value is true, INIT-FORM will not have been evaluated.  When
    the value is false, INIT-FORM (if supplied) will have been
    evaluated.
    
    When KEYWORD-NAME is supplied, it is used as the keyword name;
    otherwise, a keyword with the same name as VAR is used.
    
    When supplied, VAR and SUPPLIED-P-PARAMETER must be symbols,
    INIT-FORM must be a form, and KEYWORD-NAME may be any value.

For example:

  (option-clause-bind (foo bar) '((:foo 1 2) (:bar 3)) . BODY) ==
  (destructuring-bind (&key foo bar) '(:foo (1 2) :bar (3)) . BODY)

Due to the constraint that NAMEs must be unique, note that:

  (option-clause-bind VARS EXPRESSION . BODY) NOT==
  (destructuring-bind (&key . VARS) (alist->plist EXPRESSION) . BODY)"
  (check-type* lambda-list list)
  `(destructuring-bind (&key ,@lambda-list)
       (option-clause-list->plist ,expression)
     ,@body))

(defun option-clause-list->plist (clauses)
  (check-type* clauses list)
  (let ((non-cons-clauses (remove-if #'consp clauses)))
    (unless (endp non-cons-clauses)
      (error "Non-cons element~P in list of option clauses:~{ ~S~}"
	     (length non-cons-clauses) non-cons-clauses)))
  (let ((duplicate-clauses (find-duplicates clauses :key #'first)))
    (unless (endp duplicate-clauses)
      (error "Duplicate names in list of option clauses:~{ ~S~}"
	     duplicate-clauses)))
  (alist->plist clauses))
