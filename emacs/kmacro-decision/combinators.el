;;; combinators.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2012 Jason Feng
;; 
;; URL: http://github.com/funkenblatt/combinators.el
;; Version: 0.0.1

(require 'cl)

(eval-and-compile
  (defun cur (f &rest args)
    "Specialize a function on its first few arguments."
    (lambda (&rest more-args)
      (apply f (append args more-args)))))

(defun comp (&rest funcs)
  "Compose functions together.  Function on the left
gets called last."
  (lambda (arg)
    (reduce (lambda (a fun) (funcall fun a))
            (reverse funcs)
            :initial-value arg)))

(defmacro cut (&rest stuff)
  "See SRFI-26.
But basically, (cut foo <> bar baz <>) => (lambda (arg1 arg2) (foo arg1 bar baz arg2))."
  (let ((args (mapcar
               (lambda (x) (gensym))
               (remove-if-not (cur 'eq '<>) stuff))))
    `(lambda ,args
       ,(mapcar (lambda (x)
                  (if (eq x '<>) (pop args) x))
                stuff))))

(defun orfn (&rest fns)
  "Returns a function whose result is non-nil if the result of
any of the given FNs applied to the argument is non-nil."
  (lambda (x)
    (some (cut funcall <> x) fns)))

(defun orfn2 (&rest fns)
  "Returns a function whose result is non-nil if the result of
any of the given FNs applied to the arguments is non-nil."
  (lambda (&rest args)
    (some (cut apply <> args) fns)))

(defun andfn (&rest fns)
  "Returns a function whose result is T if all of the
given FNS return non-nil when applied to the argument."
  (lambda (x)
    (every (cut funcall <> x) fns)))

(defun andfn2 (&rest fns)
  "Returns a function whose result is T if all of the
given FNS return non-nil when applied the arguments."
  (lambda (&rest args)
    (every (cut apply <> args) fns)))

(defun juxt (&rest funcs)
  "Given a bunch of functions, return a new function that juxtaposes
the results of calling said functions on an argument."
  (lambda (&rest args) (mapcar (cut apply <> args) funcs)))

(defun juxtcons (a b)
  "Returns a function that, given functions A and B,
returns (cons (funcall a arg) (funcall b arg))."
  (lambda (&rest args) 
    (cons (apply a args) (apply b args))))

(defun maybe (fun pred)
  "Returns a function that calls FUN on its argument if the result of
PRED on its argument is non-nil, else returns nil"
  (lambda (arg)
    (if (funcall pred arg) (funcall fun arg) nil)))

(defun const (c)
  "Returns a function that returns C regardless of arguments."
  (lambda (&rest args) c))

(defun hashfn (h)
  "Returns a function that retrieves from hash table H."
  (cut gethash <> h))

(provide 'combinators)

;;; combinators.el ends here
