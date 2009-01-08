;;; Compiled by f2cl version:
;;; ("$Id: zdiv.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: zdiv.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: zdiv.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: zdiv.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: zdiv.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: zdiv.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: zdiv.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $")

;;; Using Lisp CMU Common Lisp Snapshot 2008-12 (19E)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(defun zdiv (ar ai br bi cr ci)
  (declare (type (double-float) ci cr bi br ai ar))
  (prog ((bm 0.0) (ca 0.0) (cb 0.0) (cc 0.0) (cd 0.0))
    (declare (type (double-float) cd cc cb ca bm))
    (setf bm (coerce (realpart (/ 1.0 (zabs br bi))) 'double-float))
    (setf cc (* br bm))
    (setf cd (* bi bm))
    (setf ca (* (+ (* ar cc) (* ai cd)) bm))
    (setf cb (* (- (* ai cc) (* ar cd)) bm))
    (setf cr ca)
    (setf ci cb)
    (go end_label)
   end_label
    (return (values nil nil nil nil cr ci))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zdiv fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (double-float) (double-float) (double-float))
           :return-values '(nil nil nil nil fortran-to-lisp::cr
                            fortran-to-lisp::ci)
           :calls '(fortran-to-lisp::zabs))))

