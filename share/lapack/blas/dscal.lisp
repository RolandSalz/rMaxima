;;; Compiled by f2cl version:
;;; ("$Id: dscal.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: dscal.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: dscal.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: dscal.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: dscal.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: dscal.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: dscal.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $")

;;; Using Lisp CMU Common Lisp Snapshot 2008-12 (19E)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :blas)


(defun dscal (n da dx incx)
  (declare (type (array double-float (*)) dx)
           (type (double-float) da)
           (type (f2cl-lib:integer4) incx n))
  (f2cl-lib:with-multi-array-data
      ((dx double-float dx-%data% dx-%offset%))
    (prog ((i 0) (m 0) (mp1 0) (nincx 0))
      (declare (type (f2cl-lib:integer4) nincx mp1 m i))
      (if (or (<= n 0) (<= incx 0)) (go end_label))
      (if (= incx 1) (go label20))
      (setf nincx (f2cl-lib:int-mul n incx))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i incx))
                    ((> i nincx) nil)
        (tagbody
          (setf (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)
                  (* da (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)))
         label10))
      (go end_label)
     label20
      (setf m (mod n 5))
      (if (= m 0) (go label40))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i m) nil)
        (tagbody
          (setf (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)
                  (* da (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)))
         label30))
      (if (< n 5) (go end_label))
     label40
      (setf mp1 (f2cl-lib:int-add m 1))
      (f2cl-lib:fdo (i mp1 (f2cl-lib:int-add i 5))
                    ((> i n) nil)
        (tagbody
          (setf (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)
                  (* da (f2cl-lib:fref dx-%data% (i) ((1 *)) dx-%offset%)))
          (setf (f2cl-lib:fref dx-%data%
                               ((f2cl-lib:int-add i 1))
                               ((1 *))
                               dx-%offset%)
                  (* da
                     (f2cl-lib:fref dx-%data%
                                    ((f2cl-lib:int-add i 1))
                                    ((1 *))
                                    dx-%offset%)))
          (setf (f2cl-lib:fref dx-%data%
                               ((f2cl-lib:int-add i 2))
                               ((1 *))
                               dx-%offset%)
                  (* da
                     (f2cl-lib:fref dx-%data%
                                    ((f2cl-lib:int-add i 2))
                                    ((1 *))
                                    dx-%offset%)))
          (setf (f2cl-lib:fref dx-%data%
                               ((f2cl-lib:int-add i 3))
                               ((1 *))
                               dx-%offset%)
                  (* da
                     (f2cl-lib:fref dx-%data%
                                    ((f2cl-lib:int-add i 3))
                                    ((1 *))
                                    dx-%offset%)))
          (setf (f2cl-lib:fref dx-%data%
                               ((f2cl-lib:int-add i 4))
                               ((1 *))
                               dx-%offset%)
                  (* da
                     (f2cl-lib:fref dx-%data%
                                    ((f2cl-lib:int-add i 4))
                                    ((1 *))
                                    dx-%offset%)))
         label50))
      (go end_label)
     end_label
      (return (values nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dscal fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (double-float)
                        (array double-float (*)) (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil)
           :calls 'nil)))

