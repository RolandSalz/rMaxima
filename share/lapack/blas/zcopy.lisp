;;; Compiled by f2cl version:
;;; ("$Id: zcopy.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: zcopy.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: zcopy.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: zcopy.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: zcopy.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: zcopy.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $"
;;;  "$Id: zcopy.lisp,v 1.3 2009-01-08 18:25:23 rtoy Exp $")

;;; Using Lisp CMU Common Lisp Snapshot 2008-12 (19E)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :blas)


(defun zcopy (n zx incx zy incy)
  (declare (type (array f2cl-lib:complex16 (*)) zy zx)
           (type (f2cl-lib:integer4) incy incx n))
  (f2cl-lib:with-multi-array-data
      ((zx f2cl-lib:complex16 zx-%data% zx-%offset%)
       (zy f2cl-lib:complex16 zy-%data% zy-%offset%))
    (prog ((i 0) (ix 0) (iy 0))
      (declare (type (f2cl-lib:integer4) iy ix i))
      (if (<= n 0) (go end_label))
      (if (and (= incx 1) (= incy 1)) (go label20))
      (setf ix 1)
      (setf iy 1)
      (if (< incx 0)
          (setf ix
                  (f2cl-lib:int-add
                   (f2cl-lib:int-mul (f2cl-lib:int-sub 1 n) incx)
                   1)))
      (if (< incy 0)
          (setf iy
                  (f2cl-lib:int-add
                   (f2cl-lib:int-mul (f2cl-lib:int-sub 1 n) incy)
                   1)))
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf (f2cl-lib:fref zy-%data% (iy) ((1 *)) zy-%offset%)
                  (f2cl-lib:fref zx-%data% (ix) ((1 *)) zx-%offset%))
          (setf ix (f2cl-lib:int-add ix incx))
          (setf iy (f2cl-lib:int-add iy incy))
         label10))
      (go end_label)
     label20
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf (f2cl-lib:fref zy-%data% (i) ((1 *)) zy-%offset%)
                  (f2cl-lib:fref zx-%data% (i) ((1 *)) zx-%offset%))
         label30))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zcopy fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::complex16 (*))
                        (fortran-to-lisp::integer4))
           :return-values '(nil nil nil nil nil)
           :calls 'nil)))

