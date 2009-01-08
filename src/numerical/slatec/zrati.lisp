;;; Compiled by f2cl version:
;;; ("$Id: zrati.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: zrati.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: zrati.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: zrati.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: zrati.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: zrati.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: zrati.lisp,v 1.11 2009-01-08 18:25:34 rtoy Exp $")

;;; Using Lisp CMU Common Lisp Snapshot 2008-12 (19E)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':simple-array)
;;;           (:array-slicing nil) (:declare-common nil)
;;;           (:float-format double-float))

(in-package :slatec)


(let ((czeror 0.0)
      (czeroi 0.0)
      (coner 1.0)
      (conei 0.0)
      (rt2 1.4142135623730951))
  (declare (type (double-float) czeror czeroi coner conei rt2))
  (defun zrati (zr zi fnu n cyr cyi tol)
    (declare (type (simple-array double-float (*)) cyi cyr)
             (type (f2cl-lib:integer4) n)
             (type (double-float) tol fnu zi zr))
    (prog ((i 0) (id 0) (idnu 0) (inu 0) (itime 0) (k 0) (kk 0) (magz 0)
           (ak 0.0) (amagz 0.0) (ap1 0.0) (ap2 0.0) (arg 0.0) (az 0.0)
           (cdfnui 0.0) (cdfnur 0.0) (dfnu 0.0) (fdnu 0.0) (flam 0.0)
           (fnup 0.0) (pti 0.0) (ptr 0.0) (p1i 0.0) (p1r 0.0) (p2i 0.0)
           (p2r 0.0) (rak 0.0) (rap1 0.0) (rho 0.0) (rzi 0.0) (rzr 0.0)
           (test 0.0) (test1 0.0) (tti 0.0) (ttr 0.0) (t1i 0.0) (t1r 0.0))
      (declare (type (double-float) t1r t1i ttr tti test1 test rzr rzi rho rap1
                                    rak p2r p2i p1r p1i ptr pti fnup flam fdnu
                                    dfnu cdfnur cdfnui az arg ap2 ap1 amagz ak)
               (type (f2cl-lib:integer4) magz kk k itime inu idnu id i))
      (setf az (coerce (realpart (zabs zr zi)) 'double-float))
      (setf inu (f2cl-lib:int fnu))
      (setf idnu (f2cl-lib:int-sub (f2cl-lib:int-add inu n) 1))
      (setf magz (f2cl-lib:int az))
      (setf amagz
              (coerce (the f2cl-lib:integer4 (f2cl-lib:int-add magz 1))
                      'double-float))
      (setf fdnu (coerce (the f2cl-lib:integer4 idnu) 'double-float))
      (setf fnup (max amagz fdnu))
      (setf id (f2cl-lib:int-sub idnu magz 1))
      (setf itime 1)
      (setf k 1)
      (setf ptr (/ 1.0 az))
      (setf rzr (* ptr (+ zr zr) ptr))
      (setf rzi (* (- ptr) (+ zi zi) ptr))
      (setf t1r (* rzr fnup))
      (setf t1i (* rzi fnup))
      (setf p2r (- t1r))
      (setf p2i (- t1i))
      (setf p1r coner)
      (setf p1i conei)
      (setf t1r (+ t1r rzr))
      (setf t1i (+ t1i rzi))
      (if (> id 0) (setf id 0))
      (setf ap2 (coerce (realpart (zabs p2r p2i)) 'double-float))
      (setf ap1 (coerce (realpart (zabs p1r p1i)) 'double-float))
      (setf arg (/ (+ ap2 ap2) (* ap1 tol)))
      (setf test1 (f2cl-lib:fsqrt arg))
      (setf test test1)
      (setf rap1 (/ 1.0 ap1))
      (setf p1r (* p1r rap1))
      (setf p1i (* p1i rap1))
      (setf p2r (* p2r rap1))
      (setf p2i (* p2i rap1))
      (setf ap2 (* ap2 rap1))
     label10
      (setf k (f2cl-lib:int-add k 1))
      (setf ap1 ap2)
      (setf ptr p2r)
      (setf pti p2i)
      (setf p2r (- p1r (- (* t1r ptr) (* t1i pti))))
      (setf p2i (- p1i (+ (* t1r pti) (* t1i ptr))))
      (setf p1r ptr)
      (setf p1i pti)
      (setf t1r (+ t1r rzr))
      (setf t1i (+ t1i rzi))
      (setf ap2 (coerce (realpart (zabs p2r p2i)) 'double-float))
      (if (<= ap1 test) (go label10))
      (if (= itime 2) (go label20))
      (setf ak (coerce (realpart (* (zabs t1r t1i) 0.5)) 'double-float))
      (setf flam (+ ak (f2cl-lib:fsqrt (- (* ak ak) 1.0))))
      (setf rho (min (/ ap2 ap1) flam))
      (setf test (* test1 (f2cl-lib:fsqrt (/ rho (- (* rho rho) 1.0)))))
      (setf itime 2)
      (go label10)
     label20
      (setf kk (f2cl-lib:int-sub (f2cl-lib:int-add k 1) id))
      (setf ak (coerce (the f2cl-lib:integer4 kk) 'double-float))
      (setf t1r ak)
      (setf t1i czeroi)
      (setf dfnu (+ fnu (f2cl-lib:int-sub n 1)))
      (setf p1r (/ 1.0 ap2))
      (setf p1i czeroi)
      (setf p2r czeror)
      (setf p2i czeroi)
      (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                    ((> i kk) nil)
        (tagbody
          (setf ptr p1r)
          (setf pti p1i)
          (setf rap1 (+ dfnu t1r))
          (setf ttr (* rzr rap1))
          (setf tti (* rzi rap1))
          (setf p1r (+ (- (* ptr ttr) (* pti tti)) p2r))
          (setf p1i (+ (* ptr tti) (* pti ttr) p2i))
          (setf p2r ptr)
          (setf p2i pti)
          (setf t1r (- t1r coner))
         label30))
      (if (or (/= p1r czeror) (/= p1i czeroi)) (go label40))
      (setf p1r tol)
      (setf p1i tol)
     label40
      (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5)
          (zdiv p2r p2i p1r p1i (f2cl-lib:fref cyr (n) ((1 n)))
           (f2cl-lib:fref cyi (n) ((1 n))))
        (declare (ignore var-0 var-1 var-2 var-3))
        (setf (f2cl-lib:fref cyr (n) ((1 n))) var-4)
        (setf (f2cl-lib:fref cyi (n) ((1 n))) var-5))
      (if (= n 1) (go end_label))
      (setf k (f2cl-lib:int-sub n 1))
      (setf ak (coerce (the f2cl-lib:integer4 k) 'double-float))
      (setf t1r ak)
      (setf t1i czeroi)
      (setf cdfnur (* fnu rzr))
      (setf cdfnui (* fnu rzi))
      (f2cl-lib:fdo (i 2 (f2cl-lib:int-add i 1))
                    ((> i n) nil)
        (tagbody
          (setf ptr
                  (+ cdfnur
                     (- (* t1r rzr) (* t1i rzi))
                     (f2cl-lib:fref cyr ((f2cl-lib:int-add k 1)) ((1 n)))))
          (setf pti
                  (+ cdfnui
                     (+ (* t1r rzi) (* t1i rzr))
                     (f2cl-lib:fref cyi ((f2cl-lib:int-add k 1)) ((1 n)))))
          (setf ak (coerce (realpart (zabs ptr pti)) 'double-float))
          (if (/= ak czeror) (go label50))
          (setf ptr tol)
          (setf pti tol)
          (setf ak (* tol rt2))
         label50
          (setf rak (/ coner ak))
          (setf (f2cl-lib:fref cyr (k) ((1 n))) (* rak ptr rak))
          (setf (f2cl-lib:fref cyi (k) ((1 n))) (* (- rak) pti rak))
          (setf t1r (- t1r coner))
          (setf k (f2cl-lib:int-sub k 1))
         label60))
      (go end_label)
     end_label
      (return (values nil nil nil nil nil nil nil)))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::zrati fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((double-float) (double-float) (double-float)
                        (fortran-to-lisp::integer4)
                        (simple-array double-float (*))
                        (simple-array double-float (*)) (double-float))
           :return-values '(nil nil nil nil nil nil nil)
           :calls '(fortran-to-lisp::zdiv fortran-to-lisp::zabs))))

