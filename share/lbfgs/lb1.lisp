;;; Compiled by f2cl version:
;;; ("$Id: lb1.lisp,v 1.1 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: lb1.lisp,v 1.1 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: lb1.lisp,v 1.1 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: lb1.lisp,v 1.1 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: lb1.lisp,v 1.1 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: lb1.lisp,v 1.1 2009-01-08 18:25:34 rtoy Exp $"
;;;  "$Id: lb1.lisp,v 1.1 2009-01-08 18:25:34 rtoy Exp $")

;;; Using Lisp CMU Common Lisp Snapshot 2008-12 (19E)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package :common-lisp-user)


(defun lb1 (iprint iter nfun gnorm n m x f g stp finish)
  (declare (type f2cl-lib:logical finish)
           (type (array double-float (*)) g x)
           (type (double-float) stp f gnorm)
           (type (f2cl-lib:integer4) m n nfun iter)
           (type (array f2cl-lib:integer4 (*)) iprint))
  (let ()
    (symbol-macrolet ((mp (lb3-mp *lb3-common-block*)))
      (f2cl-lib:with-multi-array-data
          ((iprint f2cl-lib:integer4 iprint-%data% iprint-%offset%)
           (x double-float x-%data% x-%offset%)
           (g double-float g-%data% g-%offset%))
        (prog ((i 0))
          (declare (type (integer) i))
          (cond
            ((= iter 0)
             (f2cl-lib:fformat mp
                               ("*************************************************"
                                "~%"))
             (f2cl-lib:fformat mp
                               ("  N=" 1 (("~5D")) "   NUMBER OF CORRECTIONS="
                                1 (("~2D")) "~%" "       INITIAL VALUES" "~%")
                               n
                               m)
             (f2cl-lib:fformat mp
                               (" F= " nil (("~22,15,2,1,'*,,'DE"))
                                "   GNORM= " nil (("~22,15,2,1,'*,,'DE")) "~%")
                               f
                               gnorm)
             (cond
               ((>= (f2cl-lib:fref iprint (2) ((1 2))) 1)
                (f2cl-lib:fformat mp (" VECTOR X= " "~%"))
                (f2cl-lib:fformat mp
                                  (4 ("~2@T" nil (("~22,15,2,1,'*,,'DE")))
                                   "~%")
                                  (do ((i 1 (f2cl-lib:int-add i 1))
                                       (%ret nil))
                                      ((> i n) (nreverse %ret))
                                    (declare (type f2cl-lib:integer4 i))
                                    (push
                                     (f2cl-lib:fref x-%data%
                                                    (i)
                                                    ((1 n))
                                                    x-%offset%)
                                     %ret)))
                (f2cl-lib:fformat mp (" GRADIENT VECTOR G= " "~%"))
                (f2cl-lib:fformat mp
                                  (4 ("~2@T" nil (("~22,15,2,1,'*,,'DE")))
                                   "~%")
                                  (do ((i 1 (f2cl-lib:int-add i 1))
                                       (%ret nil))
                                      ((> i n) (nreverse %ret))
                                    (declare (type f2cl-lib:integer4 i))
                                    (push
                                     (f2cl-lib:fref g-%data%
                                                    (i)
                                                    ((1 n))
                                                    g-%offset%)
                                     %ret)))))
             (f2cl-lib:fformat mp
                               ("*************************************************"
                                "~%"))
             (f2cl-lib:fformat mp
                               ("~%" "   I  NFN" "~5@T" "FUNC" "~20@T" "GNORM"
                                "~19@T" "STEPLENGTH" "~%" "~%")))
            (t
             (if
              (and
               (= (f2cl-lib:fref iprint-%data% (1) ((1 2)) iprint-%offset%) 0)
               (and (/= iter 1) (not finish)))
              (go end_label))
             (cond
               ((/= (f2cl-lib:fref iprint (1) ((1 2))) 0)
                (cond
                  ((or
                    (=
                     (mod (f2cl-lib:int-add iter (f2cl-lib:int-sub 1))
                          (f2cl-lib:fref iprint (1) ((1 2))))
                     0)
                    finish)
                   (if
                    (and
                     (>
                      (f2cl-lib:fref iprint-%data% (2) ((1 2)) iprint-%offset%)
                      1)
                     (> iter 1))
                    (f2cl-lib:fformat mp
                                      ("~%" "   I  NFN" "~5@T" "FUNC" "~20@T"
                                       "GNORM" "~19@T" "STEPLENGTH" "~%" "~%")))
                   (f2cl-lib:fformat mp
                                     (2 (1 (("~4D")) "~1@T") "~3@T" 3
                                      (nil (("~22,15,2,1,'*,,'DE")) "~2@T")
                                      "~%")
                                     iter
                                     nfun
                                     f
                                     gnorm
                                     stp))
                  (t
                   (go end_label))))
               (t
                (if
                 (and
                  (> (f2cl-lib:fref iprint-%data% (2) ((1 2)) iprint-%offset%)
                     1)
                  finish)
                 (f2cl-lib:fformat mp
                                   ("~%" "   I  NFN" "~5@T" "FUNC" "~20@T"
                                    "GNORM" "~19@T" "STEPLENGTH" "~%" "~%")))
                (f2cl-lib:fformat mp
                                  (2 (1 (("~4D")) "~1@T") "~3@T" 3
                                   (nil (("~22,15,2,1,'*,,'DE")) "~2@T") "~%")
                                  iter
                                  nfun
                                  f
                                  gnorm
                                  stp)))
             (cond
               ((or (= (f2cl-lib:fref iprint (2) ((1 2))) 2)
                    (= (f2cl-lib:fref iprint (2) ((1 2))) 3))
                (cond
                  (finish
                   (f2cl-lib:fformat mp (" FINAL POINT X= " "~%")))
                  (t
                   (f2cl-lib:fformat mp (" VECTOR X= " "~%"))))
                (f2cl-lib:fformat mp
                                  (4 ("~2@T" nil (("~22,15,2,1,'*,,'DE")))
                                   "~%")
                                  (do ((i 1 (f2cl-lib:int-add i 1))
                                       (%ret nil))
                                      ((> i n) (nreverse %ret))
                                    (declare (type f2cl-lib:integer4 i))
                                    (push
                                     (f2cl-lib:fref x-%data%
                                                    (i)
                                                    ((1 n))
                                                    x-%offset%)
                                     %ret)))
                (cond
                  ((= (f2cl-lib:fref iprint (2) ((1 2))) 3)
                   (f2cl-lib:fformat mp (" GRADIENT VECTOR G= " "~%"))
                   (f2cl-lib:fformat mp
                                     (4 ("~2@T" nil (("~22,15,2,1,'*,,'DE")))
                                      "~%")
                                     (do ((i 1 (f2cl-lib:int-add i 1))
                                          (%ret nil))
                                         ((> i n) (nreverse %ret))
                                       (declare (type f2cl-lib:integer4 i))
                                       (push
                                        (f2cl-lib:fref g-%data%
                                                       (i)
                                                       ((1 n))
                                                       g-%offset%)
                                        %ret)))))))
             (if finish
                 (f2cl-lib:fformat mp
                                   ("~%"
                                    " THE MINIMIZATION TERMINATED WITHOUT DETECTING ERRORS."
                                    "~%" " IFLAG = 0" "~%")))))
          (go end_label)
         end_label
          (return (values nil nil nil nil nil nil nil nil nil nil nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::lb1 fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array fortran-to-lisp::integer4 (2))
                        (fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (double-float) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (double-float) (array double-float (*)) (double-float)
                        fortran-to-lisp::logical)
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil)
           :calls 'nil)))

