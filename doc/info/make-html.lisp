;; Written by Michel Talon, 25/2/2017, public.

(require 'asdf)
(import 'asdf::os-unix-p)
(import 'asdf::os-windows-p)
(import 'asdf::getenv)
(import 'asdf::getenvp)
(import 'asdf::getcwd)
(import 'asdf::chdir)
(import 'asdf::delete-directory-tree)
(import 'asdf::delete-file-if-exists)
(shadowing-import 'asdf::run-program)

(defmacro strcat (&body strings)
  `(concatenate 'string ,@strings))

(load "extract-categories.lisp")

(defparameter *origin* (namestring (getcwd)))

(defvar *temp*)

(defun make-wdir ()
  (let* ((place (if (getenv "TMPDIR") (getenv "TMPDIR") '"/tmp"))
		(wdir
			(cond ((os-unix-p) (strcat place 
				"/maxima-texinfo-categories-"
				(format nil "~S" (random 1000 (make-random-state t)))
				"/"))
			((os-windows-p) (strcat place 
				"\\maxima-texinfo-categories-"
				(format nil "~S" (random 1000 (make-random-state t)))
				"\\")))))
    (ensure-directories-exist wdir) ; aka mkdir wdir
    (cond ((os-unix-p) 
	   (run-program (strcat "cp -R *.texi figures " wdir)))
	  ((os-windows-p)
	   (run-program (strcat "xcopy *.texi " wdir))
	   (run-program (strcat "xcopy figures " wdir))))
    (chdir wdir) (setq *temp* wdir)))

(defun clean-wdir (dir) 
  (delete-directory-tree (truename dir) :validate t))

(defun trap-run-script ()
  (let ((wdir "")) (unwind-protect 
			(progn
			  (setq wdir (make-wdir ))
			  (run-script))
		     (clean-wdir *temp*))))

(defun run-script ()
  (process-all-texi-files *temp*)
  (run-perl "single" *origin* *temp*)
  (run-perl "split" *origin* *temp*)
  (process-all-html-files *temp*)
  (move-back-html-files *temp*)
  (chdir *origin*))

(defun move-back-html-files (dir)
  (dolist (hfile (get-all "html" dir))
		(delete-file-if-exists (strcat *origin* hfile))
		(rename-file (strcat dir hfile) (strcat  *origin* hfile))
		))

(trap-run-script)
