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

; We load the threaded version of extract-categories.lisp
(load "extract-categories-threaded.lisp")

(defparameter *origin* (namestring (getcwd)))

(defun make-wdir ()
  ;; In Windows TMPDIR is e.g. C:\\Users\\Roland\\AppData\\Local\\Temp
  (let* ((place (if (getenvp "TMPDIR") (getenv "TMPDIR") '"/tmp"))
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
    (chdir wdir) 
    (setf *default-pathname-defaults* (getcwd))
    wdir))

(defun clean-wdir (dir) 
  (delete-directory-tree (truename dir) :validate t))

(defun trap-run-script ()
  (let ((wdir "")) (unwind-protect 
			(progn
			  (setq wdir (make-wdir))
			  (run-script))
		     (clean-wdir wdir))))

(defun run-script ()
  (process-all-texi-files)
  (progn
  (run-perl "single" *origin*)
  (run-perl "split" *origin*))
  (process-all-html-files)
  (move-back-html-files)
  (chdir *origin*)
  (setf *default-pathname-defaults* (getcwd)))

(defun move-back-html-files ()
  (dolist (hfile (get-all "html"))
		(delete-file-if-exists (strcat *origin* hfile))
    (rename-file hfile (strcat  *origin* hfile))))

(trap-run-script)
