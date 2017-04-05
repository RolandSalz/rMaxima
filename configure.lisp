; 1. Step of lisp only build process: 
; Configure several files by substituting variables for placeholders (list of substitutions in line 147 ff)
; creating original files from template files  "<original filename>.in" (list of files in line 112).

; Modifications done by Roland Salz 03/2017:
;		Add comment lines.
;		Is-win32 parameter deleted and is-not-win32 added. Default is now win32.
;		Maxima-version variable added, default is get-version. 
;		Manual-version variable added, default is get-version.
;		Mathjax-enable variable added, default is "".
;		Mathjax-script variable added, default is "".
;		Processing of doc/info/include-maxima.texi.in and doc/info/texi2html.init.in included.

; Replace in the string <in-string> the substring <old> by the substring <new>, multiple times.
; Called from various other functions.
(defun replace-substring (in-string old new) 
  (let ((result ""))
    (do ((begin 0)
	 (end (search old in-string)
	      (search old in-string :start2 begin)))
	((>= begin (length in-string)) 'done)
      (if end
	  (progn (setf result (concatenate 'string result
					   (subseq in-string begin end)
					   new))
		 (setf begin (+ end (length old))))
	  (progn (setf result (concatenate 'string result
					   (subseq in-string begin
						   (length in-string))))
		 (setf begin (length in-string)))))
    result))

; Do the substitutions reading file <in-filename> and writing to <out-filename>.
; Each file is processed once from beginning to end, testing every line for each of the substitutions.
(defun process-file (in-filename out-filename substitutions)
  (with-open-file (in in-filename :direction :input)
    (with-open-file (out out-filename :direction :output
			 :if-exists :supersede)
      (do ((line (read-line in nil 'eof)
		 (read-line in nil 'eof)))
	  ((eql line 'eof))
	(mapc #'(lambda (pair)
		  (setf line (replace-substring line 
						(first pair)
						(rest pair))))
	      substitutions)
	(format out "~a~%" line)))))

; Prompt with default and interactively read user input for the arguments for configure.
(defun read-with-default (prompt default)
  (format t "~a [~a]: " prompt default)
  (terpri)
  (let ((response (string-right-trim '(#\Return) (read-line))))
    (if (string= response "") default response)))

; Return the current directory as a namestring, e.g. "C:/maxima-build/.", to be used as default for the maxima-directory.
(defun default-directory-string ()
  (string-right-trim 
   "\\" (string-right-trim 
	 "/"	(namestring  
				#+allegro (excl:current-directory)
				#+clisp (#+lisp=cl ext:default-directory 
						#-lisp=cl lisp:default-directory)
				#+cmu (ext:default-directory)
				#+scl (unix-namestring (ext:default-directory))
				#+cormanlisp (ccl:get-current-directory)
				#+lispworks (hcl:get-working-directory)
				#+lucid (lcl:working-directory)
				#-(or allegro clisp cmu scl cormanlisp lispworks lucid) 
				(truename ".")))))

; Get version number from configure.ac
(defun get-version ()
  (with-open-file (in "configure.ac" :direction :input)
    (do ((line (read-line in nil 'eof)
               (read-line in nil 'eof))
         (version "")
         temp)
        ((eq line 'eof)
         (when (string= version "")
           (format t "Warning: No version information found.~%~%"))
         version)
      (when (search "AC_INIT([maxima]," line)
        (setq line (string-trim '(#\Return) line))
        (setq temp
              (replace-substring line "AC_INIT([maxima], [" ""))
        (setq version
              (replace-substring temp "])" ""))
        (when (or (string= temp line)
                  (string= temp version))
          ; Failed substitution
          (format t "Warning: Problem parsing version information. ")
          (format t "Found: \"~a\"~%~%" version))))))

(defvar *maxima-lispname* #+clisp "clisp"
	#+cmu "cmucl"
	#+scl "scl"
	#+sbcl "sbcl"
	#+gcl "gcl"
	#+allegro "acl"
	#+openmcl "openmcl"
  #+abcl "abcl"
  #+ecl "ecl"
	#-(or clisp cmu scl sbcl gcl allegro openmcl abcl ecl) "unknownlisp")

; Top level function: 
; Collect arguments from defaults, function arguments and interactively given user arguments;
; create list of input-files (templates) to be processed;
; create list of substitutions (stored as dotted pairs) to be done in the files to be processed;
; if verbose, then print a list of the substitutions to be done;
; process each input-file using "process-file", creating each output-file.
(defun configure (&key (interactive t) (verbose nil)
		  maxima-directory 
			is-not-win32 
		  posix-shell
			maxima-version
			manual-version
			mathjax-enable
			mathjax-script
		  clisp-name
		  cmucl-name
		  scl-name
		  acl-name
		  openmcl-name
		  sbcl-name
		  ecl-name
		  gcl-name)

  (let ((prefix (if maxima-directory maxima-directory (default-directory-string)))
			(win32-string (if is-not-win32 "false" "true"))
			(maxima-version-name (if maxima-version maxima-version (get-version)))
			(manual-version-name (if manual-version manual-version (get-version)))
			(mathjax-enable-string (if mathjax-enable mathjax-enable ""))
			(mathjax-script-string (if mathjax-script mathjax-script ""))
			(shell (if posix-shell posix-shell "/bin/sh"))
			(clisp (if clisp-name clisp-name "clisp"))
			(cmucl (if cmucl-name cmucl-name "lisp"))
			(scl (if scl-name scl-name "lisp"))
			(acl (if acl-name acl-name "acl"))
			(openmcl (if openmcl-name openmcl-name "mcl"))
			(sbcl (if sbcl-name sbcl-name "sbcl"))
			(ecl (if ecl-name ecl-name "ecl"))
			(gcl (if gcl-name gcl-name "gcl"))
			(files (list "maxima-local.in" "src/maxima.in" "src/maxima.bat.in" "src/autoconf-variables.lisp.in" 
			     "doc/info/include-maxima.texi.in" "doc/info/texi2html.init.in"))
			(substitutions))

  (if interactive
	(progn
	  (setf prefix (read-with-default "Enter the Maxima directory" prefix))
	  (setf win32-string (read-with-default "Is this a Windows system? (true/false)" win32-string))
	  (setf shell (read-with-default "Posix shell (optional)" shell))
	  (setf maxima-version-name (read-with-default "Maxima version" maxima-version-name))
	  (setf manual-version-name (read-with-default "Manual version" manual-version-name))
	  (setf mathjax-enable-string (read-with-default "Mathjax enable string" mathjax-enable-string))
	  (setf mathjax-script-string (read-with-default "Mathjax script string" mathjax-script-string))
	  (setf clisp (read-with-default "Name of the Clisp executable (optional)" clisp))
	  (setf cmucl (read-with-default "Name of the CMUCL executable (optional)" cmucl))
	  (setf scl (read-with-default "Name of the SCL executable (optional)" scl))
	  (setf acl (read-with-default "Name of the Allegro executable (optional)" acl))
	  (setf openmcl (read-with-default "Name of the OpenMCL executable (optional)" openmcl))
	  (setf ecl (read-with-default "Name of the ECL executable (optional)" ecl))
	  (setf gcl (read-with-default "Name of the GCL executable (optional)" gcl))
	  (setf sbcl (read-with-default "Name of the SBCL executable (optional)" sbcl))))

		(setf substitutions (list 
						(cons "@prefix@" (replace-substring prefix "\\" "\\\\"))
			      (cons "@PACKAGE@" "maxima")
			      (cons "@VERSION@" maxima-version-name)
			      (cons "@manual_version@" manual-version-name)
			      (cons "@mathjax_enable@" mathjax-enable-string)
			      (cons "@mathjax_script@" mathjax-script-string)
			      (cons "@host@" "unknown")
			      (cons "@win32@" win32-string)
			      (cons "@default_layout_autotools@" "false")
			      (cons "@POSIX_SHELL@" shell)
			      (cons "@expanded_top_srcdir@" (replace-substring prefix "\\" "\\\\"))
			      (cons "@lisp_only_build@" "t")
			      (cons "@DEFAULTLISP@" *maxima-lispname*)
			      (cons "@CLISP_NAME@" clisp)
			      (cons "@CMUCL_NAME@" cmucl)
			      (cons "@SCL_NAME@" scl)
			      (cons "@ACL_NAME@" acl)
			      (cons "@OPENMCL_NAME@" openmcl)
			      (cons "@ECL_NAME@" ecl)
			      (cons "@GCL_NAME@" gcl)
			      (cons "@SBCL_NAME@" sbcl)))

	(if verbose
	(mapc #'(lambda (pair) (format t "~a=~a~%" (first pair) (rest pair)))
	      substitutions))

  (mapc #'(lambda (filename)
		; We don't want "texi2html.init.in" to be reduced by (replace-substring filename ".in" "") to "texi2htmlit". So we use subseq instead.
	  (let ((out-filename (subseq filename 0 (- (length filename) 3))))
			(process-file filename out-filename substitutions)
			(format t "Created ~a~%" out-filename))) files)))
