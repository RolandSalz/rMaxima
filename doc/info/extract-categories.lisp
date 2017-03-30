;; Written by Michel Talon, 9-25/2/2017, public.

(declaim (optimize speed))

(defvar *ifile*) ; input .texi file
(defvar *ofile*) ; output decorated .texi file with anchors
(defvar *cfile*) ; corresponding category file
(defparameter *items* '())
(defparameter *categories* (make-hash-table :test 'equal))

(defvar *regex0* (pregexp "^@(?:deffn|defvr|node)"))
(defvar *regex1* (pregexp "^@(?:deffn|defvr) *{[^}]*}  *(\\S*).*"))
(defvar *regex2* (pregexp "^@(deffnx|defvrx|deffn|defvr|end deffn|end defvr|category|node)"))
(defvar *regex3* (pregexp "^@def\\S+ *{[^}]*}  *(\\S*).*"))
(defvar *regex4* (pregexp "^@node  *([^,]*).*"))
(defvar *regex5* (pregexp "@category *{([^}]*)} *"))

#| 
Lines beginning by @node @deffn or @defvr are flagged by regex1 and generate
an anchor, for reference. The last two can be followed resp by deffnx or defvrx
which do not generate an anchor. An example is in Operators, for + - * / and ^.
Such a collection is ended by @end deffn or @end defvr (functions and variables).
All of these , and categories are flagged by regex2.
Categories are on lines beginning by @category, but there may be several of them
on the line. Definitions are of the form @deffn{Function} name ..., and similar for 
def(fn|vr|fnx|vrx). We pick the name with regex3.
Nodes begin by @node followed by texts separated by commas. We pick the 
first text by regex4. For all the previous cases one adds an Item marker with
an appropriate text.
For categories, regex5 picks the name of the category, and one adds a
corresponding entry in a dictionary, with values related Items. Note there is
a bug in pregxp such that the pattern "@category{(.." does not work, since {
is interpreted like in y{3}, that is 3 copies of y. Workaround y *{ works.
Another feature of pregexp is that one has to put deffnx before deffn in regex2
if one wants to pick either deffn or deffnx correctly. Finally one has to pick by
hand the multiple matches in @category, pregexp picks the first one.
|#

(defun pick-multiple (pat  line)
"Returns list of (sub)matches of pattern in line."
(let ((spos 0)  (matches '()) result)
  (loop
     (setq result (pregexp-match-positions pat line spos))
     (when (not result) (return))
     (push (subseq line (caadr result) (cdadr result)) matches)
     (setq spos (cdar result)))
  (nreverse matches)))

(defun add-anchor (match)
  (write-line  (strcat "@anchor{Item: " match "}") *ofile*))

#|
One filters each line of all the .texi files and pick those
which produce an item marker. A list *items* starts from empty, begins
to be filled by node items. Encountering a definition (deffn or defvr)
the list is reset to its item, then continues to be filled by ensuing
items produced by deffnx or defvrx until one encounters the
corresponding enddef where the list is emptied again. However if one
encounters a node definition in between it resets the *items* list. I
think this is buggy.
|#

(defun filter-item-lines (match line) 
  (cond ((or (equal match "deffn") (equal match "defvr")) 
	 (let ((match1 (second (pregexp-match *regex3* line))))
	   (setf  *items* (list match1)))) 
	((or (equal match "deffnx") (equal match "defvrx")) 
	 (let ((match1 (second (pregexp-match *regex3* line))))
	   (pushnew  match1 *items* :test #'equal)))
   	((equal match "node") 
	 (let ((match1 (second (pregexp-match *regex4* line))))
	   (setf  *items* (list match1))))
	((equal match "category")
	 (setf *items* (nreverse *items*))
	 (dolist (match1 (pick-multiple *regex5* line))
	   (regist-category match1  *items* *categories*)))))

#|  
The first ref is to the deffn defvr, the second ref lists the extensions.
Hence the purpose of the duplication. The registration to one category may appear multiple times
in the .texi files, so one has to concatenate the various contributions.
Here i have borrowed from Jinsong Zhao's patch.
|#

(defun regist-category (match items dict)
  (flet ((do-pairs (mylist)
	   (let ((list-pairs '()) (item0 (car mylist)))
	     (dolist (item mylist) (push (list item0 item) list-pairs))
	     (nreverse list-pairs))))
;; the following works if match is not in the dictionary, it creates an entry,
;; and if match is in the dictionary it appends to the entry. The probability
;; of adding duplicate pairs is very small
    (setf (gethash match dict)
	  (append (gethash match dict) (do-pairs items)))))

 
(defun write-category-file (dict dir)
  ;; Dumps the dictionary to a category file, when it is completely filled
  ;; Borrowed from Jinsong Zhao

  (loop for key being the hash-keys in dict using (hash-value items) do 
       (setq *cfile* (open (strcat dir "Category-" key ".texi")
			   :direction :output :if-exists :overwrite
			   :if-does-not-exist :create))
       (format *cfile* "~&@anchor{Category: ~A}" key)
       (format *cfile* "~&@opencatbox")
       (format *cfile* "~&@b{Category: ~A}~%~%" key)
       (if (> (length items) 0)
	   (flet ((sort-pairs (x y) 
		    (if (equal (first x) (first y)) 
			(string< (second x)(second y))  
			(string< (first x)(first y)))))
	     (setf items (sort items #'sort-pairs))))
       (format *cfile* "~&@ref{Item: ~A, ~A}" (car (car items)) (cadr (car items)))
       (loop for pair in (rest items) do
	    (format *cfile* "~&@html~%&middot;~%@end html")
	    (format *cfile* "~&@ref{Item: ~A, ~A}" (car pair) (cadr pair)))
       (format *cfile* "~&@closecatbox")
       (format *cfile* "~&")
       (close *cfile*)))

#| 
One has to specify the complete path of files, otherwise file
operations work on the original files instead of on the copies in the
working dir.
|#

(defun get-all (ext dir)  ; list all files with given extension in dir
(let ((regex (strcat "([^/|\\\\]+)." ext "$"))
      (pattern (strcat dir "*." ext)))
  ;; Take care of the Unix and Windows conventions for /
  (flet ((path-sanitize (path)
	   (car (pregexp-match regex (namestring path)))))
    (mapcar  #'path-sanitize (directory pattern)))))


(defun run-perl (type origin dir)
  (let ((texi2html (strcat origin "texi2html"))
	(manual.css (strcat origin "manual.css"))
	(texi2html.init (strcat origin "texi2html.init"))
	(selector (if (equal type "split") " -split_chapter" " "))
	(place (if (equal type "split") (strcat " --output=" dir)
		   " --output=maxima_singlepage.html")))
    (run-program (strcat "perl " texi2html  selector " --lang=en "
			 place " --css-include=" manual.css 
			 " --init-file " texi2html.init " maxima.texi"))))

(defmacro process-files ((ext dir) &body body) 
  "Factor common actions for texi and html files"
  `(dolist (ifile (get-all ,ext ,dir))
     (when (equal ,ext "texi")
       (setq *items* '())
       (if (equal ifile "maxima.texi") (go next)))
     (setq *ifile* (open (strcat ,dir ifile)))
     (setq *ofile* (open (strcat ,dir ifile ".tmp") 
			 :direction :output :if-exists :overwrite
			 :if-does-not-exist :create))
     (loop 
	(let ((line (read-line *ifile* nil nil)))
	  (if (not line) (return))
	  ,@body))
	  (close *ifile*)
	  (close *ofile*)
		(delete-file (strcat ,dir ifile))
	  (rename-file (strcat ,dir ifile ".tmp") (strcat ,dir ifile))
     next))


(defun  process-all-texi-files (dir)
  (process-files ("texi" dir)
		 ;; add anchors for deffn defvr and nodes
		 (if (pregexp-match *regex0* line)
		     (let ((match (pregexp-match *regex1* line)) 
			   (match1 (pregexp-match *regex4* line)))
		       (if match
			   (add-anchor (second match))
			   (if match1 (add-anchor (second match1))))))
		 (write-line line *ofile* )
		 ;; build the dictionary
		 (let ((match (pregexp-match *regex2* line)))
		   (if match  (filter-item-lines (second match) line))))
  (write-category-file *categories* dir)
  (fix-maxima-texi dir))

(defun fix-maxima-texi (dir)
;; actions are very specific
(with-open-file (*ifile* (strcat dir "maxima.texi")) 
(with-open-file (*ofile* (strcat dir "maxima.texi.tmp") :direction :output 
			 :if-exists :overwrite :if-does-not-exist :create)
(setq *regex0* (pregexp "^@bye")) ; only do this after process-all-texi-files
(loop (let ((line (read-line *ifile* nil nil)))
	(if (not line) (return))
	(if (pregexp-match *regex0* line)
	    (write-line '" " *ofile*)
	    (write-line line *ofile*))))
(write-line "@node Documentation Categories" *ofile*)
(write-line "@chapter Documentation Categories" *ofile*)
(loop for key being the hash-keys in *categories* do 
     (let ((line (strcat "@include Category-" key ".texi")))
       (write-line line *ofile*)))
(write-line '"@bye" *ofile*)))
(delete-file (strcat dir "maxima.texi"))
(rename-file (strcat dir "maxima.texi.tmp") (strcat dir "maxima.texi")))

;;  Clean up the texi2html output. Only do this after process-all-texi-files

(defun process-all-html-files (dir)
  (setq *regex1* (pregexp "^&middot;$"))
  (setq *regex2* (pregexp "<p>(<a href=\".*\">Category: .*</a>)"))
  (setq *regex3* (pregexp "<a href=\"(.*)\">Category: (.*)</a>"))
  (setq *regex4* (pregexp "<a href=\"(.*)\">Item: (.*)</a>"))
  (process-files ("html" dir)
		 (setq line (pregexp-replace *regex1* line ""))
		 (setq line (pregexp-replace *regex2* line 
					     "<p>Categories:\&nbsp;\&nbsp;\\1"))
		 (setq line (pregexp-replace *regex3* line
					     "<a href=\"\\1\">\\2</a>"))
		 (setq line (pregexp-replace *regex4* line
					     "<a href=\"\\1\">\\2</a>")) 
		 (write-line line *ofile* )))
