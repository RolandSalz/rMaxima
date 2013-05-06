;; This file should be loaded into a Maxima image, rather than run "stand-alone"
;;
;; Use something like
;;
;;   maxima-local -l sbcl --very-quiet \
;;     -r ':lisp (progn (load (compile-file "/path/to/parse-info.lisp")) (funcall (find-symbol "make-info-offsets" :parse-info) #p"maxima.info"))'
;;
;; Yes, yuck, but ":lisp" doesn't allow more than one line.

(defpackage parse-info
  (:use :cl :cl-info)
  (:export :make-info-offsets)
  (:import-from :maxima :*maxima-lispname*)
  (:import-from :cl-info :info-offset-name :with-open-info-file))
(in-package :parse-info)

;; The output format
;;
;;   The file contains a single, READable, list. The elements of the list are
;;   lists of the form (NAME FILENAME START LENGTH).
;;
;;   START and LENGTH in the above may be byte offsets or character offsets or
;;   pretty much anything else, but we know that they are appropriate values for
;;   FILE-OFFSET on the current lisp. (This may vary between lisps, which is why
;;   we output an implementation-specific offset table). FILENAME is the file
;;   name of the portion of the info document that we read. (We don't use
;;   pathnames, since then you have to worry about accidentally including
;;   absolute directory info which then breaks when you install)

;; How the code works:
;;
;;   We read in the top-level info file ("maxima.info"), taking note of the
;;   Indirect and Tag Table sections. The former lists other files needed to
;;   get the whole of the info document, together with their line offsets within
;;   the composite document.
;;
;;   Next, we read in the last file of the document. Parsing its last section
;;   gives us an index, which gives topics with line offsets relative to a
;;   parent node (by name).
;;
;;   While doing so, we scanned the last file to get a list of nodes with
;;   their line ranges, together with a list of the starts of sections and
;;   their line numbers and the positions of function or variable declarations
;;   (together with starting and ending line number).
;;
;;   Now we can integrate this information with our index and tag table. For
;;   any indexed topic that is defined in this file, we look in our list of
;;   function and variable declarations to calculate the correct starting and
;;   ending line numbers. Then we have enough information to replace it with a
;;   complete-info-topic. Similarly, we can combine our list of starting line
;;   numbers with the nodes in the current file to calculate an offset and
;;   length for those nodes.
;;
;;   Now we map through the rest of the files, integrating their contents as
;;   above.

;; The classes to represent info documents ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass complete-info-topic (doc-topic)
  ((start :reader complete-topic-start :initarg :start)
   (length :reader complete-topic-length :initarg :length)
   (pathname :reader complete-topic-pathname :initarg :pathname))
  (:documentation
   "A topic where we know enough to actually extract and print it"))

(defclass partial-info-node (doc-topic)
  ((pathname :reader info-node-pathname :initarg :pathname)
   (stripped-name :reader info-node-stripped-name :initarg :stripped-name))
  (:documentation "Represents an info node of some info document. PATHNAME
points to the file where it is defined. STRIPPED-NAME is the name minus commas
since Texinfo 'helpfully' strips them from the names in indices."))

;; Put dependencies this way around so that specialising on PARTIAL-INFO-NODE
;; doesn't trump specialising on COMPLETE-INFO-TOPIC.
(defclass complete-info-node (complete-info-topic partial-info-node)
  ((first-line :reader info-node-first-line :initarg :first-line)
   (last-line :reader info-node-last-line :initarg :last-line)
   (numbering :reader info-node-numbering :initarg :numbering))
  (:documentation
   "FIRST-LINE and LAST-LINE are the first and last line indices of the
node. NUMBERING is the section number of the node, represented like (1 2 3) for
section 1.2.3."))

(defclass partial-info-index-entry (doc-topic)
  ((line-offset :reader info-index-line-offset :initarg :line-offset)
   (stripped-node :reader info-index-stripped-node :initarg :stripped-node))
  (:documentation
   "LINE-OFFSET is the number of lines into the node to the end of the header of
the topic (this might not point to the first line of the topic if the header is
broken across multiple lines)."))

(defclass info-doc (doc)
  ((nodes :accessor info-doc-nodes :initarg :nodes)
   (node-lookup :accessor info-doc-node-lookup :initform nil)
   (index :accessor info-doc-index :initarg :index :initform nil)
   (pathname :reader info-doc-pathname :initarg :pathname))
  (:documentation
   "Represents a Texinfo document, possibly spread over multiple files."))

;; Utility functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *info-separator* "")
(defvar *info-tag-table-separator* (schar "" 0))

(defun info-separator-p (line) (string= line *info-separator*))

(defun starts-with-p (sequence start)
  (and (>= (length sequence) (length start))
       (equal (subseq sequence 0 (length start)) start)))

(defmacro set-next-line (stream line &optional from)
  "Used to set the next line from stream in a loop, returning if we've got EOF."
  `(when (eq (setf ,line (read-line ,stream nil :eof)) :eof)
     ,(if from
          `(return-from ,from)
          '(return))))

(defun jump-to-info-section (stream section-header)
  "Jump forward in STREAM until finding a section that starts with a line equal
to the string SECTION-HEADER. Returns T if we found one and NIL otherwise."
  (let (line (expecting t))
    (loop
       (set-next-line stream line)
       (cond
         ((info-separator-p line)
          (setf expecting t))
         (expecting
          (when (string= line section-header)
            (return t))
          (setf expecting nil))
         (t
          (setf expecting nil))))))

(defmacro collecting-loop (&body forms)
  "Run FORMS in a loop with #'COLLECT bound to a collector."
  (let ((acc (gensym)))
    `(let ((,acc))
       (flet ((collect (x) (push x ,acc)))
         (loop ,@forms)
         (nreverse ,acc)))))

;; Multi-file handling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun info-files-alist (pathname)
  "Calculate a lookup alist for the info file at PATHNAME, as used by
INFO-DOCUMENT. See INFO-INDIRECT-FILES for the format. The list is in order of
OFFSET."
  (cons (cons 0 pathname) (info-indirect-files pathname)))

(defun info-indirect-files (pathname)
  "Search the info file at PATHNAME for an \"Indirect:\" section. If found,
return a list of conses of the form (OFFSET . PATHNAME) where OFFSET is the
start of the data represented by the line in the logical data stream and
PATHNAME is the resolved pathname of the relevant file."
  (with-open-info-file (stream pathname)
    (when (jump-to-info-section stream "Indirect:")
      (let ((line))
        (collecting-loop
          (set-next-line stream line)
          (when (info-separator-p line) (return))
          (let ((parsed (parse-info-csv-line line)))
            (unless (= 1 (length parsed))
              (format *error-output*
                      "Warning! Malformed Indirect line:~%~A~%~%" line))
            ;; Here we have ((namestring . "offset-num"))
            (let ((namestring (caar parsed))
                  (offset-string (cdar parsed)))
              (collect
                  (cons (handler-case (parse-integer offset-string)
                          (parse-error ()
                            (error "Invalid offset ~S in indirect line ~S"
                                   offset-string line)))
                        (parse-info-namestring pathname namestring))))))))))

(defun parse-info-csv-line (line &optional (offset 0))
  "Parse text that looks like A: <foo>, B: <bar>, C: <baz> into '((A . foo) (B
. bar) (C . baz)). Assumes that the keys do not contain commas or spaces and
there is exactly one space after a key's colon before the value. Starts at
offset."
  (let* ((colon1 (search ": " line :start2 offset))
         (colon2 (when colon1 (position #\: line :start (1+ colon1))))
         (comma (when colon2 (position #\, line :from-end t :end colon2)))
         (next (when colon2
                 (1+ (position #\Space line :from-end t :end colon2)))))
    (when colon1
      (cons (cons (subseq line offset colon1)
                  (subseq line (+ colon1 2) comma))
            (when next (parse-info-csv-line line next))))))

(defun parse-info-namestring (parent-path namestring)
  ;; This handler-case is to handle namestring parsing errors a bit more
  ;; informatively for the user. Triggering this to test is a pain on my laptop
  ;; at least. One way to do so is insert a ^0 character.
  (let* ((where (format nil "in the indirect section of ~S" parent-path))
         (parsed
          (handler-case (parse-namestring namestring)
            (parse-error ()
              (error "Invalid file name ~S found ~A." namestring where))))
         (merged
          (merge-pathnames parsed
                           (make-pathname
                            :directory (pathname-directory parent-path)))))
    ;; Wildcards shouldn't be here, so we throw an error if we found one. I
    ;; suppose it would be cleverer to tell the lisp to parse the namestring
    ;; "literally", but I don't know how to do that and I'm pretty certain we'll
    ;; never have a genuine info file with "*" in the pathname!
    ;;
    ;; Oh, and GCL doesn't have WILD-PATHNAME-P. I guess we'll just be a little
    ;; less bullet-proof there.
    #-gcl
    (when (wild-pathname-p merged)
      (error "Found an unexpected wild pathname ~S ~A." namestring where))
    merged))

;; The tag table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun info-read-tag-table (pathname)
  "Try to read a Texinfo tag table at PATHNAME. Returns the list of tags in
format (name . offset). If unsuccessful, raises an error (since every top-level
Texinfo file should have one)."
  (let ((line) (first t))
    (with-open-info-file (stream pathname)
      (unless (jump-to-info-section stream "Tag Table:")
        (error "Couldn't find tag table in Texinfo file: ~S." pathname))
      (collecting-loop
        (set-next-line stream line)
        (when (info-separator-p line) (return))
        ;; Format is <type>: <name>^?<offset>, except the first line might
        ;; be "(Indirect)"
        (unless (and first (string= line "(Indirect)"))
          (let ((colon (search ": " line)) separator offset)
            (unless colon
              (error "Couldn't find colon in tag table line: ~A." line))
            (cond
              ;; Ignore Ref: lines
              ((string= "Ref" (subseq line 0 colon)))
              ((string= "Node" (subseq line 0 colon))
               (setf separator (position *info-tag-table-separator*
                                         line :start (+ colon 2)))
               (unless separator
                 (error "Couldn't find end of title in tag table line: ~A."
                        line))
               (handler-case (setf offset (parse-integer
                                           (subseq line (1+ separator))))
                 (parse-error ()
                   (error "Couldn't parse offset in tag table line: ~A" line)))
               (collect (cons (subseq line (+ colon 2) separator) offset)))
              (t
               (error "Unrecognised node type in tag table: ~A."
                     (subseq line 0 colon))))))))))

(defun tag-table-to-nodes (files-alist tag-table)
  "Take FILES-ALIST and TAG-TABLE, both calculated from the top-level file of an
info document and return a list of INFO-NODE objects representing the tags."
  (do* ((alist files-alist)
        (tags tag-table (cdr tags))
        (acc))
       ((null tags) (nreverse acc))
    (loop
       while (and (cdr alist) (>= (cdar tags) (caadr alist)))
       do (pop alist))
    (push (make-instance 'partial-info-node
                         :pathname (cdar alist)
                         :name (caar tags)
                         :stripped-name (strip-section-title (caar tags)))
          acc)))

;; Reading the index ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun read-info-index (stream)
  "Read an Info index from STREAM. We assume that this is the last section in
the stream, so we don't bother checking for ^_ and stopping. Return a
list (TOPIC-NAME NODE-NAME DELTA-LINES), where DELTA-LINES is the number of
lines from the start of the node for the start of our topic."
  (flet ((space-p (ch) (eql ch #\Space))
         (find-bracket (line start)
           (search "(line" line :from-end t :start2 start)))
    (let ((line))
      (collecting-loop
        (set-next-line stream line)
        ;; The lines we're interested in look like
        ;;  '* <topic-name>:     <node-name>. (line <line-number>)
        ;; except sometimes the last bit is on the following line.
        (when (starts-with-p line "*")
          (let* ((colon (search ": " line :start2 2))
                 (name-start (when colon
                               (position-if-not
                                #'space-p line :start (1+ colon))))
                 (bracket (when name-start (find-bracket line name-start))))
            (when (and colon name-start)
              (let ((topic-name (subseq line 2 colon))
                    (node-name
                     (string-right-trim
                      "."
                      (subseq line name-start
                              (when bracket
                                (1+ (position-if-not #'space-p line
                                                     :from-end t
                                                     :end (1- bracket))))))))
                (unless bracket
                  (set-next-line stream line)
                  (setf bracket (find-bracket line 0)))
                (when bracket
                  (let ((line-number
                         (parse-integer line
                                        :start (+ bracket 5) :junk-allowed t)))
                    (when line-number
                      ;; With Texinfo 4.*, if you jump <line-number> lines from
                      ;; the ^_ line starting the node, you land on the first
                      ;; line after that header has finished. When there is only
                      ;; one header line for the topic, this means you land at
                      ;; the start of the corresponding text.
                      ;;
                      ;; With Texinfo 5.*, you instead land on the relevant
                      ;; header line (eg. "-- System variable: blah").
                      ;;
                      ;; We subtract one from this number, since as far as we're
                      ;; concerned the node starts on the line after the ^_
                      ;; line.
                      (collect
                          (make-instance
                           'partial-info-index-entry
                           :line-offset (- line-number 1)
                           :name topic-name
                           :stripped-node (strip-section-title node-name)
                           :section node-name)))))))))))))

;; The main (beast of a) parser ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scan-info-file (pathname)
  "Read through an info file, taking note of the positions of nodes and the
positions of functions / variable definitions. Returns a list matching (&key
NODE-DATA FV-LINE-INTERVALS LINE-POSITIONS SECTIONS).

NODE-DATA is a list of tuples (NAME LINE-START LINE-END) where NAME is the name
of the node we spotted, LINE-START is the number of the first line and LINE-END
is the start of the first line not in the node. Here and in FV-LINE-INTERVALS,
line numbers start at 1.

FV-LINE-INTERVALS is a list of conses (LINE-START . LINE-END), giving the
starting and ending line for each function or variable definition in the
file. It is ordered so the numbers are decreasing.

LINE-POSITIONS is a vector of integers whose n'th element is the file-position
of the start of the n'th line.

SECTIONS is a list of lists (TITLE LINE-NUMBER POS STRIPPED NUMBERING), one for
each section found. STRIPPED is the title, stripped of any commas since Texinfo
seems to eat them when making tag tables etc. NUMBERING is a list of numbers,
section 1.2.3 represented by (1 2 3)."
  (with-open-info-file (stream pathname)
    (let ((line) (line-number 0) (pos)
          (nodes) (fv-line-intervals) (lps)
          (se-starts) (this-node)
          (expect-node t) (last-blank t) (node) (sections))
      (flet ((finish-node (&optional (delta 0))
               (when this-node
                 (push (append this-node (list (- line-number delta))) nodes)
                 (setf this-node nil)))
             (finish-se (&optional (delta 0))
               (when se-starts
                 (dolist (start (nreverse se-starts))
                   (push (cons start (- line-number delta)) fv-line-intervals))
                 (setf se-starts nil))))
        (loop
           (setf pos (file-position stream))
           (set-next-line stream line)
           (push pos lps)
           (incf line-number)
           (cond
             ;; Blank line
             ((= 0 (length line)))

             ;; Node separator
             ((info-separator-p line)
              (setf expect-node t)
              (finish-node 1)
              (finish-se 1))

             ;; Node
             ((when expect-node
                (setf expect-node nil
                      node (nth-value 1 (parse-info-node-line line))))
              (setf this-node (list node line-number)))

             ;; Section header
             ((multiple-value-bind (title numbering)
                  (parse-section-header line)
                (when title
                  (push (list title line-number pos
                              (strip-section-title title) numbering)
                        sections)
                  t)))

             ;; Function / variable definition.
             ((and (starts-with-p line " -- ")
                   (>= (length line) 5)
                   (not (eql (elt line 5) #\Space)))
              (when last-blank (finish-se 2))
              (push line-number se-starts)))
           ;; Used next time around!
           (setf last-blank (= 0 (length line))))
        (finish-se 1)
        (finish-node 1)
        (list :node-data (nreverse nodes)
              :fv-line-intervals fv-line-intervals
              :line-positions (coerce (nreverse lps) 'simple-vector)
              :sections (nreverse sections))))))

(defun parse-info-node-line (line)
  "Parse the header for an info node and return the File and Node values if they
exist or NIL if the line is malformed."
  (let* ((parsed (parse-info-csv-line line))
         (file (cdr (assoc "File" parsed :test #'string=)))
         (node (cdr (assoc "Node" parsed :test #'string=))))
    (when (and file node)
      (values file node))))

(defun parse-dotted-number (string &optional (start 0))
  "Read a string and try to parse a dotted integer, ie. one of the form
\"1.2.3\" from it. Return two values: firstly, all the matching numbers as a
list and secondly, the index of the first character that doesn't match."
  (multiple-value-bind (major end)
      (parse-integer string :start start :junk-allowed t)
    (cond
      ((not major) (values nil end))
      ((or (= (length string) end)
           (not (eql (schar string end) #\.))) (values (list major) end))
      (t
       (multiple-value-bind (rest end) (parse-dotted-number string (1+ end))
         (values (cons major rest) end))))))

(defun parse-section-header (line)
  "Parse a line of the form that starts with a dotted list of numbers then a
space then a title. If a match, return <name>."
  (when (and (> (length line) 4)
             (not (eql #\Space (schar line 0)))
             (member (schar line 0) '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)))
    (multiple-value-bind (dotted end) (parse-dotted-number line)
      (when (and dotted
                 (< end (length line))
                 (eql #\Space (schar line end)))
        (values (subseq line (+ end 1)) dotted)))))

;; This tries to canonicalize titles so that we ignore some of the minor
;; variation that Texinfo throws in (just to keep us on our toes). Texinfo:
;;   (1) Occasionally throws away spaces
;;   (2) Sometimes ignores commas
;;   (3) Takes a prefix of long titles (50 chars?)
(defun strip-section-title (title)
  (string-downcase
   (remove-if (lambda (char) (member char '(#\, #\. #\Space)))
              (subseq title 0 (min (length title) 50)))))

;; Integrating new data ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun integrate-info-file! (pathname doc)
  "Integrate the info file at PATHNAME into DOC. Modifies DOC."
  (destructuring-bind (&key node-data fv-line-intervals line-positions sections)
      (scan-info-file pathname)
    (let ((current-index
           (or (info-doc-index doc)
               ;; The index should be the last node in DOC
               (let ((last-node (car (last (info-doc-nodes doc))))
                     (last-node-data (car (last node-data))))
                 (unless (typep last-node 'partial-info-node)
                   (error "The index node has already been completed but ~
                           we have no index."))
                 (unless (string= (doc-topic-name last-node)
                                  (first last-node-data))
                   (error "Trying to integrate the document index, we have a ~
                           file that ends with a section called ~S. We ~
                           expected ~S."
                          (first last-node-data) (doc-topic-name last-node)))
                 (with-open-info-file (s (info-node-pathname last-node))
                   (file-position s (elt line-positions
                                         (1- (second last-node-data))))
                   (read-info-index s))))))
      (setf (info-doc-nodes doc)
            (delete nil
                    (mapcar (lambda (node)
                              (maybe-updated-node node pathname node-data
                                                  line-positions sections))
                            (info-doc-nodes doc))))
      ;; Update the node lookup hash
      (setf (info-doc-node-lookup doc)
            (let ((ht (make-hash-table :test 'equal)))
              (dolist (node (info-doc-nodes doc))
                (setf (gethash (info-node-stripped-name node) ht) node))
              ht))

      (setf (info-doc-index doc)
            (mapcar (lambda (entry)
                      (maybe-updated-index-entry
                       entry doc pathname fv-line-intervals line-positions))
                    current-index))
      (values))))

(defun maybe-updated-node (node pathname node-data line-positions sections)
  "Either return NODE or a new COMPLETE-INFO-NODE object representing NODE, but
with the relevant fields filled in using NODE-DATA, SECTIONS and
LINE-POSITIONS (as returned by SCAN-INFO-FILE). PATHNAME is the current
path-name. If NODE should be at PATHNAME, but we can't find a corresponding
section then return NIL (this might happen if the node had a tag in the
top-level file, but we're ignoring it because we don't expect any interesting
content)"
  (let ((section-data))
    (cond
      ((or (typep node 'complete-info-node)
           (not (equal pathname (info-node-pathname node)))) node)

      ;; Setting SECTION-DATA isn't quite as trivial as you might think it
      ;; should be. Texinfo inserts spaces, commas and full stops seemingly
      ;; randomly and doesn't always reproduce the whole of the name in the tag
      ;; table.
      ((not (setf section-data
                  (or (find (info-node-stripped-name node) sections
                            :test #'string= :key #'fourth)
                      (let ((partial-hits
                             (remove-if-not
                              (lambda (section)
                                (starts-with-p (fourth section)
                                               (info-node-stripped-name node)))
                              sections)))
                        (and (not (cdr partial-hits))
                             (car partial-hits))))))
       nil)

      (t
       (destructuring-bind (title line-number pos stripped numbering)
           section-data
         (let ((this-node-data
                (find-if
                 (lambda (line-interval)
                   (destructuring-bind (line-start line-end) line-interval
                     (<= line-start line-number line-end)))
                 node-data :key #'cdr)))
           (unless this-node-data
             (error "Couldn't find a containing node for section ~A, ~
                      which should be at path ~A, line ~A."
                    title (info-node-pathname node) line-number))
           (make-instance 'complete-info-node
                          :name (doc-topic-name node)
                          :pathname (info-node-pathname node)
                          :stripped-name stripped
                          :numbering numbering
                          :start pos
                          :first-line (second this-node-data)
                          :last-line (third this-node-data)
                          :length (- (elt line-positions
                                          (third this-node-data))
                                     pos))))))))

(defun maybe-updated-index-entry (entry doc pathname
                                  fv-line-intervals line-positions)
  "Returns an INFO-INDEX-ENTRY object based on ENTRY. If ENTRY is already a
COMPLETE-INFO-TOPIC or points to a node that has not yet been fully expanded
then we just return ENTRY. Otherwise, we assume that the node was expanded on
the file pointed to by PATHNAME and use FV-LINE-INTERVALS and LINE-POSITIONS to
complete the index entry."
  (let ((node))
    (cond
      ((typep entry 'complete-info-topic) entry)

      ((not (setf node (gethash (info-index-stripped-node entry)
                                (info-doc-node-lookup doc))))
       (error "Couldn't find the parent node, ~S, for index entry ~S."
              (doc-topic-section entry) (doc-topic-name entry)))

      ((not (typep node 'complete-info-node)) entry)

      (t
       (unless (equal (info-node-pathname node) pathname)
         (error "Found a complete node, ~S, for the index entry ~S, but ~
                 it doesn't seem to have been expanded from the current ~
                 pathname, ~A, but rather from ~A."
                (doc-topic-name entry) (doc-topic-name entry)
                pathname (info-node-pathname node)))
       (let* ((node-start (info-node-first-line node))
              (node-end (info-node-last-line node))
              (defn-start-guess (+ (info-index-line-offset entry)
                                   (info-node-first-line node)))
              (last-pair
               (or (range-contains-defn-p
                    node-start defn-start-guess fv-line-intervals)
                   ;; We didn't find a definition! This is unusual (in fact, as I
                   ;; write this there are only two instances in the index), so
                   ;; the code needn't run fast.
                   (let ((next-topic
                          (car (find-if (lambda (line-num)
                                          (<= defn-start-guess line-num))
                                        fv-line-intervals
                                        :key #'car
                                        :from-end t))))
                     (cons defn-start-guess
                           (if (and next-topic (< next-topic node-end))
                               (1- next-topic)
                               node-end)))))
              (pos-start (elt line-positions (1- (car last-pair))))
              (pos-end (elt line-positions (1- (cdr last-pair)))))
         (make-instance 'complete-info-topic
                        :name (doc-topic-name entry)
                        :section (doc-topic-section entry)
                        :pathname pathname
                        :start pos-start
                        :length (- pos-end pos-start)))))))

(defun range-contains-defn-p (start end fv-line-intervals)
  ;; I would use
  ;;
  ;; (find-if (lambda (line-num) (<= start line-num end))
  ;;          fv-line-intervals :key #'car)
  ;;
  ;; but this runs *much* slower on GCL (factor of 10) and slightly slower on
  ;; other implementations.
  (dolist (se fv-line-intervals nil) (when (<= start (car se) end) (return se))))

;; The "top-level" functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;::::::::::
(defun read-info (pathname)
  "Finds a Texinfo document at PATHNAME (eg #p\"maxima.info\") and read in all
related files (such as #p\"maxima.info-1\" etc.), letting us work out all the
offsets to the relevant bits of the document. Returns the resulting document."
  (let* ((files-alist (info-files-alist pathname))
         (d (make-instance 'info-doc
                           :name (namestring pathname)
                           :pathname pathname
                           :nodes (tag-table-to-nodes
                                   files-alist
                                   (info-read-tag-table pathname)))))
    ;; We have to read the last file first, since that contains the index, so
    ;; just reverse the alist, but missing out the first entry (maxima.info),
    ;; since we've already got everything we want from that.
    (mapc (lambda (cons) (integrate-info-file! (cdr cons) d))
          (reverse (cdr files-alist)))
    d))

(defgeneric info-topic-as-list (topic))
(defmethod info-topic-as-list ((topic complete-info-topic))
  (let ((pathname (complete-topic-pathname topic)))
    (list (doc-topic-name topic)
          (concatenate 'string (pathname-name pathname)
                       "." (pathname-type pathname))
          (complete-topic-start topic)
          (complete-topic-length topic))))

(defun dump-info (info-doc stream)
  "Dump out the two parsed offset tables to a stream, using prin1"
  (let ((*print-length* nil))
    (prin1 (mapcar (lambda (topic) (info-topic-as-list topic))
                   (append (info-doc-nodes info-doc)
                           (info-doc-index info-doc)))
           stream)))

(defun make-info-offsets (pathname)
  (let ((d (read-info pathname)))
    (with-open-info-file (stream (info-offset-name pathname)
                                 :direction :output
                                 :if-exists :supersede)
      (dump-info d stream))
    (values)))