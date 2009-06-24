(in-package "COMMON-LISP-USER")

(defclass snoop ()
  ((name :initarg :name)
   (count :initarg :count)
   (ghostery-page :initarg :ghostery-page)
   (homepage :initarg :homepage)))

(defvar *snoops* (make-hash-table :test #'equal))

(defmacro snoop-info (name) `(gethash ,name *snoops*))

(defun load-snoops (pathname)
  (clrhash *snoops*)
  (with-open-file (s pathname)
    (loop for line? = (read-line s nil nil)
          while line?
          as (count homepage other)
            = (cl-ppcre:split "\\s+" line? :limit 3)
          as (name ghostery-page) = (cl-ppcre:split "\\s+http:" other :limit 2)
          as info = (make-instance 'snoop
                         :name name
                         :count (read-from-string count)
                         :ghostery-page ghostery-page
                         :homepage homepage)
          do
       (setf (snoop name) info)))
  *snoops*)


(defun tops ()
  (loop
    with snoops = (loop for s being each hash-value of *snoops* collect s)
    with sorted-snoops = (sort snoops  #'> :key #'(lambda (x) (or (slot-value x 'count) 0)))
    for snoop in sorted-snoops
    do (with-slots (count name) snoop
         (format t "~&~6d ~A" count name))))
