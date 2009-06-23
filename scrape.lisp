(in-package "COMMON-LISP-USER")

;;; in the tradition of forth

(defvar *pad*)

(defun get-page (url &key parameters)
  (setf *pad*
        (drakma:http-request url :parameters parameters)))

(defun pluck (pattern)
  (multiple-value-bind (match regs) (cl-ppcre:scan-to-strings pattern *pad*)
    (declare (ignore match))
    (if regs
        (svref regs 0)
        nil)))



;;; List of the known bugs gleaned from ghostery sources.

(defvar *bugs* nil)

(defun establish-bug-list-if-necessary ()
  (unless *bugs*
    (reset-bug-list)))

(defun reset-bug-list ()
  (let* (result
         (url "http://ghostery.googlecode.com/svn/trunk/firefox/ghostery-statusbar/ghostery/chrome/content/ghostery-bugs.js")
         (page (drakma:http-request url)))
    (cl-ppcre:do-register-groups (bug) ("\"name\": \"([^\"]*)\"" page) 
      (push bug result))
    (setf *bugs* (nreverse result))))


;;; The actual scrapping

(defun scrape-tracker-page (url)
  "Pluck junk out of pages like http://www.ghostery.com/apps/chartbeat."
  (get-page url)
  (format t "~&~6A ~30A ~20A ~A"
          (let ((n (pluck "found on.*<b>([\\d,]+)</b>")))
            (when n
              (parse-integer (delete #\, n))))
          (pluck "Website: <a rel=\"nofollow\" href=\"([^\"]*)\"")
          (pluck "<h1>([^<]*)</h1>")
          url))

(defun scrape-trackers-via-google ()
  "Use google to find apps pages on ghostery.com, then scrape em."
  (loop
    with u = "http://www.google.com/search?q=site:ghostery.com+%22Application+Owner%22&hl=en&sa=N"
    for i in '("0" "10" "20" "30" "40")
    as p = (get-page u :parameters `(("start" . ,i)))
    do
 (cl-ppcre:do-register-groups (link)
     ("href=\"(http://www.ghostery.com/apps/[^\"]*)\"" p)
   (scrape-tracker-page link))))

(defun scrape-trackers-via-bugs ()
  (establish-bug-list-if-necessary)
  (loop for bug in *bugs*
        as url = (concatenate 'string "http://www.ghostery.com/apps/"
                              (nstring-downcase
                               (substitute #\_ #\space bug)))
        do (scrape-tracker-page url)))

