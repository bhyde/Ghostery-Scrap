(package "COMMON-LISP-USER")

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

(defun scrap-trackers-via-google ()
  (loop
    with u = "http://www.google.com/search?q=site:ghostery.com+%22Application+Owner%22&hl=en&sa=N"
    for i in '("0" "10" "20" "30" "40")
    as p = (get-page u :parameters `(("start" . ,i)))
    do
 (cl-ppcre:do-register-groups (link)
     ("href=\"(http://www.ghostery.com/apps/[^\"]*)\"" p)
   (format t "~&~{~4A ~40A ~A~}"  (scrap-tracker-page link)))))

(defun scrap-tracker-page (url)
  (get-page url)
  (list
   (pluck "found on.*<b>(\\d+)</b>")
   (pluck "Website: <a rel=\"nofollow\" href=\"([^\"]*)\"")
   (pluck "<h1>([^<]*)</h1>")))))

