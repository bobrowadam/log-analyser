(ql:quickload :jonathan)
(ql:quickload :uuid)
(defpackage :log-analiser
  (:use :common-lisp :jonathan :uuid)
  (:export :main))

(defvar *analised-log* nil)

(defun plist-keys (plist)
  (loop
     for (key value . rest) on plist
     by #'cddr
     collect key))

(defun list-contains? (l-1 l-2)
  (if (equal (intersection l-1 l-2) l-1)
      t
      nil))

(defun find-aggregat (log-line aggregate-list)
  (find-if (lambda (list-entry)
             (list-contains? (plist-keys list-entry)
                             (getf log-line :matchers)))
           aggregate-list))

(defun process-line (line)
  (format t "processing line: ~{~a, ~}" line)
  (let* ((aggregator (or (find-aggregat line *analised-log*)
                         (create-incident line)))
         (processed-line (plist-kv aggregator :events line)))
    (push processed-line *analised-log*)))

(defun create-incident (event)
  (let ((matchers (plist-keys event))
        (id (uuid:make-v4-uuid))
        (events '())
        (start-time (get-universal-time)))
    (list
     :matchers matchers
     :id id
     :start-time start-time
     :events events)))

(defun dump-saved-log ()
  (dolist (line *analised-log*)
    (format t "~{key: ~t~a~%~}~%" line)))

(defun prompt-read (prompt)
  (format *query-io* "~a " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun read-log ()
  (let* ((log-path (prompt-read "Enter log file path:"))
         (in (open log-path)))
    (loop (process-line (jonathan:parse (read-line in)))
       (if (not (y-or-n-p "Read next line? [y/n]: ")) (return)))
    (close in)))

(defvar *menu-options*
  '(1 "Read log"
    2 "Print log"
    3 "Delete log"
    4 "Exit"))

(defun delete-log ()
  (setf *analised-log* nil))

(defun main-menu ()
  (loop (format t "~{~a:~10t~a~%~}~%" *menu-options*)
     (let* ((option (prompt-read "Choose an option")))
       (cond ((equal option "1") (read-log))
             ((equal option "2") (dump-saved-log))
             ((equal option "3") (delete-log))
             ((equal option "4") (return))
             (t (return))))))

(defun main ()
  (main-menu))
(setf mama '((:a (1))))
(push (list :a ((getf (assoc :a mama) :a))) mama)
(assoc :a mama )
(getf (cons :c (cons 2 '(:a 1 :b 2))) :c)

(defmacro plist-kv (plist k v)
  `(cons ,k (cons ,v ,plist)))
