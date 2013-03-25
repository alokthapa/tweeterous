(ql:quickload "xmls")
(ql:quickload "csv-parser")
(ql:quickload "cl-fad")

(defvar *db* (list))

;;utility fns 
(defun file-string (path)
  "Sucks up an entire file from PATH into a freshly-allocated string,
      returning two values: the string and the number of bytes read."
  (with-open-file (s path)
    (let* ((len (file-length s))
           (data (make-string len)))
      (values data (read-sequence data s)))))

(defun split-by (string sep)
  (loop for i = 0 then (1+ j)
     as j = (position sep string :start i)
     collect (subseq string i j)
     while j))

(defun split-by-one-space (string)
  (split-by string #\Space))

;;

(defun parse-file (path)
  (let ((str (file-string path)))
    (xmls:parse str)))

(defun get-posts (nodes)
  (cdddr (cddddr (cddddr (cdaddr nodes)))))

(defun get-title (item)
  (caddr (nth 2  item)))

(defun get-date (item)
  (caddr (nth 4 item)))

(defun get-text (item)
  (caddar (remove-if-not #'(lambda (x) 
                             (and (consp x) 
                                  (consp (car x))
                                  (stringp (caar x))
                                  (string= (caar x)
                                           "encoded")))
                         item)))

(defun get-comments (item)
  (let ((comments 
         (remove-if-not #'(lambda (x) 
                            (and (consp x) 
                                 (consp (car x))
                                 (stringp (caar x))
                                 (string= (caar x)
                                          "comment")))
                        item)))
    (mapcar #'(lambda (comment)
                (list :author (get-comment-author comment)
                      :date (parse-post-date (get-comment-date comment))
                      :text (get-comment-text comment)))
            comments)))


(defun get-comment-author (comment)
  (caddr (cadddr comment)))

(defun get-comment-date (comment)
  (car (cddadr (cddddr comment))))

(defun get-comment-text (comment)
  (caddr(cadddr (cddddr comment))))


(defparameter months '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun parse-post-date (date)
  (destructuring-bind (weekday month day time zone year) (split-by-one-space date)
    (destructuring-bind (hour mins secs) (split-by time #\:)
                      (encode-universal-time 
                       (parse-integer secs)
                       (parse-integer mins)
                       (parse-integer hour)
                       (parse-integer day)
                       (1+ (position month months :test #'equal))
                       (parse-integer year)
                       (/ (parse-integer zone) 100)))))

(defun add-post-to-db (item)
  (push (list 
         :type :post
         :title (get-title item)
         :date (parse-post-date (get-date item))
         :text (get-text item)
         :comments (get-comments item))
        *db*))

(defun handle-posterous-export (file)
  (if (probe-file file)
      (progn
        (mapc #'(lambda (item)
                    (add-post-to-db item))
                (get-posts (parse-file file)))
        (print "successfully imported file"))
      (print "could not find the file")))

(defun prompt-post-path ()
  (print "Please input the path to your posterous wordpress_export_1.xml [blank to quit]: ")
  (let ((file (read-line)))
    (if (string/= file "")
        (progn
          (handle-posterous-export file)
          (prompt-post-path)))))

;; read tweets

(defparameter tweetcsv "/home/alokt/projs/oldtweets/data/csv/2007_10.csv")

(defparameter tweetstr (file-string tweetcsv))

(defun parse-tweet-date (times)
  (destructuring-bind (date time zone) (split-by times #\Space) 
    (destructuring-bind (year month day) (split-by date #\-) 
      (destructuring-bind (hour mins secs) (split-by time #\:)
        (list year month day hour mins secs zone )
        (encode-universal-time 
         (parse-integer secs)
         (parse-integer mins)
         (parse-integer hour)
         (parse-integer day)
         (parse-integer month)
         (parse-integer year)
         (/ (parse-integer zone) 100))))))
      
(defun add-tweet-to-db (item)
  (push (list 
         :type :tweet
         :title (concatenate 'string  "Tweeted on " (nth 5 item))
         :date (parse-tweet-date (nth 5 item))
         :text (nth 7 item)
         :comments nil)
        *db*))

(defun parse-tweet-file (file)
  (csv-parser:map-csv-file file #'add-tweet-to-db :skip-lines 1))

(defun add-all-tweets (path)
  (mapc #'(lambda (file)
            (parse-tweet-file file))
        (cl-fad:list-directory path)))

(defun prompt-tweet-path ()
  (print "Please input your tweet csv folder: ")
  (let ((path (read-line)))
    (if (cl-fad:directory-exists-p path)
        (add-all-tweets path))))


(defun top-prompt ()
  (progn
    (prompt-tweet-path)
    (prompt-post-path)))
        
   
  


  





        
        
    


