(defun fetch-bbc-rss ()
  (let ((tmp "/tmp/bbc.xml"))
    (sb-ext:run-program "curl"
                        '("-sL" "https://feeds.bbci.co.uk/news/rss.xml" "-o" "/tmp/bbc.xml")
                        :output t :error t :search t)
    (with-open-file (in tmp :direction :input :external-format :utf-8)
      (loop for line = (read-line in nil)
            while line
            collect line))))

(defun clean-cdata (text)
  (let ((start (search "<![CDATA[" text))
        (end (search "]]>" text)))
    (if (and start end)
        (subseq text (+ start 9) end)
        text)))

(defun extract-titles (lines)
  (let ((titles '()))
    (dolist (line lines)
      (when (and (search "<title>" line)
                 (not (search "BBC News" line)))
            (let* ((start (+ (search "<title>" line) (length "<title>")))
                   (end (search "</title>" line))
                   (raw (subseq line start end)))
              (when end
                    (push (string-trim " " (clean-cdata raw)) titles))))
      (when (>= (length titles) 3)
            (return)))
    (nreverse titles)))

(defun save-headlines (headlines)
  (let* ((date (multiple-value-bind (sec min hour day mon year) (get-decoded-time)
                 (declare (ignore sec min hour))
                 (format nil "~4,'0d/~2,'0d-~2,'0d" year mon day)))
         (year (subseq date 0 4))
         (filename (format nil "news/~A/~A.md" year (subseq date 5))))
    (ensure-directories-exist filename)
    (with-open-file (out filename
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create
                         :external-format :utf-8)
      (format out "# Top 3 Headlines from BBC on ~A~%~%" date)
      (loop for h in headlines
            for i from 1
            do (format out "~A. ~A~%" i h)))))

(defun main ()
  (let ((lines (fetch-bbc-rss)))
    (let ((headlines (extract-titles lines)))
      (save-headlines headlines))))

(main)
