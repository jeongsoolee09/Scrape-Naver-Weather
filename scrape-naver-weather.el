;;; scrape_weather.el ---  -*- lexical-binding: t; -*-

(require 'cl-lib)


;;; Parse the target website's HTML. ================
;;; =================================================


(defun parse-naver-weather-html ()
  "parse the HTML document retrieved by making a request to a given url."
  (set-buffer (url-retrieve-synchronously "https://weather.naver.com/"))
  (let ((naver (libxml-parse-html-region (point-min) (point-max))))
    (caddr naver)))


(defun find-by-tag (data tag)
  (let ((res))
    (subst nil nil data :test (lambda (a b)
                                (when (and (listp b)
                                           (eql (car b) tag))
                                  (push b res))
                                nil))
    res))


(defun find-by-tag-and-attribute (data tag attr)
  "find the tag containing"
  (let ((res))
    (subst nil nil data :test (lambda (a b)
                                (when (and (listp b)
                                           (eql (car b) tag)
                                           (-contains? (get-attribute-list b) attr))
                                  (push b res))
                                nil))
    res))


(defun dotted-pairp (list)
  (and (listp list) (not (listp (cdr list))))) 


(defun get-attribute-list (parsed-html-fragment)
  (let ((data (cadr parsed-html-fragment)))
    (when (-all? #'dotted-pairp data)
      data)))


(defun has-attribute-list (tag)
  (-all? #'dotted-pairp (cadr tag)))


(defun get-attribute-by-name (attr-list attr-name)
  (dolist (attr attr-list)
    (when (eql (car attr) attr-name)
      (return (cdr attr)))))


(defun extract-hourly-weather-single (list)
  (assert (eql (car list) 'li))
  (assert (has-attribute-list list))
  (let* ((attr-list (get-attribute-list list))
         (tmpr (get-attribute-by-name attr-list 'data-tmpr))
         (ymdt (get-attribute-by-name attr-list 'data-ymdt))
         (weather (get-attribute-by-name attr-list 'data-wetr-txt)))
    (list tmpr ymdt weather)))


(defun split-ymdt (ymdt)
  (let ((year (substring ymdt 0 4))
        (month (substring ymdt 4 6))
        (day (substring ymdt 6 8))
        (time (substring ymdt 8 10)))
    (concat year "-" month "-" day " " time "시")))


(defun get-hourly-weather (parsed-html)
  (let ((lists (find-by-tag parsed-html 'li))
        (acc))
    (dolist (list lists)
      (destructuring-bind (tmpr ymdt weather) (extract-hourly-weather-single list)
        (when (or (not (null tmpr))
                  (not (null ymdt))
                  (not (null weather)))
          (push (list tmpr (split-ymdt ymdt) weather) acc))))
    acc))


(defun info-to-string-inner (acc list)
  (if (null list)
      acc
    (destructuring-bind (tmpr ymdt weather) (car list)
      (info-to-string-inner (concat acc ymdt " " tmpr "도 " weather "\n") (cdr list)))))


(defun info-to-string (info)
  (info-to-string-inner "" (seq-take info 5)))


(defun display-hourly-weather ()
  (interactive)
  (-> (get-hourly-weather (parse-naver-weather-html))
      (info-to-string)
      (message)))


(defun has-attribute-value (attr-list value)
  (reduce (lambda (acc attr) (or acc (equal (cdr attr) value)))
          attr-list
          :initial-value nil))


(defun extract-sunset-uv-fd-ufd (ul)
  (assert (has-attribute-value (get-attribute-list ul) "today_chart_list"))
  (let* ((raw-data (find-by-tag ul 'strong))
         (data (mapcar #'caddr raw-data))
         (raw-values (find-by-tag ul 'em))
         (values (mapcar #'caddr raw-values)))
    (-zip data values)))


(defun get-sunset-uv-fd-ufd (parsed-html)
  (interactive)
  (let ((ul (find-by-tag-and-attribute
             parsed-html
             'ul
             '(class . "today_chart_list"))))
    (extract-sunset-uv-fd-ufd (car ul))))


;;; Organize the information on a new org-buffer. ====
;;; ==================================================


(defun create-buffer ()
  (let ((new-buffer (set-buffer (generate-new-buffer "Naver Weather"))))
    (with-current-buffer new-buffer
      (org-mode))
    new-buffer))


(defun insert-title (buffer)
  "Insert the title for the org-buffer, and insert two newlines."
  (with-current-buffer buffer
      (insert "* Naver Weather")
    (move-to-column (point-max))
    (newline)
    (newline))
  buffer)


(defun weather-data->org-table-inner (acc weather-data)
  (if (null weather-data)
      acc
    (progn
      (destructuring-bind (temp date weather) (car weather-data)
        (let ((concatted (concat acc "| " date " | " weather " | " temp " |\n")))
          (weather-data->org-table-inner concatted (cdr weather-data)))))))


(defun weather-data->org-table (buffer weather-data)
  (with-current-buffer buffer
    (let ((concatted (weather-data->org-table-inner "" weather-data)))
      (progn
        (insert concatted)
        (org-table-align)
        (previous-line)
        (org-table-insert-hline)
        (insert-hline-at-top))))
  buffer)


(defun goto-top-of-table ()
  (goto-char (org-table-begin)))


(defun insert-hline-at-top ()
  (let ((current-prefix-arg 4))         ; emulate C-u
    (progn
      (goto-top-of-table)
      (call-interactively 'org-table-insert-hline))))


(defun finedust->org-table-inner (acc fd-data)
  (if (null fd-data)
      acc
    (progn
      (let* ((slice (car fd-data))
             (index (car slice))
             (value (cdr slice)))
        (let ((concatted (concat acc "| " index "| " value "|\n")))
          (finedust->org-table-inner concatted (cdr fd-data)))))))


(defun finedust->org-table (buffer fd-data)
  (with-current-buffer buffer
    (let ((concatted (finedust->org-table-inner "" fd-data)))
      (progn
        (insert concatted)
        (org-table-align)
        (previous-line)
        (org-table-insert-hline)
        (insert-hline-at-top))))
  buffer)


(defun goto-end-effect (buffer)
  (progn
    (goto-char (point-max))
    buffer))


(defun insert-newline-effect (buffer)
  (progn
    (newline)
    buffer))


(defun display-org-buffer (weather-data fd-data)
  (-> (create-buffer)
      (insert-title)
      (weather-data->org-table weather-data)
      (goto-end-effect)
      (insert-newline-effect)
      (finedust->org-table fd-data)
      (display-buffer)))


(defun get-naver-weather ()
  (interactive)
  (display-org-buffer
   (get-hourly-weather (parse-naver-weather-html))
   (get-sunset-uv-fd-ufd (parse-naver-weather-html))))


(provide 'scrape-naver-weather)

;;; scrape_weather.el ends here
