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


(defun info-to-string ()
  (info-to-string-inner "" (seq-take (get-hourly-weather (parse-naver-weather-html)) 13)))


(defun display-hourly-weather ()
  (interactive)
  (message (info-to-string)))


(defun has-attribute-value (attr-list value)
  (reduce (lambda (acc attr) (or acc (equal (cdr attr) value)))
          attr-list
          :initial-value nil))


(defun extract-sunset-uv-fd-ufd (ul)
  (assert (has-attribute-value (get-attribute-list ul) "today_chart_list"))  ; attribute 중에 "today_chart_list"가 있는지 확인
  (let* ((raw-data (find-by-tag ul 'strong))
         (data (mapcar #'caddr raw-data))
         (raw-values (find-by-tag ul 'em))
         (values (mapcar #'caddr raw-values)))
    (-zip data values)))


(defun get-sunset-uv-fd-ufd ()
  (interactive)
  (let ((ul (find-by-tag-and-attribute (parse-naver-weather-html) 'ul '(class . "today_chart_list"))))
    (extract-sunset-uv-fd-ufd (car ul))))


;;; Organize the information on a new org-buffer. ====
;;; ==================================================


(defun create-buffer ()
  (let ((new-buffer (set-buffer (generate-new-buffer "Naver Weather"))))
    (with-current-buffer new-buffer
      (org-mode))
    new-buffer))


(defun insert-title (buffer)
  "Precondition: the buffer's major mode should be org-mode"
  (insert "* Naver Weather"))


(defun create-org-table (buffer)
  ;; TODO
  )


;; TODO draw a nice table containing information retrieved by get-hourly-weather.
;; TODO find a means to calculate the table width


;;; scrape_weather.el ends here
