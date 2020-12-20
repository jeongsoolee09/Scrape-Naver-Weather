;;; scrape_weather.el ---  -*- lexical-binding: t; -*-

(require 'cl-lib)


;;; Parse the target website's HTML.


(defun parse-html (url)
  "parse the HTML document retrieved by making a request to a given url."
  (set-buffer (url-retrieve-synchronously url))
  (libxml-parse-html-region (point-min) (point-max)))


(defvar *naver-weather-html* (caddr (parse-html "https://weather.naver.com")))


(defun find-by-tag (data tag)
  (let ((res))
    (subst nil nil data :test (lambda (a b)
                                (when (and (listp b) (eql (car b) tag))
                                  (push b res))
                                nil))
    res))


(defun dotted-pairp (list)
  (and (listp list) (not (listp (cdr list))))) 


(defun get-attribute-list (tag)
  (let ((data (cadr tag)))
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


;;; Organize the information on a new org-buffer.


(defun create-buffer ()
  (let ((new-buffer (set-buffer (generate-new-buffer "Naver Weather"))))
    (with-current-buffer new-buffer
      (org-mode))
    new-buffer))


(defun create-org-table (buffer)
  )

;; TODO draw a nice table containing information retrieved by get-hourly-weather.
;; TODO find a means to calculate the table width


;;; scrape_weather.el ends here
