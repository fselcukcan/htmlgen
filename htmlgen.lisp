(in-package :rtickle)

;;;works both
;Definitions
(defun f (tag &rest rest)
  (with-output-to-string (*standard-output*)
    (format t "<~a ~{~a='~a' ~}>~{~a ~}</~a>" tag (car rest) (cdr rest) tag)))
;"generic any tag markup generator. for attributes pass them with #'list and as a plist, or '() or nil."

(defmacro ff (tag)
  `(defun ,tag (&rest c)
     (with-output-to-string (*standard-output*)
       (format t "<~a ~{~a='~a' ~}>~{~a~}</~a>" ',tag (car c) (cdr c) ',tag))))

;"markup generator tag generator. for attributes pass them with #'list and as a plist, empty list or nil."

(progn
  (ff html)
  (ff title)
  (ff head)
  (ff meta)
  (ff link)
  (ff style)
  (ff script)
  (ff body)
  (ff span)
  (ff div)
  (ff section)
  (ff p)
  (ff form)
  (ff input)
  (ff button)
  (ff textarea)
  (ff a)
  (ff h1)
  (ff h2)
  (ff h3)
  (ff h4)
  (ff h5)
  (ff h6)
  (ff ol)
  (ff ul)
  (ff li)
  (ff hr)
  (ff br)
  (ff table)
  (ff tr)
  (ff td)
  (ff image)
  (ff button))
;(defparameter *tag-list* '(html head title style script link body div section h1 h1 h3 h4 h5 h6 p a ol ul li table tr td form input button textarea))

;html iterator
(defmacro iterate-html1 (n &body html-code)
  `(with-output-to-string (s)
    (dotimes (i ,n)
      (princ ,@html-code s))))

(defmacro iterate-html (dos-dotimes-dolist vars-list &body html-code)"use one of do do* dotimes dolist, use corresponding args in a list, and hmtl-code s-expressions."
  `(with-output-to-string (s)
    (,dos-dotimes-dolist ,@vars-list
      (princ ,@html-code s))))
;;examles
;;;(iterate-html dolist ((i '(q w e r))) (p () i))
;;;(iterate-html dotimes ((i 5)) (p () i))

#|
(iterate-html  do (((i 0 (+ i 2))
                    (j 1 (+ j 2)))
                    ((= i 10)))
                  (p () i))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;tests

#|
(setf lst '((1 "2014-03-24 02:33:21+02" "farukscan" "faruk" "can" "anewtitle" "asdasdasdafasdafasfafasfadasfafadfaf asfadFA FASFAF" T :NULL :NULL :NULL) NIL (1 "2014-03-24 02:37:54+02" "selcukcan" "selcuk" "can" "a111newtitle" "asdasdasdafasdafasfafasfadasfafadfaf asfadFA FASFAF" T :NULL :NULL :NULL) (1 "2014-03-24 02:38:29+02" "fscan" "fs" "can" "a2222newtitle" "asdasdasdafasdafasfafasfadasfafadfaf asfadFA FASFAF" T 0 10 10)))

(iterate-html dolist ((i (gather-all-artickles)))
              (li ()
                  (iterate-html dolist ((j i))
                                (p () j))))


(iterate-html dolist ((j (gather-all-artickles)));j is an rtickle in i.
              (li ()
                  #|(iterate-html dolist ((k '(:title :body :name :surname :authorname :date :upvotes :downvotes :votes)));k is a property in the plist j.
                  (p () (getf j k))))|#
                  (p () (getf j :title));title
                  (p () (getf j :body));body
                  (p () (span () (getf j :name))(span () (getf j :surname)));; name surname
                  (p () (getf j :authorname))
                  (p () (getf j :date));
                  (p () (getf j :upvotes))
                  (p () (getf j :downvotes))
                  (p () (getf j :votes))))

(iterate-html dolist ((j (gather-all-artickles)))
              (li ()
                  (iterate-html dolist
                                ((k '(:title :body :name :surname :authorname :date :upvotes :downvotes :votes)));k is a property in the plist j.
                                (p () (getf j k)))))

|#
                  