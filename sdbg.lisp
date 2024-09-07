(defpackage :sdbg
  (:use :cl)
  (:export dbg defobj setf-values let*-values))

(in-package :sdbg)

;; the lists gotta be the same length :)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun zip (&rest lists)
    (%zip lists nil))
  (defun %zip (lists acc)
    (cond
      ((null (Car lists))
       (nreverse acc))
      (t
       (%zip (mapcar #'cdr lists)
             (cons (mapcar #'car lists) acc))))))

;; simple debug macro, you pass it expressions and it prints out the
;; expressions and their values
(defmacro dbg (&body exps)
  (let ((gensyms (loop for i in exps collect (gensym))))
    `(let (,@(mapcar #'(lambda (gensym exp) (list gensym exp)) gensyms exps))
       (progn
                 (format *debug-io*
                         (concatenate 'string
                                      ,@(mapcar #'(lambda (exp)
                                                    (format nil "~A: ~~A~~%" exp))
                                                exps))
                         ,@gensyms)
                 (values ,@gensyms)))) )

(defmacro defobj (name &body slots)
  (let ((ret (gensym)))
    `(progn
       (defclass ,name ()
         (,@(mapcar #'(lambda (slot-sym)
                        `(,slot-sym :initarg ,(intern (symbol-name slot-sym) 'keyword)
                                    :accessor ,slot-sym))
                    slots)))
       (defun ,(alexandria:symbolicate "MAKE-" name) ,slots
         (let ((,ret (make-instance
                      ',name 
                      ,@(alexandria:flatten
                         (zip (mapcar
                               #'(lambda (slot)
                                   (intern (symbol-name slot)
                                           'keyword))
                               slots)
                              slots)))))
           ,ret))
       (defun ,(alexandria:symbolicate name "-P") (obj)
         (typep obj ',name))
       ,(let ((g (gensym)))
          `(defmethod print-obj ((,g ,name))
             (format
              nil
              (concatenate
               'string "("
               ,(string-downcase (symbol-name name))
               " "
               ,@(mapcar #'(lambda (slot)
                             (concatenate 'string
                                          "("
                                          (string-downcase (symbol-name slot))
                                          " ~A) "))
                         slots)
               ")")
              ,@(mapcar #'(lambda (slot)
                            `(cond
                               ((not (nth-value 1 (ignore-errors (print-obj (,slot ,g)))))
                                (print-obj (,slot ,g)))
                               ((listp (,slot ,g))
                                (apply #'concatenate 'string
                                       (loop for thing in (,slot ,g)
                                             collect
                                             (concatenate 'string (print-obj thing) " ")))
                                )
                               (t
                                ;; (when (not (or (stringp (,slot ,g))
                                ;;                (numberp (,slot ,g))
                                ;;                (symbolp (,slot ,g))))
                                ;;   (print-obj (,slot ,g)))
                                (format nil "~A" (,slot ,g)))) )
                        slots)))))))


(defmacro setf-values ((&rest vars) values-expression)
  (let ((gensyms (loop for var in vars collect (gensym))))
    `(multiple-value-bind
           ,gensyms ,values-expression
       (setf ,@(alexandria:flatten (mapcar #'(lambda (x y) (list x y)) vars gensyms)))
       (values ,@vars))))


(defmacro let*-values (bindings &body body)
  (cond
    ((null bindings)
     `(progn ,@body))
    (t
     (destructuring-bind ((&rest vars) value-expression) (car bindings)
       `(multiple-value-bind ,vars ,value-expression
          (let*-values ,(cdr bindings) ,@body))))))

