(define (load-here path)
  (load (merge-pathnames (directory-pathname (current-load-pathname))
                         (->pathname path))))

;; Load "../common/load.scm".
(let ((path (current-load-pathname)))
  (let ((directory (pathname-directory path)))
    (let ((common-directory
           (append (drop-right directory 1)
                   (list "common"))))
      (load (pathname-new-directory path common-directory)))))

(load-here "symbol.scm")
(load-here "expand.scm")
