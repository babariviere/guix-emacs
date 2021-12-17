(define-module (emacs packages melpa)
  #:use-module ((emacs packages melpa-generated) #:prefix g/)
  #:use-module ((gnu packages emacs-xyz) #:prefix e/)
  #:use-module (guix packages)
  #:replace (emacs-emacsql))

(eval-when (eval load compile)
  (let ((i (module-public-interface (current-module))))
    (module-use! i (resolve-interface `(emacs packages melpa-generated)))))

(define-public emacs-emacsql
  (package
    (inherit g/emacs-emacsql)
    (arguments
     `(,@(package-arguments g/emacs-emacsql)
       ,@(package-arguments e/emacs-emacsql)))
    (inputs (package-inputs e/emacs-emacsql))))
