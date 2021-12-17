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
       #:modules ((emacs build melpa-build-system)
                  (guix build utils)
                  (guix build emacs-utils)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'patch-elisp-shell-shebangs
           (lambda _
             (substitute* (find-files "." "\\.el")
               (("/bin/sh") (which "sh")))))
         (add-after 'patch-elisp-shell-shebangs 'setenv-shell
           (lambda _
             (setenv "SHELL" "sh")))
         (add-after 'setenv-shell 'build-emacsql-sqlite
           (lambda _
             (invoke "make" "binary" (string-append "CC=" ,(cc-for-target)))))
         (add-after 'build-emacsql-sqlite 'install-emacsql-sqlite
           ;; This build phase installs emacs-emacsql binary.
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "sqlite/emacsql-sqlite"
                           (string-append (assoc-ref outputs "out") "/bin"))))
         (add-after 'install-emacsql-sqlite 'patch-emacsql-sqlite.el
           ;; This build phase removes interactive prompts
           ;; and makes sure Emacs look for binaries in the right places.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((file "emacsql-sqlite.el"))
               (chmod file #o644)
               (emacs-substitute-sexps file
                 ;; Make sure Emacs looks for ‘GCC’ binary in the right place.
                 ("(executable-find" (which "gcc"))
                 ;; Make sure Emacs looks for ‘emacsql-sqlite’ binary
                 ;; in the right place.
                 ("(defvar emacsql-sqlite-executable"
                  (string-append (assoc-ref outputs "out")
                                 "/bin/emacsql-sqlite")))))))))
    (inputs (package-inputs e/emacs-emacsql))))
