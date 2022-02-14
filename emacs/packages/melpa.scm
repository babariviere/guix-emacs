(define-module (emacs packages melpa)
  #:use-module (emacs build-system melpa)
  #:use-module ((emacs packages melpa-generated) #:prefix g/)
  #:use-module ((gnu packages emacs-xyz) #:prefix e/)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages sqlite)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:replace (emacs-emacsql-sqlite emacs-emacsql-sqlite3 emacs-vterm emacs-guix emacs-geiser-guile))

(eval-when (eval load compile)
  (let ((i (module-public-interface (current-module))))
    (module-use! i (resolve-interface `(emacs packages melpa-generated)))))

(define-public emacs-emacsql-sqlite
  (package
    (inherit g/emacs-emacsql-sqlite)
    (arguments
     `(,@(package-arguments g/emacs-emacsql-sqlite)
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
                 ("(defvar emacsql-sqlite-executable-path"
                  (string-append (assoc-ref outputs "out")
                                 "/bin/emacsql-sqlite")))))))))
   (inputs (package-inputs e/emacs-emacsql))))


(define-public emacs-emacsql-sqlite3
  (package
    (inherit g/emacs-emacsql-sqlite3)
    (arguments
     `(,@(package-arguments g/emacs-emacsql-sqlite3)
       #:tests? #t
       #:test-command '("emacs" "-Q" "--batch" "-L" "."
                        "--load" "emacsql-sqlite3-test.el"
                        "-f" "ert-run-tests-batch-and-exit")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'embed-path-to-sqlite3
           (lambda _
             (substitute* "emacsql-sqlite3.el"
               (("\\(executable-find \"sqlite3\"\\)")
                (string-append "\"" (which "sqlite3") "\""))))))))
    (native-inputs
     (list g/emacs-ert-runner))
    (inputs
     (list sqlite))))

(define-public emacs-vterm
  (package
   (inherit g/emacs-vterm)
   (arguments
    `(#:modules ((emacs build melpa-build-system)
                 ((guix build cmake-build-system) #:prefix cmake:)
                 (guix build emacs-utils)
                 (guix build utils))
      #:imported-modules (,@%melpa-build-system-modules
                          (guix build cmake-build-system))
      #:phases
      (modify-phases %standard-phases
                     (add-after 'unpack 'substitute-vterm-module-path
                                (lambda* (#:key outputs #:allow-other-keys)
                                  (chmod "vterm.el" #o644)
                                  (emacs-substitute-sexps "vterm.el"
                                                          ("(require 'vterm-module nil t)"
                                                           `(module-load
                                                             ,(string-append (assoc-ref outputs "out")
                                                                             "/lib/vterm-module.so"))))))
                     (add-after 'build 'configure
                                ;; Run cmake.
                                (lambda* (#:key outputs #:allow-other-keys)
                                  ((assoc-ref cmake:%standard-phases 'configure)
                                   #:outputs outputs
                                   #:out-of-source? #f
                                   #:configure-flags '("-DUSE_SYSTEM_LIBVTERM=ON"))))
                     (add-after 'configure 'make
                                ;; Run make.
                                (lambda* (#:key (make-flags '()) outputs #:allow-other-keys)
                                  ;; Compile the shared object file.
                                  (apply invoke "make" "all" make-flags)
                                  ;; Move the file into /lib.
                                  (install-file
                                   "vterm-module.so"
                                   (string-append (assoc-ref outputs "out") "/lib")))))
      #:tests? #f))
   (native-inputs
    (package-native-inputs e/emacs-vterm))))

(define-public emacs-geiser-guile
  (package
    (inherit g/emacs-geiser-guile)
    (inputs (package-inputs e/emacs-geiser-guile))))

(define-public emacs-guix
  (package
    (inherit g/emacs-guix)
    (build-system gnu-build-system)
    (arguments (package-arguments e/emacs-guix))
    (native-inputs (package-native-inputs e/emacs-guix))
    (inputs (package-inputs e/emacs-guix))
    (propagated-inputs
     (list g/emacs-dash
           g/emacs-geiser
           emacs-geiser-guile
           g/emacs-bui
           g/emacs-magit-popup
           g/emacs-edit-indirect))))
