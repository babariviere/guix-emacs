(define-module (emacs packages elpa)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (guix build-system emacs)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public emacs-compat
  (package
   (name "emacs-compat")
   (version "28.1.1.0")
   (source
    (origin
     (method url-fetch)
     (uri (string-append
           "https://elpa.gnu.org/packages/compat-"
           version
           ".tar"))
     (sha256
      (base32 "0y1c6d624rpjk5n839xq1fvnb5hid4v8n5bjjcyi2w8pk38yi1cn"))))
   (build-system emacs-build-system)
   (propagated-inputs (list emacs-nadvice))
   (home-page "https://sr.ht/~pkal/compat")
   (synopsis "Compatibility Library")
   (description
    "To allow for the usage of Emacs functions and macros that are defined in newer
versions of Emacs, compat.el provides definitions that are installed ONLY if
necessary.  These reimplementations of functions and macros are at least subsets
of the actual implementations.  Be sure to read the documentation string to make
sure.

Not every function provided in newer versions of Emacs is provided here.  Some
depend on new features from the core, others cannot be implemented to a
meaningful degree.  The main audience for this library are not regular users,
but package maintainers.  Therefore commands and user options are usually not
implemented here.")
   (license license:gpl3+)))
