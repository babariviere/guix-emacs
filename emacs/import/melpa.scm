(define-module (emacs import melpa)
  #:use-module (gcrypt hash)
  #:use-module (guix base32)
  #:use-module ((guix download) #:select (download-to-store))
  #:use-module (guix git)
  #:use-module (guix http-client)
  #:use-module (guix import utils)
  #:use-module ((guix serialization) #:select (write-file))
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-35)
  #:use-module (web uri)
  #:export (melpa->guix-package))

(define %root
  (canonicalize-path
   (find (lambda (path)
	  (and
	   (file-exists? (string-append path "/.guix-channel"))
	   (file-exists? (string-append path "/emacs/import/melpa.scm"))
	   (file-exists? (string-append path "/scripts"))))
	%load-path)))

(define (melpa-name->package-name name)
  (let ((package-name-prefix "emacs-"))
    (if (string-prefix? package-name-prefix name)
	(string-downcase name)
	(string-append package-name-prefix (string-downcase name)))))

(define emacs-standard-library?
  (let ((libs
	 (cons* "emacs"
		;; Generated with:  rg '^\(provide (\'([\w-]+))+\)' -r '$2' -N --no-heading --no-filename -o | sort
		(with-input-from-file (string-append %root "/etc/emacs-libs")
		  (lambda ()
		    (remove string-null?
			    (string-split (get-string-all (current-input-port))
					  #\newline)))))))
    (lambda (lib)
      (member lib libs))))

(define (filter-dependencies names)
  (remove emacs-standard-library? names))

(define (melpa-dependencies->names deps)
  (match deps
    (((names _ ...) ...)
     (map symbol->string names))))

(define %melpa-url
  "https://melpa.org/packages")

(define (melpa-fetch-archive)
  (let* ((url (string-append %melpa-url "/archive-contents"))
	 (port (http-fetch/cached (string->uri url)
				  #:ttl (* 6 3600)))
	 (data (read port)))
    (close-port port)
    data))

(define (is-melpa-package? name pkg-spec)
  (eq? (first pkg-spec) (string->symbol name)))

(define (melpa-package-info name)
  (let* ((archive (melpa-fetch-archive))
	 (pkgs (match archive ((version pkg-spec ...) pkg-spec)))
	 (info (filter (cut is-melpa-package? name <>) pkgs)))
    (if (pair? info) (first info) #f)))

(define-record-type <melpa-package>
  (make-melpa-package name version inputs synopsis kind home-page source-url)
  melpa-package?
  (name melpa-package-name)
  (version melpa-package-version)
  (inputs melpa-package-inputs)
  (synopsis melpa-package-synopsis)
  (kind melpa-package-kind)
  (home-page melpa-package-home-page)
  (source-url melpa-package-source-url))

(set-record-type-printer! <melpa-package>
			  (lambda (package port)
			    (format port "#<melpa-package ~a@~a"
				    (melpa-package-name package)
				    (melpa-package-version package))))

(define (melpa-version->string version)
  (if (pair? version)
      (let-values (((ms rest) (match version ((ms . rest)
					      (values ms rest)))))
	(fold (lambda (n s) (string-append s "." (number->string n)))
	      (number->string ms) rest))
      #f))

(define (full-url name suffix version)
  (string-append %melpa-url "/" name "-" version suffix))

(define (package-source-url kind name version)
  (case kind
    ((single) (full-url name ".el" version))
    ((tar) (full-url name ".tar" version))
    (else #f)))

(define (package-home-page alist)
  (or (assq-ref alist ':url) "unspecified"))

(define (ensure-list alist)
  (if (eq? alist 'nil)
      '()
      alist))

(define (fetch-melpa-package name)
  "Fetch package NAME."
  (let ((pkg (melpa-package-info name)))
    (match pkg
      ((name version reqs synopsis kind . rest)
       (let* ((name (symbol->string name))
	      (ver (melpa-version->string version))
	      (url (package-source-url kind name ver)))
	 (make-melpa-package name ver
			     (ensure-list reqs)
			     synopsis
			     kind
			     (package-home-page (match rest
						  (() #f)
						  ((one) one)))
			     url)))
      (_ #f))))

(define (package-name->recipe package-name)
  "Fetch the MELPA recipe for PACKAGE-NAME, represented as an alist ffrom keywords to values."
  (define recipe-url
    (string-append "https://raw.githubusercontent.com/melpa/melpa/master/recipes/" package-name))

  (define (data->recipe data)
    (match data
      (() '())
      ((key value . tail)
       (cons (cons key value) (data->recipe tail)))))

  (let* ((port (http-fetch/cached (string->uri recipe-url)
				  #:ttl (* 6 3600)))
	 (data (read port)))
    (close-port port)
    (data->recipe (cons ':name data))))

(define (download-git-repository url ref)
  "Fetch the given REF from the Git repository at URL."
  (with-store store
    (latest-repository-commit store url #:ref ref)))

(define (vcs-file? file stat)
  (case (stat:type stat)
    ((directory)
     (member (basename file) '(".bzr" ".git" ".hg" ".svn" "CVS")))
    ((regular)
     ;; Git sub-modules have a '.git' file that is a regular text file.
     (string=? (basename file) ".git"))
    (else
     #f)))

(define (file-hash file select? recursive?)
  (if recursive?
      (let-values (((port get-hash) (open-sha256-port)))
        (write-file file port #:select? select?)
        (force-output port)
        (get-hash))
      (call-with-input-file file port-sha256)))

(define (git-repository->origin recipe url)
  "Fetch origin details from the Git repository at URL for the provided RECIPE."
  (define ref
    (cond
     ((assoc-ref recipe #:branch)
      => (lambda (branch) (cons 'branch branch)))
     ((assoc-ref recipe #:commit)
      => (lambda (commit) (cons 'commit commit)))
     (else
      '())))

  (let-values (((directory commit) (download-git-repository url ref)))
    `(origin
       (method git-fetch)
       (uri (git-reference
	     (url ,url)
	     (commit ,commit)))
       (sha256
	(base32
	 ,(bytevector->nix-base32-string
	   (file-hash directory (negate vcs-file?) #t)))))))

(define (melpa-recipe->origin recipe)
  "Fetch origin details from the MELPA recipe and associated repository
for the package named PACKAGE-NAME."
  (match (assq-ref recipe ':fetcher)
    ('github (git-repository->origin recipe (string-append "https://github.com/" (assq-ref recipe ':repo) ".git")))
    ('gitlab (git-repository->origin recipe (string-append "https://gitlab.com/" (assq-ref recipe ':repo) ".git")))
    ('git    (git-repository->origin recipe (assq-ref recipe ':url)))
    ;; ('hg     )
    (_ (warning (G_ "Unsupported MELPA fetcher: ~a, falling back to unstable MELPA source.~%")
		(assq-ref recipe ':fetcher))
       #f)))

;; NOTE: k8s-mode is a good example for the complexity of ':files
(define (melpa-recipe->maybe-arguments recipe)
  "Extract arguments for the build system from MELPA-RECIPE."
  (let ((files (assq-ref recipe ':files)))
    (if files
	`((arguments '(#:files ,files)))
	'())))

(define (melpa-package->sexp pkg)
  (define recipe (package-name->recipe (melpa-package-name pkg)))

  (define name (melpa-package-name pkg))

  (define version (melpa-package-version pkg))

  (define source-url (melpa-package-source-url pkg))

  ;; TODO: to filter dependencies, source emacs and run rg to fetch all libs inside it
  (define dependencies-name
    (filter-dependencies (melpa-dependencies->names
			              (melpa-package-inputs pkg))))

  (define dependencies
    (map (lambda (n)
	        (let ((new-n (melpa-name->package-name n)))
	          (string->symbol new-n)))
	      dependencies-name))

  (define (maybe-inputs input-type inputs)
    (match inputs
      (()
       '())
      ((inputs ...)
       (list (list input-type
		           `(list ,@inputs))))))

  (define melpa-source
    (catch #t
      (lambda ()
	    (melpa-recipe->origin recipe))
      (lambda* (#:rest e)
        (format #t "failed to fetch from origin, falling back to melpa: ~a~%" e)
	    #f)))

  (values
   `(package
      (name ,(melpa-name->package-name name))
      (version ,version)
      (source ,(or melpa-source
		           (let ((tarball (with-store store
					                (download-to-store store source-url))))
		             `(origin
		                (method url-fetch)
		                (uri (string-append ,@(factorize-uri source-url version)))
		                (sha256
		                 (base32
			              ,(if tarball
			                   (bytevector->nix-base32-string (file-sha256 tarball))
			                   (raise
			                    (condition
			                     (&message (message "failed to download package")))))))))))
      (build-system melpa-build-system)
      ,@(maybe-inputs 'propagated-inputs dependencies)
      ,@(if melpa-source
	        (melpa-recipe->maybe-arguments recipe)
	        '())
      (home-page ,(melpa-package-home-page pkg))
      (synopsis ,(melpa-package-synopsis pkg))
      (description ,(string-append "Documentation at https://melpa.org/#/" name))
      (license #f))
   dependencies-name))

(define (melpa->guix-package name)
  (match (fetch-melpa-package name)
    (#false (raise (condition (&message (message "couldn't find meta-data for elpa package.")))))
    (package (melpa-package->sexp package))))

#;(second (melpa-fetch-archive))
#;(melpa-recipe->origin (package-name->recipe "magit"))

#;(fetch-melpa-package "magit")

#;(melpa->guix-package "magit")
