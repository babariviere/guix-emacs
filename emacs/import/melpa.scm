(define-module (emacs import melpa)
  #:use-module (gcrypt hash)
  #:use-module (guix base32)
  #:use-module (guix git)
  #:use-module (guix http-client)
  #:use-module ((guix serialization) #:select (write-file))
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (web uri))


(define %melpa-url
  "https://melpa.org/packages")

(define (melpa-fetch-archive)
  (let* ((url (string-append %melpa-url "/archive-contents"))
	 (port (http-fetch/cached (string->uri url)
				  #:ttl (* 6 3600)))
	 (data (read port)))
    (close-port port)
    data))

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

(second (melpa-fetch-archive))
(melpa-recipe->origin (package-name->recipe "magit"))
