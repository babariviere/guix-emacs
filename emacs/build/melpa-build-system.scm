;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2018, 2019 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (emacs build melpa-build-system)
  #:use-module ((guix build emacs-build-system) #:prefix emacs:)
  #:use-module ((guix build utils) #:hide (delete))
  #:use-module (guix build emacs-utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:export (%standard-phases
            %default-files-spec
            melpa-build))

;; Commentary:
;;
;; Builder-side code of the build procedure for ELPA Emacs packages.
;;
;; Code:

;;; The location in which Emacs looks for packages.  Emacs Lisp code that is
;;; installed there directly will be found when that directory is added to
;;; EMACSLOADPATH.  To avoid clashes between packages (particularly considering
;;; auxiliary files), we install them one directory level below, however.
;;; This indirection is handled by ‘expand-load-path’ during build and a
;;; profile hook otherwise.
(define %install-dir "/share/emacs/site-lisp")

(define %default-files-spec
  ;; This contains more than just the things contained in %default-include and
  ;; %default-exclude, presumably because this includes source files (*.in,
  ;; *.texi, etc.) which have already been processed for releases.
  ;;
  ;; Taken from:
  ;; https://github.com/melpa/melpa/blob/e8dc709d0ab2b4a68c59315f42858bcb86095f11/package-build/package-build.el#L580-L585
  '("*.el" "*.el.in" "dir"
    "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el")))

(define (glob->regexp glob)
  (string-append
   "^/"
   (regexp-substitute/global #f "\\\\\\*(\\\\\\*)?" (regexp-quote glob)
                             'pre
                             (lambda (m)
                               (if (string= (match:substring m 0) "**")
                                   ".*"
                                   "[^/]*"))
                             'post)
   "$"))

(define (glob-match? dir glob)
  (let ((regexp (make-regexp (glob->regexp glob))))
    (lambda (file stat)
      (and (not (string-suffix? "-autoloads.el" file))
	   (regexp-exec regexp (string-drop file (string-length dir)))))))

(define* (expand-file-specs dir specs #:optional subdir allow-empty)
  (let ((prefix (if subdir (format #f "~a/" subdir) ""))
	(lst '()))
    (for-each
     (lambda (entry)
       (set! lst
	     (if (list? entry)
		 (if (eq? ':exclude (car entry))
		     (lset-difference! equal?
				       lst
				       (expand-file-specs
					dir
					(cdr entry)
					#f
					#t))
		     (append lst
			     (expand-file-specs
			      dir
			      (cdr entry)
			      (string-append prefix (car entry))
			      #t)))
		 (append
		  lst (map (lambda (f)
			     (let* ((bf (basename f))
				    (m (string-match  "\\.el\\.in\\'" bf)))
			       (cons f
				     (string-append prefix
						    (if m (regexp-substitute #f
									     m
									     'pre ".el" 'post)
							bf)))))
			   (find-files dir (glob-match? dir entry) #:directories? #t))))))
     specs)
    (when (and (null? lst) (not allow-empty))
      (format #t "No matching file(s) found in ~a: ~a~%" dir specs))
    lst))

(define (install--config-file-list files)
  (cond
   ((null? files)
    %default-files-spec)
   ((eq? ':defaults (car files))
    (append %default-files-spec (cdr files)))
   (else
    files)))

(define* (install #:key outputs
                  (files %default-files-spec)
                  #:allow-other-keys)
  "Install the package contents."
  (define source (getcwd))

  (define file-list (expand-file-specs source (install--config-file-list files)))

  (let* ((out (assoc-ref outputs "out"))
         (el-dir (elpa-directory out)))
    (cond
     ((not (null? file-list))
      (for-each
       (lambda (file)
         (let* ((source (car file))
		(target (string-append el-dir "/" (cdr file))))
           (format #t "`~a' -> `~a'~%" source target)
	   (if (directory-exists? source)
	       (copy-recursively source target)
               (install-file source (dirname target)))))
       file-list)
      #t)
     (else
      (format #t "error: No files found to install.\n")
      #f))))

(define (emacs-byte-compile-dir dir)
  "Byte compile all files in DIR and its sub-directories."
  (let ((expr `(progn
                (setq byte-compile-debug nil) ; disable byte-compile's error (they are only warning)
		(add-to-list 'load-path ,dir)
                (byte-recompile-directory (file-name-as-directory ,dir) 0 1))))
    (emacs-batch-eval expr)))

(define* (build #:key outputs inputs #:allow-other-keys)
  "Compile .el files."
  (let* ((emacs (string-append (assoc-ref inputs "emacs") "/bin/emacs"))
         (out (assoc-ref outputs "out")))
    (setenv "SHELL" "sh")
    (parameterize ((%emacs emacs))
      (emacs-byte-compile-dir (elpa-directory out)))))

(define (emacs-package? name)
  "Check if NAME correspond to the name of an Emacs package."
  (string-prefix? "emacs-" name))

(define (package-name-version->elpa-name-version name-ver)
  "Convert the Guix package NAME-VER to the corresponding ELPA name-version
format.  Essentially drop the prefix used in Guix."
  (if (emacs-package? name-ver)  ; checks for "emacs-" prefix
      (string-drop name-ver (string-length "emacs-"))
      name-ver))

(define (store-directory->elpa-name-version store-dir)
  "Given a store directory STORE-DIR return the part of the basename after the
second hyphen.  This corresponds to 'name-version' as used in ELPA packages."
  ((compose package-name-version->elpa-name-version
            strip-store-file-name)
   store-dir))

(define (elpa-directory store-dir)
  "Given the store directory STORE-DIR return the absolute install directory
for libraries following the ELPA convention."
  (string-append store-dir %install-dir "/"
                 (store-directory->elpa-name-version store-dir)))

(define %standard-phases
  (modify-phases emacs:%standard-phases
    (replace 'build build)
    (replace 'install install)
    ;; HACK: find a solution to avoid removing el files patching.
    (delete 'patch-el-files)
    ;; HACK: compiled autoload files can fail to find symbols
    (delete 'enable-autoloads-compilation)
    (delete 'validate-compiled-autoloads)))

(define* (melpa-build #:key inputs (phases %standard-phases)
                      #:allow-other-keys #:rest args)
  "Build the given Emacs package, applying all of PHASES in order."
  (apply emacs:emacs-build
         #:inputs inputs #:phases phases
         args))

;;; melpa-build-system.scm ends here
