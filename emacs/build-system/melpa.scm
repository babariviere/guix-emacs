;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2020 Morgan Smith <Morgan.J.Smith@outlook.com>
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

(define-module (emacs build-system melpa)
  #:use-module ((emacs build melpa-build-system)
                #:select (%default-files-spec))
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix search-paths)
  #:use-module (guix build-system)
  #:use-module (guix build-system gnu)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-26)
  #:export (%melpa-build-system-modules
            melpa-build
            melpa-build-system)
  #:re-export (%default-files-spec))


;; Commentary:
;;
;; Standard build procedure for Emacs packages.  This is implemented as an
;; extension of 'gnu-build-system'.
;;
;; Code:

(define %melpa-build-system-modules
  ;; Build-side modules imported by default.
  `((emacs build melpa-build-system)
    (guix build emacs-utils)
    (guix build emacs-build-system)
    ,@%gnu-build-system-modules))

(define (default-emacs)
  "Return the default Emacs package."
  ;; Lazily resolve the binding to avoid a circular dependency.
  (let ((emacs-mod (resolve-interface '(gnu packages emacs))))
    (module-ref emacs-mod 'emacs-minimal)))

(define* (lower name
                #:key source inputs native-inputs outputs system target
                (emacs (default-emacs))
                #:allow-other-keys
                #:rest arguments)
  "Return a bag for NAME."
  (define private-keywords
    '(#:target #:emacs #:inputs #:native-inputs))

  (and (not target)                               ;XXX: no cross-compilation
       (bag
         (name name)
         (system system)
         (host-inputs `(,@(if source
                              `(("source" ,source))
                              '())
                        ,@inputs

                        ;; Keep the standard inputs of 'gnu-build-system'.
                        ,@(standard-packages)))
         (build-inputs `(("emacs" ,emacs)
                         ,@native-inputs))
         (outputs outputs)
         (build melpa-build)
         (arguments (strip-keyword-arguments private-keywords arguments)))))

(define* (melpa-build name inputs
                      #:key source
                      (tests? #f)
                      (parallel-tests? #t)
                      (test-command ''("make" "check"))
                      (phases '(@ (emacs build melpa-build-system)
                                  %standard-phases))
                      (outputs '("out"))
                      (files %default-files-spec)
                      (search-paths '())
                      (system (%current-system))
                      (guile #f)
                      (imported-modules %melpa-build-system-modules)
                      (modules '((emacs build melpa-build-system)
                                 (guix build utils)
                                 (guix build emacs-utils))))
  "Build SOURCE using EMACS, and with INPUTS."
  (define builder
    (with-imported-modules imported-modules
      #~(begin
          (use-modules #$@(sexp->gexp modules))
          (melpa-build #:name #$name
                       #:source #+source
                       #:system #$system
                       #:test-command #$test-command
                       #:tests? #$tests?
                       #:parallel-tests? #$parallel-tests?
                       #:phases #$phases
                       #:outputs #$(outputs->gexp outputs)
                       #:files '#$files
                       #:search-paths '#$(map search-path-specification->sexp
                                              search-paths)
                       #:inputs #$(input-tuples->gexp inputs)))))

  (mlet %store-monad ((guile (package->derivation (or guile (default-guile))
                                                  system #:graft? #f)))
    (gexp->derivation name builder
                      #:system system
                      #:guile-for-build guile)))

(define melpa-build-system
  (build-system
    (name 'melpa)
    (description "The build system for Emacs packages")
    (lower lower)))

;;; emacs.scm ends here
