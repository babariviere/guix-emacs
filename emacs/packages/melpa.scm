;;; guix-emacs --- Reproducible development environment.
;;;
;;; Copyright © 2014 Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2013-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016, 2017, 2018, 2019 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2015, 2016, 2018, 2020 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2016 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 David Thompson <davet@gnu.org>
;;; Copyright © 2016 Matthew Jordan <matthewjordandevops@yandex.com>
;;; Copyright © 2016, 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016, 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016-2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2016, 2017, 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2017, 2018, 2019, 2020, 2022 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017, 2018, 2019, 2020, 2021, 2022 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2017, 2018, 2022 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 George Clemmer <myglc2@gmail.com>
;;; Copyright © 2017, 2018 Feng Shu <tumashu@163.com>
;;; Copyright © 2017, 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017, 2018, 2019, 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2017 Mekeor Melire <mekeor.melire@gmail.com>
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Mike Gerwitz <mtg@gnu.org>
;;; Copyright © 2017, 2018, 2019, 2020, 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2018 Sohom Bhattacharjee <soham.bhattacharjee15@gmail.com>
;;; Copyright © 2018, 2019 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2018, 2019, 2020, 2021 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018, 2019, 2020 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2018, 2019 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2018 Alex Branham <alex.branham@gmail.com>
;;; Copyright © 2018 Thorsten Wilms <t_w_@freenet.de>
;;; Copyright © 2018, 2019, 2020, 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2018, 2019, 2020, 2021 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2019, 2020 Dimakakos Dimos <bendersteed@teknik.io>
;;; Copyright © 2019, 2020 Brian Leung <bkleung89@gmail.com>
;;; Copyright © 2019 mikadoZero <mikadozero@yandex.com>
;;; Copyright © 2019 Gabriel Hondet <gabrielhondet@gmail.com>
;;; Copyright © 2019, 2020, 2021 Joseph LaFreniere <joseph@lafreniere.xyz>
;;; Copyright © 2019 Todor Kondić <tk.code@protonmail.com>15669
;;; Copyright © 2019 Amar Singh <nly@disroot.org>
;;; Copyright © 2019 Baptiste Strazzulla <bstrazzull@hotmail.fr>
;;; Copyright © 2019 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2019 Jens Mølgaard <jens@zete.tk>
;;; Copyright © 2019, 2020 Amin Bandali <bandali@gnu.org>
;;; Copyright © 2019 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2019 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2019 Stephen Webber <montokapro@gmail.com>
;;; Copyright © 2019, 2021 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2019 David Wilson <david@daviwil.com>
;;; Copyright © 2020 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2020 Robert Smith <robertsmith@posteo.net>
;;; Copyright © 2020 Evan Straw <evan.straw99@gmail.com>
;;; Copyright © 2020, 2021 Masaya Tojo <masaya@tojo.tokyo>
;;; Copyright © 2020, 2021 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2020, 2021, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 6033fe7de85d <6033fe7de85d@airmail.cc>
;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Jérémy Korwin-Zmijowski <jeremy@korwin-zmijowski.fr>
;;; Copyright © 2020 Alberto Eleuterio Flores Guerrero <barbanegra+guix@posteo.mx>
;;; Copyright © 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 pinoaffe <pinoaffe@airmail.cc>
;;; Copyright © 2020, 2021, 2022 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Ryan Desfosses <rdes@protonmail.com>
;;; Copyright © 2020 Marcin Karpezo <sirmacik@wioo.waw.pl>
;;; Copyright © 2020, 2022 Fredrik Salomonsson <plattfot@posteo.net>
;;; Copyright © 2020 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2020, 2021 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2020 Peng Mei Yu <i@pengmeiyu.com>
;;; Copyright © 2020, 2021, 2022 Niklas Eklund <niklas.eklund@posteo.net>
;;; Copyright © 2020 Marco Grassi <marco.au.grassi98@protonmail.com>
;;; Copyright © 2020 Tomás Ortín Fernández <tomasortin@mailbox.org>
;;; Copyright © 2020, 2021 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2020 Adam Kandur <rndd@tuta.io>
;;; Copyright © 2020 Tim Howes <timhowes@lavabit.com>
;;; Copyright © 2020 Noah Landis <noahlandis@posteo.net>
;;; Copyright © 2020, 2021 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2020, 2022 André A. Gomes <andremegafone@gmail.com>
;;; Copyright © 2020 Jonathan Rostran <rostranjj@gmail.com>
;;; Copyright © 2020, 2021 Noah Evans <noah@nevans.me>
;;; Copyright © 2020 Brit Butler <brit@kingcons.io>
;;; Copyright © 2021, 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2021 Yurii Kholodkov <urist.mckorobochka@gmail.com>
;;; Copyright © 2021 Alexey Abramov <levenson@mmer.org>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021, 2022 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2021 Eugene Klimov <lipklim@mailbox.org>
;;; Copyright © 2021 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2021 David Dashyan <mail@davie.li>
;;; Copyright © 2021 Dhruvin Gandhi <contact@dhruvin.dev>
;;; Copyright © 2021 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2021 Noisytoot <noisytoot@disroot.org>
;;; Copyright © 2021 Simon South <simon@simonsouth.net>
;;; Copyright © 2021 la snesne <lasnesne@lagunposprasihopre.org>
;;; Copyright © 2021 Brian Kubisiak <brian@kubisiak.com>
;;; Copyright © 2021, 2022 Taiju HIGASHI <higashi@taiju.info>
;;; Copyright © 2022 Brandon Lucas <br@ndon.dk>
;;; Copyright © 2022 Jai Vetrivelan <jaivetrivelan@gmail.com>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2022 Dominic Martinez <dom@dominicm.dev>
;;; Copyright © 2022 Peter Polidoro <peter@polidoro.io>
;;; Copyright © 2022 Luis Felipe López Acevedo <luis.felipe.la@protonmail.com>
;;; Copyright © 2022 Thomas Albers Raviola <thomas@thomaslabs.org>
;;; Copyright © 2022 Haider Mirza <haider@haider.gq>
;;; Copyright © 2022 by Bastien Riviere <me@babariviere.com>
;;;
;;; This file is part of guix-emacs.
;;;
;;; guix-emacs is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; guix-emacs is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with guix-emacs.  If not, see <http://www.gnu.org/licenses/>.

(define-module (emacs packages melpa)
  #:use-module (emacs build-system melpa)
  #:use-module ((gnu packages emacs-xyz) #:prefix e/)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages sqlite)
  #:use-module (guix build-system gnu)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define %channel-root
  (canonicalize-path
   (find (lambda (path)
	   (and
	    (file-exists? (string-append path "/.guix-channel"))
	    (file-exists? (string-append path "/emacs"))
	    (file-exists? (string-append path "/emacs/packages/melpa-generated"))))
	 %load-path)))

(load (string-append %channel-root "/emacs/packages/melpa-generated"))

(define-syntax define-override
  (syntax-rules ()
    ((_ name var exp)
     (let ((var name))
       (set! name exp)))))

(define-override emacs-emacsql-sqlite pkg
  (package
    (inherit pkg)
    (arguments
     `(,@(package-arguments pkg)
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


(define-override emacs-emacsql-sqlite3 pkg
  (package
    (inherit pkg)
    (arguments
     `(,@(package-arguments pkg)
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
     (list emacs-ert-runner))
    (inputs
     (list sqlite))))

(define-override emacs-vterm pkg
  (package
   (inherit pkg)
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

(define-override emacs-geiser-guile pkg
  (package
    (inherit pkg)
    (inputs (package-inputs e/emacs-geiser-guile))))

(define-override emacs-guix pkg
  (package
    (inherit pkg)
    (build-system gnu-build-system)
    (arguments (package-arguments e/emacs-guix))
    (native-inputs (package-native-inputs e/emacs-guix))
    (inputs (package-inputs e/emacs-guix))
    (propagated-inputs
     (list emacs-dash
           emacs-geiser
           emacs-geiser-guile
           emacs-bui
           emacs-magit-popup
           emacs-edit-indirect
           emacs-transient))))
