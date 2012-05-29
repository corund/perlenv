;;; perlenv.el -- A simple perlbrew & local::lib wrapper for emacs

;; Copyright (C) 2012 Kim Jin

;; Author: Kim Jin
;; Keywords: Emacs, Perl, perlbrew, local::lib

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Usage:

;; (require 'perlenv)
;; ;; unless perlbrew root is $HOME/perl5/perlbrew, you should set perlbrew root
;; ;; (setq perlenv-perlbrew-root (substitute-in-file-name "$HOME/myperlbrew"))
;; (perlenv-perlbrew-use "5.12.4")
;; ;; local::lib path
;; (perlenv-set-local-lib (substitute-in-file-name "$HOME/perl5/local/mydist"))
;;
;; Later on, the following commands are available
;;  M-x perlenv-perldb
;;  M-x perlenv-perlbrew-use
;;  M-x perlenv-set-local-lib

;; The following functions are available
;;  (perlenv-get-perl-path)
;;  (perlenv-get-perl-inc-args)
;;  (perlenv-build-exec-path)
;;  (perlenv-build-process-environment)

(defcustom perlenv-perlbrew-root (substitute-in-file-name "$HOME/perl5/perlbrew")
  "perlbrew root path"
  :type 'string)

(defvar perlenv-perlbrew-use-version nil "using perl version in perlbrew")
(defvar perlenv-local-lib-path nil "perl local::lib path")
(defvar perlenv-perl-archname nil "perl archname")

(defun perlenv-perlbrew-use (version)
  "Using this version perl in perlbrew"
  (interactive (list (completing-read "Version to set: " (perlenv-perlbrew-list))))
  (if (file-accessible-directory-p (concat perlenv-perlbrew-root "/perls/perl-" version))
      (progn
        (setq perlenv-perlbrew-use-version version)
        (setq perlenv-perl-archname (shell-command-to-string
                                     (concat perlenv-perlbrew-root
                                             "/perls/perl-"
                                             perlenv-perlbrew-use-version
                                             "/bin/perl -MConfig -e 'print $Config{archname}'")))
        (if (interactive-p)
            (princ (format "perlbrew use version %s" version))))
    (if (interactive-p)
        (princ (format "%s is not installed perl version. check your perlbrew config" version))
      (error "%s is not installed perl version" version))))

(defun perlenv-set-local-lib (path)
  "Set local::lib path"
  (interactive "flocal::lib path to set (reset when blank): ")
  (if (equal path "")
      (progn
        (setq perlenv-local-lib-path nil)
        (if (interactive-p)
            (princ "reset local::lib path")))
    (progn
      (setq perlenv-local-lib-path (expand-file-name path))
      (if (interactive-p)
          (princ (format "set local::lib path to %s" perlenv-local-lib-path))))))

(defun perlenv-build-exec-path ()
  "build exec-path for using perlbrew and local::lib"
  (let ((temp-list (if (and perlenv-perlbrew-root perlenv-perlbrew-use-version)
                       (cons (concat perlenv-perlbrew-root "/perls/perl-" perlenv-perlbrew-use-version "/bin")
                             (remove-if (lambda (p)
                                          (string-match (concat "^" perlenv-perlbrew-root) p))
                                        exec-path))
                     exec-path)))
    (if perlenv-local-lib-path
        (cons (concat perlenv-local-lib-path "/bin")
              temp-list)
      temp-list)))

(defun perlenv-build-process-environment ()
  "build process-environment for using perlbrew and local::lib"
  (let ((temp-list (if perlenv-local-lib-path
                       (progn
                         (unless perlenv-perl-archname
                           (setq perlenv-perl-archname (shell-command-to-string
                                                        (concat (perlenv-get-perl-path)
                                                                " -MConfig -e 'print $Config{archname}'"))))
                         (cons (format "PERL5LIB=%s/lib/perl5/%s:%s/lib/perl5"
                                       perlenv-local-lib-path
                                       perlenv-perl-archname
                                       perlenv-local-lib-path)
                               process-environment))
                     process-environment)))
    (if (and perlenv-perlbrew-root perlenv-perlbrew-use-version)
        (mapcar (lambda (x)
                  (if (string-match "^PATH=\\(.*\\)$" x)
                      (format "PATH=%s:%s"
                              (concat perlenv-perlbrew-root "/perls/perl-" perlenv-perlbrew-use-version "/bin")
                              (match-string 1 x))
                    x))
                temp-list)
      temp-list)))

(defun perlenv-get-perl-path ()
  "Get current using perl path"
  (concat perlenv-perlbrew-root
          "/perls/perl-"
          perlenv-perlbrew-use-version
          "/bin/perl"))

(defun perlenv-get-perl-inc-args ()
  "Get -I... argument"
  (if perlenv-local-lib-path
      (list (format "-I%s/lib/perl5/%s" perlenv-local-lib-path perlenv-perl-archname)
            (format "-I%s/lib/perl5" perlenv-local-lib-path))
    '()))

(defun perlenv-perldb ()
  "Run perl debugger with perlbrew & local::lib"
  (interactive)
  (let ((exec-path (perlenv-build-exec-path))
        (process-environment (perlenv-build-process-environment)))
    (cperl-db)))

(defun perlenv-perlbrew-list ()
  (mapcar (lambda (s)
            (string-match "^perl-\\(.*\\)$" s)
            (match-string 1 s))
          (remove-if (lambda (ss)
                       (not (string-match "^perl-" ss)))
                     (directory-files (concat perlenv-perlbrew-root "/perls")))))

(provide 'perlenv)