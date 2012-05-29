# perlenv.el

Simple `perlbrew` and `local::lib` wrapper for emacs. It makes emacs aware of them.

## Usage

In your .emacs

    (require 'perlenv)
    (perlenv-perlbrew-use "5.12.4")
    (perlenv-set-local-lib (substitute-in-file-name "$HOME/perl5/mylocal"))

If your perlbrew is not installed on default location ($HOME/perl5/perlbrew), you should set its location
before `(perlenv-perbrew-use ...)`.

    (require 'perlenv)
    (setq perlenv-perlbrew-root (substitute-in-file-name "$HOME/myperlbrew"))
    (perlenv-perlbrew-use "5.12.4")
    ....

You can use following commands

* `M-x perlenv-perldb`: invoke perl debugger mode
* `M-x perlenv-perlbrew-use`: change perl version in perlbrew
* `M-x perlenv-set-local-lib`: change local::lib path

You can use following functions

* `perlenv-get-perl-path`
* `perlenv-get-perl-inc-args`
* `perlenv-build-exec-path`
* `perlenv-build-process-environment`

## Integration with `flymake`

    (require 'flymake)

    (defun flymake-perl-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list (perlenv-get-perl-path)
              (append (perlenv-get-perl-inc-args) (list "-MProject::Libs" "-wc" local-file)))))

    (add-hook 'cperl-mode-hook (lambda () (flymake-mode t)))

