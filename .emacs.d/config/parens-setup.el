(defvar paren-hooks '(scheme-mode-hook
                      lisp-mode-hook
                      emacs-lisp-mode-hook))

(use-package smartparens)

(use-package paredit
  :commands (paredit-mode paredit-doublequote)
  :demand t
  :init
  (defun conditionally-enable-paredit-mode ()
    "enable paredit-mode during eval-expression"
    (if (eq this-command 'eval-expression)
        (paredit-mode 1)))

  :bind (("M-r" . nil)
         ("M-j" . nil))

  :hook ((paredit-mode . smartparens-strict-mode)
         (minibuffer-setup . conditionally-enable-paredit-mode))
  :config
  (dolist (h paren-hooks)
    (add-hook h 'enable-paredit-mode)))
