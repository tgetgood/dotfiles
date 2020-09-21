(defvar paren-hooks '(scheme-mode-hook
                      lisp-mode-hook
                      emacs-lisp-mode-hook))

(use-package smartparens)

(use-package paredit
  :demand t
  :init
  (defun conditionally-enable-paredit-mode ()
    "enable paredit-mode during eval-expression"
    (if (eq this-command 'eval-expression)
        (paredit-mode 1)))

  (defun catchy-p-dq (&optional n)
    (interactive "P")
    (condition-case nil
        (paredit-doublequote)
      (error (progn
               (insert ?\")
               (insert ?\")
               (backward-char)))))

  :bind (("\"" . catchy-p-dq)
         ("M-r" . nil)
         ("M-j" . nil))

  :hook ((paredit-mode . smartparens-strict-mode)
         (minibuffer-setup . conditionally-enable-paredit-mode))
  :config
  (dolist (h paren-hooks)
	  (add-hook h 'enable-paredit-mode)))



;; Helper to wrap sexps more intuitively

;; Extensions to evil-smartparens

;;;;;


;; Paredit keymap 

(define-key evil-normal-state-map (kbd "g l") (smart-slurp paredit-forward-slurp-sexp))
(define-key evil-normal-state-map (kbd "g L") 'paredit-forward-barf-sexp)
(define-key evil-normal-state-map (kbd "g h") (smart-slurp paredit-backward-slurp-sexp))
(define-key evil-normal-state-map (kbd "g H") 'paredit-backward-barf-sexp)
(define-key evil-normal-state-map (kbd "g k k") 'cljr-raise-sexp)
(define-key evil-normal-state-map (kbd "g k K") 'paredit-splice-sexp)
(define-key evil-normal-state-map (kbd "g k h") 'cljr-splice-sexp-killing-backward)
(define-key evil-normal-state-map (kbd "g k l") 'cljr-splice-sexp-killing-forward)
(define-key evil-normal-state-map (kbd "g j") 'paredit-join-sexps)
(define-key evil-normal-state-map (kbd "g s") 'paredit-split-sexp)

(define-key evil-normal-state-map (kbd "g c") 'sp-convolute-sexp)

(define-key evil-normal-state-map (kbd "g (") (smart-wrap paredit-wrap-round))
(define-key evil-normal-state-map (kbd "g [") (smart-wrap paredit-wrap-square))
(define-key evil-normal-state-map (kbd "g {") (smart-wrap paredit-wrap-curly))
(define-key evil-normal-state-map (kbd "g \"") (smart-wrap wrap-double-quote))

(define-key evil-normal-state-map (kbd "g :") 'clojure-toggle-keyword-string)

(define-key evil-visual-state-map (kbd "g (") 'paredit-wrap-round)
(define-key evil-visual-state-map (kbd "g [") 'paredit-wrap-square)
(define-key evil-visual-state-map (kbd "g {") 'paredit-wrap-curly)
(define-key evil-visual-state-map (kbd "g \"") 'wrap-double-quote)

