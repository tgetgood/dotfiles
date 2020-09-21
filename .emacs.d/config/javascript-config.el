;;;; Something of a misnomer. All web stuff gets lumped here.

;; CSS mode is built into emacs
(setq css-indent-offset 2)

;; So is js.el
(setq js-indent-level 2)

(use-package scss-mode
  :config
  (setq scss-compile-at-save nil))

(use-package json-mode)

;; (use-package tagedit
;;   :commands (tagedit-add-experimental-features
;;              tagedit-add-paredit-like-keybindings
;;              tagedit-mode)

;;   :hook (html-mode . (lambda () (tagedit-mode 1)))

;;   :config
;;   ;; TODO: Set evil keybindings for this
;;   (tagedit-add-experimental-features)
;;   (tagedit-add-paredit-like-keybindings))

(use-package yaml-mode)

(use-package vue-mode
  :mode "\\.vue\\'" )

(use-package vue-html-mode)

(use-package nodejs-repl
  :commands (nodejs-repl-quit-or-cancel
             nodejs-repl-load-file
             nodejs-repl)

  :bind (:map nodejs-repl-mode-map
              ("TAB" . nodejs-repl-complete-from-process)))

(use-package js2-mode
  :init
  (defun load-file-in-new-node-repl ()
    (interactive)
    (let ((buff (buffer-file-name)))
      (progn
        (if (get-buffer "*nodejs*")
            (progn
              (nodejs-repl-quit-or-cancel)
              (nodejs-repl-quit-or-cancel)))
        (nodejs-repl)
        (nodejs-repl-load-file buff)
        (insert "\n")
        (goto-char (point-max)))))

  :mode "\\.js\\'" 
  :after (nodejs-repl)

  :hook  (js2-mode . (lambda ()
                       (add-hook 'before-save-hook 'whitespace-cleanup nil t)))

  :bind (("C-c M-j" . nodejs-repl)
         ("C-c C-k" . load-file-in-new-node-repl)
         ("C-c C-z" . nodejs-repl-switch-to-repl))

  :config
  (setq js2-skip-preprocessor-directives t)
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-include-node-externs t))

(use-package js2-refactor
  :after (js2-mode)
  :hook (js2-mode . js2-refactor-mode))

(use-package xref-js2)

