(use-package autodisass-llvm-bitcode)
(use-package eglot)

(use-package rustic
  :ensure t
  :demand t
  :after (eglot evil-leader)
  :config 
  (define-key rustic-mode-map (kbd "C-c SPC") 'rustic-cargo-fmt)
  (setq rustic-lsp-client 'eglot)
  (setq rustic-lsp-server 'rls)

  (setq lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer"))

  (add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode -1)))
  (setq flymake-start-on-save-buffer nil)
  (setq flymake-start-on-flymake-mode nil)

  (add-hook 'rustic-mode-hook 'cargo-minor-mode)
  (add-hook 'cargo-process-mode-hook 'visual-line-mode)

  (remove-hook 'rustic-mode-hook 'flycheck-mode)
  (remove-hook 'rustic-mode-hook 'flymake-mode)

  (evil-leader/set-key-for-mode 'rustic-mode
    "c" 'rustic-cargo-check
    "b" 'rustic-cargo-build
    "r" 'rustic-cargo-run
    "t" 'rustic-cargo-test

    "f" 'rustic-cargo-fmt

    "n" 'eglot-rename))

