(require 'rust-mode)
(require 'rustic)

(use-package rustic)

(setq rustic-lsp-client 'eglot)
(setq rustic-lsp-server 'rls)

(setq lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer"))

(add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
(setq flymake-start-on-save-buffer nil)
(setq flymake-start-on-flymake-mode nil)
(setq flymake-start-syntax-check-on-find-file nil)

(add-hook 'rustic-mode-hook 'cargo-minor-mode)
(add-hook 'racer-mode-hook 'eldoc-mode)
(add-hook 'cargo-process-mode-hook 'visual-line-mode)

(remove-hook 'rustic-mode-hook 'flycheck-mode)
(remove-hook 'rustic-mode-hook 'flymake-mode)

(setq racer-rust-src-path
			"~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")

(defun cargo-process-add (crate)
	"Adds crate to current cargo project."
	(interactive "sCrate: ")
	(cargo-process--start "Add" (concat "cargo add " crate)))

(defun cargo-process-install (crate)
	"Installs the cargo crate in the current toolchain."
	(interactive "sCrate: ")
	(cargo-process--start "Install" (concat "cargo install " crate)))

;; (evil-define-key 'normal rust-mode-map "K" 'racer-describe)

(evil-leader/set-key-for-mode 'rustic-mode
	"c" 'rustic-cargo-check
	"b" 'rustic-cargo-build
	"r" 'rustic-cargo-run
	"t" 'rustic-cargo-test

	"f" 'rustic-cargo-fmt

	"n" 'eglot-rename)

