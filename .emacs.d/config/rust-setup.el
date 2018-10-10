(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'racer-mode-hook 'eldoc-mode)
(add-hook 'cargo-process-mode-hook 'visual-line-mode)

(setq rust-format-on-save t)

(defun cargo-process-add (crate)
	"Adds crate to current cargo project."
	(interactive "sCrate: ")
	(cargo-process--start "Add" (concat "cargo add " crate)))

(defun cargo-process-install (crate)
	"Installs the cargo crate in the current toolchain."
	(interactive "sCrate: ")
	(cargo-process--start "Install" (concat "cargo install " crate)))

(evil-define-key 'normal racer-mode-map "K" 'racer-describe)

(evil-leader/set-key-for-mode 'rust-mode
	"c" 'cargo-process-check
	"b" 'cargo-process-build
	"r" 'cargo-process-run)
