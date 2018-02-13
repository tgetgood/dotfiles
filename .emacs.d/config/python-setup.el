(setq python-indent-offset 2)

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook 'flycheck-mode)

(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(define-key anaconda-mode-map (kbd "TAB") 'company-anaconda)

(elpy-enable)
