(add-hook 'markdown-mode-hook
					(lambda ()
						(progn
							(auto-fill-mode 1)
							(add-hook 'before-save-hook 'whitespace-cleanup nil t))))
