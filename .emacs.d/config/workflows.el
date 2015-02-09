;;;;;
;; cljs + clj + grunt
;;;;;

(defun start-cljs-project ()
	(interactive)
	(cider-jack-in)
	(lein-run)
	(cljsbuild-start "lein trampoline cljsbuild auto dev")
	(grunt-watch)
	)

(global-set-key (kbd "C-c C-v s") 'start-cljs-project)
