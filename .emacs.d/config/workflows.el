;;;;;
;; cljs + clj + grunt
;;;;;

(defun start-cljs-project ()
	(interactive)
	(cider-jack-in)
	(lein-run)
	(grunt-watch)
	(cljsbuild-start "lein cljsbuild auto dev"))

(global-set-key (kbd "C-c C-v s") 'start-cljs-project)
