;;; django-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (django-html-mode) "django-html-mode" "django-html-mode.el"
;;;;;;  (21305 43341 0 0))
;;; Generated autoloads from django-html-mode.el

(autoload 'django-html-mode "django-html-mode" "\
Major mode for editing Django html templates (.djhtml).

\\{django-html-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.djhtml$" . django-html-mode))

;;;***

;;;### (autoloads (django-mode) "django-mode" "django-mode.el" (21305
;;;;;;  43341 0 0))
;;; Generated autoloads from django-mode.el

(autoload 'django-mode "django-mode" "\
Major mode for Django web framework.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\<\\(models\\|views\\|handlers\\|feeds\\|sitemaps\\|admin\\|context_processors\\|urls\\|settings\\|tests\\|assets\\|forms\\).py" . django-mode))

;;;***

;;;### (autoloads nil nil ("django-mode-pkg.el") (21305 43341 263316
;;;;;;  0))

;;;***

(provide 'django-mode-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; django-mode-autoloads.el ends here
