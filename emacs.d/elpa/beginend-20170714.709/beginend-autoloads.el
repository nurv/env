;;; beginend-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "beginend" "beginend.el" (22908 44444 0 0))
;;; Generated autoloads from beginend.el

(autoload 'beginend-setup-all "beginend" "\
Use beginend on all compatible modes.
For example, this activates function `beginend-dired-mode' in `dired' and
function `beginend-message-mode' in `message-mode'.  All affected minor
modes are described in `beginend-modes'.

\(fn)" nil nil)

(autoload 'beginend-unsetup-all "beginend" "\
Remove beginend from all compatible modes in `beginend-modes'.

\(fn)" nil nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; beginend-autoloads.el ends here
