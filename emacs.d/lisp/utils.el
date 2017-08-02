;;; utils.el --- crap I need                     -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Artur Ventura

;; Author: Artur Ventura <artur.ventura@gmail.com>
;; Keywords: lisp
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defun fullscreen ()
  "Enables fullscreen."
  (interactive)
  (toggle-frame-fullscreen))

(defun delete-line ()
  "Delete without the 'kill-ring'."
  (interactive)
  (delete-region (point) (line-end-position)))

(defun move-forward-block (&optional n)
  "Move N blocks up."
  (interactive)
  (let ((n (if (null n) 1 n)))
    (re-search-forward "\n[\t\n ]*\n+" nil "NOERROR" n)))

(defun move-backward-block (&optional n)
  "Move N blocks down."
  (interactive)
  (let ((n (if (null n) 1 n))
        (-i 1))
    (while (<= -i n)
      (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR")
          (progn (skip-chars-backward "\n\t "))
        (progn (goto-char (point-min))
               (setq -i n)))
      (setq -i (1+ -i)))))

(use-package virtualenvwrapper
  :ensure t
  :init (progn
	  (venv-initialize-interactive-shells)))


(defun django-shell ()
  "Run django shell."
  (interactive)
  (let ((dir (read-directory-name "Django Project Directory: "
				  (projectile-project-root))))

    (cd (expand-file-name dir))
    (message (concat (python-shell-calculate-command)
		      " "

		      (expand-file-name (concat dir "manage.py"))
		      " shell"))
    (run-python (concat (python-shell-calculate-command)
			" "
			"manage.py"
			" shell --plain") nil t)))
;; (defun django-shell-plus ()
;;   "Run django shell."
;;   (interactive)
;;   (let ((dir (read-directory-name "Django Project Directory: " (projectile-project-root))))

;;     (cd (expand-file-name dir))
;;     (message (concat (python-shell-calculate-command)
;; 		      " "

;; 		      "manage.py"
;; 		      " shell_plus"))
;;     (run-python (concat (python-shell-calculate-command)
;; 		      " "

;; 		      "manage.py"
;; 		      " -- shell_plus --plain") nil t)))

;; (setq
;;  python-shell-interpreter "python"
;;  python-shell-prompt-regexp "In : "

;;  python-shell-completion-setup-code
;;  "from IPython.core.completerlib import module_completion"
;;  python-shell-completion-module-string-code
;;  "';'.join(module_completion('''%s'''))\n"
;;  python-shell-completion-string-code
;;  "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")


(provide 'utils)
;;; utils.el ends here
