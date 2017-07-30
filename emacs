;;; Package --- sumary
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: UTF-8 -*-
;; Copyright (C) 2016 by Artur Ventura
;;
;; File:  .emacs
;;
;; Author: Artur Ventura
;;

;;; Code:
(progn
  (setq user-full-name "Artur Ventura")
  (setq user-mail-address "artur.ventura@gmail.com")

  (setq visible-bell t)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-echo-area-message "nurv")
  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq load-prefer-newer t)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (delete-selection-mode 1)

  (if (and window-system
	   (eq system-type 'darwin))
      (setq scroll-step 1))
  (setq mac-option-modifier nil)
  (setq mac-command-modifier 'meta)
  (setq x-select-enable-clipboard t)
  
  (column-number-mode 1)
  (line-number-mode 1)
  (defvar explicit-shell-file-name "zsh")
  (defvar nurv/backup-directory "~/.tmp"))

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; ;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(progn ;    `use-package'
  (require  'use-package)
  (setq use-package-verbose t)
  (setq use-package-always-defer t)
  (setq use-package-enable-imenu-support t))

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package custom
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package server
  :config (or (server-running-p) (server-mode)))

(use-package abbrev
  :diminish ""
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package anaconda-mode
  :ensure t)

(use-package anzu
  :ensure t
  :diminish 'anzu-mode
  :config (global-anzu-mode)
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)))

(use-package autorevert
  :config
  (global-auto-revert-mode 1)

  ;; auto-update dired buffers
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))


(use-package all-the-icons
  :config (progn (require 'all-the-icons))
  :ensure t)

(use-package atom-one-dark-theme
  :ensure t
  :init (progn
	    (load-theme 'atom-one-dark t t))
  :config
  (defun go-atom ()
    "Change to atom theme."
    (interactive)
    (let ()
      (enable-theme 'atom-one-dark)
;      (leet-setup-modeline-format)
      (set-face-background hl-line-face "purple4"))))

(use-package beginend
  :ensure t
  :config
  (beginend-global-mode)
  (diminish 'beginend-global-mode))

(use-package browse-url
  :ensure t
  :config
  (setq browse-url-generic-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))

(use-package buffer-move
  :ensure t
  :bind (("M-S-<up>" . buf-move-up)
	 ("M-S-<down>" . buf-move-down)
	 ("M-S-<left>" .  buf-move-left)
	 ("M-S-<right>" . buf-move-right)))

(use-package company
  :ensure t
  :diminish ""
  :init (progn
          (add-hook 'prog-mode-hook 'company-mode))
  :config (progn
            (setq company-idle-delay 0.5)
            (setq company-tooltip-limit 10)
            (setq company-minimum-prefix-length 2)
            (setq company-tooltip-flip-when-above t)
	    (add-to-list 'company-backends 'company-anaconda)))

(use-package counsel
  :ensure t
  :demand t
  :bind (("M-x" . counsel-M-x)
         ("M-i" . counsel-imenu)
         ("M-y" . counsel-yank-pop)))

(use-package cua-mode
  :init (progn
	  (cua-mode t)))

(use-package dabbrev
  :bind (("C-c <tab>" . dabbrev-expand)))

(use-package dark-laptop-theme
  :init (progn
	  (load-theme 'dark-laptop t t))
  :config
  (defun go-black ()
    "Change to black theme."
    (interactive)
    (let ()
      
      (enable-theme 'dark-laptop)
      (set-face-background hl-line-face "purple4"))))

(use-package dired
  :bind (:map dired-mode-map
              ("M-s" . find-name-dired)
              ("C-k" . dired-kill-subdir))
  :init (progn
          (add-hook 'dired-mode-hook #'dired-hide-details-mode))
  :config (progn
            (setq dired-listing-switches "-al")
            (setq dired-dwim-target t)
            (put 'dired-find-alternate-file 'disabled nil)))

(use-package dired-x
  :after dired
  :init (progn
          (add-hook 'dired-mode-hook #'dired-omit-mode))
  :config (progn
            (setq dired-omit-files "^\\...+$")))

(use-package emmet-mode
   :demand t)

(use-package electric
  :demand t
  :bind (("C-o" . electric-newline-and-maybe-indent))
  :config (progn
	    (electric-pair-mode)))

(use-package erc
  :config (progn
            (setq erc-email-userid "nurv")))

(use-package files
  :demand t
  :config
  (setq delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t
        )
  (setq backup-directory-alist
        `((".*" . ,nurv/backup-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,nurv/backup-directory t)))
  (setq create-lockfiles nil))

(use-package flycheck
  :diminish 'flycheck-mode
  :commands (flycheck-mode)
  :init (progn
	  (add-hook 'prog-mode-hook #'flycheck-mode)
	  (add-hook 'after-init-hook #'global-flycheck-mode)))

(use-package flyspell
  :ensure t
  :diminish 'flyspell-mode
  :bind (:map flyspell-mode-map
              ("M-;" . nil))
  :init (progn
          (add-hook 'prog-mode-hook #'flyspell-prog-mode)
          (dolist (mode-hook '(text-mode-hook org-mode-hook LaTeX-mode-hook))
            (add-hook mode-hook #'flyspell-mode))))

(use-package flx
  :ensure t)

(use-package ftgp
  :demand t)


;; (use-package git-gutter
;;   :ensure t
;;   :init (progn
;; 	    (global-git-gutter-mode)))

(use-package diff-hl
  :config (progn
            (global-diff-hl-mode)
            (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t)))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package iso-transl
  :demand t)

(use-package ivy
  :diminish ""
  :demand t
  :config (progn
            (ivy-mode 1)
            (setq ivy-use-virtual-buffers t)
	    (setq ivy-re-builders-alist
		  '((ivy-switch-buffer . ivy--regex-plus)
		    (t . ivy--regex-fuzzy)))
	    (setq ivy-initial-inputs-alist nil)))

(use-package jedi
  :init (progn
	  (add-hook 'python-mode-hook 'jedi:setup)
	  (setq jedi:complete-on-dot t)))


(use-package hl-line-mode
  :init (progn
	    (add-hook 'prog-mode-hook #'hl-line-mode)))

(use-package lisp-mode
  :config
  (defun indent-spaces-mode ()
    (setq indent-tabs-mode nil))
  (add-hook 'lisp-interaction-mode-hook #'indent-spaces-mode))

(use-package linum
  :bind (("C-c l" . linum-mode))
  :init (progn
	  (add-hook 'prog-mode-hook #'linum-mode))
  :config (progn
	    (setq linum-format "%4d \u2502")))

(use-package magit
  :bind (("C-x g"   . magit-status)))

(use-package multiple-cursors
  :bind (("M-RET" . mc/edit-lines)
         ("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-M-<" . mc/unmark-next-like-this)
         ("C-M->" . mc/unmark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package open-url-at-point
  :config (defun open-url-at-point (&optional arg)
  "Browse the url at point.
Open the url in a www browser or, when called with a prefix
argument, in Emacs."
  (interactive "P")
  (let ((url (browse-url-at-point)))
   (if arg
       (browse-url-emacs url)
     (browse-url url))))
  :bind ("C-c C-o" . open-url-at-point))

(use-package paren
  :demand t
  :config (show-paren-mode 1))

(use-package projectile
  :demand t
  :diminish ""
  :bind ("M-t" . counsel-projectile-find-file)
  :config (progn
            (projectile-mode)
            (require 'counsel-projectile)
            (counsel-projectile-on)))

(use-package rainbow-mode
  :ensure t
  :init
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'web-mode-hook 'rainbow-mode))

(use-package recentf
  :ensure t
  :demand t
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.htm\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-hook 'web-mode-hook  'emmet-mode)
  (add-hook 'web-mode-hook  'yas-minor-mode)
  (set-face-attribute 'web-mode-html-tag-face nil :foreground "#61AFEF")
  (set-face-attribute 'web-mode-html-attr-name-face nil :foreground "#E06C75")

  (setq etq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t))

(use-package whitespace
  :ensure t
  :bind (("C-c w" . whitespace-mode))
  :config
  (setq whitespace-display-mappings
        '(
          (space-mark 32 [183] [46]) ; normal space, ·
          (space-mark 160 [164] [95])
          (space-mark 2208 [2212] [95])
          (space-mark 2336 [2340] [95])
          (space-mark 3616 [3620] [95])
          (space-mark 3872 [3876] [95])
          (newline-mark 10 [182 10]) ; newlne, ¶
          (tab-mark 9 [9655 9] [92 9]) ; tab, ▷
          )))

(use-package winner
  :bind (("C-c <left>" . winner-undo)
	 ("C-c <right>" . winner-redo))
  :config
  (when (fboundp 'winner-mode)
    (winner-mode 1)))

(use-package pabbrev
  :ensure t)

(use-package prog-mode
  :bind (("C-;" . comment-line)))

(use-package python-mode
  :init (progn
	  (add-hook 'python-mode-hook 'anaconda-mode)
	  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))

(use-package redo+
  :ensure t
  :bind (("C-S-z" . redo)))

(use-package yasnippet
  :diminish 'yas-minor-mode
  :ensure t
  :demand t
  :init (progn
          (yas-global-mode)
	  (yas-reload-all)
	  (add-to-list 'yas-snippet-dirs (expand-file-name "~/.emacs.d/snippets/django"))
	  (add-hook 'python-mode-hook #'yas-minor-mode)))

(use-package zoom-frm
  :ensure t
  :bind (("C-+" . zoom-frm-in)
         ("C--" . zoom-frm-out)))

(use-package ido
  :ensure t
  :init (progn
	    (ido-mode t)))

;; ;; Full screen
(if window-system
    (defun fullscreen ()
      (interactive)
      (toggle-frame-fullscreen)))




;; (setq frame-title-format '(buffer-file-name "%f" ("%b")))


;; ;; Font
(if window-system
    (when (eq system-type 'darwin) 
      (set-face-attribute 'default nil :family "SF Mono")
      (set-face-attribute 'default nil :height 120)
      (setq-default line-spacing 3)))


(defun xah-forward-block (&optional n)
  (interactive "p")
  (let ((n (if (null n) 1 n)))
    (re-search-forward "\n[\t\n ]*\n+" nil "NOERROR" n)))

(defun xah-backward-block (&optional n)
  (interactive "p")
  (let ((n (if (null n) 1 n))
        (-i 1))
    (while (<= -i n)
      (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR")
          (progn (skip-chars-backward "\n\t "))
        (progn (goto-char (point-min))
               (setq -i n)))
      (setq -i (1+ -i)))))

(global-set-key (kbd "<M-up>") 'xah-backward-block)
(global-set-key (kbd "<M-down>") 'xah-forward-block)


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

;; (defmacro cached-for (secs &rest body)
;;   "Cache for SECS the result of the evaluation of BODY."
;;   (declare (debug t))
;;   (let ((cache (make-symbol "cache"))
;;         (last-run (make-symbol "last-run")))
;;     `(let (,cache ,last-run)
;;        (lambda ()
;;          (when (or (null ,last-run)
;;                    (> (- (time-to-seconds (current-time)) ,last-run)
;;                       ,secs))
;;            (setf ,cache (progn ,@body))
;;            (setf ,last-run (time-to-seconds (current-time))))
;;          ,cache))))


(defun leet-face-when-active (face)
  "Return FACE if the window is active."
  (when (leet--active-window-p)
    face))

(defun true-color-p ()
  "Return non-nil on displays that support 256 colors."
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))

(defvar leet-use-paddings-in-mode-line t)

(defvar leet-selected-window nil
  "Selected window.")

  (defun leet--set-selected-window (&rest _)
    "Set the selected window."
    (let ((window (frame-selected-window)))
      (when (and (windowp window)
                 (not (minibuffer-window-active-p window)))
        (setq leet-selected-window window))))

(defun leet--active-window-p ()
  "Return non-nil if the current window is active."
  (eq (selected-window) leet-selected-window))

(add-hook 'window-configuration-change-hook #'leet--set-selected-window)
(add-hook 'focus-in-hook #'leet--set-selected-window)
(advice-add 'select-window :after #'leet--set-selected-window)
(advice-add 'select-frame  :after #'leet--set-selected-window)

(defvar leet-buffer-coding '(:eval (unless (eq buffer-file-coding-system (default-value 'buffer-file-coding-system))
                                         mode-line-mule-info)))

(defvar leet-modeline-ro '(:eval (if buffer-read-only
                                         (if (leet--active-window-p)
                                             (progn
                                               (propertize "RO " 'face 'leet-ro-face))
                                           (propertize "RO " 'face 'bold))
                                       "")))

(defvar leet-modeline-modified '(:eval (if (buffer-modified-p (current-buffer))
                                               (all-the-icons-faicon "floppy-o"
                                                                     :height 0.9
                                                                     :v-adjust 0
                                                                     :face (if (leet--active-window-p)
                                                                               'leet-modified-face
                                                                             'mode-line-inactive))
                                             (all-the-icons-faicon "check"
                                                                   :height 0.9
                                                                   :v-adjust 0
                                                                   :face (if (leet--active-window-p)
                                                                             'leet-not-modified-face
                                                                           'mode-line-inactive)))))
(require 'all-the-icons)

(setq with-editor-emacsclient-executable nil)
(setq ispell-program-name "aspell")

(defvar leet-modeline-buffer-identification '(:eval (propertize "%b" 'face 'bold))
  "Mode line construct for displaying the position in the buffer.")

(defvar leet-modeline-position '(:eval (propertize ":%l:%c %p " 'face (if (leet--active-window-p)
                                                                              'leet-buffer-position-face
                                                                            'mode-line-inactive)))
  "Mode line construct for displaying the position in the buffer.")
(defvar leet-modeline-vc '(vc-mode ("   "
                                        (:eval (all-the-icons-faicon "code-fork"
                                                                     :height 0.9
                                                                     :v-adjust 0
                                                                     :face (when (leet--active-window-p)
                                                                             (leet-git-face))))
                                        (:eval (propertize (truncate-string-to-width vc-mode 25 nil nil "...")
                                                           'face (when (leet--active-window-p)
                                                                   (leet-git-face)))))))

(defun leet-modeline-flycheck-status ()
  "Return the status of flycheck to be displayed in the mode-line."
  (when flycheck-mode
    (let* ((text (pcase flycheck-last-status-change
                   (`finished (if flycheck-current-errors
                                  (let ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                                                 (+ (or .warning 0) (or .error 0)))))
                                    (propertize (format "✖ %s Issue%s" count (if (eq 1 count) "" "s"))
                                                'face (leet-face-when-active 'leet-error-face)))
                                (propertize "✔ No Issues"
                                            'face (leet-face-when-active 'leet-ok-face))))
                   (`running     (propertize "⟲ Running"
                                             'face (leet-face-when-active 'leet-warning-face)))
                   (`no-checker  (propertize "⚠ No Checker"
                                             'face (leet-face-when-active 'leet-warning-face)))
                   (`not-checked "✖ Disabled")
                   (`errored     (propertize "⚠ Error"
                                             'face (leet-face-when-active 'leet-error-face)))
                   (`interrupted (propertize "⛔ Interrupted"
                                             'face (leet-face-when-active 'leet-error-face)))
                   (`suspicious  ""))))
      (propertize text
                  'help-echo "Show Flycheck Errors"
                  'local-map (make-mode-line-mouse-map
                              'mouse-1 #'flycheck-list-errors)))))

(defface leet-ro-face
  '((t :foreground "#0088CC" :weight bold))
  "Face for read-only buffer in the mode-line.")

(defface leet-modified-face
  '((t :foreground "#ff6c6b" :height 0.9))
  "Face for modified buffers in the mode-line.")

(defface leet-not-modified-face
  '((t :foreground "#98be65" :height 0.9))
  "Face for not modified buffers in the mode-line.")

(defface leet-buffer-position-face
  '((t :height 0.9))
  "Face for line/column numbers in the mode-line.")

(defface leet-vc-face
  '((t :foreground "#61afef"))
  "Face for vc status in the mode-line.")

(defface leet-ok-face
  '((t :foreground "#61afef"))
  "Face for ok status in the mode-line.")

(defvar leet--git-face-cached (lambda nil (leet--git-face-intern)))

(defun leet--git-face-intern ()
  "Return the face to use based on the current repository status."
  (if (magit-git-success "diff" "--quiet")
      ;; nothing to commit because nothing changed
      (if (zerop (length (magit-git-string
                          "rev-list" (concat "origin/"
                                             (magit-get-current-branch)
                                             ".."
                                             (magit-get-current-branch)))))
          ;; nothing to push as well
          'leet-ok-face
        ;; nothing to commit, but some commits must be pushed
        'leet-warning-face)
    'leet-error-face))

(defun leet-git-face ()
  "Return the face to use based on the current repository status.
The result is cached for one second to avoid hiccups."
  (funcall leet--git-face-cached))

(defface leet-warning-face
  '((t :foreground "#da8548"))
  "Face for warning status in the mode-line.")

(defface leet-error-face
  '((t :foreground "#ff6c6b"))
  "Face for error status in the mode-line.")

(defun leet-setup-modeline-format ()
  "Setup the mode-line format for leet."
  (interactive)
  (require 'flycheck)
  (require 'magit)
  (let ((class '((class color) (min-colors 89)))
        (light (if (true-color-p) "#ccd4e3" "#d7d7d7"))
        (comment (if (true-color-p) "#687080" "#707070"))
        (purple "#c678dd")
        (mode-line (if "#1c2129" "#222222")))
    (custom-theme-set-faces
     'atom-one-dark

     ;; Mode line faces
     `(mode-line ((,class (:background ,mode-line
                                       :height 0.9
                                       :foreground ,light
                                       :box ,(when leet-use-paddings-in-mode-line
                                               (list :line-width 6 :color mode-line))))))
     `(mode-line-inactive ((,class (:background ,mode-line
                                                :height 0.9
                                                :foreground ,comment
                                                :box ,(when leet-use-paddings-in-mode-line
                                                        (list :line-width 6 :color mode-line))))))
     `(anzu-mode-line ((,class :inherit mode-line :foreground ,purple :weight bold)))
     ))

  (setq-default mode-line-format
                `("%e"
                  " "
                  ,leet-modeline-ro " "
                  ,leet-buffer-coding
                  mode-line-frame-identification " "
                  " "
                  ,leet-modeline-modified
                  " "
                  ,leet-modeline-buffer-identification
                  ,leet-modeline-position
                  ,leet-modeline-vc
                 "  "
                  (:eval (leet-modeline-flycheck-status))
                  "  " mode-line-modes mode-line-misc-info mode-line-end-spaces
                  )))

(provide 'emacs)

;;; emacs ends here
