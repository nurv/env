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

  (setq initial-buffer-choice t)
  (setq initial-scratch-message "")
  (setq load-prefer-newer t)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (when (eq system-type 'gnu/linux)
    (menu-bar-mode 0))
  (scroll-bar-mode 0)
  (tool-bar-mode 0)
  (delete-selection-mode 1)

  (if (and window-system
	   (eq system-type 'darwin))
      (setq scroll-step 1))
  (setq mac-option-modifier nil)
  (setq mac-command-modifier 'meta)
  (setq select-enable-clipboard t)
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  
  (setq with-editor-emacsclient-executable nil)
  (setq ispell-program-name "/usr/local/bin/aspell")

  (column-number-mode 1)
  (line-number-mode 1)
  (defvar explicit-shell-file-name "zsh")
  (defvar nurv/backup-directory "~/.tmp")
  
  (if window-system
      (progn
	(when (eq system-type 'darwin)
	  (set-face-attribute 'default nil :family "SF Mono")
	  (set-face-attribute 'default nil :height 150)
	  (setq-default line-spacing 5))
	(when (eq system-type 'gnu/linux)
	  (set-face-attribute 'default nil :family "SF Mono")
	  (set-face-attribute 'default nil :height 100)
	  (setq-default line-spacing 3)))))


(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("nognu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; ;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(progn ;   local packages
  (let ((dir (expand-file-name "local" user-emacs-directory)))
    (when (file-exists-p dir)
      (add-to-list 'load-path dir))))

(progn ;    `use-package'
  (require  'use-package)
  (setq use-package-verbose t)
  (setq use-package-always-defer t)
  (setq use-package-enable-imenu-support t))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (pos) 'read-face-name)
                  (get-char-property (pos) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun install-if-not-exist(package)
  (if (not (package-installed-p package))
     	 (package-install package)))

(defmacro ensure-package (package &rest config)
  `(progn
     (install-if-not-exist ',package)
     ,(cons 'use-package (cons package config))))
  
     
      
(ensure-package try
  :ensure t)

(ensure-package which-key
  :ensure t
  :config
  (which-key-mode))

(setq ring-bell-function
      (lambda ()
        (let ((orig-fg (face-foreground 'mode-line))
	      (orig-bg (face-background 'mode-line)))
          (set-face-background 'mode-line "#820333")
          (set-face-foreground 'mode-line "#ffffff")
          (run-with-idle-timer 0.1 nil
                               (lambda (fg bg)
				 (set-face-foreground 'mode-line fg)
				 (set-face-background 'mode-line bg))
                               orig-fg orig-bg))))

(use-package custom
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(ensure-package server
  :config (or (server-running-p) (server-mode)))

(use-package abbrev
  :diminish ""
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(ensure-package anaconda-mode
  :ensure t)

(ensure-package anzu
  :ensure t
  :diminish 'anzu-mode
  :config (global-anzu-mode)
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp)))

(ensure-package autorevert
  :config
  (global-auto-revert-mode 1)

  ;; auto-update dired buffers
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))


(ensure-package all-the-icons
  :config (progn (require 'all-the-icons))
  :ensure t)

(use-package leet-modeline
  :init (progn (require 'leet-modeline)))

(ensure-package atom-one-dark-theme
  :ensure t
  :init (progn
	    (load-theme 'atom-one-dark t t))
  :config
  (defun go-atom ()
    "Change to atom theme."
    (interactive)
    (let ()
      (enable-theme 'atom-one-dark)
;;      (leet-setup-modeline-format)
      (set-face-background hl-line-face "purple4"))))

(ensure-package doom-themes
		:ensure t
		:init (progn
			(load-theme 'doom-one t t))
		:config
		(defun go-doom ()
		  "Change to doom theme"
		  (interactive)
		  (let ()
		    (enable-theme 'doom-one))))

(ensure-package beginend
  :ensure t
  :config
  (beginend-global-mode)
  (diminish 'beginend-global-mode))

(ensure-package browse-url
  :ensure t
  :config
  (setq browse-url-generic-program "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))

(ensure-package buffer-move
  :ensure t
  :bind (("M-S-<up>" . buf-move-up)
	 ("M-S-<down>" . buf-move-down)
	 ("M-S-<left>" .  buf-move-left)
	 ("M-S-<right>" . buf-move-right)))

(ensure-package company
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

(ensure-package counsel
  :ensure t
  :demand t
  :bind (("M-x" . counsel-M-x)
         ("M-i" . counsel-imenu)
         ("M-y" . counsel-yank-pop)))

(use-package cua-mode
  :init (progn
	  (cua-mode t)))

(ensure-package dabbrev
  :bind (("C-c <tab>" . dabbrev-expand)))

(install-if-not-exist 'color-theme-modern)
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
  :init (progn)
  :config (progn
            (setq dired-omit-files "^\\...+$")))

(ensure-package emmet-mode
   :demand t)

(ensure-package electric
  :demand t
  :bind (("C-o" . electric-newline-and-maybe-indent))
  :config (progn
	    (electric-pair-mode)))

(ensure-package erc
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

(ensure-package flycheck
  :diminish 'flycheck-mode
  :commands (flycheck-mode)
  :init (progn
	  (add-hook 'prog-mode-hook #'flycheck-mode)
	  (setq-default flycheck-flake8-maximum-line-length 120)
	  (add-hook 'after-init-hook #'global-flycheck-mode)))

(ensure-package ispell
  :defer t
  :bind (("C-:" . ispell-word)))

(ensure-package flyspell
  :ensure t
  :diminish 'flyspell-mode
  :bind (:map flyspell-mode-map
              ("M-;" . nil))
  :init (progn
          (add-hook 'prog-mode-hook #'flyspell-prog-mode)
          (dolist (mode-hook '(text-mode-hook org-mode-hook LaTeX-mode-hook))
            (add-hook mode-hook #'flyspell-mode))))

(ensure-package flx
  :ensure t)

(ensure-package diff-hl
  :ensure t
  :init (progn
            (global-diff-hl-mode)
            (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t)))

(ensure-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package iso-transl
  :demand t)

(ensure-package ido
  :ensure t
  :init (progn
	    (ido-mode t)))

(ensure-package ivy
  :diminish ""
  :demand t
  :config (progn
            (ivy-mode 1)
            (setq ivy-use-virtual-buffers t)
	    (setq ivy-re-builders-alist
		  '((ivy-switch-buffer . ivy--regex-plus)
		    (t . ivy--regex-fuzzy)))
	    (setq ivy-initial-inputs-alist nil)))

(ensure-package jedi
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

(ensure-package linum
  :bind (("C-c l" . linum-mode))
  :init (progn
					;(add-hook 'prog-mode-hook #'linum-mode))
	  )
  :config (progn
	    (setq linum-format "%4d \u2502")))

(ensure-package magit
  :bind (("C-x g"   . magit-status)))

(ensure-package multiple-cursors
  :bind (("M-RET" . mc/edit-lines)
         ("C->" . mc/mark-previous-like-this)
         ("M-d" . mc/mark-next-like-this)
         ("C-M-d" . mc/unmark-next-like-this)
         ("C-M-<" . mc/unmark-previous-like-this)
         ("C-d" . mc/mark-all-like-this)))

(use-package open-url-at-point
  :bind ("C-c C-o" . open-url-at-point))

(ensure-package paren
  :demand t
  :config (show-paren-mode 1))

(install-if-not-exist 'counsel-projectile)
(ensure-package projectile
  :demand t
  :diminish ""
  :bind (("M-t" . counsel-projectile-find-file)
	 ("M-p" . 'projectile-command-map))
  :config (progn
            (projectile-mode)
            (require 'counsel-projectile)))

(ensure-package rainbow-mode
  :ensure t
  :init
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'web-mode-hook 'rainbow-mode))

(ensure-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(ensure-package recentf
  :ensure t
  :demand t
  :config (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:"))

(ensure-package web-mode
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

  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-enable-current-element-highlight t))

(ensure-package whitespace
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

(ensure-package winner
  :bind (("C-c <left>" . winner-undo)
	 ("C-c <right>" . winner-redo))
  :config
  (when (fboundp 'winner-mode)
    (winner-mode 1)))

(ensure-package pabbrev
  :ensure t)

(use-package prog-mode
  :bind (("C-;" . comment-line)))

(ensure-package neotree
  :ensure t
  :bind ("C-c C-y" . neotree-toggle)
  :config (progn
	    (setq-default neo-smart-open t)
	    (setq-default neo-dont-be-alone t)
	    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
	    (bind-keys
	     :map neotree-mode-map
	     ("RET" . neotree-enter)
	     ("c" . neotree-create-node)
	     ("d" . neotree-delete-node)
	     ("r" . neotree-rename-node)
	     ("c" . neotree-create-node)
	     ("r" . neotree-rename-node)
	     ("d" . neotree-delete-node)
	     ("g" . neotree-refresh)
	     ("C" . neotree-change-root)
	     ("H" . neotree-hidden-file-toggle))))


(ensure-package python-mode
  :init (progn
	  (add-hook 'python-mode-hook 'anaconda-mode)
	  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)))

;; (ensure-package redo+
;;   :ensure t
;;   :bind (("C-S-z" . redo)))

(ensure-package yasnippet
  :diminish 'yas-minor-mode
  :ensure t
  :demand t
  :init (progn
          (yas-global-mode)
	  (yas-reload-all)
	  (add-to-list 'yas-snippet-dirs (expand-file-name "~/.emacs.d/snippets/django"))
	  (add-hook 'python-mode-hook #'yas-minor-mode)))

(ensure-package forge)

;; (ensure-package zoom-frm
;;   :ensure t
;;   :bind (("C-+" . zoom-frm-in)
;;          ("C--" . zoom-frm-out)))

(use-package utils
  :load-path "~/.emacs.d/local"
  :bind (([f10] . fullscreen)
	 ("M-<up>" . move-backward-block)
	 ("M-<down>" . move-forward-block)
	 ("C-k" . delete-line)))


(provide 'emacs)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))


(ensure-package tide
  :ensure t
  :config
  (setq company-tooltip-align-annotations t)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))


(ensure-package k8s-mode)

(ensure-package kubernetes
		:config
		(setq kubernetes-poll-frequency 300)
		(setq kubernetes-redraw-frequency 300))

(ensure-package git-link)

(global-set-key (kbd "C-w") 'backward-kill-word)

(server-start)
;;; emacs ends here

