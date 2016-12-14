;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: UTF-8 -*-
;; Copyright (C) 2016 by Artur Ventura
;;
;; File:  .emacs
;;    
;; Author: Artur Ventura
;;

(setq user-full-name "Artur Ventura")
(setq user-mail-address "artur.ventura@gmail.com")

(line-number-mode      t)
(column-number-mode    t)
(setq visible-bell t)
(tool-bar-mode 0)
(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t)
(global-hl-line-mode)
(setq scroll-step 1)
(setq scroll-up 1)
(setq scroll-down 1)
(global-linum-mode t)
(show-paren-mode 1)

(ido-mode t)

(setq explicit-shell-file-name "zsh")

;; Fullscreen
(if window-system
    (defun fullscreen ()
      (interactive)
      (toggle-frame-fullscreen)))

;; Theme
(defun go-black ()
  (interactive)
  (let ()
    (load-theme 'dark-laptop t t)
    (enable-theme 'dark-laptop)
    (set-face-background hl-line-face "purple4")))


;; Adding MELPA
(require 'package) 
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))


(package-initialize) 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b4fd44f653c69fb95d3f34f071b223ae705bb691fb9abaf2ffca3351e92aa374" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

 ;; disable hl-line in terms
(add-hook 'eshell-mode-hook (lambda ()
                                    (setq-local global-hl-line-mode
                                                nil)))
(add-hook 'term-mode-hook (lambda ()
                                    (setq-local global-hl-line-mode
                                                nil)))

;; Font
(if window-system
    (when (eq system-type 'darwin)
      (set-face-attribute 'default nil :family "SF Mono")
      (set-face-attribute 'default nil :height 140)))

;; Python 
(eval-after-load "company"
 '(add-to-list 'company-backends 'company-anaconda))
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook #'yas-minor-mode)

(require 'yasnippet)
(yas-reload-all)

(require 'git-gutter)
(global-git-gutter-mode)

(add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; (defun set-flychecker-executables ()
;;   "Configure virtualenv for flake8 and lint."
;;   (when (get-current-buffer-flake8)
;;     (flycheck-set-checker-executable (quote python-flake8)
;; 				     (get-current-buffer-flake8)))
;;   (when (get-current-buffer-pylint)
;;     (flycheck-set-checker-executable (quote python-pylint)
;; 				     (get-current-buffer-pylint))))
;; (add-hook 'flycheck-before-syntax-check-hook
;; 	  #'set-flychecker-executables 'local)
