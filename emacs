;;; package --- Sumary
;; -*- Mode: Emacs-Lisp -*-
;; -*- coding: UTF-8 -*-
;; Copyright (C) 2016 by Artur Ventura
;;
;; File:  .emacs
;;
;; Author: Artur Ventura
;;

;;; Code:
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

(defvar explicit-shell-file-name "zsh")

;; Fullscreen
(if window-system
    (defun fullscreen ()
      (interactive)
      (toggle-frame-fullscreen)))

;; Theme
(defun go-black ()
  "Change to black theme."
  (interactive)
  (let ()
    (load-theme 'dark-laptop t t)
    (enable-theme 'dark-laptop)
    (set-face-background hl-line-face "purple4")))

(defun go-atom ()
  "Change to atom theme."
  (interactive)
  (let ()
    (load-theme 'atom-one-dark t t)
    (enable-theme 'atom-one-dark)
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
    ("08b8807d23c290c840bbb14614a83878529359eaba1805618b3be7d61b0b0a32" "b4fd44f653c69fb95d3f34f071b223ae705bb691fb9abaf2ffca3351e92aa374" default)))
 '(package-selected-packages
   (quote
    (multiple-cursors web-mode exec-path-from-shell company-jedi jedi magit markdown-mode multi-term atom-one-dark-theme yasnippet pyenv-mode pip-requirements git-gutter flycheck emmet-mode company-anaconda color-theme-modern)))
 '(show-paren-mode t))
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
      (set-face-attribute 'default nil :height 130)
      (setq-default line-spacing 3)))

;; Python 
(eval-after-load "company"
  '(add-to-list 'company-backends 'company-anaconda))
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(add-hook 'python-mode-hook #'yas-minor-mode)

(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (expand-file-name "~/.emacs.d/snippets/django"))

(yas-reload-all)

(require 'git-gutter)
(global-git-gutter-mode)

(add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup)

(add-hook 'after-init-hook #'global-flycheck-mode)

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

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

(setq linum-format "%4d \u2502")

(electric-pair-mode)
(global-set-key (kbd "C-x g") 'magit-status)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(cua-mode t)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(require 'emmet-mode)
(add-hook 'web-mode-hook  'emmet-mode)
(add-hook 'web-mode-hook  'yas-minor-mode)

(set-face-attribute 'web-mode-html-tag-face nil :foreground "#61AFEF")
(set-face-attribute 'web-mode-html-attr-name-face nil :foreground "#E06C75")

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-current-element-highlight t)

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-i") 'electric-newline-and-maybe-indent)

(provide 'emacs)

;;; emacs ends here
