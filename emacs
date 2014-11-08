
(if window-system
    (let ()
      (set-default-font "-apple-monaco-medium-r-normal--12-90-72-72-m-90-mac-roman")
      ;;(setq mac-allow-anti-aliasing nil)
      ))

; config standard
; ============================================================================
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(ecb-directories-menu-user-extension-function nil)
 '(ecb-history-menu-user-extension-function nil)
 '(ecb-methods-menu-user-extension-function nil)
 '(ecb-options-version "2.27")
 '(ecb-sources-menu-user-extension-function nil)
 '(global-font-lock-mode t nil (font-lock))
 '(linum ((t (:inherit shadow :background "grey7"))))
 '(load-home-init-file t t)
 '(mouse-wheel-mode t nil (mwheel))
 '(safe-local-variable-values (quote ((Syntax . COMMON-LISP) (Package . FLEXI-STREAMS) (Base . 10))))
 '(show-paren-mode t)
 '(transient-mark-mode t))

(defmacro add-to-load-path (path)
 `(add-to-list 'load-path ,path))

; paths
; ============================================================================
(setq home (getenv "HOME"))
(add-to-load-path (concat home "/.emacs.d/lisp"))

(add-to-load-path (concat home "/.emacs.d/lisp/mmm-mode"))

(add-to-load-path (concat home "/.emacs.d/lisp/yasnippet"))
(add-to-load-path "/usr/local/share/emacs/site-lisp")
(add-to-load-path "/usr/share/emacs/site-lisp/w3m")
(add-to-load-path (concat home "/.emacs.d/lisp/erc"))
(add-to-load-path (concat home "/.emacs.d/lisp/django-mode"))
(add-to-load-path (concat home "/.emacs.d/lisp/jde/lisp"))
;(add-to-load-path (concat home "/.emacs.d/cedet/common"))
(add-to-load-path (concat home "/.emacs.d/lisp/elib"))
(add-to-load-path (concat home "/.emacs.d/lisp/w3m"))
(add-to-load-path (concat home "/.emacs.d/lisp/org-mode/lisp"))
;(setq swank-clojure-binary nil)

(setq jde-jdk `("/System/Library/Framework/JavaVM.framework/Versions/CurrentJDK/Home"))


; requires
;============================================================================

(require 'ido)
;(require 'cedet)
;(require 'eieio)
;(require 'jde)
(require 'tramp)

(require 'django-html-mode)
(require 'django-mode)
(require 'php-mode)
(require 'color-theme)
(require 'mic-paren)
(require 'yaml-mode)
(require 'two-mode-mode)

(require 'generic-x)

(require 'pabbrev)
(require 'wide-column)
(require 'org-install)
(require 'psvn)
(require 'linum)
(if window-system
    (require 'w3m-load))
(require 'redo)
(require 'cua-base)
(require 'pabbrev)
(require 'undo-tree)

(global-undo-tree-mode)



; misc
; ============================================================================
(setq user-full-name "Artur Ventura")
(setq user-mail-address "artur.ventura@gmail.com")

(line-number-mode      t)
(column-number-mode    t)
(fset 'yes-or-no-p 'y-or-n-p)

;tema
(setq inhibit-startup-message t)
(tool-bar-mode 0)
;(color-theme-dark-laptop)

(setq mouse-drag-copy-region nil)
(setq scroll-bar-mode-explicit t)
(set-scroll-bar-mode `right)
(setq scroll-step 1)
(setq scroll-up 1)
(setq scroll-down 1)

(ido-mode t)

;; mic-parent
(paren-activate)
(setq paren-sexp-mode t)

(setq common-lisp-hyperspec-root
      (concat "file://" (getenv "HOME") "/.emacs.d/HyperSpec/"))

; funcões aux para as keybinding!
; transfere: muda o controlo para o buffer do slime
(defun transfere ()
  (interactive)
  (let ((buffer-slime nil))
    (mapcar (lambda (x)
              (if (string-match "\*slime-repl .*" (buffer-name x))
                  (setf buffer-slime x)))
            (buffer-list))
    (if (null buffer-slime) (error "Slime not found"))
    (switch-to-buffer-other-window buffer-slime)))

; usa o open para abrir o ficheiro actual
(defun send-buffer-to-open ()
  (interactive)
  (unless
      (string= ""
               (shell-command-to-string
                (concat "open " buffer-file-name)))
    (message "Requesting Finder.app to open file...")
    (start-process (concat "open " buffer-file-name)
                   nil "open " buffer-file-name)
    (message "Requesting Finder.app to open file... done")))

; abre o cltl
(defun open-cltl ()
  (interactive)
  (browse-url "file:///Users/nurv/.emacs.d/cltl/clm/node1.html"))


; pure liquid awsom
(defun w3m-browse-url-other-window (url &optional newwin)
  (interactive
   (browse-url-interactive-arg "w3m URL: "))
  (let ((pop-up-frames nil))
    (switch-to-buffer-other-window
     (w3m-get-buffer-create "*w3m*"))
    (w3m-browse-url url)))

(setq browse-url-browser-function
      (list (cons "^ftp:/.*"  (lambda (url &optional nf)
                                (call-interactively #'find-file-at-point url)))
            '("hyperspec" .  w3m-browse-url-other-window )
            '("cltl" .  w3m-browse-url-other-window )
            '("." . browse-url-default-macosx-browser)))

;keybinding
; ============================================================================

;(require 'mac-key-mode)
(cua-mode t)
;(setq mac-option-modifier 'meta)
;(setq mac-command-modifier 'alt)
;(setq mac-pass-command-to-system t)   ;; avoid hiding with M-h

(defun region-active-p ()
    mark-active)

(global-set-key [(alt x)] (lambda (arg)
                            (interactive "P")
                            (execute-extended-command nil)))
;(global-set-key [(alt c)] 'cua-copy-region)
;(global-set-key [(alt v)] 'cua-paste)

;(global-set-key [(alt a)] 'mark-whole-buffer)
;(global-set-key [(alt s)] 'save-buffer)
;(global-set-key [(alt S)] 'write-file)

;(global-set-key [(alt p)] 'ps-print-buffer)
;(global-set-key [(alt o)] (lambda()(interactive)(let(last-nonmenu-event)(menu-find-file-existing))))

;(global-set-key [(alt q)] 'save-buffers-kill-emacs)
;(global-set-key [(alt w)] 'kill-this-buffer)

(global-set-key [(alt z)] 'undo)
(global-set-key  [(alt shift z)] 'redo) ; requires redo

(global-set-key  [(alt up)] 'scroll-down)

(global-set-key  [(alt down)] 'scroll-up)
(global-set-key  [(alt left)] 'beginning-of-line)
(global-set-key [(alt right)] 'end-of-line)

;(global-set-key [(alt f)] 'isearch-forward)
;(define-key isearch-mode-map [(alt f)] (lookup-key isearch-mode-map "\C-s"))
;(define-key isearch-mode-map [(alt d)] (lookup-key isearch-mode-map "\C-r"))
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-delete-char)

;(global-set-key [(alt meta f)] 'occur)
;(global-set-key [(alt shift f)] 'isearch-repeat-forward)

;(global-set-key [(alt r)] 'query-replace)
(global-set-key [(alt g)] 'goto-line)
(global-set-key [(alt m)] 'iconify-frame)
;(global-set-key [(alt n)] 'new-frame)

(global-set-key [kp-delete] 'delete-char)
(global-set-key [(control kp-home)] 'beginning-of-buffer)
(global-set-key [(control kp-end)] 'end-of-buffer)

;(global-set-key [(alt i)] 'mac-key-show-in-finder)
(global-set-key [A-mouse-1] 'browse-url-at-mouse)
;(global-set-key [C-down-mouse-1] 'mac-key-context-menu)
;;(global-set-key [mouse-3] 'mac-key-context-menu)
;(global-set-key [C-mouse-1] 'mac-key-context-menu)
(global-set-key [A-S-mouse-1] 'mouse-buffer-menu)
;(global-set-key [S-down-mouse-1] 'mac-key-shift-mouse-select)
;(global-set-key [?\C-x ?\C-v] 'term)
(global-set-key (kbd "<backspace>") 'backward-delete-char)
(global-set-key (kbd "<delete>")    'delete-char)

;(global-set-key [(control tab)] 'other-buffer)
;(global-set-key "\C-g" 'compile)
;(global-set-key [(alt shift o)]  #'send-buffer-to-open)
;(define-key lisp-mode-map [(control q)] #'transfere)
(define-key lisp-mode-map [(control f11)] 'hyperspec-lookup)
;(global-set-key [(control t)] 'delete-window)
;(global-set-key [(control w)] 'comment-or-uncomment-region)
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
(global-set-key (kbd "C-c C-t") 'indent-region)
(global-set-key (kbd "C-c C-h") 'hs-toggle-hiding)

(global-set-key "\M-2" (lambda ()
                              (interactive)
                              (insert "@")))

(global-set-key "\M-8" (lambda ()
                              (interactive)
                              (insert "[]")
                              (backward-char)))

(global-set-key "\M-9" (lambda ()
                         (interactive)
                         (if (null (char-after))
                             (insert "]")
                             (if (eql (char-after) ?\])
                                 (forward-char)
                                 (insert "]")))))
;; Filetypes and Hoocks
;; ================================================================

(add-to-list 'auto-mode-alist '("\\.jsc$" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.jscx$" . javascript-mode))



(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))

(setq-default indent-tabs-mode nil)


(defun comments-in-lisp (map)
    (define-key map (kbd ";") (lambda ()
                              (interactive)
                              (insert ";; "))))

(defun configure-parens-for-map (map)

  (define-key map (kbd "[") (lambda ()
                              (interactive)
                              (insert "[]")
                              (backward-char)))
  (define-key map (kbd "{") (lambda ()
                              (interactive)
                              (insert "{}")
                              (backward-char)))

  (if (not (or (eql map lisp-mode-map)
               (eql map emacs-lisp-mode-map)
               (eql map lisp-interaction-mode-map)))
      (progn
        (define-key map (kbd "'") (lambda ()
                                    (interactive)
                                    (insert "''")
                                    (backward-char)))))

  (define-key map (kbd "\"") (lambda ()
                               (interactive)
                               (insert "\"\"")
                               (backward-char))))

(mapcar 'configure-parens-for-map (list emacs-lisp-mode-map
                                        lisp-mode-map
                                        lisp-interaction-mode-map))

(eval-after-load "cc-mode" '(mapcar 'configure-parens-for-map
                                    (list c-mode-map c++-mode-map)))

(eval-after-load "ruby-mode" '(mapcar 'configure-parens-for-map
                                    (list ruby-mode-map)))

(eval-after-load "lisp-mode" '(mapcar 'configure-parens-for-map
                                      (list lisp-mode-map)))

(eval-after-load "lisp-mode" '(mapcar 'comments-in-lisp
                                    (list lisp-mode-map)))

(eval-after-load "emacs-lisp-mode" '(mapcar 'comments-in-lisp
                                            (list emacs-lisp-mode-map)))

(eval-after-load "java-mode" '(mapcar 'configure-parens-for-map
                                    (list java-mode-map)))

(eval-after-load "php-mode" '(mapcar 'configure-parens-for-map
                                    (list php-mode-map)))

(eval-after-load "rails-mode" '(mapcar 'configure-parens-for-map
                                       (list rails-mode-map)))

(eval-after-load "css-mode" '(mapcar 'configure-parens-for-map
                                       (list cssm-mode-map)))

(eval-after-load "html-mode" '(mapcar 'configure-parens-for-map
                                      (list html-mode-map)))


;; PHP-Mode

(add-hook 'php-mode-user-hook 'turn-on-font-lock)

;; CSS-Mode
(autoload 'css-mode "css-mode")
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level '2)

;; javascript-generic-mode
(autoload 'javascript-mode "javascript" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))


(add-to-list 'auto-mode-alist '("\\.inc\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.php[34]?\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.[A-Za-z]?html?\\'" . html-mode))



(defun lisp-mode-more-brackets ()
  (modify-syntax-entry ?\[ "(]  ")
  (modify-syntax-entry ?\] ")[  ")
            
  (modify-syntax-entry ?\{ "(}  ")
  (modify-syntax-entry ?\} "){  "))

(add-hook 'emacs-lisp-mode-hook (lambda ()
				  (hs-minor-mode t) (pabbrev-mode t)
				  (setq indent-tabs-mode nil)
                                  (eldoc-mode t)))

;; Hooks para Lisp:
(add-hook 'lisp-mode-hook (lambda ()
                            ;; obrigatorio slime-mode
                            (slime-mode t)
			    (hs-minor-mode t) (pabbrev-mode t)			    
                            (require 'lisp-mode)
                            ;; newline and indent quando se carrega em C
                            ;(local-set-key "\r" 'newline-and-indent)
                            ;; indent decente para cl
                            (setq lisp-indent-function
                                  'common-lisp-indent-function)
                            (setq indent-tabs-mode nil)
			    (lisp-mode-more-brackets)))

(add-hook 'slime-repl-mode-hook (lambda()
				  (lisp-mode-more-brackets)))


;; C-mode hooks:
(add-hook 'c-mode-common-hook '(lambda ()
				 (local-set-key (kbd "<return>") 'newline-and-indent)
				 (hs-minor-mode t) (pabbrev-mode t)
				 (c-set-style "gnu")
				 (setq c-basic-offset 3)
				 (setq c-indent-level 3)
				 (setq c-tab-always-indent t)
				 (setq tab-width 4)
				 ;; indent com tabs
				 (setq indent-tabs-mode nil)))

(add-hook 'java-mode-common-hook '(lambda ()
				    (c-set-style "k&r")
				    (local-set-key (kbd "<return>") 'newline-and-indent)
				    (hs-minor-mode t) (pabbrev-mode t) (pabbrev-mode t)
				    (setq c-basic-offset 4)
				    (setq c-indent-level 4)
				    (setq c-tab-always-indent nil)
				    (setq tab-width 4)
				    ;; indent com tabs
				    (setq indent-tabs-mode nil)))

(pabbrev-mode t)
(defun go-black ()
  (interactive)
  (let ()
    (set-face-background 'paren-face-match "ForestGreen")
    (color-theme-dark-laptop)
    (set-face-background 'highline-face "purple4")))

(defun go-white ()
  (interactive)
  (let ()
    (set-face-background 'paren-face-match "PaleTurquoise1")
    (color-theme-emacs-21)
        (set-face-background 'highline-face "lemon chiffon")))

(set-face-background 'paren-face-match "PaleTurquoise1")

(add-hook 'after-init-hook (lambda () (highline-mode t)))
(defun hl-off () (highline-mode 0))
(add-hook 'term-mode-hook (lambda () (hl-off)))


(add-hook 'find-file-hooks 'auto-insert)
(add-hook 'find-file-hook '(lambda () (linum-mode 1)))



(load-library "autoinsert")
(setq auto-insert-copyright (user-full-name))
(setq auto-insert-query nil)

(setq auto-insert-alist
    '(
      ((perl-mode . "Perl Program")
      nil
      "#! /usr/bin/perl -w\n\n"
      "# -*- Mode: Perl -*-\n"
      "# -*- coding: UTF-8 -*-\n"
      "# Copyright (C) " (substring (current-time-string) -4)
      " by " auto-insert-copyright "\n#\n"
      "# File: " (file-name-nondirectory buffer-file-name) "\n"
       "# Time-stamp: "(current-time-string)"\n#\n"
      
     
      "# Author: "(user-full-name) "\n#\n"
      ""
      )

      ((python-mode . "Python Program")
      nil
      "#! /usr/bin/python\n\n"
      "# -*- Mode: Python -*-\n"
      "# -*- coding: UTF-8 -*-\n"
      "# Copyright (C) " (substring (current-time-string) -4)
      " by " auto-insert-copyright "\n#\n"
      "# File: " (file-name-nondirectory buffer-file-name) "\n"
       "# Time-stamp: "(current-time-string)"\n#\n"
      
     
      "# Author: "(user-full-name) "\n#\n"
      ""
      )

      ((lisp-mode . "Lisp Program")
       nil
       ";; -*- Mode: Lisp -*-\n"	;
       ";; -*- coding: UTF-8 -*-\n"
       ";; Copyright (C) " (substring (current-time-string) -4)
       "by " auto-insert-copyright "\n;;\n"
       ";; File: " (file-name-nondirectory buffer-file-name) "\n"
       ";; Time-stamp: "(current-time-string)"\n;;\n"
       
       ";; Author: "(user-full-name) "\n;;\n"
       ""
       )

      ((emacs-lisp-mode . "Emacs Lisp Program")
       nil
       ";; -*- Mode: Emacs-Lisp -*-\n"
       ";; -*- coding: UTF-8 -*-\n"       
       ";; Copyright (C) " (substring (current-time-string) -4)
       " by " auto-insert-copyright "\n ;;\n"
       ";; File: " (file-name-nondirectory buffer-file-name) "\n"
       ";; Time-stamp: "(current-time-string)"\n ;;\n"
       
       ";; Author: "(user-full-name) "\n ;;\n"
       ""
       )

      ((c-mode . "C Program")
       nil
       "/* -*- Mode: C -*-\n"
       " * -*- coding: UTF-8 -*-\n"
       " * Copyright (C) " (substring (current-time-string) -4)
       " by " auto-insert-copyright "\n *\n"
       " * File: " (file-name-nondirectory buffer-file-name) "\n"
       " * Time-stamp: "(current-time-string)"\n *\n"
       
       " * Author: "(user-full-name) "\n *\n"
       " */"
       ""
       )

      ((c++-mode . "C++ Program")
       nil
       "// -*- Mode: C++ -*-\n"
       "// -*- coding: UTF-8 -*-\n"
       "/* Copyright (C) " (substring (current-time-string) -4)
       " by " auto-insert-copyright "\n *\n"
       " * File: " (file-name-nondirectory buffer-file-name) "\n"
       " * Time-stamp: "(current-time-string)"\n *\n"
       
       " * Author: "(user-full-name) "\n *\n"
       " */"
       )
      ((javascript-generic-mode . "Js Program")
       nil
       "/* -*- Mode: Javascript -*-\n"
       " * -*- coding: UTF-8 -*-\n"
       " * Copyright (C) " (substring (current-time-string) -4)
       " by " auto-insert-copyright "\n *\n"
       " * File: " (file-name-nondirectory buffer-file-name) "\n"
       " * Time-stamp: "(current-time-string)"\n *\n"
       
       " * Author: "(user-full-name) "\n"
       " */"
       )
      
      ((java-mode . "java Program")
       nil
       "// -*- Mode: Java -*-\n"
       "// -*- coding: UTF-8 -*-\n"
       "/* Copyright (C) " (substring (current-time-string) -4)
       " by " auto-insert-copyright "\n *\n"
       " * File: " (file-name-nondirectory buffer-file-name) "\n"
       " * Time-stamp: "(current-time-string)"\n *\n"
       
       " * Author: "(user-full-name) "\n *\n"
       " */\n"
       )
))


(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(add-hook 'python-mode-hook
          (lambda ()
            (hs-minor-mode t) (pabbrev-mode t)
            (set (make-variable-buffer-local 'beginning-of-defun-function)
                 'py-beginning-of-def-or-class)
            (setq outline-regexp "def\\|class ")))

(setq visible-bell nil)

;; (defcustom echo-area-bell-string "*DING* " ;"♪"
;;   "Message displayed in mode-line by `echo-area-bell' function."
;;   :group 'user)
;; (defcustom echo-area-bell-delay 0.1
;;   "Number of seconds `echo-area-bell' displays its message."
;;   :group 'user)
;; (defun echo-area-bell ()
;;   "Briefly display a highlighted message in the echo-area.
;;     The string displayed is the value of `echo-area-bell-string',
;;     with a red background; the background highlighting extends to the
;;     right margin.  The string is displayed for `echo-area-bell-delay'
;;     seconds.
;;     This function is intended to be used as a value of `ring-bell-function'."
;;   (message (propertize
;;            (concat
;;             (propertize
;;              "x"
;;              'display
;;              `(space :align-to (- right ,(+ 1 (length echo-area-bell-string)))))
;;             echo-area-bell-string)
;;            'face `(:background ,(face-foreground 'default) :foreground ,(face-background 'default))))
;;   (sit-for echo-area-bell-delay)
;;   (message ""))
;; (setq ring-bell-function 'echo-area-bell)

;; Set your lisp system and, optionally, some contribs
(setq inferior-lisp-program "~/abcl/abcl.sh")
(setq slime-contribs '(slime-fancy))
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(if window-system
    (defun fullscreen ()
      (interactive)
      (toggle-frame-fullscreen)))
      
