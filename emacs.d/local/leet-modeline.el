;;; leet-modeline.el --- crap I need                     -*- lexical-binding: t; -*-

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


(defmacro cached-for (secs &rest body)
  "Cache for SECS the result of the evaluation of BODY."
  (declare (debug t))
  (let ((cache (make-symbol "cache"))
        (last-run (make-symbol "last-run")))
    `(let (,cache ,last-run)
       (lambda ()
         (when (or (null ,last-run)
                   (> (- (time-to-seconds (current-time)) ,last-run)
                      ,secs))
           (setf ,cache (progn ,@body))
           (setf ,last-run (time-to-seconds (current-time))))
         ,cache))))

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

(defvar leet--git-face-cached (cached-for 1 (leet--git-face-intern)))



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

(provide 'leet-modeline)
;;; leet-modeline.el ends here
