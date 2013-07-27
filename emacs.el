;;
;; Zane's .emacs
;; https://github.com/ZaneA/Dotfiles
;;

(require 'cl)
(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))

(setq packages
      '(auto-complete bind-key birds-of-paradise-plus-theme
        chicken-scheme color-theme direx edit-server evil fold-this
        git-gutter git-gutter-fringe git-messenger golden-ratio
        jade-mode jinja2-mode js2-mode kpm-list legalese less-css-mode
        linum-relative magit markdown-mode nginx-mode nyan-mode org
        php-mode popup popwin pos-tip powerline r5rs
        rainbow-delimiters rainbow-mode scratch scss-mode skewer-mode
        slime solarized-theme soothe-theme starter-kit starter-kit-js
        starter-kit-ruby surround use-package web-mode
        writegood-mode))

(package-initialize)

; Install missing packages
(let ((refreshed nil))
  (loop for package in packages
        when (not (package-installed-p package))
        do (progn
             (unless refreshed
               (package-refresh-contents)
               (setq refreshed t))
             (package-install package))))

(require 'use-package)

; Color theme
(use-package color-theme
  :init
  ;(load-theme 'solarized-light t))
  (load-theme 'solarized-dark t))
  ;(load-theme 'soothe t))
  ;(load-theme 'birds-of-paradise-plus t))

(setq-default line-spacing 3)

; Font
(set-frame-font "SourceCodePro-10.5")
(add-to-list 'default-frame-alist
             '(font . "SourceCodePro-10.5"))

;(use-package powerline
;  :init
;  (progn
;    (powerline-vim-theme)
;    (set-face-attribute 'mode-line nil :box nil :font "SourceCodePro-9")
;    (set-face-attribute 'mode-line-inactive nil :box nil)
;    (set-face-attribute 'powerline-active1 nil :background "#282828")
;    (set-face-attribute 'powerline-inactive1 nil :background "#282828")))

(use-package nyan-mode
  :init (nyan-mode t))

(set-face-attribute 'mode-line nil :box nil :font "SourceCodePro-9")
(set-face-attribute 'mode-line-inactive nil :box nil)

; Evil
(use-package evil
  :init
  (progn
    (setq evil-default-cursor t)
    (evil-mode t)

    ; A nice evil shortcut for indirectly narrowing a region of the
    ; current vi selection
    (evil-define-operator evil-narrow-indirect (beg end type)
      "Indirectly narrow the region from BEG to END."
      (interactive "<R>")
      (evil-normal-state)
      (narrow-to-region-indirect beg end))

    (define-key evil-normal-state-map "m" 'evil-narrow-indirect)
    (define-key evil-visual-state-map "m" 'evil-narrow-indirect)

    (evil-define-operator evil-join-unfill (beg end)
      "Join the selected lines. Uses fill-region so that
adaptive-fill-mode is effective when joining."
      :motion evil-line
      (evil-with-single-undo
        (end-of-line)
        (let* ((fill-column (point-max))
               (lines (count-lines beg end))
               (end (if (> lines 1)
                        end
                      (save-excursion
                        (goto-char end)
                        (forward-line)
                        (point)))))
          (fill-region beg end))))

    (define-key evil-normal-state-map "J" 'evil-join-unfill)
    (define-key evil-visual-state-map "J" 'evil-join-unfill)))

(use-package fold-this
  :init
  (progn
    (evil-define-operator evil-fold-operator (beg end type)
      "Fold the region from BEG to END."
      (interactive "<R>")
      (fold-this beg end))

    (define-key evil-normal-state-map "f" 'evil-fold-operator)
    (define-key evil-visual-state-map "f" 'evil-fold-operator)
    ))

; Show current song in frame title
(defun update-frame-title ()
  (setq frame-title-format
        `((buffer-name "%f" ("%b")) " (" mode-name ")")))

(setq update-frame-title-timer (run-with-timer 0 30 'update-frame-title))

(setq make-backup-files nil)
(setq auto-save-default nil)

(setq mouse-autoselect-window t)
(setq-default cursor-in-non-selected-windows nil)

(setq visible-bell nil)
(setq echo-keystrokes 0.1)
(setq max-mini-window-height 0.75)

(setq vc-follow-symlinks t)

; Remove word-wrapping
(setq-default truncate-lines t)
(setq-default indicate-empty-lines nil)
(setq-default fill-column 70)
(setq adaptive-fill-mode t)

; 2 spaces for indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default evil-shift-width 2)
(setq-default css-indent-offset 2)
(setq-default js2-basic-offset 2)
(setq-default c-basic-offset 2)

(server-start)

; Remove hooks I don't like
(use-package starter-kit
  :bind ("C-c C-e" . esk-eval-and-replace)
  :init
  (progn
    (remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
    (remove-hook 'prog-mode-hook 'idle-highlight-mode)
    (remove-hook 'text-mode-hook 'turn-on-auto-fill)))

(use-package ido
  :init
  (progn
    (ido-everywhere t)
    ; Vertical ido setup
    (setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
    (defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
    (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
    (defun my-ido-keys ()
      (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
      (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
      (define-key ido-completion-map (kbd "C-w") 'ido-delete-backward-word-updir)
      (define-key ido-completion-map (kbd "C-<backspace>") 'ido-delete-backward-word-updir))
    (add-hook 'ido-setup-hook 'my-ido-keys)))

;; Global modes

(use-package kpm-list
  :init
  (defadvice kpm-list (after no-evil-kpm-list activate)
    "Make kpm-list play along with evil-mode."
    (with-current-buffer (get-buffer kpm-list-buffer-name)
      (define-key evil-normal-state-local-map (kbd "<RET>") 'kpm-list-select-buffer)
      (define-key evil-normal-state-local-map "q" 'quit-window)
      (define-key evil-normal-state-local-map "o" 'kpm-list-select-other-window)
      (define-key evil-normal-state-local-map "j" 'kpm-list-next-buffer)
      (define-key evil-normal-state-local-map "k" 'kpm-list-prev-buffer)
      (define-key evil-normal-state-local-map "d" 'kpm-list-kill-buffer))))

(use-package surround
  :init
  (progn
    (define-globalized-minor-mode global-surround-mode
      surround-mode (lambda () (surround-mode t)))
    (global-surround-mode t)))

(use-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message)
  :init
  (setq git-messenger:show-detail t))

; FIXME temporarily disabled as it inteferes with popwin
;(use-package golden-ratio
;  :init
;  (progn
;    (add-to-list 'golden-ratio-extra-commands 'handle-select-window)
;    (golden-ratio-mode t)))

(use-package rainbow-delimiters
  :init
  (global-rainbow-delimiters-mode t))

(electric-pair-mode t)

(use-package rainbow-mode
  :init
  (add-hook 'css-mode-hook (lambda () (rainbow-mode t))))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

(use-package chicken-scheme
  :mode ("\\.scm$" . scheme-mode)
  :init
  (progn
    (font-lock-add-keywords 'scheme-mode '(("(\\(lambda\\*\\)[ \n]" 1 'font-lock-keyword-face)))
    (put 'define* 'scheme-indent-function 1)
    (put 'lambda* 'scheme-indent-function 1)
    (add-hook 'scheme-mode-hook
              (lambda ()
                (setq adaptive-fill-mode t)
                (esk-pretty-lambdas)))))

(use-package popwin
  :init
  (popwin-mode t))

(use-package direx
  :init
  (progn
    (setq direx:closed-icon "+ ")
    (setq direx:leaf-icon "| ")
    (setq direx:open-icon "> ")
    (define-key direx:direx-mode-map [mouse-1] 'direx:mouse-2)
    (define-key direx:direx-mode-map [mouse-3] 'direx:mouse-1)
    (push '(direx:direx-mode :position left :width 30 :dedicated t :stick t :noselect t) popwin:special-display-config)))

(use-package direx-project
  :bind ("C-x C-j" . direx-project:jump-to-project-root-other-window))

; Auto-complete
(use-package pos-tip)
(use-package auto-complete-config
  :init
  (progn
    (ac-config-default)
    (setq-default ac-sources '(ac-source-abbrev
                               ac-source-dictionary
                               ac-source-words-in-same-mode-buffers))
    (setq tab-always-indent 'complete)
    (add-to-list 'ac-modes 'html-mode t)
    (add-to-list 'ac-modes 'less-css-mode t)
    (add-to-list 'completion-styles 'initials t)
    (define-key ac-complete-mode-map (kbd "C-p") 'ac-previous)
    (define-key ac-complete-mode-map (kbd "C-n") 'ac-next)))

(use-package git-gutter-fringe
  :init
  (progn
    (setq-default left-fringe-width 20)
    (fringe-helper-define 'git-gutter-fr:modified nil
      "........"
      "XXXXXXXX"
      "XXXXXXXX"
      "........"
      "........"
      "XXXXXXXX"
      "XXXXXXXX"
      "........")
    (global-git-gutter-mode t)))

(use-package linum-relative
  :init
  (global-linum-mode t))

(use-package js2-mode
  :mode ("\\.js$" . js2-mode))

(use-package markdown-mode
  :mode ("\\.md$" . markdown-mode))

; Clean up buffers
(use-package midnight
  :init
  (progn
    (add-to-list 'clean-buffer-list-kill-buffer-names
                 '("*Packages*"
                   "*Completions*"
                   "*magit"
                   "*Backtrace*"))

    (setq clean-buffer-list-delay-special 0)

    (run-with-idle-timer 300 t 'clean-buffer-list)))

(use-package web-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (defun web-mode-hook ()
      (font-lock-mode 0)
      (setq web-mode-markup-indent-offset 2)
      (setq web-mode-css-indent-offset 2)
      (setq web-mode-code-indent-offset 2)
      (setq web-mode-indent-style 2)
      (setq web-mode-style-padding 2)
      (setq web-mode-script-padding 2))
    (add-hook 'web-mode-hook 'web-mode-hook)))

;; Custom methods

(defun sort-words (reverse beg end)
  (interactive "*P\nr")
  (sort-regexp-fields reverse "\\(\\sw\\|\\s_\\)+" "\\&" beg end))

(defun make-windows-with-modes (modes)
  "Open split windows for each mode specified (space separated)."
  (interactive "sModes: ")
  (save-selected-window
    (dolist (mode (split-string modes " "))
      (let ((buffer (generate-new-buffer (format "*%s*" mode))))
        (split-window-vertically)
        (switch-to-buffer-other-window buffer)
        (funcall (intern mode))))))

(defun narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region, indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil))
        (mode (guess-mode-of-region start end)))
    (with-current-buffer buf
      (narrow-to-region start end)
      (funcall mode))
    (switch-to-buffer buf)))

(defun intelligent-close ()
  "Intelligently close the frame or quit Emacs."
  (interactive)
  (if (eq (car (visible-frame-list)) (selected-frame))
      (if (> (length (visible-frame-list)) 1)
          (delete-frame (selected-frame))
        (save-buffers-kill-emacs))
    (delete-frame (selected-frame))))

(defun rename-file-and-buffer (new-name)
  "Rename current buffer and file names."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun move-region-to-new-file (file-name beg end)
  "Move the active region to a new file."
  (interactive "FFile name: \nr")
  (kill-region beg end)
  (deactivate-mark)
  (find-file file-name)
  (yank))

(defvar guess-mode-syntax-rules
  '(("html-mode"     . "<.*?>")
    ("js2-mode"      . "var\\|[A-Za-z]+\\.[A-Za-z]+(")
    ("less-css-mode" . "[A-Za-z]+\s?{")
    ("php-mode"      . "$[A-Za-z]+"))
  "Rules for guessing a mode from some text.")

(defun guess-mode-of-region (beg end)
  "Guess the mode of region, in the dumbest possible way."
  (save-restriction
    (narrow-to-region beg end)
    (loop for match = major-mode
          for rule in guess-mode-syntax-rules
          until (save-excursion
                  (when (search-forward-regexp (cdr rule) nil t)
                    (setq match (intern (car rule)))))
          finally return match)))

(defun guess-mode-of-region-and-switch (beg end)
  "Guess the mode of region and switch to it."
  (interactive "r")
  (let ((mode (guess-mode-of-region beg end)))
    (funcall mode)))

;; Advice

; From http://www.emacswiki.org/EmacsNiftyTricks
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent active processes query when quitting Emacs."
  (cl-flet ((process-list ())) ad-do-it))

(defadvice quit-window (before quit-window-and-kill activate)
  "Kill buffer when quitting a window."
  (ad-set-arg 0 t))

(defun fullscreen ()
  (interactive)
  (shell-command (concat "wmctrl -i -r " (frame-parameter nil 'outer-window-id) " -btoggle,maximized_vert,maximized_horz")))

(global-set-key (kbd "<f11>") 'fullscreen)

; Other keybindings
(global-set-key (kbd "C-x C-c") 'intelligent-close)
(global-set-key (kbd "M-\\") 'align-regexp)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Org-mode customizations
(use-package org
  :bind ("C-c c" . org-capture)
  :init
  (progn
    (evil-define-key 'normal org-mode-map (kbd "RET") 'org-return)
    (setq org-agenda-files (directory-files "~/Documents/org" t ".org$"))
    (setq org-default-notes-file "~/.emacs.d/capture.org")
    (setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
    (setq org-refile-use-outline-path 'file)
    (setq org-outline-path-complete-in-steps nil)
    (setq org-refile-allow-creating-parent-nodes 'confirm)
    (setq org-startup-align-all-tables t)
    (setq org-completion-use-ido t)
    (setq org-agenda-skip-deadline-if-done t)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-return-follows-link t)
    (setq org-startup-folded nil)

    ; Set up agenda
    (appt-activate t)
    (add-hook 'org-finalize-agenda-hook (lambda () (org-agenda-to-appt t)))
    (org-agenda-list)))

(setq custom-file "~/.emacs-custom.el")
(load custom-file)
