;;
;; Zane Ashbys .emacs
;;

(add-to-list 'load-path "~/.emacs.d")

; Clean up the window a bit
;(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq initial-frame-alist '((width . 100) (height . 50))) ; Set frame width/height

; My custom color theme :D
(defun color-theme-hash ()
  (interactive)
  (color-theme-install
   '(color-theme-hash
      ((background-color . "#181818")
      (background-mode . dark)
      (border-color . "#000000")
      (cursor-color . "#ddffdd")
      (foreground-color . "#cccccc")
      (mouse-color . "black"))
     (fringe ((t (:background "#181818"))))
     (mode-line ((t (:foreground "#aaaaaa" :background "#333333" :box (:color "#181818")))))
     (mode-line-inactive ((t (:foreground "#333333" :background "#111111" :box (:color "#080808")))))
     (region ((t (:background "#383838"))))
     (font-lock-builtin-face ((t (:foreground "#82b8f2"))))
     (font-lock-comment-face ((t (:foreground "#8d6d6d" :italic t))))
     (font-lock-comment-delimiter-face ((t (:foreground "#2b2b2b"))))
     (font-lock-constant-face ((t (:foreground "#6a88d7"))))
     (font-lock-doc-string-face ((t (:foreground "#2b2b2b" :italic t))))
     (font-lock-doc-face ((t (:foreground "#4b4b4b" :italic t))))
     (font-lock-reference-name-face ((t (:foreground "red"))))
     (font-lock-operator-face ((t (:foreground "#555555" :bold t))))
     (font-lock-negation-char-face ((t (:foreground "#dd4444" :bold t))))
     (font-lock-function-name-face ((t (:foreground "#dd8888" :bold t))))
     (font-lock-keyword-face ((t (:foreground "#8aa8e7"))))
     (font-lock-preprocessor-face ((t (:foreground "#cb99e1"))))
     (font-lock-string-face ((t (:foreground "#cb99e1"))))
     (font-lock-type-face ((t (:foreground"#b7e234" :bold t))))
     (font-lock-variable-name-face ((t (:foreground "#888888"))))
     (font-lock-paren-face ((t (:foreground "#555555" :bold t))))
     (minibuffer-prompt ((t (:foreground "#85c0ff" :bold t))))
     (font-lock-warning-face ((t (:foreground "#ddbbbb"))))
     (show-paren-match-face ((t (:foreground "black" :background "#85c0ff" :bold t))))
     (org-link ((t (:foreground "#7788aa"))))
     (org-date ((t (:foreground "#88bbff"))))
     (org-agenda-date ((t (:foreground "#88bbff"))))
     (org-level-1 ((t (:foreground "#dddddd" :bold t :height 1.3))))
     (org-level-2 ((t (:foreground "#aaaaaa" :height 1.2))))
     (org-level-3 ((t (:foreground "#777777"))))
     (org-level-4 ((t (:foreground "#555555"))))
     (org-tag ((t (:foreground "#dddda0"))))
     (org-todo ((t (:foreground "#ddbbbb" :bold t))))
     (org-done ((t (:foreground "#44ff88" :bold t))))
     (org-warning ((t (:foreground "#775555" :italic t))))
     (eshell-prompt ((t (:foreground "#444444"))))
    )))

(require 'color-theme)
(color-theme-hash)

;; Misc settings

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq-default truncate-lines t)
(setq visible-bell t)
(setq ring-bell-function (lambda () ()))
(setq mouse-autoselect-window t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(setq x-stretch-cursor t)
(setq x-select-enable-clipboard t)
(setq default-fill-column 79)
(delete-selection-mode t)

(setq require-final-newline t)
(setq auto-save-default nil) ; Get rid of ugly #backup# files
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs-backup")))

(setq vc-follow-symlinks t)

(setq-default c-basic-offset 8)
(setq-default tab-width 8)
(setq-default indent-tabs-mode nil)

(setq read-file-name-completion-ignore-case t)

(desktop-save-mode t) ; Save buffers to desktop file
(setq display-time-string-forms
      '((concat " " 12-hours ":" minutes " ")))
(display-time-mode t) ; Show time in modeline
(setq linum-format "%4d ")
(global-linum-mode t) ; Show line numbers
(show-paren-mode t) ; Show matching parens

(setq pop-up-windows nil)

(setq scheme-program-name "csi -:c")

(setq compilation-auto-jump-to-first-error t)

;; Global keybindings

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-\\") 'align-regexp)
(global-set-key (kbd "M-/") 'hippie-expand)

;; Google!

(defun google (query)
  (interactive "sGoogle: ")
  (browse-url (concat "http://google.com/search?q=" query)))

(defun google-im-feeling-lucky (query)
  (interactive "sGoogle: ")
  (browse-url (concat "http://google.com/search?btnI&q=" query)))

(defun google-word-at-point ()
  (interactive)
  (google-im-feeling-lucky (current-word)))

(global-set-key (kbd "<f1>") 'google-word-at-point)
(global-set-key (kbd "<f2>") (lambda () (interactive) (call-interactively 'google)))

(setq compilation-read-command nil)
(global-set-key (kbd "<f5>") 'smart-compile)

;; ctrl-tab,ctrl-shift-tab to move between buffers/windows
(defun switch-tab-or-window-forward ()
  (interactive)
  (if (one-window-p)
      (tabbar-forward-tab)
    (other-window 1)))

(defun switch-tab-or-window-backward ()
  (interactive)
  (if (one-window-p)
      (tabbar-backward-tab)
    (other-window -1)))

(global-set-key (kbd "<C-tab>") 'switch-tab-or-window-forward)
(add-hook 'org-mode-hook (lambda () (define-key org-mode-map (kbd "<C-tab>") 'switch-tab-or-window-forward)))
(global-set-key (kbd "<C-S-iso-lefttab>") 'switch-tab-or-window-backward)
(global-set-key (kbd "<C-S-tab>") 'switch-tab-or-window-backward)
(global-set-key (kbd "<C-f4>") 'delete-window)

(global-set-key "\C-x\C-c"
		(lambda ()
		  "Intelligent Close Frame"
		  (interactive)
		  (if (eq (car (visible-frame-list)) (selected-frame))
		      (if (> (length (visible-frame-list)) 1)
			  (delete-frame (selected-frame))
			(save-buffers-kill-emacs))
		    (delete-frame (selected-frame)))))

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-c a") 'org-agenda)

(setq default-mode-line-format
      (quote
       (""
        global-mode-string
        mode-line-frame-identification
        mode-line-buffer-identification
        default-directory
        "   "
        (:propertize mode-name help-echo (format-mode-line minor-mode-alist))
        mode-line-process
        "   "
        (-3 . "%P")
        "   "
        (:propertize urgent-org-mode-line face 'org-warning)
        "   "
        "-%-"
        )))

(setq org-agenda-files '("~/todo.org"))

(appt-activate 1) ; Enable appointment notification

(defun update-org ()
  "Update org-mode related stuff, appointments, modeline etc"
  (setq urgent-org-mode-line
        (mapconcat 'identity
                   (org-map-entries
                    (lambda ()
                      (nth 4 (org-heading-components)))
                    "PRIORITY={.}|TIMESTAMP<=\"<+1w>\"|DEADLINE<=\"<+1w>\"|SCHEDULED<=\"<+1w>\"" 'agenda) ", "))
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt)
  nil)

(run-at-time nil 300 'update-org)

(add-hook 'org-mode-hook
          (lambda ()
            (variable-pitch-mode t)
            (add-hook 'local-write-file-hooks 'update-org)))

;; Load some libraries

; nXhtml
(load "nxhtml/autostart")
(setq mumamo-chunk-coloring 'submode-colored)
(setq nxhtml-skip-welcome t)
(setq rng-nxml-auto-validate-flag nil)

(if (eq system-type 'windows-nt)
    (setq todochiku-command ""))

; Uniquify
(setq 
 uniquify-buffer-name-style 'reverse
 uniquify-separator ":")

(setq scss-sass-command "~/.gem/ruby/1.8/bin/sass")

; And some other modes
(dolist (lib '(vimpulse rainbow-mode lambda-mode iimage espresso
               autopair todochiku smart-compile uniquify scss-mode))
  (require lib))

(defun my-after-change-major-mode-hook ()
  "Apply my minor modes and custom font locks"
  (progn
    (rainbow-mode 1)
    (lambda-mode 1)
    (autopair-mode 1)
    (font-lock-add-keywords
     nil
     '(("\\.\\|\\+\\|=\\|\\&\\||\\|-\\|\\/\\|\\%\\|\\*\\|,\\|>\\|<" . 'font-lock-operator-face)
       ("!" . 'font-lock-negation-char-face)
       ("(\\|)\\|{\\|}\\|\\[\\|\\]" . 'font-lock-paren-face)))))

; Apply my minor modes and custom font locks after a mode change
(add-hook 'after-change-major-mode-hook 'my-after-change-major-mode-hook)

;(add-hook 'css-mode-hook (lambda ()
;                           (iimage-mode 1)))
(add-hook 'mail-mode-hook (lambda ()
                            (variable-pitch-mode t)
                            (setq truncate-lines nil)))

(defun my-scheme-mode-hook ()
  "Apply scheme mode stuff"
  (define-key scheme-mode-map [f1]
    '(lambda ()
       (interactive)
       (ignore-errors
         (let ((symbol (thing-at-point 'symbol)))
           (info "(r5rs)")
           (Info-index symbol))))))

(add-hook 'scheme-mode-hook 'my-scheme-mode-hook)

(defun my-compilation-hook (buffer string)
  "Compilation Hook"
  (fit-window-to-buffer (get-buffer-window buffer t)))

(add-hook 'compilation-finish-functions 'my-compilation-hook)

; comptroll.el
(defun compilation-troll (buffer msg)
  (unless (string-match "^finished" msg)
    (insert-image (create-image "~/.emacs.d/troll.png"))
    (insert "\n")))

(add-to-list 'compilation-finish-functions 'compilation-troll)

(autoload 'geben "geben" "PHP Debugger on Emacs" t)

;(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
; 
;(add-hook 'espresso-mode-hook 'espresso-custom-setup)
;(defun espresso-custom-setup ()
;  (moz-minor-mode 1))


; Set up the tabbar how I like it
(setq tabbar-buffer-groups-function
      (lambda ()
	(list "All")))

(setq tabbar-buffer-list-function
      (lambda ()
	(remove-if
	 (lambda (buffer)
	   (find (aref (buffer-name buffer) 0) " *"))
	 (buffer-list))))

; Set up IRC
(setq rcirc-default-nick "Hash")

;; Custom set variables

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(tabbar-cycle-scope (quote tabs))
 '(tabbar-mode t nil (tabbar))
 '(tabbar-use-images nil))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:height 100 :foundry "microsoft" :family "Consolas"))))
 '(variable-pitch ((t (:height 100 :foundry "microsoft" :family "Corbel"))))
 '(linum ((t (:inherit (shadow default) :foreground "grey30"))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode3 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode4 ((((class color) (min-colors 88) (background dark)) nil)))
 '(tabbar-button ((t (:inherit tabbar-default :foreground "grey30"))))
 '(tabbar-default ((((class color grayscale) (background dark)) (:inherit variable-pitch :background "black" :foreground "grey40" :height 1.1))))
 '(tabbar-selected ((t (:inherit tabbar-default :bold t :foreground "grey80" :box (:line-width 6 :color "black")))))
 '(tabbar-unselected ((t (:inherit tabbar-default :box (:line-width 6 :color "black"))))))

;; Start Emacs Server
(server-start)
