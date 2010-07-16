;;
;; Zane Ashbys .emacs
;;

(add-to-list 'load-path "~/.emacs.d")

; Clean up the window a bit
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq initial-frame-alist '((width . 100) (height . 50))) ; Set frame width/height

; My custom color theme :D
(defun color-theme-hash ()
  (interactive)
  (color-theme-install
   '(color-theme-hash
      ((background-color . "#111111")
      (background-mode . black)
      (border-color . "#000000")
      (cursor-color . "#ddffdd")
      (foreground-color . "#cccccc")
      (mouse-color . "black"))
     (fringe ((t (:background "#111111"))))
     (mode-line ((t (:foreground "#666666" :background "#2e2e2e"))))
     (region ((t (:background "#383838"))))
     (font-lock-builtin-face ((t (:foreground "#82b8f2"))))
     (font-lock-comment-face ((t (:foreground "#4b4b4b" :italic t))))
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
     (font-lock-paren-face ((t (:foreground "grey30"))))
     (minibuffer-prompt ((t (:foreground "#85c0ff" :bold t))))
     (font-lock-warning-face ((t (:foreground "#ddbbbb"))))
     (show-paren-match-face ((t (:foreground "black" :background "#85c0ff" :bold t))))
    )))

(require 'color-theme)
(color-theme-hash)

;; Misc settings

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq-default truncate-lines t)
(setq visible-bell t)
(setq browse-url-generic-program "chromium-browser"
      browse-url-browser-function 'browse-url-generic)
(fset 'yes-or-no-p 'y-or-n-p)

(setq frame-title-format '(buffer-file-name "%f" ("%b")))
(setq x-stretch-cursor t)
(setq x-select-enable-clipboard t)
(setq default-fill-column 79)
(delete-selection-mode t)

(setq require-final-newline t)
(setq auto-save-default nil) ; Get rid of ugly # backup # files

(setq read-file-name-completion-ignore-case t)

(column-number-mode t) ; Show column number in modeline
(desktop-save-mode t) ; Save buffers to desktop file
(display-time-mode t) ; Show time in modeline
(setq linum-format "%4d ")
(global-linum-mode t) ; Show line numbers
(show-paren-mode t) ; Show matching parens

;; Global keybindings

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-\\") 'align-regexp)
(global-set-key (kbd "M-/") 'hippie-expand)

;; ctrl-tab,ctrl-shift-tab to move between bufffers
(global-set-key (kbd "<C-tab>") 'tabbar-forward-tab)
(add-hook 'org-mode-hook (lambda () (define-key org-mode-map (kbd "<C-tab>") 'tabbar-forward-tab)))
(global-set-key (kbd "<C-S-iso-lefttab>") 'tabbar-backward-tab)

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

;; Load some libraries

; nXhtml
(load "nxhtml/autostart")

; And some other modes
(dolist (lib '(vimpulse rainbow-mode lambda-mode iimage espresso autopair))
  (require lib))

; Apply my minor modes and custom font locks after a mode change
(add-hook 'after-change-major-mode-hook
          (lambda ()
	    "Apply my minor modes and custom font locks"
	    (progn
	      (rainbow-mode 1)
	      (lambda-mode 1)
	      (iimage-mode 1)
	      (autopair-mode 1)
	      (font-lock-add-keywords
               nil
               '(("\\.\\|\\+\\|=\\|\\&\\||\\|-\\|\\/\\|\\%\\|\\*\\|,\\|>\\|<" . 'font-lock-operator-face)
		 ("!" . 'font-lock-negation-char-face)
		 ("(\\|)\\|{\\|}\\[\\|\\]" . 'font-lock-paren-face))))))

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
;(rcirc-connect "irc.demonastery.org")

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
 '(default ((t (:height 107 :foundry "microsoft" :family "Consolas"))))
 '(linum ((t (:inherit (shadow default) :foreground "grey30"))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode1 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode2 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode3 ((((class color) (min-colors 88) (background dark)) nil)))
 '(mumamo-background-chunk-submode4 ((((class color) (min-colors 88) (background dark)) nil)))
 '(tabbar-button ((t (:inherit tabbar-default :foreground "grey30"))))
 '(tabbar-default ((((class color grayscale) (background dark)) (:inherit variable-pitch :background "black" :foreground "grey40" :height 0.9))))
 '(tabbar-selected ((t (:inherit tabbar-default :foreground "grey80" :box (:line-width 5 :color "black")))))
 '(tabbar-unselected ((t (:inherit tabbar-default :box (:line-width 5 :color "black"))))))
