;;
;; Zane's .emacs
;; https://github.com/ZaneA/Dotfiles
;;

(require 'cl)
(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))

(setq packages
      '(angular-snippets auto-complete bind-key chicken-scheme
        edit-server evil fold-this git-gutter git-gutter-fringe
        git-messenger go-snippets golden-ratio google-this
        inkpot-theme jade-mode js2-mode kpm-list legalese
        less-css-mode linum-relative magit markdown-mode org php-mode
        popup pos-tip r5rs rainbow-delimiters rainbow-mode scratch
        scss-mode simple-mode-line skewer-mode slime starter-kit
        starter-kit-js starter-kit-ruby surround use-package
        writegood-mode yasnippet))

(package-initialize)

; Install missing packages
(loop for package in packages
      when (not (package-installed-p package))
      when (y-or-n-p (format "Install %s? " package))
      do (package-install package))

(require 'use-package)

; Color theme
(use-package color-theme
  :init
  (load-theme 'birds-of-paradise-plus t))
  ;(load-theme 'phoenix-dark-mono t))
  ;(load-theme 'subatomic t))
  ;(load-theme 'phoenix-dark-pink t))
  ;(load-theme 'twilight-bright t))
  ;(load-theme 'inkpot t))

(setq-default line-spacing 3)

; Font
(set-frame-font "SourceCodePro-10.5")
(add-to-list 'default-frame-alist
             '(font . "SourceCodePro-10.5"))

(use-package simple-mode-line
  :init
  (progn
    (activate-simple-mode-line)
    (set-face-attribute 'mode-line-inactive nil
                        :height (face-attribute 'default :height)
                        :box (list :line-width 10 :color "black" :style nil))
    ))

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
        `((buffer-name "%f" ("%b")) " (" mode-name ") | "
          ,(replace-regexp-in-string
            "\n$" ""
            (shell-command-to-string "audtool current-song")))))

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

(use-package google-this
  :bind ("<f2>" . google-this)
  :init
  (google-this-mode t))

(use-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message)
  :init
  (setq git-messenger:show-detail t))

(use-package golden-ratio
  :init
  (progn
    (add-to-list 'golden-ratio-extra-commands 'handle-select-window)
    (golden-ratio-mode t)))

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

; Snippets
(use-package angular-snippets)
(use-package yasnippet
  :init
  (yas-global-mode t))

; Auto-complete
(use-package pos-tip)
(use-package auto-complete-config
  :init
  (progn
    (ac-config-default)
    (setq-default ac-sources '(ac-source-yasnippet
                              ac-source-abbrev
                              ac-source-dictionary
                              ac-source-words-in-same-mode-buffers))
    (setq tab-always-indent 'complete)
    (add-to-list 'ac-modes 'html-mode t)
    (add-to-list 'completion-styles 'initials t)))

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
  ;(shell-command (concat "wmctrl -i -r " (frame-parameter nil 'outer-window-id) " -btoggle,fullscreen")))

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

(defvar audacious-playlist-buffer-name
  "*audacious-playlist*"
  "Name of the buffer that will hold the Audacious playlist.")

(defun audacious-show-playlist ()
  "Show the playlist buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create audacious-playlist-buffer-name))
  (audacious-mode)
  (audacious-update-playlist))

(defun audacious-update-playlist ()
  "Update the playlist buffer."
  (interactive)
  (let ((buffer (get-buffer audacious-playlist-buffer-name)))
    (if buffer
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (start-process "audtool" (current-buffer) "audtool" "playlist-display")))
      (when (called-interactively-p)
        (error "No Audacious buffer open")))))

(defun audacious-jump-to-current ()
  "Jump to current song in playlist buffer."
  (interactive)
  (let ((buffer (get-buffer audacious-playlist-buffer-name)))
    (if buffer
        (with-current-buffer buffer
          (remove-overlays)
          (let ((position (replace-regexp-in-string
                           "\n$" ""
                           (shell-command-to-string "audtool playlist-position"))))
            (beginning-of-buffer)
            (search-forward position nil t)
            (overlay-recenter (point))
            (beginning-of-line)
            (overlay-put (make-overlay (line-beginning-position) (line-end-position)) 'face 'bold)))
      (when (called-interactively-p)
        (error "No Audacious buffer open")))))

(defun audacious-track-at-point ()
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "^.* | \\(.*\\) - .* - \\(.*?\\)[ ]* | .*$" nil t)
        (cons (match-string 1) (match-string 2))
      nil)))

(defun audacious-show-lyrics ()
  "Show lyrics of track at point in Audacious buffer."
  (interactive)
  (let ((match (audacious-track-at-point)))
    (if match
        (let ((artist (car match))
              (track (cdr match)))
          (browse-url
           (format "http://lyrics.wikia.com/%s:%s"
                   artist track)))
      (error "Couldn't find track at point"))))

(defun audacious-play-position ()
  "Play the song at current position."
  (interactive)
  (let ((buffer (get-buffer audacious-playlist-buffer-name)))
    (if buffer
        (with-current-buffer buffer
          (save-excursion
            (beginning-of-line)
            (let ((position (number-to-string (read (current-buffer)))))
              (call-process "audtool" nil nil nil "playlist-jump" position)
              (call-process "audtool" nil nil nil "playback-play")
              (audacious-jump-to-current))))
      (when (called-interactively-p)
        (error "No Audacious buffer open")))))

(defvar audacious-font-lock
  '(("^\\(.*\\) | \\(.*\\) | \\(.*\\)$"
     (1 font-lock-variable-name-face t)
     (2 font-lock-string-face t)
     (3 font-lock-constant-face t)))
  "Font lock for Audacious mode.")

(define-derived-mode audacious-mode special-mode
  "Audacious"
  "A major mode for controlling an Audacious playlist."
  (set (make-local-variable 'font-lock-defaults) '(audacious-font-lock))

  (hl-line-mode 1)

  (define-key audacious-mode-map (kbd "q") 'quit-window)
  (define-key audacious-mode-map (kbd "r") 'audacious-update-playlist)
  (define-key audacious-mode-map (kbd "c") 'audacious-jump-to-current)
  (define-key audacious-mode-map (kbd "k") 'previous-line)
  (define-key audacious-mode-map (kbd "j") 'next-line)
  (define-key audacious-mode-map (kbd "?") 'describe-mode)
  (define-key audacious-mode-map (kbd "L") 'audacious-show-lyrics)
  (define-key audacious-mode-map (kbd "RET") 'audacious-play-position)

  (define-key evil-normal-state-local-map (kbd "q") 'quit-window)
  (define-key evil-normal-state-local-map (kbd "r") 'audacious-update-playlist)
  (define-key evil-normal-state-local-map (kbd "c") 'audacious-jump-to-current)
  (define-key evil-normal-state-local-map (kbd "k") 'previous-line)
  (define-key evil-normal-state-local-map (kbd "j") 'next-line)
  (define-key evil-normal-state-local-map (kbd "?") 'describe-mode)
  (define-key evil-normal-state-local-map (kbd "L") 'audacious-show-lyrics)
  (define-key evil-normal-state-local-map (kbd "RET") 'audacious-play-position))

;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"])
 '(ansi-term-color-vector [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"])
 '(custom-safe-themes (quote ("75d4ccc5e912b93f722e57cca3ca1a15e079032cd69fd9bc67268b4c85639663" "405fda54905200f202dd2e6ccbf94c1b7cc1312671894bc8eca7e6ec9e8a41a2" "24cb1b9c182198f52df7cebf378ee9ecca93a2daeb9a90049a2f1f556119c742" "c1dd7946f725d23a46eb63af40a22b312b1bd3b70adcc068390b1264b0921412" "0c1fbb6f05ccd8a8e8420a28c78af1300e37bac0e6ed9e84d1170cf4870be9e3" "a8959e608df4ef23945b03a3bec8f78b05df7cdd88e87671355222ef257a50a8" "f851f8aa76ca04c9e052910bb116592721f78cc7e0dc3db7f1b462ae56126abd" "7e38d8ade108c0abf5138dd0123179527803be758a36e87a6d4a79f218a03e09" "82cc9490e4a49cd12a4b2fd8f37e67f5cdd6f243636a5f9ad064987afe1abebc" "bbf8b524322ae009d8f6a9410cb4b261648fdf23ebb3214973db47ddfa229ec3" "b8795891e523f49ae4455793ad185df846ea1cad0b96c816ae1132d5c02bbea5" "760e09d7acbc963590c97ddd27652ea67e2c918452251c0ad56ea7579cb5a68d" "79079f35dedfeb0a76f33a4339e6d49f64875bfd6ec8aa3721133441268ce8c3" "da9ac29e5f74119cd70eb2e33e500b0ff9448f16b8fee4f7dd7dbb5e74d3eda1" "2cafc5ae0989cf934adb98c23c5bc2bf45f8c8bd35ce69a020bdb4957c5243bb" "c02fb5690dff0a6e599ba388c2ee167990333a1cd96338cba8abfc4eb0ba1fb9" "66bb5247f8f409414813b3353f15761802f1a996b2d32cbb873ce220c296402b" "a1d9ee31e3ecd85a45c82efe0280f1f7b303e5135c2eb7755ecaba788a2487ad" "bef599043372a954dbf1f30fa372b24736ea62124c4dbdc9aa2b88e99af330c3" "e4f90650031231c035a30cf110dfe40c03291a7b1b8c3826ffc31fc15b787fe4" "90ea1f27b41b936e8cbbb1d194a780e010676413a8e52ceab1cfb846f2f7db76" "a54d42e2c032cc8633feedb0da5445bbcdf1ba3dc550c0d6841fcbe25bfcd95a" "644a9b6d0b382ba0e96ae7df6aa9172c77790ef4c65ece93a6d15177ac32905b" "134d090f2473516f8aec4ab1ec388f567e1032bd1ca4f4aa72945b7dff675219" "8eb5a19ecd21b0305db248f2540bad2f43a253cbb851fe81786caa1533c43601" "06f5145c01ec774a0abb49eeffa3980743ce2f997112b537effeb188b7c51caf" "b6f7795c2fbf75baf3419c60ef7625154c046fc2b10e3fdd188e5757e08ac0ec" "936e5cac238333f251a8d76a2ed96c8191b1e755782c99ea1d7b8c215e66d11e" "446c73cdfb49f1dab4c322e51ac00a536fb0e3cb7e6809b9f4616e0858012e92" "4dacec7215677e4a258e4529fac06e0231f7cdd54e981d013d0d0ae0af63b0c8" "8f6537eb6f9d66b060c736f5f680f5c661e0a6b311b86defa293bc5ba104a030" "3bd9497fb8f39c28ab58a9e957152ba2dc41223c23c5520ef10fc7bd6b222384" "246a51f19b632c27d7071877ea99805d4f8131b0ff7acb8a607d4fd1c101e163" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "15fa54dffe7ef4c91033739a8d2eba0fb897337dffe1f98b0629978183690c42" "1c27614df08fb5f4457d97d15c50a118d3cb143431e7dbd7eb833e94b880c286" "1cee2deb577b631e22b773120c95a9201a1e994f297802862daa30d311155c8c" "35cdc21c39fce0c36d86dc17a0b68682756511af6ffe165236f59bab47078f73" "d51911fb5c7315afee507a137c763f014ca162cd14736ee945586355229c607c" "fe155bdda1ce7f8cd362b3964f430c7ad0313ee8d32e2dd561319f392d528174" "06ffa5c99dfe0042b6639b28dc9c5fbc2137ca6577cd8b901a664e59f8fab0fb" "e0d089b338f8af95d12e51f73e87c516edf750a9449dec850409f301c3047da9" "48f0724fdd47bdc2be2df019e7ea2a33edc8a08791d03b28b1386aa9730c5d88" "253bd40645913cc95b9f8ef0533082cb9a4cb0810f854c030f3ef833ee5b9731" "059db9c5e55631d154bf3e53233cdc30616020a40f13530d005912ca104a4178" "6fc907278bbe283967372527ad3a98546b4a53cf5a78746cadd6d46bd8d8d05c" "9d9ba63d13aa4401627c7e2f59f3ee1e30dc931b02505c8c54727b696e46b350" "d5e5040c58f860f2b1ad6e652bb00383b7b200125c72d3d99091934afb0567b0" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "5339210234ec915d7d3fd87bfeb506bfc436ff7277a55516ab1781ec85c57224" "281e88e0dfab4980a157331b368fb2e5eba315c38f38099d2d9153980a8047ba" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#d9d9d9")
 '(org-agenda-files (quote ("/home/zanea/Documents/org/events.org" "/home/zanea/Documents/org/finances.org" "/home/zanea/Documents/org/iAko.org" "/home/zanea/Documents/org/log.org" "/home/zanea/Documents/org/recurring.org" "/home/zanea/Documents/org/techxpo.org" "/home/zanea/Documents/org/todo.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
