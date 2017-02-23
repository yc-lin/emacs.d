
(require 'package)
(package-initialize)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)

(add-to-list
 'package-archives
 '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(add-to-list 'load-path "~/.emacs.d/pkg")
;;------------------------------------------------------------------------------
;; Configuration
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq ring-bell-function #'ignore)
(setq visible-bell t)
(setq-default indent-tabs-mode nil)
(setq tramp-verbose 2)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(when (featurep 'menu-bar) (menu-bar-mode -1))
(when (featurep 'tool-bar) (menu-bar-mode -1))
(when (featurep 'scroll-bar) (menu-bar-mode -1))
;(show-paren-mode t)
;(setq show-paren-style 'expression)
;(global-linum-mode 1)
;(setq linum-format "%d ")
(font-lock-add-keywords
 'c-mode
 '(("\\<\\(\\sw+\\) ?(" 1 'font-lock-function-name-face)))

(font-lock-add-keywords
 'c++-mode
 '(("\\[=+><?:\\]" 1 'font-lock-function-name-face)))

(setq-default
 whitespace-line-column 80
 whitespace-style       '(face lines-tail))
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;(show-paren-mode t)
;(setq show-paren-style 'expression)

(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))

(setq c-default-style "k&r")

(setq airline-utf-glyph-separator-left      #xe0b0
      airline-utf-glyph-separator-right     #xe0b2
      airline-utf-glyph-subseparator-left   #xe0b1
      airline-utf-glyph-subseparator-right  #xe0b3
      airline-utf-glyph-branch              #xe0a0
      airline-utf-glyph-readonly            #xe0a2
      airline-utf-glyph-linenumber          #xe0a1)

(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

;------------------------------------------------------------------------------
; Packages
(require 'darkokai-theme)
(load-theme 'darkokai)

(require 'powerline)
(powerline-center-evil-theme)

(require 'airline-themes)
(load-theme 'airline-molokai t)

(require 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

(require 'highlight-operators)
(add-hook 'prog-mode-hook 'highlight-operators-mode)


(add-hook 'prog-mode-hook 'whitespace-cleanup-mode)
;(require 'highlight-parentheses)
;(add-hook 'prog-mode-hook 'highlight-parentheses-mode)

;(require 'autopair)
;(autopair-global-mode)

(require 'expand-region)

(setq evil-toggle-key "")
(require 'evil)
(evil-mode 1)

(with-eval-after-load 'evil
  (require 'evil-anzu)
  (require 'evil-mc)
  (global-evil-mc-mode 1)
  )

(require 'ace-window)
(setq aw-dispatch-always t)


(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'git-gutter+)
(global-git-gutter+-mode)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;(require 'google-c-style)
;(add-hook 'c-mode-common-hook 'google-set-c-style)

(require 'clang-format)
(setq clang-format-style "Google")
;; (add-hook 'c-mode-hook
;; 	  (lambda() (add-hook 'before-save-hook 'clang-format-buffer)))
;; (add-hook 'c++-mode-hook
;; 	  (lambda() (add-hook 'before-save-hook 'clang-format-buffer)))


; helm-ag
(require 'helm-ag)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#ff9da4" "#d1f1a9" "#ffeead" "#bbdaff" "#ebbbff" "#99ffff" "#003f8e"))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-day)))
 '(custom-safe-themes
   (quote
    ("70403e220d6d7100bae7775b3334eddeb340ba9c37f4b39c189c2c29d458543b" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "69831e572dc46ced47c5309bff8fc2f4a9e237e2bad2c76f313da814a4628694" default)))
 '(fci-rule-color "#003f8e")
 '(helm-ag-fuzzy-match t)
 '(helm-ag-ignore-buffer-patterns
   (quote
    ("*GNU Emacs*" "Ibuffer" "*Messages*" "*Completions*")))
 '(helm-ag-use-agignore t)
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#ff9da4")
     (40 . "#ffc58f")
     (60 . "#ffeead")
     (80 . "#d1f1a9")
     (100 . "#99ffff")
     (120 . "#bbdaff")
     (140 . "#ebbbff")
     (160 . "#ff9da4")
     (180 . "#ffc58f")
     (200 . "#ffeead")
     (220 . "#d1f1a9")
     (240 . "#99ffff")
     (260 . "#bbdaff")
     (280 . "#ebbbff")
     (300 . "#ff9da4")
     (320 . "#ffc58f")
     (340 . "#ffeead")
     (360 . "#d1f1a9"))))
 '(vc-annotate-very-old-color nil))

; company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 1)
(setq company-dabbrev-other-buffers t)
(setq company-dabbrev-code-other-buffers 'all)

; irony
(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

; company-irony
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(add-hook 'c-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'company-mode)

(require 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list 'company-backends '(company-irony-c-headers company-irony)))

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;------------------------------------------------------------------------------
;; custom Script
(setq my-skippable-buffers '("*Messages*"
			     "*scratch*"
			     "*Help*"
			     "*Completions*"
			     "*helm-ag*"
			     "*helm buffers*"
			     "*helm find"
			     "*helm find files*"
			     "*Buffer List*"
			     "*Backtrace*"
			     "*Compile-Log*"
			     "*GNU Emacs*"))

(defun my-change-buffer (change-buffer)
  "Call CHANGE-BUFFER until current buffer is not in `my-skippable-buffers'."
  (let ((initial (current-buffer)))
    (funcall change-buffer)
    (let ((first-change (current-buffer)))
      (catch 'loop
	(while (member (buffer-name) my-skippable-buffers)
	  (funcall change-buffer)
	  (when (eq (current-buffer) first-change)
	    (switch-to-buffer initial)
	    (throw 'loop t)))))))

(defun my-next-buffer ()
  "`next-buffer' that skips `my-skippable-buffers'."
  (interactive)
  (my-change-buffer 'next-buffer))

(defun my-previous-buffer ()
  "`previous-buffer' that skips `my-skippable-buffers'."
  (interactive)
  (my-change-buffer 'previous-buffer))

(defun my-linux-kernel-style ()
  (interactive)
  (setq c-default-style "linux")
  (setq c-basic-offset 8))

;;------------------------------------------------------------------------------
;; key binding
(global-set-key [remap next-buffer] 'my-next-buffer)
(global-set-key [remap previous-buffer] 'my-previous-buffer)

(global-set-key [f7] (lambda() (interactive) (find-file user-init-file)))
(global-set-key (kbd "M-=") 'ace-window)
(global-set-key (kbd "M--") 'next-multiframe-window)
;(global-set-key (kbd "M-0") 'delete-window)

(global-set-key (kbd "M-`") 'ace-jump-buffer)
(global-set-key (kbd "M-1") 'next-buffer)
(global-set-key (kbd "M-2") 'kill-buffer)

(global-set-key (kbd "M-9") 'er/expand-region)

;(global-unset-key (kbd "C-x f"))
;(global-set-key (kbd "C-x f f") 'helm-find-files)
(global-set-key (kbd "M-3") 'helm-find)
;(global-set-key (kbd "M-3") 'helm-find-files)
;(global-set-key (kbd "C-x f b") 'helm-buffers-list)
;(global-set-key (kbd "C-x f g") 'helm-ls-git-ls)
;(global-set-key (kbd "C-x f s") 'helm-do-ag-this-file)
(global-set-key (kbd "M-4") 'helm-do-ag-this-file)
(global-set-key (kbd "M-5") 'helm-do-ag-project-root)
(global-set-key (kbd "M-6") 'clang-format-buffer)
(global-set-key (kbd "M-0") 'avy-goto-line)
;(global-set-key (kbd "C-x f S") 'helm-do-ag)

;(global-unset-key (kbd "c-z"))
;(global-set-key (kbd "c-z") 'suspend-frame)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
