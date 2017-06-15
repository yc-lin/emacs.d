(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" ."http://melpa.org/packages/")
             t)
(add-to-list 'package-archives
             '("melpa-stable" ."http://melpa-stable.milkbox.net/packages/")
             t)
(add-to-list 'load-path "~/.emacs.d/pkg")

;;------------------------------------------------------------------------------
;; custom Script
(setq my-skippable-buffers '("*Messages*"
                             "*scratch*"
                             "*Help*"
                             "*Completions*"
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

(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))

(defun edit-init-file ()
  (interactive)
  (find-file user-init-file))

(setq hippie-expand-try-function-list '(try-expand-debbrev
                                        try-expand-debbrev-all-buffers
                                        try-expand-debbrev-from-kill
                                        try-complete-file-name-partially
                                        try-complete-file-name
                                        try-expand-all-abbrevs
                                        try-expand-list
                                        try-expand-line
                                        try-complete-lisp-symbol-partially
                                        try-complete-lisp-symbol))

;; (defface font-lock-func-face
;;   '((nil (:foreground "#00ffd7" :weight bold))
;;     (t (:bold t :italic t)))
;;   "Font Lock mode face used for function calls."
;;   :group 'font-lock-highlighting-faces)

;; (font-lock-add-keywords
;;    'common-lisp-mode
;;    '(("(\\s-*\\(\\_<\\(?:\\sw\\|\\s_\\)+\\)\\_>"
;;           1 'font-lock-func-face)))

(defvar yc-a 1 "Initial setting for the `a` global variable.")

(defun my-switch ()
  "Doc-string for `my-switch` function."
  (interactive)
  (cond
    ((= yc-a 1)
     (message "true")
     (setq yc-a 2))
    ((= yc-a 2)
     (message "false")
     (setq yc-a 1))))

;;------------------------------------------------------------------------------
;; Configuration

(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq ring-bell-function #'ignore)
(setq visible-bell t)
(setq-default indent-tabs-mode nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(global-set-key [(control ?h)]
                'delete-backward-char)
(when (featurep 'menu-bar)
  (menu-bar-mode -1))
(when (featurep 'tool-bar)
  (menu-bar-mode -1))
(when (featurep 'scroll-bar)
  (menu-bar-mode -1))
;;(global-linum-mode 1)
;;(setq linum-format "%d ")
(font-lock-add-keywords 'c-mode
                        '(("\\<\\(\\sw+\\) ?(" 1 'font-lock-function-name-face)))

(font-lock-add-keywords 'c++-mode
                        '(("\\[=+><?:\\]" 1 'font-lock-function-name-face)))


;;(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

(add-hook 'emacs-lisp-mode-hook 'highlight-defined-mode)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (global-set-key (kbd "M-7")
                            'srefactor-lisp-format-buffer)))
(add-hook 'lisp-mode-hook 'highlight-defined-mode)
(add-hook 'lisp-mode
          (lambda ()
            (global-set-key (kbd "M-7")
                            'srefactor-lisp-format-buffer)))
(add-hook 'c++-mode-hook
          (lambda ()
            (global-set-key (kbd "M-7")
                            'clang-format-buffer)))

(add-hook 'c-mode-hook
          (lambda ()
            (global-set-key (kbd "M-7")
                            'clang-format-buffer)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (which-function-mode t)))

(add-hook 'c++-mode-common-hook
          (lambda ()
            (which-function-mode t)))


(setq kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                        kill-buffer-query-functions))
(put 'dired-find-alternate-file 'disabled
     nil)
(with-eval-after-load 'dired
                      (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

(require 'verilog-mode)
(setq verilog-auto-newline nil)

;;------------------------------------------------------------------------------
;; Packages
;;
(setq inferior-lisp-program "/usr/bin/sbcl")
(require 'slime)
(require 'slime-autoloads)
(setq slime-contribs '(slime-scratch slime-editing-commands slime-cl-indent))
(slime-setup '(slime-fancy slime-company slime-cl-indent))

(require 'srefactor)
(require 'srefactor-lisp)

(require 'tramp)
(setq tramp-verbose 2)

(require 'cc-mode)
(setq c-default-style "k&r")

(require 'darkokai-theme)
(load-theme 'darkokai t)

(require 'airline-themes)
(load-theme 'airline-cool t)

(setq airline-utf-glyph-separator-left #xe0b0
      airline-utf-glyph-separator-right #xe0b2 airline-utf-glyph-subseparator-left
      #xe0b1 airline-utf-glyph-subseparator-right
      #xe0b3 airline-utf-glyph-branch #xe0a0 airline-utf-glyph-readonly
      #xe0a2 airline-utf-glyph-linenumber #xe0a1)


(require 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

(require 'highlight-operators)
(add-hook 'prog-mode-hook 'highlight-operators-mode)

(global-hl-line-mode t)
(set-face-background hl-line-face "#303030")

(require 'paren)
(show-paren-mode t)
(setq show-paren-delay 0)
(set-face-foreground 'show-paren-match "#00aF00")

(add-hook 'prog-mode-hook 'whitespace-cleanup-mode)
;; (require 'highlight-parentheses)
;; (add-hook 'prog-mode-hook 'highlight-parentheses-mode)

;; (require 'autopair)
;; (autopair-global-mode)


(require 'expand-region)
;; must setting two variable to enable in evil-modeA
;; ignore EMACS MODE
;; (setq evil-toggle-key "")
;; Using VIM word forword behavior to replace EMCAS behavior
;; (Defalias #'forward-evil-word #'forward-evil-symbol)


(global-evil-leader-mode)
(require 'evil)
(evil-mode 1)

(with-eval-after-load 'evil
                      (require 'evil-anzu)
                      (require 'evil-mc)
                      (global-evil-mc-mode 1)
                      (defalias #'forward-evil-word #'forward-evil-symbol)
                      ;;(define-key evil-motion-state-map "\\" 'helm-swoop)
                      (define-key evil-motion-state-map "\\" 'swiper)
                      (remove-hook 'evil-insert-state-exit-hook 'expand-abbrev t)

                      (evil-define-motion evil-yc-jump-down
                                          ()
                                          :type line
                                          :jump t
                                          "Evil motion down 4 line. Count has no effect."
                                          (forward-line 4))
                      (evil-define-motion evil-yc-jump-up
                                          ()
                                          :type line
                                          :jump t
                                          "Evil motion up 4 line. Count has no effect."
                                          (forward-line -4))
                      ;; example mapping
                      (define-key evil-motion-state-map (kbd "[") #'evil-yc-jump-up)
                      (define-key evil-motion-state-map (kbd "]") #'evil-yc-jump-down))

(require 'ace-window)
(setq aw-dispatch-always t)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'git-gutter+)
(global-git-gutter+-mode)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(require 'clang-format)
(setq clang-format-style "Google")
;; (add-hook 'c-mode-hook
;; 	  (lambda() (add-hook 'before-save-hook 'clang-format-buffer)))
;; (add-hook 'c++-mode-hook
;; 	  (lambda() (add-hook 'before-save-hook 'clang-format-buffer)))



;;helm-ag
;;(require 'helm-ag)

;;company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 1)
(setq company-dabbrev-other-buffers t)
(setq company-dabbrev-code-other-buffers 'all)

;;irony
(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point] 'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol] 'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; company-irony
(eval-after-load 'company
                 '(add-to-list 'company-backends 'company-irony))
(add-hook 'c-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'company-mode)

(require 'company-irony-c-headers)
(eval-after-load 'company
                 '(add-to-list 'company-backends
                               '(company-irony-c-headers company-irony)))
(eval-after-load 'flycheck
                 '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;etags
;;(require 'helm-etags-plus)
;;(setq tags-table-list '("./TAGS"))

(autoload 'turn-on-ctags-auto-update-mode
          "ctags-update" "turn on 'ctags-auto-update-mode'."
          t)
(add-hook 'c-mode-common-hook 'turn-on-ctags-auto-update-mode)
(add-hook 'c++-mode-common-hook 'turn-on-ctags-auto-update-mode)

;;------------------------------------------------------------------------------
;;key binding
(global-set-key [remap next-buffer]
                'my-next-buffer)
(global-set-key [remap previous-buffer]
                'my-previous-buffer)

(global-set-key (kbd "M-1")
                'ace-jump-buffer)
(global-set-key (kbd "M-2")
                'next-buffer)
(global-set-key (kbd "M-3")
                'next-multiframe-window)
(global-set-key (kbd "M-4")
                'swap-buffers-in-windows)
(global-set-key (kbd "M-8")
                'er/expand-region)
(global-set-key (kbd "M-]")
                'helm-etags-plus-history-go-back)
(global-set-key (kbd "M-=")
                'enlarge-window-horizontally)
(global-set-key (kbd "M--")
                'shrink-window-horizontally)
(global-set-key (kbd "M-0")
                'enlarge-window)
(global-set-key (kbd "M-9")
                'shrink-window)
(global-set-key (kbd "M-x")
                ;;'helm-smex)
                'counsel-M-x)
(global-set-key (kbd "M-s")
                'slime-selector)
;;(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
;;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(require 'evil-leader)
(global-evil-leader-mode)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  ;;"f" 'helm-find
  "f" 'counsel-git
  "F" 'counsel-find-files
  ;;"F" 'helm-find-files
  ;;"a" 'helm-do-ag
  "a" 'counsel-ag
  ;;"A" 'helm-do-ag-project-root
  "A" 'counsel-git-grep
  ;;"@" 'helm-imenu
  "@" 'counsel-imenu
  ;;"t" 'helm-etags-plus-select
  "t" 'counsel-gtags-find-symbol
  "T" 'counsel-find-symbol
  "l" 'list-buffers
  "k" 'kill-buffer
  "w" 'ace-window
  "ci" 'evilnc-comment-or-uncomment-lines
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  "ss" 'slime-selector)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(require 'yasnippet)
(yas-global-mode 1)


;;------------------------------------------------------------------------------
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  )

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(ansi-color-faces-vector
     [default bold shadow italic underline bold bold-italic bold])
  '(ansi-color-names-vector
     (vector "#ffffff" "#ff9da4" "#d1f1a9" "#ffeead" "#bbdaff" "#ebbbff" "#99ffff" "#003f8e"))
  '(compilation-message-face (quote default))
  '(custom-safe-themes
     (quote
       ("70403e220d6d7100bae7775b3334eddeb340ba9c37f4b39c189c2c29d458543b" default)))
  '(fci-rule-color "#003f8e")
  '(highlight-changes-colors (quote ("#ff8eff" "#ab7eff")))
  '(highlight-tail-colors
     (quote
       (("#424748" 0.0)
        ("#63de5d" 0.2)
        ("#4BBEAE" 0.3)
        ("#1DB4D0" 0.5)
        ("#9A8F21" 0.6)
        ("#A75B00" 0.7)
        ("#F309DF" 0.85)
        ("#424748" 0.1))))
  '(ivy-mode t)
  '(magit-diff-use-overlays nil)
  '(package-selected-packages
     (quote
       (counsel counsel-gtags ivy yasnippet highlight-defined srefactor slime-company slime-theme slime use-package evil-nerd-commenter elisp-format ctags-update whitespace-cleanup-mode rainbow-delimiters iedit highlight-symbol highlight-quoted highlight-parentheses highlight-operators highlight-numbers grizzl git-gutter git-gutter+ flycheck-irony expand-region evil-visualstar evil-smartparens evil-mc evil-leader evil-anzu darkokai-theme company-irony-c-headers company-irony clang-format autopair airline-themes ace-window ace-jump-buffer)))
  '(pos-tip-background-color "#E6DB74")
  '(pos-tip-foreground-color "#242728")
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
  '(vc-annotate-very-old-color nil)
  '(weechat-color-list
     (unspecified "#242728" "#424748" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff")))
