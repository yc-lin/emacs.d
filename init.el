(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" ."http://melpa.org/packages/")
             t)
(add-to-list 'package-archives
             '("melpa-stable" ."http://melpa-stable.milkbox.net/packages/")
             t)
(add-to-list 'load-path "~/.emacs.d/pkg/")

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


(defvar ivy-regex-switch-value 1 "Initial setting for the `a` global variable.")
(defun ivy-regex-switch ()
  "Doc-string for `ivy-regex-switch` function."
  (interactive)
  (cond
   ((= ivy-regex-switch-value 1)
     (message "Select ivy-regex")
     (setq ivy-re-builders-alist
           '((swiper . ivy--regex)
             (t . ivy--regex-plus)))
     (setq ivy-regex-switch-value 2))
   ((= ivy-regex-switch-value 2)
     (message "Select ivy-regex-plus")
     (setq ivy-re-builders-alist
           '((swiper . ivy--regex-plus)
             (t . ivy--regex-plus)))
     (setq ivy-regex-switch-value 3))
   ((= ivy-regex-switch-value 3)
     (message "Select ivy-regex-ignore-order")
     (setq ivy-re-builders-alist
           '((swiper . ivy--regex-ignore-order)
             (t . ivy--regex-plus)))
     (setq ivy-regex-switch-value 4))
   ((= ivy-regex-switch-value 4)
     (message "Select ivy-regex-fuzzy")
     (setq ivy-re-builders-alist
           '((swiper . ivy--regex-fuzzy)
             (t . ivy--regex-plus)))
     (setq ivy-regex-switch-value 1))
    ))

(setq yc-ag-base-command '"ag --follow --nocolor --nogroup %s")
(setq yc-ag-arg-context '"ag --follow --nocolor --nogroup -C 2 %s")
(setq counsel-ag-base-command yc-ag-base-command)

(defvar yc-counsel-ag-arg 1 "counsel-ag-arg")
(defun yc-counsel-ag-argument-switch ()
  "Doc-string for `counsel-ag-argument-switch` function."
  (interactive)
  (cond
   ((= yc-counsel-ag-arg 1)
     (message "with %s" yc-ag-base-command)
     (setq counsel-ag-base-command yc-ag-base-command)
     (setq yc-counsel-ag-arg 2))
   ((= yc-counsel-ag-arg 2)
     (message "with %s" yc-ag-arg-context)
     (setq counsel-ag-base-command yc-ag-arg-context)
     (setq yc-counsel-ag-arg 1))
    ))

(setq systemverilog-imenu-generic-expression
      '(("Class" "^class \\(.+\\)" 1)
        ))

(add-hook 'verilog-mode-hook (lambda () (setq imenu-generic-expression systemverilog-imenu-generic-expression)))

;;------------------------------------------------------------------------------
;; Configuration
(setq x-select-enable-clipboard t)
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


;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

(add-hook 'c-mode-common-hook
          (lambda ()
            (counsel-gtags-mode t)))

(add-hook 'c++-mode-common-hook
          (lambda ()
            (counsel-gtags-mode t)))

(add-hook 'verilog-mode-common-hook
          (lambda ()
            (counsel-gtags-mode t)))

(add-to-list 'auto-mode-alist '("\\.svi\\'" . verilog-mode))
(add-to-list 'auto-mode-alist '("\\.svip\\'" . verilog-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . verilog-mode))

(setq kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                        kill-buffer-query-functions))
(put 'dired-find-alternate-file 'disabled
     nil)
(with-eval-after-load 'dired
                      (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

(require 'verilog-mode)
(setq verilog-auto-newline nil)
  (custom-set-variables
    '(verilog-align-ifelse t)
    '(verilog-auto-delete-trailing-whitespace t)
    '(verilog-auto-inst-param-value t)
    '(verilog-auto-inst-vector nil)
    '(verilog-auto-lineup (quote all))
    '(verilog-auto-newline nil)
    '(verilog-auto-save-policy nil)
    '(verilog-auto-template-warn-unused t)
    '(verilog-case-indent 4)
    '(verilog-cexp-indent 4)
    '(verilog-highlight-grouping-keywords t)
    '(verilog-highlight-modules t)
    '(verilog-indent-level 4)
    '(verilog-indent-level-behavioral 4)
    '(verilog-indent-level-declaration 4)
    '(verilog-indent-level-module 4)
    '(verilog-tab-to-comment t))

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

;;(require 'darkokai-theme)
;;(load-theme 'darkokai t)
(load-theme 'sanityinc-tomorrow-night t)

(require 'ivy)
(ivy-mode 1)
(custom-set-faces
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
'(ivy-highlight-face ((t (:background "#FFFFFF")))))


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
                      (define-key evil-motion-state-map (kbd "]") #'evil-yc-jump-down)
                      (evil-ex-define-cmd "Wq" 'evil-save-and-close)
                      (evil-ex-define-cmd "Wqall" 'evil-save-and-quit)
                      (evil-ex-define-cmd "Wqa" "wqall")
                      (evil-ex-define-cmd "W" "write")
                      (evil-ex-define-cmd "Wall" 'evil-write-all)
                      (evil-ex-define-cmd "Wa" "wall")
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

(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

(require 'clang-format)
(setq clang-format-style "Google")
;; (add-hook 'c-mode-hook
;; 	  (lambda() (add-hook 'before-save-hook 'clang-format-buffer)))
;; (add-hook 'c++-mode-hook
;; 	  (lambda() (add-hook 'before-save-hook 'clang-format-buffer)))

;;company
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 1)
(setq company-dabbrev-other-buffers t)
(setq company-dabbrev-code-other-buffers 'all)
(setq company-backends '((company-dabbrev-code company-gtags)))

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
;; irony build example
;; cmake -DLIBCLANG_LIBRARY=/home/yclin/Downloads/clang/lib/libclang.so \
;; -DLIBCLANG_INLCUDE_DIR=/home/yclin/Downloads/clang/include
;;

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

(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(setq smooth-scroll-margin 3)
(setq scroll-setup 1
      scroll-conservatively 1000)

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
(global-set-key (kbd "M-5")
                'ivy-occur)
(global-set-key (kbd "M-8")
                'er/expand-region)
(global-set-key (kbd "M-9")
                'shrink-window)
(global-set-key (kbd "M-=")
                'enlarge-window-horizontally)
(global-set-key (kbd "M--")
                'shrink-window-horizontally)
(global-set-key (kbd "M-0")
                'enlarge-window)
(global-set-key (kbd "M-s")
                'slime-selector)
(global-set-key (kbd "M-r")
                'ivy-regex-switch)

(require 'ivy-smex)
(global-set-key (kbd "M-x")
                'ivy-smex)
(require 'evil-leader)
(global-evil-leader-mode)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
  "f" 'counsel-git
  "F" 'counsel-find-file
  "a" 'counsel-ag
  "A" 'yc-counsel-ag-argument-switch
  "@" 'counsel-imenu
  ;; "tf" 'counsle-gtags-go-forward
  ;; "tb" 'counsle-gtags-go-backward
  "ts" 'counsel-gtags-find-symbol
  "tr" 'counsel-gtags-find-reference
  "td" 'counsel-gtags-find-definition
  "T" 'counsel-find-symbol
  "l" 'list-buffers
  "k" 'kill-buffer
  "w" 'ace-window
  "ci" 'evilnc-comment-or-uncomment-lines
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  "ss" 'slime-selector
  "gm" 'ivy-wgrep-chtnge-to-wgrep-mode
  "gc" 'wgrep-finish-edit)

(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(require 'yasnippet)
(yas-global-mode 1)


;;------------------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#c5c8c6" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#373b41"))
 '(compilation-message-face (quote default))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "70403e220d6d7100bae7775b3334eddeb340ba9c37f4b39c189c2c29d458543b" default)))
 '(fci-rule-color "#373b41")
 '(highlight-changes-colors (quote ("#ff8eff" "#ab7eff")))
 '(highlight-tail-colors
   (quote
    (("#424748" 0.0)
     ("#63de5d" 0.2)
     ("#4BBEAE" 0.3)
     ("#9A8F21" 0.6)
     ("#A75B00" 0.7)
     ("#F309DF" 0.85)
     ("#424748" 0.1))))
 '(ivy-mode t)
 '(magit-diff-use-overlays nil)
 '(package-selected-packages
   (quote
    (smooth-scrolling vlf color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow wgrep rainbow-mode cycle-themes adaptive-wrap counsel-gtags ivy yasnippet highlight-defined srefactor slime-company slime-theme slime use-package evil-nerd-commenter elisp-format whitespace-cleanup-mode rainbow-delimiters iedit highlight-symbol highlight-quoted highlight-parentheses highlight-operators highlight-numbers grizzl git-gutter git-gutter+ flycheck-irony expand-region evil-visualstar evil-smartparens evil-mc evil-leader evil-anzu company-irony-c-headers company-irony clang-format autopair airline-themes ace-window ace-jump-buffer)))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#242728" "#424748" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff")))
