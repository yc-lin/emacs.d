(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

(add-to-list 'load-path "/home/yclin/.emacs.d/pkg/")
(package-initialize)

(setq inhibit-startup-screen t initial-buffer-choice  nil)
(setq org-startup-indented t)


(require 'ivy)
(require 'ivy-smex)
(require 'ivy-xref)
(require 'ivy-rich)
(ivy-mode 1)
(global-set-key (kbd "M-x") 'ivy-smex)
;(ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
(define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
(define-key ivy-switch-buffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
(define-key ivy-mode-map (kbd "<escape>") 'minibuffer-keyboard-quit)

(setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)


(defvar ivy-regex-switch-value 1 "Initial setting for the `a` global variable.")
(defun ivy-regex-switch ()
 "Doc-string for `ivy-regex-switch` function."
 (interactive)
 (cond
  ((= ivy-regex-switch-value 1)
   (message "Select ivy-regex")
   (setq ivy-re-builders-alist '((swiper . ivy--regex)
                                 (t . ivy--regex-plus)))
   (setq ivy-regex-switch-value 2))
  ((= ivy-regex-switch-value 2)
   (message "Select ivy-regex-plus")
   (setq ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                 (t . ivy--regex-plus)))
   (setq ivy-regex-switch-value 3))
  ((= ivy-regex-switch-value 3)
   (message "Select ivy-regex-ignore-order")
   (setq ivy-re-builders-alist '((swiper . ivy--regex-ignore-order)
                                 (t . ivy--regex-plus)))
   (setq ivy-regex-switch-value 4))
  ((= ivy-regex-switch-value 4)
   (message "Select ivy-regex-fuzzy")
   (setq ivy-re-builders-alist '((swiper . ivy--regex-fuzzy)
                                 (t . ivy--regex-plus)))
   (setq ivy-regex-switch-value 1))))


;;------------------------------------------------------------------------------
;; custom Script
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
                    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(setq yc-skippable-buffers '("*Messages*" "*Help*" "*Completions*"
                             "*Buffer List*" "*Backtrace*" "*Compile-Log*"
                             "*GNU Emacs*" "*scratch*"
                             "*lsp-cquery stderr*"
                             "*lsp-cquery stderr*<2>"
                             "*xref*"))
(defun yc-change-buffer (change-buffer)
  "Call CHANGE-BUFFER until current buffer is not in `yc-skippable-buffers'."
  (let ((initial (current-buffer)))
    (funcall change-buffer)
    (let ((first-change (current-buffer)))
      (catch 'loop
        (while (member (buffer-name) yc-skippable-buffers)
          (funcall change-buffer)
          (when (eq (current-buffer) first-change)
            (switch-to-buffer initial)
            (throw 'loop t)))))))

(defun yc-next-buffer ()
  "`next-buffer' that skips `yc-skippable-buffers'."
  (interactive)
  (yc-change-buffer 'next-buffer))

(defun yc-previous-buffer ()
  "`previous-buffer' that skips `yc-skippable-buffers'."
  (interactive)
  (yc-change-buffer 'previous-buffer))

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

(setq hippie-expand-try-function-list '(try-expand-debbrev try-expand-debbrev-all-buffers
                                                           try-expand-debbrev-from-kill try-complete-file-name-partially
                                                           try-complete-file-name try-expand-all-abbrevs
                                                           try-expand-list try-expand-line try-complete-lisp-symbol-partially
                                                           try-complete-lisp-symbol))
(setq *is-a-mac* (eq system-type 'darwin))
(setq *cygwin* (eq system-type 'cygwin))
(setq *linux* (or (eq system-type 'gnu/linux)
                  (eq system-type 'linux)))
(defun copy-to-x-clipboard ()
  (interactive)
  (if (region-active-p)
      (progn
        (cond
         ((and (display-graphic-p)
               x-select-enable-clipboard)
          (x-set-selection 'CLIPBOARD
                           (buffer-substring (region-beginning)
                                             (region-end))))
         (t (shell-command-on-region (region-beginning)
                                     (region-end)
                                     (cond
                                      (*cygwin* "putclip")
                                      (*is-a-mac* "pbcopy")
                                      (*linux* "xsel -ib")))))
        (message "Yanked region to clipboard!")
        (deactivate-mark))
    (message "No region active; can't yank to clipboard!")))

(defun paste-from-x-clipboard ()
  (interactive)
  (cond
   ((and (display-graphic-p)
         x-select-enable-clipboard)
    (insert (x-get-selection 'CLIPBOARD)))
   (t (shell-command (cond
                      (*cygwin* "getclip")
                      (*is-a-mac* "pbpaste")
                      (t "xsel -ob"))
                     1))))
(setq save-interprogram-paste-before-kill t)

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
    (setq yc-counsel-ag-arg 1))))

                                        ;(defcustom counsel-rg-base-command "rg -i --no-heading --line-number --color never %s ."
(setq yc-rg-base-command-str '"rg --no-heading --line-number --color never --follow")
(setq yc-rg-base-command-str '"rg --no-heading --line-number --color never ")
(setq yc-rg-arg-context-str '" --context 1")
(setq yc-rg-arg-ignorecase-str '" --ignore-case")
(setq yc-rg-arg-regexp-str '" --regexp")
(setq yc-rg-arg-follow-str '" --follow")
(setq yc-rg-arg-value 0)
(setq yc-rg-cmd-str yc-rg-base-command-str)

(defun yc-rg-argument-change ()
  "Doc-string for `counsel-rg-arg rebuild` function."
  (progn
    (setq yc-rg-cmd-str yc-rg-base-command-str)
    (if (= (logand yc-rg-arg-value 1) 1)
        (setq yc-rg-cmd-str (concat yc-rg-cmd-str yc-rg-arg-context-str)))
    (if (= (logand yc-rg-arg-value 2) 2)
        (setq yc-rg-cmd-str (concat yc-rg-cmd-str yc-rg-arg-ignorecase-str)))
    (if (= (logand yc-rg-arg-value 4) 4)
        (setq yc-rg-cmd-str (concat yc-rg-cmd-str yc-rg-arg-regexp-str)))
    (if (= (logand yc-rg-arg-value 4) 8)
        (setq yc-rg-cmd-str (concat yc-rg-cmd-str yc-rg-arg-follow-str)))
    (setq yc-rg-cmd-str (concat yc-rg-cmd-str " %s"))
    (message "rg command is: %s" yc-rg-cmd-str)
    (setq counsel-rg-base-command yc-rg-cmd-str)))

(defun yc-rg-arg-set (value)
  "Doc-string for `counsel-rg-arg select` function."
  (progn
    (if (= value 0)
        (setq yc-rg-arg-value 0))
    (if (= value 1)
        (setq yc-rg-arg-value (+ yc-rg-arg-value 1)))
    (if (= value 2)
        (setq yc-rg-arg-value (+ yc-rg-arg-value 2)))
    (if (= value 4)
        (setq yc-rg-arg-value (+ yc-rg-arg-value 4)))
    (if (= value 8)
        (setq yc-rg-arg-value (+ yc-rg-arg-value 8)))
    (yc-rg-argument-change)))
(yc-rg-argument-change)

(defun yc-rg-arg-set-default ()
  (interactive)
  "Doc-string for set default"
  (yc-rg-arg-set 0))
(defun yc-rg-arg-set-context ()
  (interactive)
  "Doc-string for set context"
  (yc-rg-arg-set 1))
(defun yc-rg-arg-set-ignorecase ()
  (interactive)
  "Doc-string for set ignorecase"
  (yc-rg-arg-set 2))
(defun yc-rg-arg-set-regrexp ()
  (interactive)
  "Doc-string for set regrexp"
  (yc-rg-arg-set 4))
(defun yc-rg-arg-set-follow ()
  (interactive)
  "Doc-string for set follow"
  (yc-rg-arg-set 8))
(defun yc-rg-arg-show-cmd-str ()
  (interactive)
  "Doc-string for show query cmd"
  (message "rg command is: %s" counsel-rg-base-command))

(setq systemverilog-imenu-generic-expression '(("class" "^class \\(.+\\)\\( \\)*" 1)
                                               ;;("function" "\\( *\\)function\\( +\\)\\(.+\\)\\( +\\)\\(.+\\)" 3)
                                               ;;("task" "task \\(.+\\)\\( \\)" 1)
                                               ("module" "^module \\(.+\\)\\( \\)" 1)
                                               ("interface" "^interface \\(.+\\)\\( \\)"
                                                1)))

;(add-hook 'org-mode-hook 'evil-org-mode)

(add-hook 'verilog-mode-hook
          (lambda ()
            (setq imenu-generic-expression systemverilog-imenu-generic-expression)))

(defpowerline powerline-minor-modes-yc
  (mapconcat (lambda (mm)
               (propertize mm
                           'mouse-face 'mode-line-highlight
                           'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
                           'local-map (let ((map (make-sparse-keymap)))
                                        (define-key map
                                          [mode-line down-mouse-1]
                                          (powerline-mouse 'minor 'menu mm))
                                        (define-key map
                                          [mode-line mouse-2]
                                          (powerline-mouse 'minor 'help mm))
                                        (define-key map
                                          [mode-line down-mouse-3]
                                          (powerline-mouse 'minor 'menu mm))
                                        (define-key map
                                          [header-line down-mouse-3]
                                          (powerline-mouse 'minor 'menu mm))
                                        map)))
             (split-string (format-mode-line minor-mode-alist))
             (propertize " " 'face face)))

(setq-default frame-title-format "%b (%f)")


(defun yank-browse (string)
      "Browse the `kill-ring' to choose which entry to yank."
      (interactive
       (minibuffer-with-setup-hook #'minibuffer-completion-help
         (let* ((kills (delete-dups (append kill-ring-yank-pointer kill-ring nil)))
                (entries
                 (mapcar (lambda (string)
                           (let ((pos 0))
                             ;; FIXME: Maybe we should start by removing
                             ;; all properties.
                             (setq string (copy-sequence string))
                             (while (string-match "\n" string pos)
                               ;; FIXME: Maybe completion--insert-strings should
                               ;; do that for us.
                               (put-text-property
                                (match-beginning 0) (match-end 0)
                                'display (eval-when-compile
                                           (propertize "\\n" 'face 'escape-glyph))
                                string)
                               (setq pos (match-end 0)))
                             ;; FIXME: We may use the window-width of the
                             ;; wrong window.
                             (when (>= (* 3 (string-width string))
                                       (* 2 (window-width)))
                               (let ((half (- (/ (window-width) 3) 1)))
                                 ;; FIXME: We're using char-counts rather than
                                 ;; width-count.
                                 (put-text-property
                                  half (- (length string) half)
                                  'display (eval-when-compile
                                             (propertize "……" 'face 'escape-glyph))
                                  string)))
                             string))
                         kills))
                (table (lambda (string pred action)
                         (cond
                          ((eq action 'metadata)
                           '(metadata (category . kill-ring)))
                          (t
                           (complete-with-action action entries string pred))))))
           ;; FIXME: We should return the entry from the kill-ring rather than
           ;; the entry from the completion-table.
           ;; FIXME: substring completion doesn't work well because it only matches
           ;; subtrings before the first \n.
           ;; FIXME: completion--insert-strings assumes that boundaries of
           ;; candidates are obvious enough, but with kill-ring entries this is not
           ;; true, so we'd probably want to display them with «...» around them.
           (list (completing-read "Yank: " table nil t)))))
      (setq this-command 'yank)
      (insert-for-yank string))
;;------------------------------------------------------------------------------
;; Configuration
(set-frame-font "Dejavu Sans Mono for Powerline 9" nil t)
(setq x-select-enable-clipboard t)
(setq inhibit-startup-screen t)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq visible-bell nil)
(setq ring-bell-function #'ignore)
(setq-default indent-tabs-mode nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(global-set-key [(control ?h)]
                'delete-backward-char)
(when (featurep 'menu-bar)
  (menu-bar-mode -1))
(when (featurep 'tool-bar)
  (tool-bar-mode -1))
(when (featurep 'tooltip)
  (tooltip-mode -1))
(when (featurep 'scroll-bar)
  (scroll-bar-mode -1))
(font-lock-add-keywords 'c-mode
                        '(("\\<\\(\\sw+\\) ?(" 1 'font-lock-function-name-face)))

(font-lock-add-keywords 'c++-mode
                        '(("\\[=+><?:\\]" 1 'font-lock-function-name-face)))


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

;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (which-function-mode t)))

;; (add-hook 'c++-mode-common-hook
;;           (lambda ()
;;             (which-function-mode t)))

(add-to-list 'auto-mode-alist
             '("\\.svi\\'" . verilog-mode))
(add-to-list 'auto-mode-alist
             '("\\.svip\\'" . verilog-mode))
(add-to-list 'auto-mode-alist
             '("\\.inc\\'" . verilog-mode))

(setq kill-buffer-query-functions (remq 'process-kill-buffer-query-function
                                        kill-buffer-query-functions))
(put 'dired-find-alternate-file 'disabled
     nil)
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

(require 'verilog-mode)
(setq verilog-auto-newline nil)

(setq bookmark-default-file "~/.emacs.d/bookmarks"
      bookmark-save-flag 1)


;;------------------------------------------------------------------------------
;; Packages
;;
;;(setq inferior-lisp-program "/usr/bin/sbcl")
;;(require 'slime)
;;(require 'slime-autoloads)
;;(setq slime-contribs '(slime-scratch slime-editing-commands slime-cl-indent))
;;(slime-setup '(slime-fancy slime-company slime-cl-indent))

;;(defvar ac-slime-modes
;;  '(lisp-mode))
;;
;;(defun ac-slime-candidates ()
;;  "Complete candidates of the symbol at point."
;;  (if (memq major-mode ac-slime-modes)
;;      (let* ((end (point))
;;             (beg (slime-symbol-start-pos))
;;             (prefix (buffer-substring-no-properties beg end))
;;             (result (slime-simple-completions prefix)))
;;        (destructuring-bind (completions partial) result
;;          completions))))
;;
;;(defvar ac-source-slime
;;  '((candidates . ac-slime-candidates)
;;    (requires-num . 3)))
;;
;;(ac-config-default)
;;
;;(add-hook 'lisp-mode-hook (lambda ()
;;                            (slime-mode t)
;;                            (push 'ac-source-slime ac-sources)
;;                                (auto-complete-mode)))
;;
                                        ;(require 'cc-mode)

(global-hl-line-mode t)
(set-face-background hl-line-face "#303030")
(show-paren-mode t)
(setq show-paren-delay 0)
(set-face-foreground 'show-paren-match "#00aF00")

(use-package cc-mode
  :config
  (setq c-default-style "k&r")
  )


(load-theme 'material t)
(use-package powerline)

(use-package airline-themes
  :init
  (progn
    (require 'airline-themes)
    (load-theme 'airline-cool t))
  :config
  (progn
    (setq powerline-utf-8-separator-left        #xe0b0
          powerline-utf-8-separator-right       #xe0b2
          airline-utf-glyph-separator-left      #xe0b0
          airline-utf-glyph-separator-right     #xe0b2
          airline-utf-glyph-subseparator-left   #xe0b1
          airline-utf-glyph-subseparator-right  #xe0b3
          airline-utf-glyph-branch              #xe0a0
          airline-utf-glyph-readonly            #xe0a2
          airline-utf-glyph-linenumber          #xe0a1
          airline-display-directory nil)))

(use-package highlight-numbers
  :ensure t
  :init (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package highlight-operators
  :ensure t
  :init (add-hook 'prog-mode-hook 'highlight-operators-mode))

(use-package expand-region :ensure t)
(use-package smex :ensure t)


(use-package evil
  :ensure t
  :init
  (setq evil-toggle-key "")
  (use-package evil-leader
    :init (global-evil-leader-mode)
    :config
    (progn
      (setq evil-leader/in-all-states t)
      (evil-leader/set-leader "<SPC>")
      (evil-leader/set-key
        "<SPC>" 'ace-window
        "aa" 'counsel-rg
        "m"  'counsel-bookmark
        "@"  'counsel-imenu
        "fg" 'counsel-git
        "ff" 'counsel-find-file
        "ar" 'yc-rg-arg-set-default
        "ai" 'yc-rg-arg-set-ignorecase
        "ac" 'yc-rg-arg-set-context
        "ae" 'yc-rg-arg-set-regrexp
        "as" 'yc-rg-arg-show-cmd-str
        "gc" 'wgrep-finish-edit
        "xc" 'copy-to-x-clipboard
        "xp" 'paste-from-x-clipboard
        "rr" 'ivy-regex-switch
        "gm" 'ivy-wgrep-chtnge-to-wgrep-mode
        "bl" 'ivy-switch-buffer
        "bo" 'ivy-switch-buffer-other-window
        "s"  'ivy-yasnippet
        "bb" 'ace-jump-buffer
        "bk" 'kill-buffer
        "bs" 'swap-buffers-in-windows
        "n"  'neotree-toggle
        "y"  'yank-browse
        "td" 'xref-find-definitions
        "tr" 'xref-find-references
        "tb" 'xref-pop-marker-stack
        "ci" 'evilnc-comment-or-uncomment-lines
        "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
        "cc" 'evilnc-copy-and-comment-lines
        "cp" 'evilnc-comment-or-uncomment-paragraphs
        "cr" 'comment-or-uncomment-region
        "cv" 'evilnc-toggle-invert-comment-line-by-line
       )))
  :config
    (evil-mode 1)
    (global-evil-mc-mode 1)
    (global-evil-leader-mode)
    (evil-snipe-override-mode 1)
    (evil-surround-mode 1)
    (defalias #'forward-evil-word #'forward-evil-symbol)
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
    (evil-ex-define-cmd "Wq" 'evil-save-and-close)
    (evil-ex-define-cmd "Wqall" 'evil-save-and-quit)
    (evil-ex-define-cmd "Wqa" "wqall")
    (evil-ex-define-cmd "W" "write")
    (evil-ex-define-cmd "Wall" 'evil-write-all)
    (evil-ex-define-cmd "Wa" "wall")
    (define-key evil-normal-state-map [?\r] 'yc-next-buffer)
    ;(define-key evil-normal-state-map (kbd "S-RET") 'yc-previous-buffer)
    (define-key evil-normal-state-map (kbd "TAB") 'next-multiframe-window)
    (define-key evil-motion-state-map (kbd "[") #'evil-yc-jump-up)
    (define-key evil-motion-state-map (kbd "]") #'evil-yc-jump-down)
    (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)
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

(add-hook 'c-mode-hook
	  (lambda() (eldoc-mode -1)))
(add-hook 'c++-mode-hook
	  (lambda() (eldoc-mode -1)))

;;company
(use-package company
  :ensure t
  :config
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-idle-delay 0.1)
    (setq company-minimum-prefix-length 2)
    (setq company-dabbrev-other-buffers t)
    (setq company-dabbrev-code-other-buffers 'all))
;(setq company-backends '((company-dabbrev-code company-gtags)))

(require 'company-lsp)
(push 'company-lsp company-backends)

(add-hook 'prog-major-mode #'lsp-prog-major-mode-enable)

(defun cquery//enable ()
  (condition-case nil
      (lsp-cquery-enable)
    (user-error nil)))

(use-package cquery
  :commands lsp-cquery-enable
  :init
    ;(setq cquery-extra-args '("--log-file=/tmp/cq.log"))
    (setq cquery-executable "/home/yclin/repo/cquery/bin/cquery")
    (setq cquery-cache-dir "/tmp/cq_cache")
    (add-hook 'c-mode-common-hook #'cquery//enable))

(use-package lsp-ui
  :ensure t
  :init
    (add-hook 'c-mode-common-hook 'flycheck-mode)
    (add-hook 'c++-mode-common-hook 'flycheck-mode)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :config
    (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
)

(use-package neotree
  :ensure t
  :config
    (add-hook 'neotree-mode-hook (lambda ()
                (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
                (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)))
)

(use-package yasnippet
  :ensure t
  :init
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
    (yas-global-mode 1))

;; Which Key
(use-package which-key
  :ensure t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode 1))

;;------------------------------------------------------------------------------
;; key binding
(global-set-key [remap next-buffer]
                'yc-next-buffer)
(global-set-key [remap previous-buffer]
                'yc-previous-buffer)

(global-set-key (kbd "M-w")
                'next-multiframe-window)
;(global-set-key (kbd "M-1")
;                'my-previous-buffer)
(global-set-key (kbd "M-1")
                'yc-next-buffer)
(global-set-key (kbd "M-2")
                'lsp-ui-peek-jump-forward)
(global-set-key (kbd "M-3")
                'lsp-ui-peek-jump-backward)
(global-set-key (kbd "M-4")
                'evil-mc-undo-all-cursors)
(global-set-key (kbd "M-5")
                'er/expand-region)
(global-set-key (kbd "M-6")
                'ivy-occur)
(global-set-key (kbd "M-9")
                'shrink-window)
(global-set-key (kbd "M-=")
                'enlarge-window-horizontally)
(global-set-key (kbd "M--")
                'shrink-window-horizontally)
(global-set-key (kbd "M-0")
                'enlarge-window)


;;(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-org counsel-notmuch notmuch notmuch-labeler which-key wgrep vlf use-package tree-mode smex slime-theme slime-company selected rainbow-mode rainbow-delimiters projectile-ripgrep projectile-git-autofetch parinfer neotree material-theme lsp-ui ivy-yasnippet ivy-xref ivy-rich iedit icicles highlight-symbol highlight-quoted highlight-parentheses highlight-operators highlight-numbers highlight-defined hierarchy grizzl git-gutter git-gutter+ eyebrowse expand-region evil-visualstar evil-surround evil-snipe evil-smartparens evil-quickscope evil-numbers evil-nerd-commenter evil-mc evil-leader evil-anzu elisp-format doom-themes doom-modeline dired-subtree diminish deadgrep csv-mode cquery counsel company-posframe company-lsp clang-format autopair airline-themes adaptive-wrap ace-window ac-slime))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
