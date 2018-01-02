;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; GLOBAL SETTINGS ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;; some standard default
(setq inhibit-startup-message   t)
(setq transient-mark-mode 1)
(normal-erase-is-backspace-mode 1)
(setq column-number-mode t)
(add-to-list 'load-path "~/.emacs.d/jacques")
(set-language-environment "UTF-8")
(setq custom-file "~/.emacs.d/jacques/emacs-custom.el")
(load custom-file)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq save-abbrevs nil)
(setq suggest-key-bindings t)
(setq vc-follow-symlinks t)
(load "defuns-config.el")
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; global highlight line - could we change color though?
(global-hl-line-mode 1)
;; small color change
(set-face-background hl-line-face "gray13")
;; keep line color
(set-face-foreground 'highlight nil)
;; indent like emacs 23
(electric-indent-mode t)

;; I use version control, don't annoy me with backup files everywhere
(setq make-backup-files nil)
(setq auto-save-default nil)
;; (setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; WHITESPACE / TABS ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; TAB WIDTH
;; remove tabs and display tabs/trailing whitespace
;; remove tabs and add 2 whitespace instead
(setq c-basic-indent 2)
(setq css-indent-offset 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)

;;;;;;;;;;;;;;;;;; WHITESPACE MODE
;; automatically clean up bad whitespace
(global-whitespace-mode)
(setq whitespace-action '(auto-cleanup))
;; ;; higlight line longer than 80 characters
;; (setq whitespace-line-column 79)

(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab face)) ;; only show bad whitespace
(setq show-trailing-whitespace t)
;; change the character which means "new line"
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
      '(
        (space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        ))


;; DEL Overriding
;; easier to indent: backward indent when beginning
;; of column and press 'DEL'
(defun backward-delete-whitespace-to-column ()
      "delete back to the previous column of whitespace, or just one
    char if that's not possible. This emulates vim's softtabs
    feature."
      (interactive)
      (if indent-tabs-mode
          (call-interactively 'backward-delete-char-untabify)
        ;; let's get to work
        (let ((movement (% (current-column) tab-width))
              (p (point)))
          ;; brain freeze, should be easier to calculate goal
          (when (= movement 0) (setq movement tab-width))
          (if (save-excursion
                (backward-char movement)
                (string-match "^\\s-+$" (buffer-substring-no-properties (point) p)))
              (delete-region (- p movement) p)
            (call-interactively 'backward-delete-char-untabify)))))

    (global-set-key (kbd "<DEL>") 'backward-delete-whitespace-to-column)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; INDENT-GUIDE ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'fill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; AGGRESIVE-INDENT ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (global-aggressive-indent-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; ISPELL DICO ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; check de l'orthographe, ispell mode
(autoload 'ispell-word "ispell"
  "Check the spelling of word in buffer." t)
(global-set-key "\e$" 'ispell-word)
(autoload 'ispell-region "ispell"
  "Check the spelling of region." t)
(autoload 'ispell-buffer "ispell"
  "Check the spelling of buffer." t)
(autoload 'ispell-complete-word "ispell"
  "Look up current word in dictionary and try to complete it." t)
(autoload 'ispell-change-dictionary "ispell"
  "Change ispell dictionary." t)
(autoload 'ispell-message "ispell"
  "Check spelling of mail message or news post.")
(autoload 'ispell-minor-mode "ispell"
  "Toggle mode to automatically spell check words as they are typed in.")

;; de base on utilise le dico francais, notre dico personnel est creé dans
;; la valeure de cette variable:
(setq personnal-dico "~/.ispell-dico-perso")

(setq sgml-mode-hook
      '(lambda () "Défauts pour le mode SGML."
         (setq ispell-personal-dictionary personnal-dico)
         (ispell-change-dictionary "francais")
         ))

;; english by default
(ispell-change-dictionary "english")
;; (ispell-change-dictionary "francais")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; THEME ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(color-theme-initialize)
(color-theme-billw)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; BREADCRUMBS ::::;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/lisp/breadcrumb/")
(require 'breadcrumb)

;;;;;;; breadcrumb shortcuts
(global-set-key (kbd "C-c SPC")  'bc-set)  ;; C-c space for set bookmark
(global-set-key (kbd "C-j") 'bc-previous) ;; M-j for jump to previous
(global-set-key (kbd "M-j") 'bc-next)  ;; Shift-M-j for jump to next
(global-set-key (kbd "C-c C-j") 'bc-list)  ;; C-x M-j for the bookmark menu list
;; (global-set-key [(meta p)]  'bc-local-previous) ;; M-p for local previous
;; (global-set-key [(meta n)]  'bc-local-next) ;; M-n for local next
;; (global-set-key [(control c)(j)] 'bc-goto-current) ;; C-c j for jump to current bookmark

(require 'helm-config)
;; some handy hooks for doxymacs
(require 'doxymacs)
(add-hook 'js-mode-hook 'doxymacs-mode)
(add-hook 'c-mode-common-hook 'doxymacs-mode)

(add-hook 'prog-mode-hook #'idle-highlight-mode)
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; MARKDOWN MODE ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; AUTO-COMPLETE ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'auto-complete-config)
(ac-config-default)

; Use dictionaries by default
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
; Start auto-completion after 2 characters of a word
(setq ac-auto-start 2)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; YASNIPPET ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'yasnippet)
(yas-global-mode 1)
(defalias 'yas/get-snippet-tables 'yas--get-snippet-tables)
(defalias 'yas/table-hash 'yas--table-hash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; DEPENDENCIES ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; common lisp function for emacs lisp
(require 'cl)
;; swith buffer
;; (require 'ido)
;; (ido-mode t)
;; find file at point
(ffap-bindings)
;; overrides Emacs’ default mechanism for making buffer names unique
;; color within shell buffer instead of [0x33..
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq inferior-js-mode-hook
      (lambda ()
        ;; We like nice colors
        (ansi-color-for-comint-mode-on)
        ;; Deal with some prompt nonsense
        (add-to-list
         'comint-preoutput-filter-functions
         (lambda (output)
           (replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; BROWSE KILL RING ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; browse kill-ring
;; (browse-kill-ring-default-keybindings)
(global-set-key (kbd "M-y")  'browse-kill-ring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; RECENT OPENED FILE ;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; recent opened file
(recentf-mode 1)
(setq recentf-max-menu-items 80)
(setq recentf-max-saved-items 80)
(global-set-key [f4] 'recentf-open-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; DISPLAY LINE NUMBER ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-linum-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; DIRED X ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired-x increases default dir mode
(require 'dired-x)
(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; COMPILE ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default compile command
(require 'compile)
(add-hook 'c-mode-hook
          (lambda ()
            (unless (file-exists-p "Makefile")
              (set (make-local-variable 'compile-command)
                   ;; emulate make's .c.o implicit pattern rule, but with
                   ;; different defaults for the CC, CPPFLAGS, and CFLAGS
                   ;; variables:
                   ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
                   (let ((file (file-name-nondirectory buffer-file-name)))
                     (format "%s -c -o %s.o %s %s %s"
                             (or (getenv "CC") "gcc")
                             (file-name-sans-extension file)
                             (or (getenv "CPPFLAGS") "-DDEBUG=9")
                             (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
                             file))))))

;; remove ansii color junk
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; MATLAB CONF;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; setup files ending in “.js” to open in js2-mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; JAVASCRIPT CONF;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

;;;;;;;;;;;;;;;;;;; coffee-mode shortcut
(global-set-key (kbd "C->")  'coffee-indent-shift-left)
(global-set-key (kbd "C-<")  'coffee-indent-shift-right)
(setq js-indent-level 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; SMARTPARENS ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'smartparens-config)
(setq sp-autoescape-string-quote 0)
(smartparens-global-mode 1)
(show-smartparens-global-mode 1)

(global-set-key (kbd "C-M-f") 'sp-end-of-next-sexp)
(global-set-key (kbd "C-M-b") 'sp-beginning-of-previous-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; PYTHON CONF ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
;; note that setting `venv-location` is not necessary if you
;; use the default location (`~/.virtualenvs`), or if the
;; the environment variable `WORKON_HOME` points to the right place
(setq venv-location "~/.virtualenvs/")
(venv-workon "emacs")

(defun python-add-breakpoint ()
  "Add a break point"
  (interactive)
  (newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

(global-set-key (kbd "C-c b") 'python-add-breakpoint)
;; (eval-after-load 'python-mode
;;   '(define-key (kbd "C-c b") 'python-add-breakpoint))

(defun python-interactive ()
  "Enter the interactive Python environment"
  (interactive)
  (progn
    (insert "!import code; code.interact(local=vars())")
    (move-end-of-line 1)
    (comint-send-input)))

 (global-set-key (kbd "C-c i") 'python-interactive)
;; (eval-after-load 'python-mode
;;   '(define-key (kbd "C-c i") 'python-interactive))

;; M-x isend-associate
(setq isend-skip-empty-lines nil)
(setq isend-strip-empty-lines nil)
(setq isend-delete-indentation t)
(setq isend-end-with-empty-line t)

(defadvice isend-send (after advice-run-code-sent activate compile)
  "Execute whatever sent to the (Python) buffer"
  (interactive)
  (let ((old-buf (buffer-name)))
    (progn
      (switch-to-buffer isend--command-buffer)
      (goto-char (point-max))
      (comint-send-input)
      (switch-to-buffer old-buf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; IRONY C++ AC ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; SHORTCUT CUSTOM ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; my shortcuts


(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key (kbd "C-SPC") 'set-mark-command)
(global-set-key (kbd "C-c q") 'query-replace)
(global-set-key (kbd "C-c C-v") 'bury-buffer)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key [f5] 'recompile)
(global-set-key (kbd "C-c s") 'rgrep)
(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key [f1] 'next-error)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "M-l") 'query-replace-regexp)
;; (global-set-key (kbd "C-c f") 'sp-forward-sexp)
;; (global-set-key (kbd "C-c b") 'sp-backward-sexp)
(global-set-key (kbd "C-c k") 'sp-kill-sexp)
(global-set-key [f6] 'call-last-kbd-macro)


;;;;;;; from startup class
(fset 'align-equals "\C-[xalign-regex\C-m=\C-m")
(global-set-key "\M-=" 'align-equals)
;; (global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c;" 'comment-or-uncomment-region)
(global-set-key "\M-n" 'next5)
(global-set-key "\M-p" 'prev5)
(global-set-key "\M-o" 'other-window)
(global-set-key "\M-i" 'back-window)
(global-set-key "\C-z" 'zap-to-char)
;; (global-set-key "\C-h" 'backward-delete-char)
;; (global-set-key "\M-d" 'delete-word)
;; (global-set-key "\M-h" 'backward-delete-word)
;; (global-set-key "\M-u" 'zap-to-char)
(defalias 'rr 'replace-regexp)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 115)
;; (set-face-attribute 'default nil :family "Consolas" :height 105)
;; (set-face-attribute 'default nil :family "Monofur" :height 120)
;; (set-face-attribute 'default nil :family "Inconsolata" :height 120)
;; (set-face-attribute 'default nil :family "Anonymous Pro" :height 105)
;; (set-frame-font   "Droid Sans Mono-10" nil t)

;; ;; awful font
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit autoface-default :strike-through nil :underline nil :slant normal :weight normal :height 120 :width normal :family "monaco"))))
;;  '(column-marker-1 ((t (:background "red"))))
;;  '(diff-added ((t (:foreground "cyan"))))
;;  '(flymake-errline ((((class color) (background light)) (:background "Red"))))
;;  '(font-lock-comment-face ((((class color) (min-colors 8) (background light)) (:foreground "red"))))
;;  '(fundamental-mode-default ((t (:inherit default))))
;;  '(highlight ((((class color) (min-colors 8)) (:background "white" :foreground "magenta"))))
;;  '(isearch ((((class color) (min-colors 8)) (:background "yellow" :foreground "black"))))
;;  '(linum ((t (:foreground "black" :weight bold))))
;;  '(region ((((class color) (min-colors 8)) (:background "white" :foreground "magenta"))))
;;  '(secondary-selection ((((class color) (min-colors 8)) (:background "gray" :foreground "cyan"))))
;;  '(show-paren-match ((((class color) (background light)) (:background "black"))))
;;  '(vertical-border ((t nil)))
;; )
