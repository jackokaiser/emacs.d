;;; Personal configuration -*- lexical-binding: t -*-

;; Save the contents of this file under ~/.emacs.d/init.el
;; Do not forget to use Emacs' built-in help system:
;; Use C-h C-h to get an overview of all help commands.  All you
;; need to know about Emacs (what commands exist, what functions do,
;; what variables specify), the help system can provide.

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; GUI
(load-theme 'tsdh-dark t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-display-line-numbers-mode t)
(global-hl-line-mode 1)
(setq column-number-mode t)

;; Whitespace and tabs cleanup
(global-whitespace-mode)
(setq show-trailing-whitespace t)
(setq whitespace-action '(auto-cleanup))
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab face missing-newline-at-eof)) ;; only show bad whitespace
;; change the character which means "new line"
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. try (insert-char 182 ) to see it
      '(
	(space-mark 32 [183] [46]) ; 32 SPACE, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
	(newline-mark 10 [182 10]) ; 10 LINE FEED
	(tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
	))


;; Disable splash screen
(setq inhibit-startup-screen t)
(setq read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      completion-ignore-case t)

;; Backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Snippets
(unless (package-installed-p 'yasnippet)
  (package-install 'yasnippet))
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
	))
(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.

;; Automatically pair parentheses
(electric-pair-mode t)

;; Enable LSP support by default in programming buffers
(unless (package-installed-p 'eglot)
  (package-install 'eglot))
(add-hook 'python-mode-hook #'eglot-ensure)
(global-set-key (kbd "C-c a")  'eglot-rename)

;; Enabled inline static analysis
(add-hook 'prog-mode-hook #'flymake-mode)
(setq help-at-pt-display-when-idle t)
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c p") #'flymake-goto-prev-error)
  (define-key flymake-mode-map (kbd "C-c l") #'flymake-show-buffer-diagnostics)
  )

;; Documentation
(unless (package-installed-p 'eldoc)
  (package-install 'eldoc))

;; Format code on save
(unless (package-installed-p 'format-all)
  (package-install 'format-all))
(add-hook 'prog-mode-hook 'format-all-mode)
(add-hook 'format-all-mode-hook 'format-all-ensure-formatter)


;; Browse kill ring
(unless (package-installed-p 'browse-kill-ring)
  (package-install 'browse-kill-ring))
(global-set-key (kbd "M-y")  'browse-kill-ring)

;; Auto-completion
(unless (package-installed-p 'company)
  (package-install 'company))
(add-hook 'prog-mode-hook #'company-mode)

;;; Git client
(unless (package-installed-p 'magit)
  (package-install 'magit))
(setq vc-follow-symlinks t)

;; Conda
(unless (package-installed-p 'conda)
  (package-install 'conda))
(custom-set-variables
 '(conda-anaconda-home "~/miniconda3/"))
(conda-env-activate "dsc")

;; Bind the `magit-status' command to a convenient key.
(global-set-key (kbd "C-c g") #'magit-status)

;; Show word-granularity differences within diff hunks
(setq magit-diff-refine-hunk t)

;;; Haskell Support
(unless (package-installed-p 'haskell-mode)
  (package-install 'haskell-mode))

;;; YAML Support
(unless (package-installed-p 'yaml-mode)
  (package-install 'yaml-mode))

;;; LaTeX support
(unless (package-installed-p 'auctex)
  (package-install 'auctex))
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Enable LaTeX math support
(add-hook 'LaTeX-mode-map #'LaTeX-math-mode)

;;; Markdown support
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

;;; Docker support
(unless (package-installed-p 'dockerfile-mode)
  (package-install 'dockerfile-mode))

;;; Web support
(unless (package-installed-p 'web-mode)
  (package-install 'web-mode))
(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-script-padding 0)
  (setq indent-tabs-mode nil)
  (setq create-lockfiles nil) ;; preact watch doesn't like .# files
  )
(add-hook 'web-mode-hook  'web-mode-init-hook)
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json?$" . web-mode))

;; Miscellaneous options
(setq-default major-mode
	      (lambda () ; guess major mode from file name
		(unless buffer-file-name
		  (let ((buffer-file-name (buffer-name)))
		    (set-auto-mode)))))
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(setq recentf-max-menu-items 80)
(setq recentf-max-saved-items 80)
(global-set-key [f4] 'recentf-open-files)
(defalias 'yes-or-no #'y-or-n-p)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Text navigation
(defun next5()
  (interactive)
  (next-line 5))

(defun prev5()
  (interactive)
  (previous-line 5))

(defun back-window ()
  (interactive)
  (other-window -1))
(global-set-key "\M-n" 'next5)
(global-set-key "\M-p" 'prev5)
(global-set-key "\M-o" 'other-window)
(global-set-key "\M-i" 'back-window)
(global-set-key (kbd "C-c SPC")  'bookmark-set)
(global-set-key (kbd "C-j") 'bookmark-jump)
(global-set-key (kbd "M-j") 'bookmark-bmenu-list)
(global-set-key (kbd "C-c s") 'rgrep)
(global-set-key [f1] 'next-error)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c k") 'sp-kill-sexp)
(global-set-key (kbd "C-c C-v") 'bury-buffer)

;; Text editing
(global-set-key (kbd "C-c r") 'replace-string)
(global-set-key "\C-z" 'zap-to-char)
(global-set-key (kbd "C-SPC") 'set-mark-command)
(global-set-key (kbd "C-c q") 'query-replace)
(global-set-key (kbd "M-l") 'query-replace-regexp)
(global-set-key [f6] 'call-last-kbd-macro)
(global-set-key (kbd "C-c c") 'compile)
(global-set-key [f5] 'recompile)
(defalias 'rr 'replace-regexp)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
