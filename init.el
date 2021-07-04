;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; GLOBAL SETTINGS ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (>= emacs-major-version 24)
	(require 'package)
	(add-to-list
	 'package-archives
	 '("melpa" . "https://melpa.org/packages/")
	 t)
	(package-initialize))
;; to install all needed packages, type:
;; M-X package-install-selected-packages

;; some standard default
(setq inhibit-startup-message t)
;; (normal-erase-is-backspace-mode 1)
(setq column-number-mode t)
(set-language-environment "UTF-8")
(add-to-list 'load-path "~/.emacs.d/jacques")
(setq custom-file "~/.emacs.d/jacques/emacs-custom.el")
(load custom-file)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq save-abbrevs nil)
(setq suggest-key-bindings t)
(setq vc-follow-symlinks t)
(load "defuns-config.el")
(setq select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; english by default
(ispell-change-dictionary "english")
;; (ispell-change-dictionary "francais")

;; global highlight line
(global-hl-line-mode 1)

;; I use version control, don't annoy me with backup files everywhere
(setq make-backup-files nil)
(setq auto-save-default nil)

; roslaunch highlighting
(add-to-list 'auto-mode-alist '("\\.launch$" . xml-mode))

;; cmake highlighting
(setq auto-mode-alist
			(append
			 '(("CMakeLists\\.txt\\'" . cmake-mode))
			 '(("\\.cmake\\'" . cmake-mode))
			 auto-mode-alist))

(setq magit-diff-refine-hunk (quote all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; WHITESPACE / TABS ;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;; TAB WIDTH
(setq-default tab-width 2)

;; automatically clean up bad whitespace
(global-whitespace-mode)
(setq whitespace-action '(auto-cleanup))
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
;;;;;;;;;;;;;;;;; FLYCHECK ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-flycheck-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; DISPLAY DASH ;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* (
		(glyph-en-dash (make-glyph-code ?\u002D 'font-lock-keyword-face))
		(glyph-em-dash (make-glyph-code ?\u002D 'font-lock-function-name-face)) )
	(when (not buffer-display-table)
		(setq buffer-display-table (make-display-table)))
	(aset buffer-display-table 8211 `[,glyph-en-dash ,glyph-en-dash])
	(aset buffer-display-table 8212 `[,glyph-em-dash ,glyph-em-dash ,glyph-em-dash]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; THEME ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'tsdh-dark)


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

;; some handy hooks for doxymacs
(require 'doxymacs)
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(add-hook 'python-mode-hook 'doxymacs-mode)

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
;;;;;;;;;;;;;;;;; YASNIPPET ;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Develop in ~/emacs.d/snippets, but also
;; include snippets that come with yasnippet
;; Load and initialize yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
			'("~/.emacs.d/snippets"                 ;; personal snippets
				))
(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; BROWSE KILL RING ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; browse kill-ring
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
;;;;;;;;;;;;;;;;; WEB CONF ;::::::;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(defun web-mode-init-hook ()
	"Hooks for Web mode.  Adjust indent."
	(setq web-mode-markup-indent-offset 2)
	(setq web-mode-code-indent-offset 2)
	(setq web-mode-css-indent-offset 2)
	(setq web-mode-script-padding 0)
	;; (setq indent-tabs-mode t)
	(setq create-lockfiles nil) ;; preact watch doesn't like .# files
	)
(add-hook 'web-mode-hook  'web-mode-init-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; PYTHON CONF ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--max-line-length=120"))

(with-eval-after-load 'flycheck
	(add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup))

(setq flycheck-pycheckers-max-line-length 120)

(require 'virtualenvwrapper)
;; (venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(venv-workon "emacs")

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(eval-after-load "python"
	'(define-key python-mode-map "\C-cx" 'jedi-direx:pop-to-buffer))
(add-hook 'jedi-mode-hook 'jedi-direx:setup)

(defun python-interactive ()
	"Enter the interactive Python environment"
	(interactive)
	(progn
		(insert "!import code; code.interact(local=vars())")
		(move-wend-of-line 1)
		(comint-send-input)))

 (global-set-key (kbd "C-c i") 'python-interactive)

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
(global-set-key (kbd "C-c k") 'sp-kill-sexp)
(global-set-key [f6] 'call-last-kbd-macro)

(global-set-key "\M-n" 'next5)
(global-set-key "\M-p" 'prev5)
(global-set-key "\M-o" 'other-window)
(global-set-key "\M-i" 'back-window)
(global-set-key "\C-z" 'zap-to-char)
(defalias 'rr 'replace-regexp)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
