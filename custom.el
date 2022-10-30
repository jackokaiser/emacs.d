(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(conda-anaconda-home "~/miniconda3/")
 '(format-all-default-formatters
   '(("BibTeX" emacs-bibtex)
     ("C" clang-format)
     ("C++" clang-format)
     ("CMake" cmake-format)
     ("CSS" prettier)
     ("Dockerfile" dockfmt)
     ("Emacs Lisp" emacs-lisp)
     ("HTML" html-tidy)
     ("JavaScript" prettier)
     ("JSON" prettier)
     ("JSON5" prettier)
     ("LaTeX" latexindent)
     ("Less" prettier)
     ("Markdown" prettier)
     ("Python"
      (yapf "--style={\"based_on_style: pep8, split_before_named_assigns: False, column_limit: 120, dedent_closing_brackets: False, join_multiple_lines: True, indent_width: 4\"}"))
     ("SCSS" prettier)
     ("Shell" shfmt)
     ("Terraform" terraform-fmt)
     ("TOML" prettier)
     ("TypeScript" prettier)
     ("Vue" prettier)
     ("XML" html-tidy)
     ("YAML" prettier)
     ("_Nginx" nginxfmt)))
 '(package-selected-packages
   '(auctex json-mode eglot yasnippet-snippets yasnippet-classic-snippets yaml-mode web-mode virtualenvwrapper virtualenv typescript-mode pyenv-mode py-yapf py-autopep8 ox-mediawiki markdown-preview-eww markdown-mode magit jedi-direx idle-highlight-mode haskell-mode flycheck-pycheckers dockerfile-mode conda company cmake-mode browse-kill-ring)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
