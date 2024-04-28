(let ((file-name-handler-alist nil)) ;; to improve startup time

(eval-when-compile
  (require 'cl))
(setq gc-cons-threshold 100000000)

(require 'package)
(setq package-archives '(
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(load-file "/home/michal/.emacs.d/my-elisp/my-latex-mode.el")
(load-file "/home/michal/.emacs.d/my-elisp/my-random-dashboard-image.el")
(load-file "/home/michal/.emacs.d/my-elisp/my-windows.el")
(load-file "/home/michal/.emacs.d/my-elisp/my-utils.el")
(load-file "/home/michal/.emacs.d/my-elisp/char-summary.el")
(load-file "/home/michal/.emacs.d/my-elisp/fixes.el")
(load-file "/home/michal/.emacs.d/my-elisp/my-hooks.el")
(load-file "/home/michal/.emacs.d/my-elisp/dashboard-fix.el")

(setq inhibit-startup-message t)
(scroll-bar-mode 0);
(tool-bar-mode 0)
(tooltip-mode 0);
(set-fringe-mode 0);
(menu-bar-mode 0)
(setq visible-bell nil)
(global-visual-line-mode -1)

(set-face-attribute 'variable-pitch nil
                    :font "Iosevka Aile"
                    :height 120)

(set-face-attribute 'default nil
                    :font "Source Code Pro"
                    :height 120)

(set-face-attribute 'fixed-pitch nil
                    :font "Source Code Pro"
                    :height 120)

(use-package diminish)

(use-package swiper
  :ensure t)

(use-package ivy
  :diminish
  :bind (
         ("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-previous-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :config
  ;; Remove the '^' at the beginning of counsel commands
  (setq ivy-initial-inputs-alist nil))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-h") 'backward-char)
(global-set-key (kbd "C-j") 'next-line)
(global-set-key (kbd "C-k") 'previous-line)
(global-set-key (kbd "C-l") 'forward-char)
(global-set-key (kbd "C-M-h") 'left-word)
(global-set-key (kbd "C-M-j") (lambda () (interactive) (next-line 4)))
(global-set-key (kbd "C-M-k") (lambda () (interactive) (previous-line 4)))
(global-set-key (kbd "C-M-l") 'right-word)
(global-set-key (kbd "C-m") 'back-to-indentation)
(global-set-key (kbd "RET") 'newline)

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)
(global-set-key (kbd "C-c c") 'counsel-compile)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c L") 'counsel-git-log)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-c m") 'counsel-linux-app)
(global-set-key (kbd "C-c f") 'counsel-fzf)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c J") 'counsel-file-jump)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c d") 'counsel-descbinds)
(global-set-key (kbd "C-c o") 'counsel-outline)
(global-set-key (kbd "C-c t") 'counsel-load-theme)
(global-set-key (kbd "C-c F") 'counsel-org-file)

(global-set-key (kbd "C-c w") 'toggle-truncate-lines)

;;(global-set-key (kbd "C-n") 'electric-newline-and-maybe-indent)
(global-set-key (kbd "C-f") 'kill-line)
(global-set-key (kbd "C-p") 'help-command)
(global-set-key (kbd "C-b") 'recenter-top-bottom)
(global-set-key (kbd "C-M-o") 'counsel-switch-buffer)

(global-set-key (kbd "C-t") 'goto-line-preview)

(global-set-key (kbd "M-<up>") 'move-dup-move-lines-up)
(global-set-key (kbd "M-<down>") 'move-dup-move-lines-down)
(global-set-key (kbd "C-M-<up>") 'move-dup-duplicate-up)
(global-set-key (kbd "C-M-<down>") 'move-dup-duplicate-down)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-.") 'mc/mark-next-like-this)
(global-set-key (kbd "C-,") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-,") 'mc/mark-all-like-this)

(global-set-key (kbd "C-c a") 'org-agenda)

;;(global-unset-key (kbd "<right>"))
;;(global-unset-key (kbd "<left>"))
;;(global-unset-key (kbd "<up>"))
;;(global-unset-key (kbd "<down>"))

(global-set-key (kbd "C-x K") 'mm/kill-everything)
(global-set-key (kbd "M-RET") 'mm/split-window-horizontally-and-focus-vterm)
(global-set-key (kbd "C-x 2") 'mm/split-window-vertically-and-focus)
(global-set-key (kbd "C-x 3") 'mm/split-window-horizontally-and-focus)
(global-set-key (kbd "C-r") 'mm/go-to-saved-point)
(global-set-key (kbd "C-M-s") 'mm/save-point)
(global-set-key (kbd "C-`") 'mm/toggle-vterm-below)

(define-key emacs-lisp-mode-map (kbd "C-x M-e") 'eval-buffer)

(use-package tree-sitter
  :ensure t)

(use-package tree-sitter-langs
  :defer t
  :ensure t
  :config
  (tree-sitter-require 'tsx)
  (global-tree-sitter-mode)
  (add-to-list 'treesit-language-source-alist
        '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))
  (add-to-list 'tree-sitter-major-mode-language-alist '(jtsx-jsx-mode . tsx))
  (add-to-list 'tree-sitter-major-mode-language-alist '(jtsx-tsx-mode . tsx)))
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; Snippets of code (all 3 need to be installed with package-install RET package-name RET)
(use-package yasnippet
  :ensure t
  :defer t
  :config
  (yas-global-mode)
  (use-package yasnippet-snippets
    :ensure t)
  (yas-reload-all))

;; To add ts snippets jtsx modes create a .yas-parents file in snippets directory
;; in .emacs.d directory and write 'typescript-mode'
(use-package yatemplate
  :defer t
  :ensure t)

(use-package multiple-cursors
  :ensure t)

(use-package toml-mode
  :ensure t)

(use-package beacon
  :ensure t
  :config
  (beacon-mode nil))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode 1))

(use-package yafolding
  :ensure t)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (when (file-directory-p "~/Programming")
    (setq projectile-project-search-path '("~/Programming")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;; Syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
;; Increase the amount of data which Emacs reads from the process.
;; Default value is causing a slowdown, it's too low to handle server responses.
(setq read-process-output-max (*(* 1024 1024) 3)) ;; 3Mib
(setq lsp-headerline-breadcrumb-enable nil)
(setq flycheck-clang-include-path '("/home/michal/Programming/PubHub/pubhub-server/include"))

(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))

;; Enable flycheck globally
(add-hook 'after-init-hook #'global-flycheck-mode)


(use-package treemacs
  :defer t
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

(use-package tex
  :ensure auctex)

(use-package pdf-tools
  :load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

(use-package move-dup
  :ensure t)

(use-package goto-line-preview
  :ensure t)

(use-package ess
  :ensure t)

(use-package avy
  :bind
  ("M-s" . avy-goto-char-2))

(use-package ace-window
  :ensure t
  :bind
  ("M-o" . ace-window))

(use-package restclient
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode)))

(use-package compat
  :ensure t)

(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-max-scrollback 10000)
  (add-hook 'term-exec-hook
            (function
             (lambda ()
               (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-varialbe)
  ([remap describe-key] . helpful-key))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (
           (doom-modeline-height 30)
           (doom-modeline-indent-info nil)
           (doom-modeline-time t)
           (doom-modeline-battery t)
           (doom-modeline-time t)
           (doom-modeline-env-version nil)
           (doom-modeline-buffer-encoding nil)
           (doom-modeline-buffer-file-name-style 'truncate-up-to-project)
           (display-battery-mode 1)))

(use-package dired
  :ensure nil
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-single
  :ensure t)

(setf dired-kill-when-opening-new-dired-buffer t)

(use-package doom-themes
  :ensure t)
(setq doom-themes-enable-bold t)
(setq doom-themes-enable-italic t)

(load-theme 'doom-spacegrey t)

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode
  :ensure t)

;; Refresh a file edited outside of emacs
(global-auto-revert-mode 1)

;; Improve jumping between words in pascalCase
(global-subword-mode 1)

;; Auto close (), "", {}
(electric-pair-mode 1)
(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\{ . ?\})))

(column-number-mode)
(global-display-line-numbers-mode)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable line numbers in some scenarios
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                treemacs-mode-hook
                shell-mode-hook
                vterm-mode-hook
                rustic-cargo-run-mode-hook
                rustic-cargo-test-mode-hook
                eww-mode-hook
                ))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq-default truncate-lines t)
(delete-selection-mode 1)
(setq subword-mode 1)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )

(setq-default indent-tabs-mode nil)
(setq ivy-extra-directories nil)

(pixel-scroll-precision-mode 1)

(use-package dashboard
  :ensure t
  :init
  (progn
    (setq dashboard-center-content t)
    (setq dashboard-banner-logo-title "There is no system but GNU, and Linux is one of its kernels.")
    (setq dashboard-set-file-icons t)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-footer nil)
    (setq dashboard-agenda-sort-strategy '(time-up))
    ;;(setq dashboard-startup-banner (mm/random-dashboard-image-path))
    (setq dashboard-startup-banner 'official)
    )
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  (setq dashboard-items '(
                          (recents  . 3)
                          (projects . 3)
                          (agenda . 4)
                          (bookmarks . 3)
                          )))
;;(setq dashboard-startup-banner (mm/random-dashboard-image-path)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . my-lsp-mode-hook)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  (define-key lsp-mode-map (kbd "C-c l = =") 'mm/match-lsp-formatting)
  ;; Increase the amount of data which Emacs reads from the process.
  ;; Default value is causing a slowdown, it's too low to handle server responses. 3mb
  (setq read-process-output-max (*(* 1024 1024) 3)))

(setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
(setq lsp-headerline-breadcrumb-enable nil)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'bottom))

(use-package quelpa
  :ensure t)
(use-package quelpa-use-package
  :ensure t)

(use-package copilot
  ;; :quelpa (copilot :fetcher github
  ;;                  :repo "copilot-emacs/copilot.el"
  ;;                  :branch "main"
  ;;                  :files ("dist" "*.el"))
  :ensure t
  :config
  ;; https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/
  (defvar rk/no-copilot-modes '(shell-mode
                                dashboard-mode
                                inferior-python-mode
                                eshell-mode
                                term-mode
                                vterm-mode
                                comint-mode
                                compilation-mode
                                debugger-mode
                                dired-mode-hook
                                compilation-mode-hook
                                flutter-mode-hook
                                minibuffer-mode-hook)
    "Modes in which copilot is inconvenient.")
  (defun rk/copilot-disable-predicate ()
    "When copilot should not automatically show completions."
    (member major-mode rk/no-copilot-modes))

  (add-to-list 'copilot-disable-predicates #'rk/copilot-disable-predicate)
  )

;;(add-hook 'lsp-mode-hook 'copilot-mode)
(global-set-key (kbd "C-M-=") 'copilot-next-completion)
(global-set-key (kbd "C-M--") 'copilot-previous-completion)
(global-set-key (kbd "C-M-SPC") 'copilot-accept-completion)

;; Completions and how to make them pretty
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common)
        )
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))
(setq company-tooltip-maximum-width 60)
(setq company-tooltip-margin 3)

;; Prettier completions
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))
(setq company-box-doc-enable t)

(use-package undo-tree
  :ensure t
  :config
  ;; Prevent undo tree files from polluting your git repo
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

;;emmet mode
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indent-after-insert nil)))
  (setq emmet-move-cursor-between-quotes t) ;; default nil
  (add-to-list 'emmet-jsx-major-modes 'jtsx-jsx-mode)
  (add-to-list 'emmet-jsx-major-modes 'jtsx-tsx-mode))

(with-eval-after-load "emmet-mode"
  (define-key emmet-mode-keymap (kbd "C-j") nil))

;; LSP mode for HTML
(use-package mhtml-mode
  :mode "\\.html\\'"
  :config
  (add-hook 'mhtml-mode-hook 'lsp))

;; LSP mode for CSS
(use-package css-mode
  :mode "\\..?css\\'"
  :config
  (add-hook 'css-mode-hook 'lsp)
  (setq css-indent-offset 2))

(use-package rustic
  :ensure t
  :hook (rustic-mode . lsp-deferred)
  :hook (rustic-mode . tree-sitter-hl-mode)
  :config
  (require 'lsp-rust)
  (setq lsp-rust-analyzer-completion-add-call-parenthesis t)
  (setq rust-indent-method-chain t))

(use-package flycheck-rust
  :ensure t)

(use-package prettier-js
  :ensure t)

;; (defun check-tsx ()
;;   "Check if we should switch from typescript-mode to typescript-tsx-mode."
;;   (when (not (eq major-mode 'typescript-tsx-mode))
;;     (when (string-match "\\.[jt]sx\\'" (buffer-file-name (current-buffer)))
;;       (progn
;;         (typescript-tsx-mode)
;;         (message "Toggling TSX mode")))))


;;LSP mode for Typescript
(use-package typescript-mode
  :mode "\\.[jt]s\\'"
  ;;:after (tree-sitter)
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook 'lsp-deferred)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  ;;(add-hook 'find-file-hook (lambda () (treesit-parser-create 'typescript)))
  )

;; ;; tailwind lsp working with jtsx mode 
;; (use-package lsp-tailwindcss
;;   :ensure t
;;   :init
;;   (setq lsp-tailwindcss-add-on-mode t)
;;   :config
;;   (add-to-list 'lsp-tailwindcss-major-modes 'jtsx-jsx-mode)
;;   (add-to-list 'lsp-tailwindcss-major-modes 'jtsx-tsx-mode))
;; (add-hook 'before-save-hook 'lsp-tailwindcss-rustywind-before-save)

;; requires emmet mode to work correctly
(use-package jtsx
  :ensure t
  :mode (("\\.jsx\\'" . jtsx-jsx-mode)
         ("\\.tsx\\'" . jtsx-tsx-mode))
  :commands jtsx-install-treesit-language
  ;; :hook ((jtsx-jsx-mode . hs-minor-mode)
  ;;        (jtsx-tsx-mode . hs-minor-mode))
  :custom
  ;; Optional customizations
  (js-indent-level 2)
  (typescript-ts-mode-indent-offset 2)
  (jtsx-switch-indent-offset 0)
  ;; (jtsx-indent-statement-block-regarding-standalone-parent nil)
  ;; (jtsx-jsx-element-move-allow-step-out t)
  (jtsx-enable-jsx-electric-closing-element t)
  (jtsx-enable-electric-open-newline-between-jsx-element-tags t)
  (jtsx-enable-jsx-element-tags-auto-sync nil)
  (jtsx-enable-all-syntax-highlighting-features t)
  :config
  (defun jtsx-bind-keys-to-mode-map (mode-map)
    "Bind keys to MODE-MAP."
    (define-key mode-map (kbd "C-c C-j") 'jtsx-jump-jsx-element-tag-dwim)
    (define-key mode-map (kbd "C-c C-a") 'jtsx-jump-jsx-opening-tag)
    (define-key mode-map (kbd "C-c C-s") 'jtsx-jump-jsx-closing-tag)
    (define-key mode-map (kbd "C-c C-r") 'jtsx-rename-jsx-element)
    (define-key mode-map (kbd "C-c <down>") 'jtsx-move-jsx-element-tag-forward)
    (define-key mode-map (kbd "C-c <up>") 'jtsx-move-jsx-element-tag-backward)
    (define-key mode-map (kbd "C-c C-<down>") 'jtsx-move-jsx-element-forward)
    (define-key mode-map (kbd "C-c C-<up>") 'jtsx-move-jsx-element-backward)
    (define-key mode-map (kbd "C-c C-S-<down>") 'jtsx-move-jsx-element-step-in-forward)
    (define-key mode-map (kbd "C-c C-S-<up>") 'jtsx-move-jsx-element-step-in-backward)
    (define-key mode-map (kbd "C-c  C-w") 'jtsx-wrap-in-jsx-element)
    (define-key mode-map (kbd "C-c  C-u") 'jtsx-unwrap-jsx)
    (define-key mode-map (kbd "C-c  C-d") 'jtsx-delete-jsx-node))

  (defun jtsx-bind-keys-to-jtsx-jsx-mode-map ()
    (jtsx-bind-keys-to-mode-map jtsx-jsx-mode-map))

  (defun jtsx-bind-keys-to-jtsx-tsx-mode-map ()
    (jtsx-bind-keys-to-mode-map jtsx-tsx-mode-map))

  (add-hook 'jtsx-jsx-mode-hook 'jtsx-bind-keys-to-jtsx-jsx-mode-map)
  (add-hook 'jtsx-jsx-mode-hook 'lsp)
  (add-hook 'jtsx-jsx-mode-hook 'tree-sitter-mode)
  (add-hook 'jtsx-jsx-mode-hook 'prettier-js-mode)
  (add-hook 'jtsx-jsx-mode-hook 'emmet-mode)

  (add-hook 'jtsx-tsx-mode-hook 'jtsx-bind-keys-to-jtsx-tsx-mode-map)
  (add-hook 'jtsx-tsx-mode-hook 'lsp)
  (add-hook 'jtsx-tsx-mode-hook 'tree-sitter-mode)
  (add-hook 'jtsx-tsx-mode-hook 'prettier-js-mode)
  (add-hook 'jtsx-tsx-mode-hook 'emmet-mode))

;; ;; define a custom mode that we'll toggle when needed
;; (define-derived-mode typescript-tsx-mode typescript-mode "TSX")
;; ;; use our derived mode for [jt]sx files
;; (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . typescript-tsx-mode))

;; ;;by default, typescript-mode is mapped to the treesitter typescript parser
;; ;;use our derived mode to map both .tsx AND .ts to typescript-tsx-mode to treesitter tsx
;; (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))

;; (add-hook 'typescript-mode-hook 'lsp-deferred)
;; (add-hook 'typescript-mode-hook 'check-tsx)
;; (add-hook 'typescript-mode-hook 'prettier-js-mode))

;; (use-package tsi
;;   :after tree-sitter
;;   :quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
;;   ;; define autoload definitions which when actually invoked will cause package to be loaded
;;   :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
;;   :init
;;   (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
;;   (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
;;   (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
;;   (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(use-package clang-format
  :ensure t
  :custom
  (clang-format-fallback-style "WebKit"))

(use-package lsp-java
  :ensure t
  :hook (java-mode-hook . lsp-mode))

(add-hook 'LaTeX-mode-hook 'my-LaTeX-mode-hook)

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;;(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(use-package go-mode
  :ensure t)
(add-hook 'go-mode-hook 'lsp)
(add-hook 'go-mode-hook (lambda () (setq tab-width 4)))

(setq inferior-lisp-program "/usr/bin/sbcl")
(use-package slime
  :defer t
  :init
  (load (expand-file-name "~/quicklisp/slime-helper.el")))

(defun mm/org-mode-setup ()
  (setq org-startup-indented t)
  (org-indent-mode)
  (variable-pitch-mode 1) ;;enable a non-monospace font
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (local-set-key (kbd "C-j") nil))

(use-package org
  :ensure t
  :hook (org-mode . mm/org-mode-setup)
  :config
  (setq org-ellipsis " ⏷"
        org-hide-emphasis-markers nil))

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(require 'org-indent)

(set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)

(with-eval-after-load 'org-faces
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.05)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil
                        :font "Iosevka Aile"
                        :height (cdr face))
    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
    (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
    (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
    ))

(defun mm/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :ensure t
  :hook (org-mode . mm/org-mode-visual-fill))

(add-hook 'org-mode-hook
          (lambda () ))

(with-eval-after-load 'org-mode-map (define-key org-mode-map (kbd "C-j") nil))

(setq agenda-dirs '("~/Documents/Notes/Semester-6" "~/Documents/org" "~/Programming"))
(setq org-agenda-files (-flatten-n 1 (mapcar (lambda (dir) (directory-files-recursively dir "\\.org$" nil nil t)) agenda-dirs)))

(setq org-agenda-start-with-log-mode nil)
;;(setq org-log-done 'time)
;;(setq org-log-into-drawer t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")))

(setq org-tag-alist
      '((:startgroup)
        ;; Put mutually exclusive tags here
        (:endgroup)
        ("@home" . ?H)
        ("@work" . ?W)
        ("@put" . ?p)
        ("note" . ?n)
        ("idea" . ?i)))

(shell-command "/usr/bin/xmodmap /home/michal/.Xmodmap")

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands
        '(self-insert-command
          lsp-ui-doc--handle-mouse-movement
          mwheel-scroll
          )))

(use-package esup
  :defer t
  :ensure t
  :config
  (setq esup-depth 0))

)
