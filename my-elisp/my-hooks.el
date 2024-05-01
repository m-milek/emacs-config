(defun my-lsp-mode-hook ()
  (message "EXECUTING MY LSP HOOK")
  (tree-sitter-hl-mode)
  (yas-minor-mode-on)
  (yafolding-mode)
  (copilot-mode)
  (lsp-ui-doc-mode)
  )

(defun my-c++-mode-hook ()
  (lsp)
  (setq c++-basic-offset 4)
  (setq c-basic-offset 4)
  (local-unset-key (kbd "C-M-h")))

(defun my-c-mode-hook ()
  (lsp)
  (setq c-basic-offset 4)
  (setq c++-basic-offset 4)
  (local-unset-key (kbd "C-M-h")))

(defun my-LaTeX-mode-hook ()
  (lsp)
  (local-set-key (kbd "C-c C-. M-c") 'mm/latex-compile)
  (local-set-key (kbd "C-c C-. M-v") 'mm/latex-compile-and-view)
  (lambda () (local-unset-key (kbd "C-j")))
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

