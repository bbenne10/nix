(eval-when-compile (require 'use-package))
(defvar bb-font-family "Share Tech Mono")
(defvar bb-font-size 140)
(defvar bb-default-leader-key "<f13>")

(set-face-attribute 'default nil
    :family bb-font-family
    :height bb-font-size
    :width 'normal
    :weight 'normal)

(set-face-attribute 'line-number-current-line nil
    :family bb-font-family
    :height bb-font-size
    :width 'expanded
    :weight 'normal
    :inverse-video nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq
  ; disable customizations
  custom-file null-device
  ; disable the scratch message
  initial-scratch-message ""
  ; disable the startup screen
  inhibit-startup-message t
  ; move window one line at a time when point approaches edge
  scroll-conservatively 101
  ; start scrolling 5 lines from edge
  scroll-margin 5
  ; Audible bell is cancer, but visible bell works okay
  visible-bell t
  ; Tell emacs we're okay with functions being given advice
  ad-redefinition-action 'accept
  ; Follow symlinks to vcs controlled files
  vc-follow-symlinks t
  ; copy actions copy to clipboard
  select-enable-clipboard t
  ; copy actions also copy to primary
  select-enable-primary t
  ; highlighting a section causes it to get copied (linux default behavior)
  mouse-drag-copy-region t
  ; unprettify symbols when the point hits them so we can edit them
  prettify-symbols-unprettify-at-point t
  ; Move backups to a temporary dir
  backup-directory-alist `((".*" . ,temporary-file-directory))
  ; Move auto saves to a temporary dir
  auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq-default
  ; in fill-mode, what column do we wrap at?
  fill-column 80
  ; disable line wrapping
  truncate-lines t
  ; use spaces over tabs everywhere
  indent-tabs-mode nil
  ; but when encountering a tab, how large is it?
  tab-width 2
  ; and what are the tabstop points when shifting?
  tab-stop-list (number-sequence 3 120 2))

; Stop killing windows by fatfingering this
(global-unset-key (kbd "C-x C-c"))

; Don't make me type out "yes" or "no" - even if it is important
(defalias 'yes-or-no-p 'y-or-n-p)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :custom (exec-path-from-shell-check-startup-files nil
           ;; Launch a login shell; Nix is good at putting stuff in the right place
           exec-path-from-shell-arguments nil
           exec-path-from-shell-variables '("PATH" "MANPATH" "SSL_CERT_FILE")
           ;; Use Nix path for locally managed zsh install so we get correct...everything
           exec-path-from-shell-shell-name (concat (getenv "HOME") "/.nix-profile/bin/zsh"))
  :config (exec-path-from-shell-initialize))

(use-package doom-themes
    :config
      (load-theme 'doom-nord t)
      (doom-themes-visual-bell-config)
      (doom-themes-org-config))

(use-package doom-modeline
    :init (setq doom-modeline-env-version nil)
    :config (doom-modeline-mode))

(use-package undo-tree
  :config (global-undo-tree-mode))

(use-package evil
    :after (undo-tree)
    :init (setq evil-undo-system 'undo-tree
                evil-want-keybinding nil
                evil-want-integration t)
    :config
      (evil-mode 1))

(use-package evil-leader
    :after (evil)
    :config
      (evil-leader/set-leader bb-default-leader-key)
      (global-evil-leader-mode))

(use-package evil-collection
    :after (evil)
    :config (evil-collection-init '(magit magit-todos)))

(use-package evil-matchit
    :after evil
    :config (global-evil-matchit-mode 1))

(use-package evil-surround
    :after evil
    :config (global-evil-surround-mode 1))

(use-package ivy
  :config (ivy-mode))

(use-package counsel
  :config (counsel-mode))

(use-package company
  :delight company-mode
  :custom (company-tooltip-limit 20
           company-tooltip-align-annotations t)
  :config
    (global-company-mode 1))

(use-package origami
  :hook (prog-mode . origami-mode))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :custom (git-gutter:update-interval 2))

(use-package magit
    :commands (magit-status)
    :hook (after-save . magit-after-save-refresh-status)
    :defer 5
    :custom (magit-popup-show-common-commands nil
             magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))
    :init (evil-leader/set-key "g" 'magit)

(use-package rainbow-delimiters
    :hook (prog . rainbow-delimiters))

(add-hook 'prog-mode-hook (function(lambda ()
    (prettify-symbols-mode 1)      ; show ligatures
    (show-paren-mode 1)            ; highlight matching brackets
    (global-hl-line-mode 1)        ; highlight the active line
    (display-line-numbers-mode)))) ; Show line numbers

(use-package ws-butler
  :config (ws-butler-global-mode))

(use-package direnv
  :after lsp
  :config (direnv-mode))

(use-package nix-mode)

(use-package nix-sandbox)

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package perspective
  :config (persp-mode))

(use-package persp-projectile
  :commands (projectile-persp-switch-project)
  :init (evil-leader/set-key "p" 'projectile-persp-switch-project))

(use-package projectile
  :delight projectile-mode
  :commands (projectile-switch-project projectile-find-file projectile-mode)
  :after evil-leader
  :custom (projectile-require-project-root nil
           projectile-git-command "fd . --print0 --color never"
           projectile-indexing-method 'alien
           projectile-project-search-path '("~/code"))
  :config (projectile-mode))

(use-package counsel-projectile
  :after projectile
  :custom (counsel-projectile-rg-initial-input '(ivy-thing-at-point)
           projectile-completion-system 'ivy)
  :config
  (evil-define-key 'normal 'global (kbd "<leader>b") 'counsel-projectile)
  (evil-leader/set-key
    "b" 'counsel-projectile
    evil-leader/leader 'counsel-projectile
    "/" 'counsel-projectile-rg))

(use-package fic-mode
    :commands (fic-mode)
    :init (setq fic-highlighted-words '("FIXME" "TODO" "BUG" "NOTE"))
    :hook (prog-mode . fic-mode))

(use-package rainbow-mode)

(use-package flycheck
  :custom (flycheck-highlighting-mode nil
           flycheck-indication-mode 'left-margin)
  :config
    (global-flycheck-mode)
    (add-to-list 'display-buffer-alist
      `(,(rx bos "*Flycheck errors*" eos)
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . bottom)
         (resulable-frames . visible)
         (window-height . 15)))
   :hook (flycheck-mode . flycheck-set-indication-mode))

(use-package which-key
  :config (which-key-mode 1))

(use-package lsp-mode
  :config
  (defun bb/lsp-setup()
    (let ((current (nix-current-sandbox)))
      (when current (setq lsp-pyls-server-command (nix-executable-find (nix-current-sandbox) "pyls"))))

    (setq
      lsp-idle-delay 0.5
      lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
      lsp-pyls-configuration-sources ["flake8"]
      ;; enable things we want!
      lsp-pyls-plugins-flake8-enabled t
      lsp-pyls-plugins-pydocstyle-enabled t
      ;; and now disable stuff we don't want
      lsp-pyls-plugins-pycodestyle-enabled nil
      lsp-pyls-plugins-mccabe-enabled nil
      lsp-pyls-plugins-pyflakes-enabled nil
      lsp-pyls-plugins-autopep8-enabled nil
      ;; and now just set a few variables to better defaults
      lsp-pyls-plugins-flake8-max-line-length 88)

    (lsp-register-custom-settings
     '(("pyls.plugins.pyls_black.enabled" t t)
       ("pyls.plugins.pyls_isort.enabled" t t))))
  :hook
  ((python-mode . lsp)
   (reason-mode . lsp)
   (web-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)
   (lsp-before-initialize . bb/lsp-setup)))

(use-package lsp-ui
  :config
  (defun bb/lsp-ui-setup ()
    (lsp-headerline-breadcrumb-mode 1)
    (setq lsp-ui-sideline-enable t
          lsp-ui-sideline-delay 0.5
          lsp-ui-sideline-ignore-duplicate t
          lsp-ui-doc-delay 1
          lsp-eldoc-enable-hover t
          lsp-signature-doc-lines 2
          lsp-signature-auto-activate t
          lsp-ui-doc-position 'top
          lsp-ui-doc-alignment 'window))
  :commands lsp-ui-mode
  :hook ((lsp-before-initialize . bb/lsp-ui-setup))
  :bind (:map evil-normal-state-map
          ("gd" . lsp-ui-peek-find-definitions)
          ("gr" . lsp-ui-peek-find-references)
          ("C-E" . flycheck-list-errors)
          ("C-e" . flycheck-next-error)))

(use-package web-mode
  :mode (".jsx?$" ".html$" ".css$")
  :custom (web-mode-markup-indent-offset 2
           web-mode-code-indent-offset 2
           web-mode-css-indent-offset 2
           web-mode-attr-indent-offset 2
           web-mode-enable-css-colorization t
           web-mode-enable-current-column-highlight t
           web-mode-enable-auto-quoting nil))

(use-package reason-mode
  :mode ("\\.rei?'")
  :init (setq refmt-command 'opam))

(use-package rust-mode
  :mode ("\\.rs'"))

(use-package tuareg
  :mode ("\\.mli?'"))

(use-package merlin
  :mode ("\\.mli?'"))

(use-package yaml-mode
  :mode ("\\.yaml'" "\\.yml'"))

(use-package markdown-mode
  :config (setq markdown-command "pandoc")
  :mode (("\\.md'" . gfm-mode)))

(use-package lua-mode
  :mode (".lua$"))

(use-package vterm)

(use-package zig-mode
  :mode ("\\.zig\\'"))
