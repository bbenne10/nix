;;; package --- Summary
;;; Commentary:
;;; Code:
;; -*- lexical-binding: t -*-

(eval-when-compile (require 'use-package))
(defvar bb-font-family "ShureTechMono Nerd Font")
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

(use-package general)

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
      (load-theme 'doom-gruvbox t)
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
                evil-echo-state nil
                evil-want-integration t)
    :config (evil-mode 1))

(use-package evil-collection
    :after (evil)
    :config (evil-collection-init '(magit magit-todos consult)))

(use-package evil-matchit
    :after evil
    :config (global-evil-matchit-mode 1))

(use-package evil-surround
    :after evil
    :config (global-evil-surround-mode 1))

(use-package corfu
  :custom (corfu-auto t)
  :init (corfu-global-mode))

(use-package magit
    :commands (magit-status)
    :hook (after-save . magit-after-save-refresh-status)
    :general (:prefix bb-default-leader-key
              "g" 'magit)
    :custom (magit-popup-show-common-commands nil
             magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode))

(add-hook 'prog-mode-hook (function(lambda ()
    (prettify-symbols-mode 1)      ; show ligatures
    (show-paren-mode 1)            ; highlight matching brackets
    (global-hl-line-mode 1)        ; highlight the active line
    (display-line-numbers-mode)    ; Show line numbers
    (hs-minor-mode))))             ; Add hide-show

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

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
  :general (:prefix bb-default-leader-key
            "p" 'projectile-persp-switch-project))

(use-package projectile
  :delight projectile-mode
  :general (:prefix bb-default-leader-key
            "b" 'find-file
            bb-default-leader-key 'projectile-find-file
            "/" 'consult-ripgrep)
  :custom (projectile-require-project-root nil
           projectile-git-command "fd . --print0 --color never"
           projectile-indexing-method 'alien
           projectile-project-search-path '("~/code")
           consult-project-root-function #'projectile-project-root)
  :config (projectile-mode))

(use-package fic-mode
    :commands (fic-mode)
    :init (setq fic-highlighted-words '("FIXME" "TODO" "BUG" "NOTE"))
    :hook (prog-mode . fic-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

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
  :general (:states 'normal
    "gd" 'lsp-ui-peek-find-definitions
    "gr" 'lsp-ui-peek-find-references
    "C-E" 'flycheck-list-errors
    "C-e" 'flycheck-next-error))

(use-package consult-lsp
  :config
    (consult-lsp-marginalia-mode)
    (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(use-package consult
  :custom (register-preview-delay 0
           register-preview-function #'consult-register-format
           xref-show-xrefs-function #'consult-xref
           xref-show-definitions-function #'consult-xref
           consult-project-function #'projectile-project-root)
  :init
    (advice-add #'register-preview :override #'consult-register-preview)
    (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-.")))

(use-package vertico
   :init (vertico-mode))

(use-package orderless
   :custom (completion-styles '(orderless)))

(use-package marginalia
  :general (:map minibuffer-local-map "M-A" 'marginalia-cycle)
  :init (marginalia-mode))


(use-package multi-vterm
  ;; Stolen directly from https://github.com/suonlight/multi-vterm
  :config
    (add-hook 'vterm-mode-hook
              (lambda ()
                (setq-local evil-insert-state-cursor 'box)
                (evil-insert-state)))
    (define-key vterm-mode-map [return] #'vterm-send-return)
    (setq vterm-keymap-exceptions nil)

    (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
    (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
    (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)

    ;; (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
    ;; (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
    ;; (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)

    (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
    (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
    (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume))

(use-package rust-mode
  :mode ("\\.rs'"))

(use-package yaml-mode
  :mode ("\\.yaml'" "\\.yml'"))

(use-package markdown-mode
  :config (setq markdown-command "pandoc")
  :mode (("\\.md'" . gfm-mode)))

(use-package lua-mode
  :mode (".lua$"))

(use-package vterm)
 (use-package zig-mode :mode ("\\.zig\\'"))
