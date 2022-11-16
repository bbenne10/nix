;;; package --- Summary
;;; Commentary:
;;; Code:
;; -*- lexical-binding: t -*-

(eval-when-compile (require 'use-package))
(defvar bb-default-leader-key "<f13>")

(set-face-attribute 'default nil :family "ShureTechMono Nerd Font")
(set-face-attribute 'line-number-current-line nil :family "ShureTechMono Nerd Font")

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(use-package general)

(use-package emacs
  :custom (custom-file null-device "disable customizations")
          (initial-scratch-message "" "disable the scratch message")
          (inhibit-startup-message t "disable the startup screen")
          (scroll-conservatively 101 "move window one line at a time when point approaches edge")
          (scroll-margin 5 "start scrolling 5 lines from edge")
          (visible-bell t "Audible bell is cancer, but visible bell works okay")
          (ad-redefinition-action 'accept "Tell emacs we're okay with functions being given advice")
          (vc-follow-symlinks t "Follow symlinks to vcs controlled files")
          (select-enable-clipboard t "copy actions copy to clipboard")
          (select-enable-primary t "copy actions also copy to primary")
          (mouse-drag-copy-region t "highlighting a section causes it to get copied (linux default behavior)")
          (prettify-symbols-unprettify-at-point t "unprettify symbols when the point hits them so we can edit them")
          (backup-directory-alist `((".*" . ,temporary-file-directory)) "Move backups to a temporary dir")
          (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)) "Move auto saves to a temporary dir")
          (read-process-output-max (* 1024 1024) "Up amount of output we can read from a subprocess buffer; should improve LSP")
          (gc-cons-threshold 100000000 "Increase garbage collection-threshold")

  :init
    (defun bb-prog-mode-setup ()
      (prettify-symbols-mode 1)      ; show ligatures
      (show-paren-mode 1)            ; highlight matching brackets
      (global-hl-line-mode 1)        ; highlight the active line
      (display-line-numbers-mode)    ; Show line numbers
      (hs-minor-mode))               ; Add hide-show

    (defun bb-after-init-hook ()
      (defalias 'yes-or-no-p 'y-or-n-p)  ; I don't ever want to type out "yes" or "no" - even if it is important
      (global-unset-key (kbd "C-x C-c")) ; Stop killing windows by fatfingering this bind.
      (global-unset-key (kbd "C-h h"))   ; I have *never* wanted to see the hello file.

      (setq-default
        fill-column 80                            ; in fill-mode, what column do we wrap at?
        truncate-lines t                          ; disable line wrapping
        indent-tabs-mode nil                      ; use spaces over tabs everywhere
        tab-width 2                               ; but when encountering a tab, how large is it?
        tab-stop-list (number-sequence 3 120 2))) ; and what are the tabstop points when shifting?

    :general (
       :prefix bb-default-leader-key
       "p" #'tabspaces-open-or-create-project-and-workspace
       "b" #'find-file
       bb-default-leader-key #'project-find-file)

  :hook ((prog-mode . bb-prog-mode-setup)
         (after-init . bb-after-init-hook)))

(use-package textsize
  :custom (textsize-default-points 18)
  :config (textsize-mode))

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
  :defer nil
  :init
    (defun bb-toggle-theme ()
      (interactive)

      (if (eq (car custom-enabled-themes) 'doom-earl-grey)
          (disable-theme 'doom-earl-grey)
        (load-theme 'doom-earl-grey t)))
    :config
      (load-theme 'doom-nord t)
      (doom-themes-visual-bell-config)
      (doom-themes-org-config)
  :general ("<f5>" 'bb-toggle-theme))

(use-package doom-modeline
  :after (evil)
  :custom (doom-modeline-env-version nil)
          (doom-modeline-modal-icon nil)
          (evil-normal-state-tag (propertize "⬤" 'face '((:background 'doom-modeline-info))))
          (evil-insert-state-tag (propertize "⬤" 'face '((:background 'doom-modeline-urgent))))
          (evil-motion-state-tag (propertize "⬤" 'face '((:background 'doom-modeline-buffer-path))))
          (evil-operator-state-tag (propertize "⬤" 'face '((:background 'doom-modeline-buffer-path))))
          (evil-visual-state-tag (propertize "⬤" 'face '((:background 'doom-modeline-buffer-file))))
          (evil-emacs-state-tag (propertize "⬤" 'face '((:background 'doom-modeline-warning))))
    :config (doom-modeline-mode))

(use-package evil
    :init (setq evil-undo-system 'undo-redo
                evil-want-keybinding nil
                evil-echo-state nil
                evil-want-integration t)
    :config (evil-mode 1))

(use-package evil-collection
    :after (evil)
    :config (evil-collection-init '(magit magit-todos consult)))

(use-package evil-commentary
  :after (evil)
  :config (evil-commentary-mode))

(use-package evil-matchit
    :after evil
    :config (global-evil-matchit-mode 1))

(use-package evil-surround
    :after evil
    :config (global-evil-surround-mode 1))

(use-package eldoc-box
  :hook (prog-mode . eldoc-box-hover-mode))

(use-package corfu
  :custom (corfu-auto t)
  :init (global-corfu-mode))

(use-package magit
    :hook (after-save . magit-after-save-refresh-status)
    :general (:prefix bb-default-leader-key
              "g" 'magit)
    :custom (magit-popup-show-common-commands nil
             magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
             git-commit-fill-column 72
             git-commit-summary-max-length 50))

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

(use-package fic-mode
    :commands (fic-mode)
    :init (setq fic-highlighted-words '("FIXME" "TODO" "BUG" "NOTE" "XXX"))
    :hook (prog-mode . fic-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; (use-package flycheck
;;   :custom (flycheck-highlighting-mode nil
;;            flycheck-indication-mode 'left-margin)
;;   :config
;;     (global-flycheck-mode)
;;     (add-to-list 'display-buffer-alist
;;       `(,(rx bos "*Flycheck errors*" eos)
;;          (display-buffer-reuse-window display-buffer-in-side-window)
;;          (side . bottom)
;;          (resulable-frames . visible)
;;          (window-height . 15)))
;;    :hook (flycheck-mode . flycheck-set-indication-mode))

(use-package which-key
  :config (which-key-mode 1))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

;; (use-package lsp-mode
;;   :config
;;   (defun bb/lsp-setup()
;;     (setq lsp-idle-delay 0.5)
;;           ;; lsp-disabled-clients (js-mode . (list eslist)))
;;
;;     (setq lsp-pyls-configuration-sources ["flake8"]
;;           lsp-pyls-plugins-autopep8-enabled nil
;;           lsp-pyls-plugins-flake8-max-line-length 88
;;           lsp-pyls-plugins-mccabe-enabled nil
;;           lsp-pyls-plugins-pycodestyle-enabled nil
;;           lsp-pyls-plugins-pydocstyle-enabled t
;;           lsp-pyls-plugins-pyflakes-enabled nil
;;           lsp-pyls-plugins-flake8-enabled t)
;;
;;     (lsp-register-custom-settings
;;      '(("pyls.plugins.pyls_black.enabled" t t)
;;        ("pyls.plugins.pyls_isort.enabled" t t))))
;;
;;   :hook ((python-mode . lsp)
;;          (reason-mode . lsp)
;;          (rust-mode . lsp)
;;          (js-mode . lsp)
;;          (lsp-mode . lsp-enable-which-key-integration)
;;          (lsp-before-initialize . bb/lsp-setup)))
;;
;; (use-package lsp-treemacs
;;   :config (lsp-treemacs-sync-mode 1))
;;
;; (use-package lsp-ui
;;   :after (tree-sitter)
;;   :config
;;   (defun bb/lsp-ui-setup ()
;;     (lsp-headerline-breadcrumb-mode 1)
;;     (setq lsp-ui-sideline-enable t
;;           lsp-ui-sideline-delay 0.5
;;           lsp-ui-sideline-ignore-duplicate t
;;           lsp-ui-doc-delay 1
;;           lsp-eldoc-enable-hover t
;;           lsp-signature-doc-lines 2
;;           lsp-signature-auto-activate t
;;           lsp-ui-doc-position 'top
;;           lsp-ui-doc-alignment 'window))
;;   :commands lsp-ui-mode
;;   :hook ((lsp-before-initialize . bb/lsp-ui-setup))
;;   :general (:states 'normal
;;     "gd" 'lsp-ui-peek-find-definitions
;;     "gr" 'lsp-ui-peek-find-references
;;     "C-E" 'flycheck-list-errors
;;     "C-e" 'flycheck-next-error))
;;
;; (use-package consult-lsp
;;   :config
;;     (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (js-mode . eglot-ensure))
  :custom (eglot-extend-to-xref t)
  :general (:states 'normal
		    :prefix bb-default-leader-key
                    "gd" #'xref-find-definitions
                    "gr" #'xref-find-references
                    "la" #'eglot-code-actions
                    "ll" #'eglot
                    "lq" #'eglot-shutdown
                    "lQ" #'eglot-reconnect
                    "lr" #'eglot-rename
                    "l=" #'eglot-format))

(use-package flymake-diagnostic-at-point
  :hook (flymake-mode . #'flymake-diagnostic-at-point-mode))

(use-package consult
  :general (:prefix bb-default-leader-key
            "/" 'consult-ripgrep)
  :custom (register-preview-delay 0
           register-preview-function #'consult-register-format
           xref-show-xrefs-function #'consult-xref
           xref-show-definitions-function #'consult-xref)

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

(use-package consult-project-extra)

(use-package project
  :config
    (cl-defmethod project-root ((project (head local))) (cdr project))
    (defun bb-project-find (dir)
    ;; https://michael.stapelberg.ch/posts/2021-04-02-emacs-project-override/
      (let ((local (locate-dominating-file dir ".project-root")))
        (if local
            (cons 'local local)
          nil)))

    ;; Can't use :hook as 'project-find-functions doesn't end in "-hook"
    (add-hook 'project-find-functions #'bb-project-find -90))

(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :custom (tabspaces-use-filtered-buffers-as-default t)
          (tab-bar-show nil)
  :commands (tabspaces-create-workspace
             tabspaces-create-new-project-and-workspace
             tabspaces-open-existing-project-and-workspace
             tabspaces-switch-workspace)
  :general (
    :prefix bb-default-leader-key
    "p" #'tabspaces-open-or-create-project-and-workspace))

(use-package vertico
   :init (vertico-mode))

(use-package orderless
  :custom (completion-styles '(orderless basic))
          (completion-category-defaults nil)
          (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :general (:map minibuffer-local-map "M-A" 'marginalia-cycle)
  :init (marginalia-mode))

(use-package vterm
  :custom (vterm-shell (concat (getenv "HOME") "/.nix-profile/bin/zsh")))

(use-package multi-vterm
  ;; Stolen and modified from https://github.com/suonlight/multi-vterm
  :general (:prefix bb-default-leader-key "!" #'multi-vterm-project)

           (:keymaps 'vterm-mode-map
            "<return>" #'vterm-send-return)

           (:keymaps 'vterm-mode-map
            :states 'insert
            "C-e"      #'vterm--self-insert
            "C-f"      #'vterm--self-insert
            "C-a"      #'vterm--self-insert
            "C-v"      #'vterm--self-insert
            "C-b"      #'vterm--self-insert
            "C-w"      #'vterm--self-insert
            "C-u"      #'vterm--self-insert
            "C-d"      #'vterm--self-insert
            "C-n"      #'vterm--self-insert
            "C-m"      #'vterm--self-insert
            "C-p"      #'vterm--self-insert
            "C-j"      #'vterm--self-insert
            "C-k"      #'vterm--self-insert
            "C-r"      #'vterm--self-insert
            "C-t"      #'vterm--self-insert
            "C-g"      #'vterm--self-insert
            "C-c"      #'vterm--self-insert
            "C-SPC"    #'vterm--self-insert
            "C-d"      #'vterm--self-insert)

           (:keymaps 'vterm-mode-map
            :states 'normal
            "i" #'evil-insert-resume
            "o" #'evil-insert-resume
            "<return>" #'evil-insert-resume)

  :custom (vterm-keymap-exceptions nil)

  :init (defun bb-vterm-mode-setup ()
          (setq-local evil-insert-state-cursor 'box)
          (evil-insert-state))

  :hook (vterm-mode . bb-vterm-mode-setup))

;; languages
(use-package markdown-mode
  :config (setq markdown-command "pandoc")
  :mode (("\\.md'" . gfm-mode)))

(use-package python-mode
  :hook (python-mode . (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t))))

(use-package rust-mode :mode ("\\.rs'")
  :hook (rust-mode . (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t))))

(use-package yaml-mode :mode ("\\.yaml'" "\\.yml'"))

;;; emacs.el ends here
