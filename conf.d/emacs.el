;; -*- lexical-binding: t -*-

;;; package --- Summary
;;; Commentary:
;;; Code:

(set-face-attribute 'default nil
                    :family "ShureTechMono Nerd Font"
                    :weight 'regular)
(set-face-attribute 'line-number-current-line nil
                    :family "ShureTechMono Nerd Font"
                    :weight 'light)

(defconst my/leader (if (eq system-type 'darwin) "<f13>" "<Tools>"))

;; Note: These are provided via nix in here
;; Themes come from https://github.com/bbenne10/emacs_themes
(use-package bennett-themes)

(use-package general)

(use-package hydra)

(use-package use-package-hydra)

(use-package emacs
  :ensure nil
  :custom (default-frame-alist
	   '((menu-bar-lines . 0)
	     (tool-bar-lines . 0)
	     (vertical-scroll-bars . nil)))
             (initial-scratch-message "")
             (inhibit-startup-message t)
             (scroll-conservatively 101)
             (scroll-margin 5)
             (visible-bell t)
             (ad-redefinition-action 'accept)
             (vc-follow-symlinks t)
             (select-enable-clipboard t)
             (select-enable-primary t)
             (mouse-drag-copy-region t)
             (prettify-symbols-unprettify-at-point t)

  :init
    (defun my/prog-mode-setup ()
      (display-line-numbers-mode)
      (column-number-mode 1)
      (prettify-symbols-mode 1)
      (hs-minor-mode))

    (defun my/after-init-hook ()
      (global-unset-key (kbd "C-x C-c"))
      (global-unset-key (kbd "C-h h"))
      (global-hl-line-mode 1)
      (pixel-scroll-precision-mode 1)
      (show-paren-mode 1)
      (load-theme 'bb-everforest-hard-dark t)
      (defalias 'yes-or-no-p 'y-or-n-p)

      (setq-default
        truncate-lines t
        indent-tabs-mode nil))

    (when (eq window-system 'pgtk)
      (add-to-list 'default-frame-alist '(undecorated . t)))

    :general (
       :prefix my/leader
       "b" #'find-file
       my/leader #'consult-project-extra-find)

    :hook ((prog-mode . my/prog-mode-setup)
           (after-init . my/after-init-hook)
           (before-save . 'whitespace-cleanup)))

(use-package exec-path-from-shell
  :custom (exec-path-from-shell-variables '("PATH" "SSH_AUTH_SOCK"))
          (exec-path-from-shell-shell-name "zsh")
          (exec-path-from-shell-arguments nil)
  :config (exec-path-from-shell-initialize))

(use-package textsize
   :custom (textsize-default-points (if (eq system-type 'darwin) 18 12))
   :commands textsize-mode
   :init (textsize-mode)
   :general (:prefix my/leader "t" 'my/hydra-textsize/body)
            ("C-x t" nil)
   :hydra (my/hydra-textsize (:exit nil :foreign-keys warn )
                          "
 ┌───────────────┐
 │ Text Size     │
 │───────────────│
 │ [_+_]: Increase │
 │ [_-_]: Decrease │
 │ [_r_]: Reset    │
 └───────────────┘
 "
           ("+" textsize-increment )
           ("-" textsize-decrement)
           ("r" textsize-reset :color blue)
           ("<escape>" nil)))

(use-package which-key
  :config (which-key-mode 1))

(use-package evil
    :custom (evil-undo-system 'undo-redo)
            (evil-want-keybinding nil)
            (evil-echo-state nil)
            (evil-want-integration t)
    :config (evil-mode 1))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(use-package evil-collection
    :after (evil)
    :config (evil-collection-init '(consult dired magit magit-todos notmuch)))

(use-package eldoc-box
  :hook (prog-mode . eldoc-box-hover-mode))

(use-package feline
  :config
  (defun my-feline-evil nil
    (when (boundp 'evil-state)
      (let ((mode-cons (alist-get evil-state feline-evil-state-alist)))
        (propertize (car mode-cons) 'face (cdr mode-cons)))))
  (setq ;; not done in :custom because this is not defcustom'd in feline
   feline-evil-state-alist
   '((normal . ("󰰒" . font-lock-variable-name-face))
     (insert . ("󰰃" . font-lock-string-face))
     (visual . ("󰰪" . font-lock-keyword-face))
     (replace . ("󰰞" . font-lock-type-face))
     (motion . ("󰰏" . font-lock-constant-face))
     (operator . ("󰰕". font-lock-function-name-face))
     (emacs . ("" . font-lock-builtin-face))))

  (setq-default
   mode-line-format
   '(""
     (:eval (my-feline-evil))
     "  "
     (:eval (feline-major-mode))
     " "
     (:eval (feline-buffer-id (format-mode-line "%b")))
     (:eval (propertize (if (buffer-modified-p) " ± " " ") 'face 'feline-buffer-id-face))
     mode-line-format-right-align
     (:eval (feline-project-name))
     " "
     (:eval (feline-positions))
     " "
     mode-line-misc-info))
  :custom-face (feline-position-prefix-face ((t (:inherit font-lock-comment :slant normal))))
               (mode-line-active ((t (:inherit mode-line-inactive :foreground "#d3c6aa" :overline "#A7C080"))))
               (mode-line-inactive ((t (:inherit mode-line :overline nil))))
  :custom
    (feline-line-prefix "⇅")
    (feline-column-prefix ":")
    (feline-mode-symbols
     '(emacs-lisp-mode "λ"
       lisp-interaction-mode "λ"
       python-mode ""
       python-ts-mode ""
       typescript-ts-mode "󰛦"
       nix-mode "󱄅"
       rust-mode ""
       )))


(use-package corfu
  :custom (corfu-auto t)
  :init (global-corfu-mode))

(use-package magit
    :hook (after-save . magit-after-save-refresh-status)
    :general (:prefix my/leader
              "g" 'magit)
    :custom (magit-popup-show-common-commands nil)
            (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
            (git-commit-fill-column 72)
            (git-commit-summary-max-length 50))

(use-package direnv
  :config (direnv-mode))

(use-package fic-mode
    :custom (fic-highlighted-words '("FIXME" "TODO" "BUG" "NOTE" "XXX"))
    :hook (prog-mode . fic-mode))

;; Visual flymake integration
(use-package sideline
  :hook (flymake-mode . sideline-mode))

(use-package sideline-flymake
  :custom (sideline-flymake-display-errors-whole-line 'line)
          (sideline-backends-right '((sideline-flymake . up))))

;; Language Server Support / Treesitter
(use-package eglot
  :custom (eglot-extend-to-xref t)
  :hydra my/hydra-eglot (:exit t :foreign-keys warn :hint nil)
			   "
┌──────────────────────┐┌───────────────┐┌─────────────┐┌───────────────────┐
│ Find                 ││ Edit          ││ Format      ││ Manage            │
│──────────────────────││───────────────││─────────────││───────────────────│
│ [_d_]: Declaration     ││ [_r_]: Rename   ││ [_=_]: Buffer ││ [_X_]: Shutdown     │
│ [_i_]: Implementation  ││ [_a_]: Actions  ││ [_R_]: Region ││ [_C_]: Reconnect    │
│ [_D_]: Type definition ││               ││             ││ [_E_]: Event Buffer │
│ [_u_]: Uses            ││               ││             ││                   │
│ [_n_]: Next error      ││               ││             ││                   │
│ [_p_]: Previous error  ││               ││             ││                   │
└──────────────────────┘└───────────────┘└─────────────┘└───────────────────┘
  [_X_]: Shutdown  [_C_]: Re-connect [_E_]: Display Events Buffer [_<escape>_]: Exit
"
              ("d" eglot-find-declaration)
              ("i" eglot-find-implementation)
              ("D" eglot-find-typeDefinition)
              ("u" xref-find-references)
              ("r" eglot-rename)
              ("a" eglot-code-actions)
              ("=" eglot-format-buffer)
              ("R" eglot-format)
              ("X" eglot-shutdown)
              ("C" eglot-reconnect)
              ("E" eglot-events-buffer)
              ("n" flymake-goto-next-error :color amaranth)
              ("p" flymake-goto-prev-error :color amaranth)
              ("<escape>" nil)
  :general (:prefix my/leader "e" 'my/hydra-eglot/body)
           ("<M-RET>" #'eglot-code-actions)
  :config
           (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
           (add-to-list 'eglot-server-programs
             '((rust-ts-mode rust-mode) .
               ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))

  :hook ((python-ts-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (java-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (nix-mode . eglot-ensure)))

(use-package treesit-auto
  :config
    (treesit-auto-add-to-auto-mode-alist 'all)
    (global-treesit-auto-mode))

;; Project perspectives
(use-package project
  :defer nil
  :custom (project-vc-extra-root-markers '(".projectel"))
          (project-vc-ignores '("node_modules"))
  :init
    ;; I stole the transient from https://github.com/jojojames/matcha/blob/master/matcha-project.el
    ;; And then made it a hydra
    (defun project-recentf ()
      "Show a list of recently visited files in a project."
      (interactive)
      (if (boundp 'recentf-list)
          (let* ((recent-project-files (project-recentf-files))
                 (completion-ignore-case read-file-name-completion-ignore-case)
                 (file (funcall project-read-file-name-function
                                "Find recent file" recent-project-files nil nil
                                (thing-at-point 'filename))))
            (if (string= file "")
                (user-error "You didn't specify the file")
              (find-file file)))
        (message "recentf is not enabled")))
    (defun project-recentf-files ()
      "Return a list of recently visited files in a project."
      (and (boundp 'recentf-list)
           (let* ((pr (project-current t))
                  (project-root (expand-file-name (project-root pr))))
             (cl-remove-if-not
              (lambda (f)
                (string-prefix-p project-root (expand-file-name f)))
              recentf-list))))

    (defun project-multi-occur (&optional nlines)
      "Do a `multi-occur' in the project's buffers.
       With a prefix argument, show NLINES of context."
      (interactive "P")
      (let ((pr (project-current t)))
        (multi-occur (project--buffer-list pr)
                     (car (occur-read-primary-args))
                     nlines)))

  :hydra (my/hydra-project (:exit t :foreign-keys warn :hint nil)
                           "
┌────────────────────┐┌─────────────┐┌────────────────────┐┌──────────────────────┐┌────────────────────┐
│ Find               ││ Buffers     ││ Actions            ││ Modes                ││ Search             │
│────────────────────││─────────────││────────────────────││──────────────────────││────────────────────│
│ [_f_]: File          ││ [_b_]: Buffer ││ [_R_]: Replace       ││ [_g_]: Version Control ││ [_\/_]: Find Regexp   │
│ [_F_]: File (or Ext) ││ [_K_]: Kill   ││ [_m_]: Compile       ││ [_h_]: Dired           ││ [_s_]: Multi-Occur   │
│ [_r_]: Recent File   ││             ││                    ││ [_t_]: Term            ││ [_p_]: Switch Proj   │
└────────────────────┘└─────────────┘└────────────────────┘└──────────────────────┘└────────────────────┘
"
       ("f" project-find-file)
       ("F" project-or-external-find-file)
       ("r" project-recentf)
       ("b" project-switch-to-buffer)
       ("K" project-kill-buffers)
       ("R" project-query-replace-regexp)
       ("m" project-compile)
       ("c" project-async-shell-command)
       ("C" project-shell-command)
       ("g" project-vc-dir)
       ("h" project-dired)
       ("t" multi-vterm-project)
       ("\/" project-find-regexp)
       ("A" project-or-external-find-regexp)
       ("s" project-multi-occur)
       ("p" tabspaces-open-or-create-project-and-workspace)
       ("<escape>" nil))
  :config
     :general (:prefix my/leader "P" 'my/hydra-project/body))

(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :custom (tabspaces-use-filtered-buffers-as-default t)
          (tab-bar-show nil)
  :general (
    :prefix my/leader
    "p" #'tabspaces-open-or-create-project-and-workspace))

;; completion / minibuffer
(use-package consult
  :general (:prefix my/leader
            "/" 'consult-ripgrep)
  :custom (register-preview-delay 0)
          (register-preview-function #'consult-register-format)
          (xref-show-xrefs-function #'consult-xref)
          (xref-show-definitions-function #'consult-xref)
  :init
    (advice-add #'register-preview :override #'consult-register-preview)
  :config
    (consult-customize
     consult-theme
     :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
     :preview-key "M-."))

(use-package consult-project-extra)

(use-package vertico
   :init (vertico-mode))

(use-package orderless
  :custom (completion-styles '(orderless basic))
          (completion-category-defaults nil)
          (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :general (:map minibuffer-local-map "M-A" 'marginalia-cycle)
  :init (marginalia-mode))

;; terminal-in-emacs
(use-package vterm
  :custom (vterm-shell (concat (getenv "HOME") "/.nix-profile/bin/zsh")))

(use-package multi-vterm
  ;; Stolen and modified from https://github.com/suonlight/multi-vterm
  :general (:prefix my/leader "!" #'multi-vterm-project)

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

  :init (defun my/vterm-mode-setup ()
          (setq-local evil-insert-state-cursor 'box)
          (evil-insert-state))

  :hook (vterm-mode . my/vterm-mode-setup))

;; languages
(use-package reason-mode)

(use-package markdown-mode
  :custom (markdown-command "pandoc")
  :mode (("\\.md'" . gfm-mode)))

(defun my-before-save-format-buffer ()
  "Call \"eglot-format-buffer\" before save."
  (add-hook 'before-save-hook 'eglot-format-buffer nil t))

(use-package nix-mode
  :hook (nix-mode . my-before-save-format-buffer))

(use-package python
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode)
  :hook ((python-mode . my-before-save-format-buffer)
         (python-ts-mode . my-before-save-format-buffer)))

(use-package rust-mode :mode ("\\.rs'")
  :hook ((rust-mode . my-before-save-format-buffer)
         (rust-ts-mode . my-before-save-format-buffer)))

(use-package yaml-mode :mode ("\\.yaml'" "\\.yml'"))
;;; emacs.el ends here
