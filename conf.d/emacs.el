;; -*- lexical-binding: t -*-

;;; package --- Summary
;;; Commentary:
;;; Code:

(eval-when-compile (require 'use-package))

(set-face-attribute 'default nil
                    :family "Rec Mono Semicasual"
                    :weight 'light)
(set-face-attribute 'line-number-current-line nil
                    :family "Rec Mono Semicasual"
                    :weight 'light)

(defvar my/leader "<f13>")

(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0) 
        (vertical-scroll-bars . nil)))

;; Note: These are provided via nix in here
;; Themes come from https://github.com/bbenne10/emacs_themes
(use-package bennett-themes)

(use-package ef-themes)

(use-package general)

(use-package hydra)

(use-package use-package-hydra)

(use-package emacs
  :ensure nil
  :custom (custom-file null-device)
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
          (backup-directory-alist `((".*" . ,temporary-file-directory)))
          (auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
          (read-process-output-max (* 1024 1024))
          (gc-cons-threshold 100000000)

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
      (show-paren-mode 1)
      (defalias 'yes-or-no-p 'y-or-n-p)

      (setq-default
        truncate-lines t
        indent-tabs-mode nil))

    (when (eq window-system 'pgtk)
      (setq my/leader "<MenuKB>")
      (unless (string-equal "gnome" (getenv "DESKTOP_SESSION"))
        ;; CSD is the devil, but is necessary under gnome
        (add-to-list 'default-frame-alist '(undecorated . t))))

    :general (
       :prefix my/leader
       "b" #'find-file
       my/leader #'project-find-file)

  :hook ((prog-mode . my/prog-mode-setup)
         (after-init . my/after-init-hook)
         (before-save . 'whitespace-cleanup)))

(use-package auto-dark
  :custom
    (auto-dark-dark-theme 'ef-elea-dark)
    (auto-dark-light-theme 'ef-elea-light)
  :init (auto-dark-mode))

(use-package textsize
  :custom (textsize-default-points (if (eq system-type 'darwin) 18 12))
  :config (textsize-mode)
  :general (:prefix my/leader
                    "t" 'my/hydra-textsize/body)
           ("C-x t" nil)
  :hydra (my/hydra-textsize (:exit nil :foreign-keys warn :hint nil)
                         "
┌───────────────┐
│ Text Size     │
│───────────────│
│ [_+_]: Increase │
│ [_-_]: Decrease │
│ [_r_]: Reset    │
│ [_x_]: Exit     │
└───────────────┘
"
          ("+" textsize-increment )
          ("-" textsize-decrement)
          ("r" textsize-reset :color blue)
          ("x" nil))
  )

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :custom (exec-path-from-shell-check-startup-files nil)
          ;; Launch a login shell; Nix is good at putting stuff in the right place
          (exec-path-from-shell-arguments nil)
          (exec-path-from-shell-variables '("PATH" "MANPATH" "SSL_CERT_FILE"))
          ;; Use Nix path for locally managed zsh install so we get correct...everything
          (exec-path-from-shell-shell-name (concat (getenv "HOME") "/.nix-profile/bin/zsh"))
  :config (exec-path-from-shell-initialize))

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
    :custom (evil-undo-system 'undo-redo)
            (evil-want-keybinding nil)
            (evil-echo-state nil)
            (evil-want-integration t)
    :config (evil-mode 1))

(use-package evil-collection
    :after (evil)
    :config (evil-collection-init '(consult magit magit-todos notmuch)))

(use-package eldoc-box
  :hook (prog-mode . eldoc-box-hover-mode))

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

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :custom (git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  ;; https://ianyepan.github.io/posts/emacs-git-gutter/
    (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package direnv
  :config (direnv-mode))

(use-package fic-mode
    :custom (fic-highlighted-words '("FIXME" "TODO" "BUG" "NOTE" "XXX"))
    :hook (prog-mode . fic-mode))

(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package which-key
  :config (which-key-mode 1))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(use-package eglot
  :custom (eglot-extend-to-xref t)
  :config
    (setcdr (assq 'java-mode eglot-server-programs) '("jdt-language-server"))
    (add-to-list 'eglot-server-programs '(java-ts-mode . ("jdt-language-server")))
    ;; TODO: use `eglot-alternatives` here to add new stuff when I need it?
    (add-to-list 'eglot-server-programs '(js-ts-mode . ("flow" "lsp")))
    :hydra (my/hydra-eglot (:exit t :foreign-keys warn :hint nil)
			   "
┌──────────────────────┐┌───────────────┐┌─────────────┐┌───────────────────┐
│ Find                 ││ Edit          ││ Format      ││ Manage            │
│──────────────────────││───────────────││─────────────││───────────────────│
│ [_d_]: Declaration     ││ [_r_]: Rename   ││ [_=_]: Buffer ││ [_X_]: Shutdown     │
│ [_i_]: Implementation  ││ [_a_]: Actions  ││ [_R_]: Region ││ [_C_]: Reconnect    │
│ [_D_]: Type definition ││               ││             ││ [_E_]: Event Buffer │
│ [_n_]: Next error      ││               ││             ││                   │
│ [_p_]: Previous error  ││               ││             ││                   │
└──────────────────────┘└───────────────┘└─────────────┘└───────────────────┘
  [_X_]: Shutdown  [_C_]: Re-connect [_E_]: Display Events Buffer [_<escape>_]: Exit
"
              ("d" eglot-find-declaration)
              ("i" eglot-find-implementation)
              ("D" eglot-find-typeDefinition)
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
  :hook ((python-ts-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (java-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (nix-mode . eglot-ensure)))

(use-package sideline
  :hook (flymake-mode . sideline-mode))

(use-package sideline-flymake
  :custom (sideline-flymake-display-errors-whole-line 'line)
          (sideline-backends-right '((sideline-flymake . up))))

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

(use-package project
  :defer nil
  :custom (project-vc-extra-root-markers '(".project"))
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
       ("p" tabspaces-open-or-create-project-and-workspace))
  :config
     :general (:prefix my/leader "P" 'my/hydra-project/body))

(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :custom (tabspaces-use-filtered-buffers-as-default t)
          (tab-bar-show nil)
  :general (
    :prefix my/leader
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

(use-package notmuch
  :custom
    (notmuch-saved-searches
     '((:name "Inbox" :query "tag:inbox" :key "i")
       (:name "Unread" :query "tag:inbox and tag:unread" :key "u")
       (:name "Archive" :query "tag:archive" :key "a")
       (:name "Trash" :query "tag:deleted" :key "t"))))

(use-package emms
  :custom (emms-player-mpv-parameters
           '("--quiet"
             "--really-quiet"
             "--no-audio-display"
             "--no-video"
             "--no-audio-display"
             "--force-window=no"
             "--vo=null"))
  :config
    (emms-standard)
    (emms-default-players))

;; languages
(use-package markdown-mode
  :custom (markdown-command "pandoc")
  :mode (("\\.md'" . gfm-mode)))

(use-package nix-mode
  :hook (nix-mode . (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t))))

(use-package python
  ;; set ensure nil to use bundled version of python.el
  ;; rather than grabbing from elpa
  :ensure nil
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode)
  :hook (python-ts-mode . (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t))))

(use-package rust-mode :mode ("\\.rs'")
  :hook (rust-mode . (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t))))

(use-package yaml-mode :mode ("\\.yaml'" "\\.yml'"))
;;; emacs.el ends here
