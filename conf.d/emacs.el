;;; package --- Summary
;;; Commentary:
;;; Code:
;; -*- lexical-binding: t -*-

(eval-when-compile (require 'use-package))
(defvar bb-default-leader-key "<f13>")

(set-face-attribute 'default nil
                    :family "Recursive Mono Linear Static"
                    :weight 'light)
(set-face-attribute 'line-number-current-line nil
                    :family "Recursive Mono Linear Static"
                    :weight 'light)

(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0) 
        (vertical-scroll-bars . nil)))

;; CSD is the devil...
(when (eq window-system 'pgtk)
  (add-to-list 'default-frame-alist '(undecorated . t)))

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
         (after-init . bb-after-init-hook)
         (before-save . 'whitespace-cleanup)))

(use-package hydra)

(use-package use-package-hydra)

(use-package textsize
  :custom (textsize-default-points (if (eq system-type 'darwin) 18 12))
  :config (textsize-mode)
  :general ("C-x t" 'bb-hydra-textsize/body)
  :hydra (bb-hydra-textsize (:exit nil :foreign-keys warn :hint nil)
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
          ("r" textsize-reset)
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
    :custom (evil-undo-system 'undo-redo)
            (evil-want-keybinding nil)
            (evil-echo-state nil)
            (evil-want-integration t)
    :config (evil-mode 1))

(use-package evil-collection
    :after (evil)
    :config (evil-collection-init '(magit magit-todos consult)))

(use-package eldoc-box
  :hook (prog-mode . eldoc-box-hover-mode))

(use-package corfu
  :custom (corfu-auto t)
  :init (global-corfu-mode))

(use-package magit
    :hook (after-save . magit-after-save-refresh-status)
    :general (:prefix bb-default-leader-key
              "g" 'magit)
    :custom (magit-popup-show-common-commands nil)
            (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
            (git-commit-fill-column 72)
            (git-commit-summary-max-length 50))

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :custom (git-gutter:update-interval 2))

(use-package git-gutter-fringe
  :config
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
  :config (setcdr (assq 'java-mode eglot-server-programs) '("jdt-language-server"))
  :hydra (bb-hydra-eglot (:exit t :foreign-keys warn :hint nil)
                         "
┌──────────────────────┐┌───────────────┐┌─────────────┐┌───────────────────┐
│ Find                 ││ Edit          ││ Format      ││ Manage            │
│──────────────────────││───────────────││─────────────││───────────────────│
│ [_d_]: Declaration     ││ [_r_]: Rename   ││ [_=_]: Buffer ││ [_X_]: Shutdown     │
│ [_i_]: Implementation  ││ [_a_]: Actions  ││ [_R_]: Region ││ [_C_]: Reconnect    │
│ [_D_]: Type definition ││               ││             ││ [_E_]: Event Buffer │
└──────────────────────┘└───────────────┘└─────────────┘└───────────────────┘
  [_X_]: Shutdown  [_C_]: Re-connect [_E_]: Display Events Buffer
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
              ("E" eglot-events-buffer))
  :general (:prefix bb-default-leader-key
            "/" 'consult-ripgrep
            "e" 'bb-hydra-eglot/body)
           ("<M-RET>" #'eglot-code-actions)
  :hook ((python-mode . eglot-ensure)
         (java-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (nix-mode . eglot-ensure)))

(use-package sideline
  :hook (flymake-mode . sideline-mode))

(use-package sideline-flymake
  :custom (sideline-flymake-display-errors-whole-line 'line)
          (sideline-backends-right '((sideline-flymake . up))))

(use-package consult
  :general (:prefix bb-default-leader-key
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
     :preview-key (kbd "M-.")))

(use-package consult-project-extra)

(use-package project
  :defer nil
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
  :hydra (bb-hydra-project (:exit t :foreign-keys warn :hint nil)
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
    (cl-defmethod project-root ((project (head local))) (cdr project))
    (defun bb-project-find (dir)
    ;; https://michael.stapelberg.ch/posts/2021-04-02-emacs-project-override/
      (let ((local (locate-dominating-file dir ".project-root")))
        (if local
            (cons 'local local)
          nil)))

    ;; Can't use :hook as 'project-find-functions doesn't end in "-hook"
    (add-hook 'project-find-functions #'bb-project-find -90)

     :general (:prefix bb-default-leader-key "P" 'bb-hydra-project/body))

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
  :custom (markdown-command "pandoc")
  :mode (("\\.md'" . gfm-mode)))

(use-package nix-mode
  :hook (nix-mode . (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t))))

(use-package python
  ;; set ensure nil to use packaged version of python.el
  ;; rather than grabbing from elpa
  :ensure nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook (python-mode . (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t))))

(use-package rust-mode :mode ("\\.rs'")
  :hook (rust-mode . (lambda () (add-hook 'before-save-hook 'eglot-format-buffer nil t))))

(use-package yaml-mode :mode ("\\.yaml'" "\\.yml'"))

(use-package web-mode)

;;; emacs.el ends here
