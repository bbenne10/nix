{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    alacritty
    curl
    exa
    fd
    git
    htop
    iosevka
    mpv
    neovim
    pass
    ripgrep
    weechat
    zsh

    # ((import (pkgs.fetchFromGitHub {
    #       owner = "numtide";
    #       repo = "devshell";
    #       rev = "17a8c6b64127a19b5136f9ce4dad17ca2934f4df";
    #       sha256 = "17a8c6b64127a19b5136f9ce4dad17ca2934f4df";
    #     }).devshell))
  ];

  programs.home-manager = {
    enable = true;
  };

  programs.alacritty = {
    enable = true;
    settings = {
      window = {
        dimensions = {
          columns = 0;
          lines = 0;
        };
        padding = {
          x = 12;
          y = 12;
        };
        dynamic_padding = false;
      };
      scrolling = {
        history = 10000;
        multiplier = 3;
      };
      font = {
        normal = { family = "JetBrains Mono"; };
        bold = { family = "JetBrains Mono"; };
        italic = { family = "JetBrains Mono"; };
        size = 10.0;
        offset = {
          x = 0;
          y = 0;
        };
        glyph_offset = {
          x = 0;
          y = 0;
        };
      };
      draw_bold_text_with_bright_colors = false;
      colors = {
        primary = {
          background = "0x2E3440";
          foreground = "0xD8DEE9";
        };
        cursor = {
          text = "0x2E3440";
          cursor = "0xD8DEE9";
        };
        normal = {
          black = "0x3B4252";
          red = "0xBF616A";
          green = "0xA3BE8C";
          yellow = "0xEBCB8B";
          blue = "0x81A1C1";
          magenta = "0xB48EAD";
          cyan = "0x88C0D0";
          white = "0xE5E9F0";
        };
        bright = {
          black = "0x4C566A";
          red = "0xBF616A";
          green = "0xA3BE8C";
          yellow = "0xEBCB8B";
          blue = "0x81A1C1";
          magenta = "0xB48EAD";
          cyan = "0x8FBCBB";
          white = "0xECEFF4";
        };
        # indexed_colors = []
      };
      bell = {
        animation = "EaseOutExpo";
        color = "0xffffff";
        duration = 0;
      };
      background_opacity = 0.75;
      shell = {
        program = "${pkgs.zsh}/bin/zsh";
      };
    };
  };

  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    enableNixDirenvIntegration = true;
    stdlib = ''
      : ''${XDG_CACHE_HOME:=$HOME/.cache}
      declare -A direnv_layout_dirs
      direnv_layout_dir() {
        echo "''${direnv_layout_dirs[$PWD]:=$(
          echo -n "$XDG_CACHE_HOME"/direnv/layouts/
          echo -n "$PWD" | shasum | cut -d ' ' -f 1
        )}"a
      }
    '';
  };

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [
      company
      counsel
      counsel-projectile
      direnv
      doom-modeline
      doom-themes
      editorconfig
      evil
      evil-leader
      evil-magit
      evil-matchit
      evil-surround
      exec-path-from-shell
      fic-mode
      flycheck
      git-gutter
      ivy
      jdee
      lsp-java
      lsp-mode
      lsp-ui
      lua-mode
      magit
      markdown-mode
      merlin
      origami
      persp-projectile
      perspective
      projectile
      rainbow-delimiters
      rainbow-mode
      reason-mode
      tuareg
      undo-tree
      use-package
      web-mode
      which-key
      ws-butler
      yaml-mode
    ];
  };

  xdg.configFile."emacs/init.el".text = ''
    (eval-when-compile (require 'use-package))
    (defvar bb-font-family "JetBrains Mono")
    (defvar bb-font-size 120)
    (defvar bb-default-leader-key "<f13>")
    (if (string= (system-name) "tia-bxb-d01.ctisl.gtri.org") (setq bb-default-leader-key "<XF86TouchpadOff>"))

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
      :init (setq exec-path-from-shell-check-startup-files nil
                  ;; Launch a login shell; Nix is good at putting stuff in the right place
                  exec-path-from-shell-arguments nil
                  exec-path-from-shell-variables '("PATH" "MANPATH" "SSL_CERT_FILE")
                  ;; Use Nix path for locally managed zsh install so we get correct...everything
                  exec-path-from-shell-shell-name (concat (getenv "HOME") "/.nix-profile/bin/zsh") )
      :config (exec-path-from-shell-initialize))
    (use-package doom-themes
        :config
          (load-theme 'doom-nord t)
          (doom-themes-visual-bell-config)
          (doom-themes-org-config))

    (use-package doom-modeline
        :init (setq doom-modeline-env-version nil)
        :config (doom-modeline-mode))

    (use-package undo-tree :config (global-undo-tree-mode))
    (use-package evil-leader
        :config
          (evil-leader/set-leader bb-default-leader-key)
          (global-evil-leader-mode))

    (use-package evil
        :demand t
        :after (evil-leader undo-tree)
        :defer 0.1
        :custom (evil-undo-system 'undo-tree)
        :config
          (evil-mode 1))

    (use-package evil-matchit
        :after evil
        :config (global-evil-matchit-mode 1))

    (use-package evil-surround
        :after evil
        :config (global-evil-surround-mode 1))

    (use-package ivy :config (ivy-mode))
    (use-package counsel :config (counsel-mode))
    (use-package company
      :delight company-mode
      :config
        (setq company-tooltip-limit 20
            company-tooltip-align-annotations t)
        (global-company-mode 1))

    (use-package origami
        :hook (prog-mode . origami-mode))

    (use-package git-gutter
        :hook (prog-mode . git-gutter-mode)
        :init (setq git-gutter:update-interval 2))

    (use-package magit
        :commands (magit-status)
        :hook (after-save . magit-after-save-refresh-status)
        :defer 5
        :init
        (evil-leader/set-key "g" 'magit)
        (setq magit-popup-show-common-commands nil
                magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1))

    (use-package evil-magit
        :after magit
        :init
        (setq evil-magit-want-horizontal-movement nil))

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
        :config (direnv-mode))

    (use-package editorconfig
        :config (editorconfig-mode 1))

    (use-package perspective
      :config
      (persp-mode))

    (use-package persp-projectile
      :commands (projectile-persp-switch-project)
      :init (evil-leader/set-key "p" 'projectile-persp-switch-project))

    (use-package projectile
      :delight projectile-mode
      :commands (projectile-switch-project projectile-find-file projectile-mode)
      :after evil-leader
      :init
      (setq projectile-completion-system 'ivy
            projectile-require-project-root nil
            projectile-git-command "fd . --print0 --color never"
            projectile-indexing-method 'alien
            projectile-project-search-path '("~/code"))
      :config
      (projectile-mode))

    (use-package counsel-projectile
      :after projectile
      :config
      (setq counsel-projectile-rg-initial-input '(ivy-thing-at-point))
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
      :config
        (setq flycheck-highlighting-mode nil
              flycheck-indication-mode 'left-margin
        )
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
      (defun bb/lsp-setup-python()
        (setq lsp-idle-delay 0.5
              ;;lsp-enable-symbol-highlighting nil
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
              lsp-pyls-plugins-flake8-max-line-length 88
         )

        (lsp-register-custom-settings
         '(("pyls.plugins.pyls_black.enabled" t t)
           ("pyls.plugins.pyls_isort.enabled" t t))))
      :hook
      ((python-mode . lsp)
       (reason-mode . lsp)
       (lsp-mode . lsp-enable-which-key-integration)
       (lsp-before-initialize . bb/lsp-setup-python)))

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
      :init
        (setq web-mode-markup-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-attr-indent-offset 2
              web-mode-enable-css-colorization t
              web-mode-enable-current-column-highlight t
              web-mode-enable-auto-quoting nil
        ))
    (use-package reason-mode
      :mode ("\\.rei?'")
      :init (setq refmt-command 'opam))

    (use-package tuareg
      :mode ("\\.mli?'"))

    (use-package merlin
      :mode ("\\.mli?'"))

    (use-package lsp-java
      :mode ("\\.java'"))

    (use-package yaml-mode
      :mode ("\\.yaml'" "\\.yml'"))

    (use-package markdown-mode
      :config (setq markdown-command "pandoc")
      :mode (("\\.md'" . gfm-mode)))

    (use-package lua-mode
      :mode (".lua$"))
  '';


  

  programs.fzf = {
    enable = true;
    defaultCommand = "fd --type file --follow";
    defaultOptions = [ "--height 40%" "--reverse" ];
    enableZshIntegration = true;
  };

  programs.git = {
    enable = true;
    aliases = {
      graph = "log --graph --oneline --decorate";
      up = "!git pull --ff-only && git submodule update --init --recursive";
      wip = "commit --no-sign -am WIP";
      unwip = "!git log --pretty=%B -1 | grep -iq wip && git reset HEAD~";
    };
    lfs = { enable = true; };
    signing = {
      key = "4AF3A523BC147CA2";
      signByDefault = true;
    };
    ignores = [
      "*.pyc"
      ".python-version"
      ".ropeproject"
      "auto-save-list"
      "custom.el"
      "url/"
      "#*"
      "*~"
      ".#*"
      "#*#"
      "*.log"
      ".DS_Store"
      "**/*.elc"
      ".projectile"
      ".direnv/"
      ".envrc"
    ];
    userEmail = "bbenne10@gmail.com";
    userName = "Bryan Bennett";
  };

  programs.starship = {
    enable = true;
    enableZshIntegration = true;
    settings = {
      add_newline = false;
      scan_timeout = 10;
      character.symbol = "➜";
    };
  };

  programs.zsh = {
    enable = true;
    autocd = true;
    dotDir = ".config/zsh";
    enableCompletion = true;
    enableAutosuggestions = true;
    shellAliases = {
      rmr = "rm -r";
      ls = "exa";
    };

    initExtra = ''
      setopt noclobber
      setopt chasedots
      setopt no_histverify

      bindkey "^[[1;5C" forward-word
      bindkey "^[[1;5D" backward-word

      # Accept autosuggestions with Ctrl + Spc
      bindkey "^ " autosuggest-accept

      # vterm emacs support
      function vterm_printf() {
        printf "\e]%s\e\\" "$1"
      }

      # I have no idea where this is coming from
      # But I don't need it for now
      unset RPS1;
    '';

    plugins = [
      {
        name = "fzf-tab";
        src = pkgs.fetchFromGitHub {
          owner = "Aloxaf";
          repo = "fzf-tab";
          rev = "8584ed59107f37996b977a499ea0d536d851920d";
          sha256 = "177k0wjc2rzp1kynrrmqi0zn3my198cksc6y190sw1r4ia5bsf4k";
        };
      }
      {
        name = "fast-syntax-highlighting";
        src = pkgs.fetchFromGitHub {
          owner = "zdharma";
          repo = "fast-syntax-highlighting";
          rev = "be2f385453670c18c40320a7384333f98fcd9f79";
          sha256 = "1jjhv2ag1rnj1ayrwag5mxyjbdmmf5h5yxvmlc1i44pjcqz3w4s9";
        };
      }
      {
        name = "fzf-marks";
        src = pkgs.fetchFromGitHub {
          owner = "urbainvaes";
          repo = "fzf-marks";
          rev = "5400c3a10b8f054a36dcbbf35611a1d735298034"; # 12/24/20 HEAD
          sha256 = "11klccmdxfx4xdakn1k9qw8l6bxv9qwcz0g5y69r51nvlbrhkxx1";
        };
      }
    ];
  };

}
