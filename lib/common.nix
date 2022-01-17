{ config, lib, pkgsForSystem, home-manager, system, userName, zsh-fzf_tab, zsh-fast_syntax_highlighting, zsh-fzf_marks, ...}: {

  nix = {
    trustedUsers = [ userName ];
    package = pkgsForSystem.nixUnstable;
    binaryCachePublicKeys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
    ];
    binaryCaches = [
      "https://cache.nixos.org"
      "https://nixpkgs-wayland.cachix.org"
    ];
  };

  users.users.${userName}.home = if pkgsForSystem.stdenv.isDarwin then "/Users/${userName}" else "/home/${userName}";

  fonts = {
    fontDir.enable = true;
    fonts = with pkgsForSystem; [
      shareTechMono
    ];
  };
  programs.zsh.enable = true;

  home-manager.users.${userName} = {
    home.stateVersion = "20.09";
    home.packages = with pkgsForSystem; [
      curl
      exa
      fd
      git
      gnupg
      htop
      manix
      neovim
      pandoc
      pass
      ripgrep
      weechat
      tmux
    ]; 

    programs.emacs = {
      enable = true;
      package = pkgsForSystem.emacsWithPackagesFromUsePackage {
        config = ./../conf.d/emacs.el;
        package = pkgsForSystem.emacs;
        alwaysEnsure = true;
      };
    };

    home.file.".config/emacs/init.el".source = ./../conf.d/emacs.el;

    programs.kitty = {
      enable = true;
      settings = {
        shell = "${pkgsForSystem.zsh}/bin/zsh";
        font_family = "Share Tech Mono";
        font_size = "20.0";
        font_features = "ShareTechMono-Regular -liga";
        macos_option_as_alt = true;
        macos_custom_beam_cursor = true;
        background = "#2E3440";
        foreground = "#D8DEE9";
        color0 = "#3B4252";
        color1 = "#BF616A";
        color2 = "#A3BE8C";
        color3 = "#EBCB8B";
        color4 = "#81A1C1";
        color5 = "#B48EAD";
        color6 = "#88C0D0";
        color7 = "#E5E9F0";
        color8 = "#4C566A";
        color9 = "#BF616A";
        color10 = "#A3BE8C";
        color11 = "#EBCB8C";
        color12 = "#81A1C1";
        color13 = "#B48EAD";
        color14 = "#8FBCBB";
        color15 = "#ECEFF4";
      };
    };

    programs.bat = {
      enable = true;
    };

    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv = {
        enable = true;
      };
      stdlib = ''
        # Centralize direnv layouts in $HOME/.cache/direnv/layouts
        : ''${XDG_CACHE_HOME:=$HOME/.cache}
        declare -A direnv_layout_dirs
        direnv_layout_dir() {
          echo "''${direnv_layout_dirs[$PWD]:=$(
            echo -n "$XDG_CACHE_HOME"/direnv/layouts/
            echo -n "$PWD" | shasum | cut -d ' ' -f 1
                                             )}"
        }
      '';
    };

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
        key = "41EA00B400F969701CB2D3AFEF90E3E98B8F5C0B";
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
      userEmail = "Bryan.Bennett@protonmail.com";
      userName = "Bryan Bennett";
      extraConfig = {
        pull = {
          rebase = true;
        };
      };
    };

    programs.gpg = {
      enable = true;
      mutableKeys = false;
      mutableTrust = false;
      publicKeys = [{
        text = ''
          -----BEGIN PGP PUBLIC KEY BLOCK-----

          mQENBGG435gBCAC805OTSnHrC7ciKpsy4SDwMrJRCvdqbd6JvkJKrhe09llqcVY7
          FltPPwetiTXdjDN2+v4AT2LWM9jElRRVi6IOdbQofnHVGO8JDmr6Cd8YugmAn2W6
          pAqR+1UNF2s6hUqZJNZasIxxrqXeGCFL/eb/t/Fj8aGTKA38ydJMKAB0o93UcbGo
          x1Mi7wFfNHify+xEUiYcTbtHug0ucwXWSVBT13KnQ1AagXsKu6Af4+V1bdCPvG5q
          PavY0oRN5mks1B8siwv50VrRuCb96rcNU4HlRmomi2hcZaEKpP2zKYzXjGH5cjJe
          KpBN1HiCYZ3iF88wBPHEPV7JTx2Jbq0QaooLABEBAAG0LEJyeWFuIEJlbm5ldHQg
          PEJyeWFuLkJlbm5ldHRAcHJvdG9ubWFpbC5jb20+iQFOBBMBCAA4FiEEQeoAtAD5
          aXAcstOv75Dj6YuPXAsFAmG435gCGw8FCwkIBwIGFQoJCAsCBBYCAwECHgECF4AA
          CgkQ75Dj6YuPXAvl3wf8D8ZYjMNyQO1vtM+vubEy9VX9MI8Y+TJUrx7FS7ynpIG5
          oU4x7mQfW2DpRMG0IALUqF3oVSe1oY5WrZGTnDfjFcmDGGGFm+vgPJQloRc0JTQS
          7vFVtMZqxX5CRagtLm8JyXU5rO5KGRqrwAHf0+cD+WPt61PEvt39cU+aVHnZ55uv
          szonV2InWCLzcN5ukMVEptoCDiLyk5PDK59tO364UmkGSeMIleORw4TrauINfmd1
          6P8iNw9Y4IMgiOc964pVM/gfAEIraZiTN2RNFs44JzKERVHt0dUco/ZCtuceWjs/
          o4RIRKscIEq5AlJr2Z7zfea8fFNv/zXsgb5LOQjNjLkBDQRhuN/FAQgA0LZnQOb4
          aDXDdeD3hUvNGPvgVT0gSSmo4NYZana+XcIa/lIOs71B1aSct+hWbzOlS9Lf3AeB
          sBab/QfrR5BAk3x0IEC70NaUCLmuQchH94YXOaMFVgN1jkGvRPdhfjxUfwIzSR2k
          KkgGXr2KG8UU9N6+HwpMGjg11rrbc9N9eIVdByTGT12UMR6zxKQQZpuwHHrVrLuM
          xMQsMBDgytWk+1rw1KegryEGgKkUpH2UIYqw//xO14AoU71XFW41VdhOmpEjj69H
          AGW9PD+v1413pnXVd6ISbYMxlQfM4sQn9+Ged3rQ+I+jGlVo4kZr+Sdj333Oys8M
          HY7PixZEq99vCwARAQABiQE2BBgBCAAgFiEEQeoAtAD5aXAcstOv75Dj6YuPXAsF
          AmG438UCGyAACgkQ75Dj6YuPXAvI/wf/RVjCF9zEw/Xk9rKVG74Mr1MpZbGVT8XQ
          uXJ0WzZrcfRDkhX5+3+DkF4T4RlvEZKfVsYu5rrjMenGuT9wrX8i0GRxUC8pQ7er
          aH/gwmh4ohbyqCyz/VisSasq3EeUFlR9OtiIj9BmkI+0aUgOue17ig3LBHduMvu/
          rlvVkzxFgsRFDcRrZXkOnBX0gJVl7YjY61NBx9Y0AEAfzO34CsuPrf3kt12IaVV8
          11yglK7c0LtTouKQZxHdky+b5I3JuS28RhfeJ6eWIPc+5p+90Daf8mA92QKMXDvr
          T4NpJ2/hTmCr49+VOabR0yv1iDjHagZywgT+fTxkwzonAlYXFXyMzw==
          =1nQI
          -----END PGP PUBLIC KEY BLOCK-----'';
        trust = "ultimate";
      }];
    };

    services.gpg-agent = {
      enable = true;
      enableScDaemon = true;
      enableSshSupport = true;
      sshKeys = [
        "8DA8DC50C6EA0D64FEBEF928E162CEDA7DCCA6B6"
      ];

    };

    programs.htop = {
      enable = true;
      settings = {
        show_program_path = true;
      };
    };

    programs.ssh = {
      enable = true;
      controlMaster = "yes";
      controlPersist = "10m";
      forwardAgent = true;
      matchBlocks = {
        "desktop" = {
          hostname = "tia-bxb-d02";
          user = "bbennett37";

        };
      };
    };

    programs.starship = {
      enable = true;
      enableZshIntegration = true;
      settings = {
        add_newline = false;
        scan_timeout = 10;
        character.success_symbol = "[➜](bold green)";
        character.error_symbol = "[➜](bold red)";
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
        cat = "bat";
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

        # ^Z to foreground the last suspended job
        foreground-current-job() { fg; }
        zle -N foreground-current-job
        bindkey -M emacs '^z' foreground-current-job

        gpg-connect-agent /bye
      '';

      envExtra = ''
        export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
      '';

      plugins = [
        {
          name = "fzf-tab";
          src = zsh-fzf_tab;
        }
        {
          name = "fast-syntax-highlighting";
          src = zsh-fast_syntax_highlighting;
        }
        {
          name = "fzf-marks";
          src = zsh-fzf_marks;
        }
      ];
    };
  };
}
