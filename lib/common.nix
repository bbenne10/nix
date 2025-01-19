{ deploy-rs
, pkgs
, userName
, zsh-fast_syntax_highlighting
, zsh-fzf_marks
, zsh-fzf_tab
, ...
}: {
  environment.systemPackages = builtins.attrValues {
    inherit (pkgs) 
      bashInteractive
      deploy-rs
    ;
  };

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  config = {
    programs.zsh.enable = true;
    home-manager.backupFileExtension = "bk";

    environment = {
      systemPackages = [
        pkgs.deploy-rs
      ];
      sessionVariables = {
        XDG_DESKTOP_DIR="$HOME";
        XDG_DOCUMENTS_DIR="$HOME/documents";
        XDG_DOWNLOAD_DIR="$HOME/downloads";
        XDG_MUSIC_DIR="$HOME/music";
        XDG_PICTURES_DIR="$HOME/pictures";
        XDG_PUBLICSHARE_DIR="$HOME/public";
        XDG_TEMPLATES_DIR="$HOME/templates";
        XDG_VIDEOS_DIR="$HOME/videos";
      };
    };
    programs.gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

    home-manager.users.${userName} = {
    home.enableNixpkgsReleaseCheck = true;
    home.stateVersion = "22.05";
    home.packages = builtins.attrValues {
      inherit (pkgs)
        curl
        fd
        gawk
        helix
        jq
        nil
        nix-output-monitor
        nix-tree
        nixfmt-rfc-style
        openssh
        rsync
        tree
        tmux
      ;
    };

    programs.helix = {
      enable = true;
      settings = {
        theme = "everforest_dark";
        editor = {
          statusline = {
            mode = {
              normal = "N";
              insert = "I";
              select = "S";
            };
          };
          auto-pairs = false;
        };
      };
    };

      programs.bat = {
        enable = true;
      };

      programs.direnv = {
        enable = true;
        enableZshIntegration = true;
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
          wip = "commit --no-sign -am WIP --no-verify";
          unwip = "!git log --pretty=%B -1 | grep -iq wip && git reset HEAD~";
        };
        lfs = { enable = true; };
        delta = { enable = true; };
        signing = {
          key = "5DDFE2C35409EFE83F70C32788393D9EC269636D";
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
          ".direnv/"
          ".envrc"
        ];
        userEmail = "Bryan.Bennett@proton.me";
        userName = "Bryan Bennett";
        extraConfig = {
          core.fsmonitor = true;
          init = {
            defaultBranch = "main";
          };
          pull = {
            rebase = true;
          };
          push = {
            default = "current";
            autoSetupRemote = "true";
          };
        };
      };

      programs.gpg = {
        enable = true;
        mutableKeys = false;
        mutableTrust = false;
        scdaemonSettings = {
          disable-ccid = true;
        };
        publicKeys = [
          {
            # yubikey 5
            text = ''
            -----BEGIN PGP PUBLIC KEY BLOCK-----

            mDMEZyjlExYJKwYBBAHaRw8BAQdAmqK3U2qsdh/4cuIa2PfhAUapZCK0bvFzPLdL
            rzq0fHa0J0JyeWFuIEJlbm5ldHQgPEJyeWFuLkJlbm5ldHRAcHJvdG9uLm1lPoiZ
            BBMWCgBBFiEEXd/iw1QJ7+g/cMMniDk9nsJpY20FAmco5RMCGwMFCQWjmoAFCwkI
            BwICIgIGFQoJCAsCBBYCAwECHgcCF4AACgkQiDk9nsJpY20/5QD/YUQZczM2EUC7
            Fte+SoLzSdGOM+4ketM/FiRfT5gz5zQBAJvYyzf/RjFe+2VdQ7tnQySKUo/A5Lfc
            b74vVxZSRVUCuDgEZyjlExIKKwYBBAGXVQEFAQEHQE3AcQyGlQXu8/3S3APmYJFE
            wddgGzznFhqlY2MxdyBwAwEIB4h+BBgWCgAmFiEEXd/iw1QJ7+g/cMMniDk9nsJp
            Y20FAmco5RMCGwwFCQWjmoAACgkQiDk9nsJpY23rUAEA0oUs19rCu6vnUjG6TVQq
            lG0iYfRu/bG63gOXl2pJrU0A/ip8NsgQ6szv7rtT04ZGhPTex0aWSTybQ3uHT8mC
            TWQIuDMEZyjlMxYJKwYBBAHaRw8BAQdAWNGLzVg3MLQhTht6UVfZBeagANQIyhwj
            9IT+SI3imy2IfgQYFgoAJhYhBF3f4sNUCe/oP3DDJ4g5PZ7CaWNtBQJnKOUzAhsg
            BQkFo5qAAAoJEIg5PZ7CaWNtxG4BAP1oKfO8eCE1GaaSD7g90Hw9AgDb8c1+RdXS
            6VxP2mpZAQCr153Ei5QdNnb/kYV+vAIV/sX2J6NOfEONZi0dT997Aw==
            =aMQ3
            -----END PGP PUBLIC KEY BLOCK-----

          '';
            trust = "ultimate";
          }
        ];
      };

      programs.htop = {
        enable = true;
      };

      programs.ssh = {
        enable = true;
        forwardAgent = true;
      };

      programs.ripgrep = {
        enable = true;
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
        enableCompletion = true;
        autosuggestion.enable = true;
        shellAliases = {
          rmr = "rm -r";
          ls = pkgs.lib.getExe pkgs.eza;
          cat = pkgs.lib.getExe pkgs.bat;
          gpgreset = "gpg-connect-agent killagent /bye; gpg-connect-agent updatestartuptty /bye; gpg-connect-agent /bye";
        };

        initExtra = ''
        setopt noclobber
        setopt chasedots
        setopt no_histverify

        bindkey "^[[1;5C" forward-word
        bindkey "^[[1;5D" backward-word

        # Accept autosuggestions with Ctrl + Spc
        bindkey "^ " autosuggest-accept

        function set_title () {
          echo -en "\033]1; $@ \007"
                   }

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
        if [[ "$(tty)" = "/dev/tty/0" ]]; then
          exec dwls
        fi
      '';

        sessionVariables = {
          BEMENU_OPTS = ''
          -c \
          -W0.5 \
          -l10 \
          --nb "#1E2326" \
          --ab "#1E2326" \
          --sb "#1E3236" \
          --tb "#1E2326" \
          --fb "#1E2326" \
          --tf "#D3C6AA" \
          --fn \"Rec Mono Semicasual 10\"
          --fixed-height \
          -B10 \
          --bdr "#1E2326" \
          --hf "#E69875" \
          --hb "#1E2326"
        '';
        };

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
  };
}
