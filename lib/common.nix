{ config
, deploy-rs
, lib
, pkgs
, home-manager
, nix-index-database
, system
, userName
, environment
, zsh-fzf_tab
, zsh-fast_syntax_highlighting
, zsh-fzf_marks
, ...
}:
let
  themesh = (pkgs.callPackage ../derivations/themesh.nix { });
  darkmode = (pkgs.callPackage ../derivations/darkmode.nix { });
  deploy-rs-bin = deploy-rs.packages.${system}.deploy-rs;
in
{
  environment.systemPackages = with pkgs; [
    bashInteractive
    cachix
    nixUnstable
    deploy-rs-bin
  ];

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  programs.zsh.enable = true;

  home-manager.users.${userName} = {
    home.enableNixpkgsReleaseCheck = true;
    home.stateVersion = "22.05";
    home.packages = with pkgs; [
      bitwarden-cli
      curl
      nix-index-database.packages.${pkgs.stdenv.system}.comma-with-db
      darkmode
      dtach
      dvtm
      fd
      gawk
      gnupg
      manix
      nix-tree
      nixUnstable
      nixfmt
      openssh
      ripgrep
      rnix-lsp
      rsync
      themesh
      tree
    ];

    programs.bat = {
      enable = true;
    };

    programs.htop = {
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
        wip = "commit --no-sign -am WIP --no-verify";
        unwip = "!git log --pretty=%B -1 | grep -iq wip && git reset HEAD~";
      };
      lfs = { enable = true; };
      delta = { enable = true; };
      signing = {
        key = "EE149E4215408DE9";
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

            xsFNBAAAAAABEADGOwOpy8tjQ3mHLvdG57kN/tR3PAz0dj8tzobo6oFHx5LFwOhp
            yoHblwx66GzZp37lELpt3Gyp4q9fmlG2QI20tD9qWKy3NwS69cRJMrJjdF6hdWFQ
            XQCxi7FOjScGeGNAgdaCUwdlLgtF16lfe/7gjh3JCjcwO+hW9dMJGk7iEW6lhOdT
            juNS2KbGkjCLZvinBiJCx/99duIW7vFQs+VzKD7ETK9E09yNWVvcuVmn5dFXKMQd
            F0oA1Ue+zNrervmVW0xeIY7zqDhsGlSQTFmnfrRdUcAIldpiUv1yoiQS/wupYFf1
            ZtFX3HqeUgvy+m0wCHWWvEm+Fmu3T7EHr88Kow7d3Lwrei9pAF49lWY6RCRC7/Xi
            XMEhv25eIh7JZuXGS9NZY7xnoNWMnM2vjZRMK5uynlILcHwU02s11E5fRzNFtb76
            W6H2P0CJMY8YQkv4/tvdjaFotkB2dNb4LA8OOLM1vTbvB4KRtHcXDdwUylZQdD7Z
            dMhqz7+RpkPecA0Qieen4yS/ijCRq8wxaMd/POEV6alaqTXaXzwERxsrrYvmoyQj
            6zdxin40/KidGfRZb7+MO2nyolkEK7GNK3cjAhsFa2oAUs3Tf807hmpSEQsFn4EX
            EXSP3ROIbxuYFwDDNs75pne4wjuXLOnnG1uAY3mJYQA+R6X7Xb/xOLy+xwARAQAB
            zSlyb290IChJbXBvcnRlZCBmcm9tIFNTSCkgPHJvb3RAbG9jYWxob3N0PsLBYgQT
            AQgAFgUCAAAAAAkQnE3mE4xxJ/ECGw8CGQEAANHNEAAcZ1U0rhCWQmG+yuZe7Nce
            YQdlS82AEWWET5weE170DDlpURbo3D08fQlwh3Pnr9E3ng5HAgnFRH6Elm1aXGn3
            DHtzsuQZLwMwYRmkn4cq418RpVl9hO+LfBTDNrwD+/SAQ0yHlZyZE12E49JLbTDP
            2D1QO0L3UoHQUzHSb7hwJlo+x8iwipY9WKieD1TH3BdMKZd2tbFHP/Cmo1GHZlBe
            DpzdyxOLowK9fw+WkGkWvPnTJuVwPxikvriwEOlLLyiNCMlDTX8VLwc28nT4euJr
            k7hQFocmkL9eai2CU6FCt6HCPy3uHn3zZpPZMOErG1jLaF7cFc+N19tBHM2f4ZpU
            t6AzcYv0TgjULBMlUScLNPi92cUNNFkBK666GjH363UWAxLk+MmOrLdKraQbvNV9
            3Ay6MyhJKbMkNPi37mflCQyxEuDmAYQDqRu1427OIj0Jkuf79rcJsIgt+gGHyImX
            /dTVQAlQ0krRVVjgKXGIMnWyyE+U0G0GE/h8putby6hq8tvMIVL2MZG9CVqdyBFG
            BU0b9uAuTbLGf3kSmbhV3Y8ibGOTrRXAhSxzdOFTw4sHrSCh4cFlDqBTxmw7L5In
            Y1WAw4SKGxIpKYDpSStOPrXVUcxzY45uHP6AxNDrOuU82jihZY9voJJ/kuWjpzyr
            0uOxq8VoYZsQ2QZrNDLGbg==
            =lxGb
            -----END PGP PUBLIC KEY BLOCK-----'';
          trust = "ultimate";
        }
      ];
    };

    programs.ssh = {
      enable = true;
      forwardAgent = true;
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
        ls = "${pkgs.eza}/bin/eza";
        cat = "${pkgs.bat}/bin/bat";
        gpgreset = "gpg-connect-agent killagent /bye; gpg-connect-agent updatestartuptty /bye; gpg-connect-agent /bye";
      };

      initExtra = ''
        theme.sh nord
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
