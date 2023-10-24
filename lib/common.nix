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
      mpv
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

    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv = { enable = true; };
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
        ".direnv/"
        ".envrc"
      ];
      userEmail = "Bryan.Bennett@protonmail.com";
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
      scdaemonSettings = { disable-ccid = true; };
      publicKeys = [
        {
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
        }
        {
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

    programs.htop = {
      enable = true;
    };

    programs.ssh = {
      enable = true;
      controlMaster = "yes";
      controlPersist = "10m";
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
