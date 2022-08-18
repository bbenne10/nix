{
  inputs = {
    # TODO: get this working with multiple architectures.
    nixpkgs = { url = "github:nixos/nixpkgs/nixpkgs-22.05-darwin"; };

    nix-direnv = {
      url = "github:nix-community/nix-direnv";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:lnl7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-22.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # emacs + plugins
    emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacsOsx = {
      url = "github:cmacrae/emacs";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # zsh plugins
    zsh-fzf_tab = {
      url = "github:aloxaf/fzf-tab";
      flake = false;
    };

    zsh-fast_syntax_highlighting = {
      url = "github:zdharma-continuum/fast-syntax-highlighting";
      flake = false;
    };

    zsh-fzf_marks = {
      url = "github:urbainvaes/fzf-marks";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, darwin, home-manager, nix-direnv, sops-nix, emacs, emacsOsx
    , zsh-fzf_tab, zsh-fast_syntax_highlighting, zsh-fzf_marks }:
    let
      genAttrs = list: f: nixpkgs.lib.genAttrs list f;
      systems = [ "x86_64-darwin" "x86_64-linux" ];
      pkgsBySystem = (let
        mkPkgs = system:
            import nixpkgs {
              inherit system;
              overlays = [
                emacs.overlay
                (self: super: {
                  weechat = super.weechat.override {
                    configure = { availablePlugins, ... }: {
                      scripts = with super.weechatScripts; [
                        weechat-matrix
                        colorize_nicks
                      ];
                    };
                  };
                })
              ] ++ (if system == "x86_64-darwin" then [ emacsOsx.overlay ] else []);
              config = { allowUnfree = true; };
            };
      in genAttrs systems mkPkgs);
      darwinPkgs = pkgsBySystem.x86_64-darwin;
      linuxPkgs = pkgsBySystem.x86_64-linux;
    in {
      nixosConfigurations = {
        "bennett-laptop" = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            home-manager.nixosModules.home-manager
            ./hardware/laptop.nix
            ./lib/common.nix
            ./lib/linux.nix
            ./hosts/bennett-laptop.nix
          ];
          specialArgs = {
            pkgsForSystem = linuxPkgs;
            userName = "bryan";
            system = "x86_64-linux";
            inherit home-manager nix-direnv zsh-fzf_tab
              zsh-fast_syntax_highlighting zsh-fzf_marks;
          };
        };

        "bennett-server" = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = [
            sops-nix.nixosModules.sops
            ./lib/common.nix
            ./hosts/bennett-server.nix
          ];
          specialArgs = {
            pkgsForSystem = linuxPkgs;
            userName = "bryan";
            system = "x86_64-linux";
            inherit home-manager nix-direnv zsh-fzf_tab
              zsh-fast_syntax_highlighting zsh-fzf_marks;
          };
        };
      };
      darwinConfigurations = {
        "cipher-4590" = darwin.lib.darwinSystem {
          system = "x86_64-darwin";
          specialArgs = {
            pkgsForSystem = darwinPkgs;
            userName = "bbennett37";
            system = "x86_64-darwin";
            inherit home-manager nix-direnv zsh-fzf_tab
              zsh-fast_syntax_highlighting zsh-fzf_marks;
          };
          modules = [
            home-manager.darwinModules.home-manager
            ./lib/common.nix
            ./lib/darwin.nix
            ./hosts/cipher-4590.nix
          ];
        };
      };
    };
}
