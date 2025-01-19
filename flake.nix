{
  inputs = {
    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-24.11";
    };

    lix = {
      url = "https://git.lix.systems/lix-project/nixos-module/archive/2.91.1-2.tar.gz";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-24.11";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    lanzaboote = {
      url = "github:nix-community/lanzaboote/v0.4.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixgl = {
      url = "github:guibou/nixGL";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Server specific
    deploy-rs.url = "github:serokell/deploy-rs";

    website = {
      url = "github:bbenne10/website";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # emacs + plugins
    emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs_themes_src = {
      url = "github:bbenne10/emacs_themes";
      flake = false;
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

    # Darwin specific
    mac-app-util = {
      url = "github:hraban/mac-app-util";
    };

    # Linux specific
    dwl = {
      url = "github:bbenne10/dwl";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nur = {
      url = "github:nix-community/NUR";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      darwin,
      deploy-rs,
      emacs,
      home-manager,
      lanzaboote,
      nur,
      ...
    }@inputs:
    let
      specialArgs = inputs // {
        userName = "bryan";
      };
      baseLinuxModules = [
        home-manager.nixosModules.home-manager
        inputs.lix.nixosModules.default

        ./lib/nix.nix
        ./lib/common.nix
        ./lib/home/common.nix
        ./lib/linux.nix
        {
          home.username = "bryan";
          home-manager.users.bryan.imports = [
            ./lib/home/graphical.nix
            ./lib/home/common.nix
            ./lib/home/linux.nix
          ];
        }
      ];
    in
    {
      nixosConfigurations = {
        "bennett-laptop" = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = baseLinuxModules ++ [
            lanzaboote.nixosModules.lanzaboote
            ./lib/graphical.nix
            ./lib/home/graphical.nix
            ./hardware/laptop.nix
            ./hosts/bennett-laptop.nix
            # TODO: test this on nixos with HM modules
          ];
          inherit specialArgs;
        };

        "bennett-server" = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = baseLinuxModules ++ [
            ./hosts/bennett-server.nix
          ];
          inherit specialArgs;
        };

        "home-server" = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules = baseLinuxModules ++ [
            ./hosts/home-server.nix
          ];
          inherit specialArgs;
        };
      };
      darwinConfigurations = {
        "cipher-12058" = darwin.lib.darwinSystem {
          system = "aarch64-darwin";
          specialArgs = specialArgs // {
            userName = "bbennett37";
          };
          modules = [
            inputs.home-manager.darwinModules.home-manager
            inputs.mac-app-util.darwinModules.default
            (
              { pkgs, config, ... }:
              {
                # Enable this for home-manager packages too
                home-manager.sharedModules = [
                  inputs.mac-app-util.homeManagerModules.default
                ];
              }
            )
            ./lib/common.nix
            ./lib/nix.nix
            ./lib/graphical.nix
            ./lib/darwin.nix
            ./hosts/cipher-4590.nix
            {

              home.username = "bbennett37";
              home-manager.users.bbennett37 = {
                userName = "bbennett37";
                imports = [
                  ./lib/home/graphical.nix
                  ./lib/home/common.nix
                ];
              };
            }
          ];
        };
      };
      homeConfigurations = {
        "cipher-12053" = home-manager.lib.homeManagerConfiguration {
          pkgs = import nixpkgs {
            system = "x86_64-linux";
            overlays = [
              nur.overlays.default
              emacs.overlays.default
            ];
          };
          extraSpecialArgs = inputs;
          modules = [
            ./lib/home/common.nix
            ./lib/home/graphical.nix
            ./lib/home/linux.nix
            ./lib/home/alien_linux.nix
            {
              home.username = "bbennett37";
              home.homeDirectory = "/home/bbennett37";
            }
          ];
        };
      };
      deploy.nodes = {
        server = {
          hostname = "bryan-bennett.com";
          user = "root";
          sshUser = "bryan";
          interactiveSudo = true;
          profilesOrder = [ "system" ];
          timeout = 600;
          magicRollback = false;

          profiles = {
            system = {
              path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.bennett-server;
            };
          };
        };
        home-server = {
          hostname = "home-server";
          user = "root";
          sshUser = "bryan";
          interactiveSudo = true;
          magicRollback = false;
          profiles = {
            system = {
              path = deploy-rs.lib.x86_64-linux.activate.nixos self.nixosConfigurations.home-server;
            };
          };
        };
      };
      checks = builtins.mapAttrs (system: deployLib: deployLib.deployChecks self.deploy) deploy-rs.lib;
    };
}
