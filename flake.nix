{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    darwin = {
      url = "github:bbenne10/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
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

    zsh-fzf_marks= {
      url = "github:urbainvaes/fzf-marks";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, darwin, emacs, home-manager, zsh-fzf_tab, zsh-fast_syntax_highlighting, zsh-fzf_marks }:
    let genAttrs = list: f: nixpkgs.lib.genAttrs list f;
        systems = [ "x86_64-darwin" "x86_64-linux" ];
        pkgsBySystem = (
          let mkPkgs = system: import nixpkgs {
            inherit system;
            overlays = [
              emacs.overlay
              (self: super: {
                shareTechMono = nixpkgs.legacyPackages.${system}.stdenv.mkDerivation {
                  name = "share_tech_mono";
                  version = "1.0";
                  src = ./.;
                  installPhase = ''
                     mkdir -p $out/share/fonts/opentype
                     cp -R files/ShareTechMono-regular.ttf $out/share/fonts/opentype
                   '';
                 };
              })
            ];
            config = {
              allowUnfree = true;
            };
          }; in genAttrs systems mkPkgs);
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
           pkgsForSystem=linuxPkgs;
           userName="bryan";
           system="x86_64-linux";
           inherit home-manager
                   zsh-fzf_tab
                   zsh-fast_syntax_highlighting
                   zsh-fzf_marks;
          };
        };
      };
      darwinConfigurations = {
        "cipher-4590" = darwin.lib.darwinSystem {
          system = "x86_64-darwin";
          specialArgs = {
             pkgsForSystem=darwinPkgs;
             userName="bbennett37";
	     system="x86_64-darwin";
             inherit home-manager
                     zsh-fzf_tab
                     zsh-fast_syntax_highlighting
                     zsh-fzf_marks;
          };
          modules = [
            home-manager.darwinModules.home-manager
             ./lib/common.nix
             ./hosts/cipher-4590.nix
          ];
        };
      };
  };
}
