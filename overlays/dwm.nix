(self: super:
with import <nixpkgs/lib>;
let dwmPatchBaseDir = /home/bryan/.config/nixpkgs/overlays/patches/dwm;
    isPatch = builtins.match ".+\.patch";
    fullPatchPath = attrsets.mapAttrsToList(k: v: if isPatch k != null then dwmPatchBaseDir + ("/" + k) else null);
    filterNull = filter (i: i != null);
in
{
  dwm = super.dwm.overrideAttrs (old: {
    buildInputs = with self.pkgs; old.buildInputs ++ [ 
      pango 
      pkgconfig 
      xorg.libxcb
    ];
    configurePhase = ''
      substitute ./config.def.h ./config.h \
        --replace @alacritty@ "${self.pkgs.alacritty}/bin/alacritty" \
        --replace @i3lock@ "${self.pkgs.i3lock}/bin/i3lock" \
        --replace @pamixer@ "${self.pkgs.pamixer}/bin/pamixer" \
        --replace @rofi@ "${self.pkgs.rofi}/bin/rofi"\
        --replace @rofi-pass@ "${self.pkgs.rofi-pass}/bin/rofi-pass" \
        --replace @brightnessctl@ "${self.pkgs.brightnessctl}/bin/brightnessctl"
    '';
    buildPhase = " cat config.h; make ";
    patches = (old.patches or []) ++ (filterNull (fullPatchPath (builtins.readDir dwmPatchBaseDir)));
    propagatedBuildInputs = with self.pkgs; [
      alacritty
      brightnessctl
      light
      pamixer 
      pass
      rofi 
      rofi-pass
      slock 
    ];
  });
}
)
