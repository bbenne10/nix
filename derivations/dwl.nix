{ bemenu
, brightnessctl
, dwl
, dwl-src
, emacs
, firefox
, foot
, kanshi
, pamixer
, swaylock
, waybar
, writeShellScriptBin
}:
let
  autostart = writeShellScriptBin "dwl_autostart.sh" ''
    ${kanshi}/bin/kanshi &
    ${waybar}/bin/waybar &
    ${firefox}/bin/firefox &
    ${emacs}/bin/emacs &
  '';
in
dwl.overrideAttrs (oldAttrs: rec {
  src = dwl-src;
  postPatch = ''
    substituteInPlace \
      config.def.h \
      --replace "swaylock" "${swaylock}/bin/swaylock" \
      --replace "bemenu-run" "${bemenu}/bin/bemenu-run" \
      --replace "foot" "${foot}/bin/foot" \
      --replace "pamixer" "${pamixer}/bin/pamixer" \
      --replace "brightnessctl" "${brightnessctl}/bin/brightnessctl" \
      --replace "autostart.sh" "${autostart}/bin/dwl_autostart.sh";
  '';
})


