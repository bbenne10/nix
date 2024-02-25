{ bemenu
, brightnessctl
, dwl
, dwl-src
, firefox
, foot
, kanshi
, pamixer
, swaybg
, swaylock
, waybar
, writeShellScriptBin
}:
let
  autostart = writeShellScriptBin "dwl_autostart.sh" ''
    ${kanshi}/bin/kanshi &
    ${swaybg}/bin/swaybg -c "#495156" &
    ${waybar}/bin/waybar &
    ${firefox}/bin/firefox &
  '';
in
dwl.overrideAttrs (oldAttrs: rec {
  src = dwl-src;
  postPatch = ''
    substituteInPlace \
      config.h \
      --replace "swaylock" "${swaylock}/bin/swaylock" \
      --replace "bemenu-run" "${bemenu}/bin/bemenu-run" \
      --replace "foot" "${foot}/bin/foot" \
      --replace "pamixer" "${pamixer}/bin/pamixer" \
      --replace "brightnessctl" "${brightnessctl}/bin/brightnessctl" \
      --replace "autostart.sh" "${autostart}/bin/dwl_autostart.sh";
  '';
})


