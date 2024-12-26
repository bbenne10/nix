{ bemenu
, brightnessctl
, dwl
, emacs
, firefox
, kanshi
, kitty
, pamixer
, swaylock
, writeShellScriptBin
, wlroots_0_18
}:
let
  autostart = writeShellScriptBin "dwl_autostart.sh" ''
    ${kanshi}/bin/kanshi &
    ${firefox}/bin/firefox &
    ${emacs}/bin/emacs &
  '';
in
dwl.overrideAttrs (oldAttrs: {
  buildInputs = oldAttrs.buildInputs ++ [
    wlroots_0_18
  ];
  postPatch = ''
    substituteInPlace \
      config.def.h \
      --replace-warn "swaylock" "${swaylock}/bin/swaylock" \
      --replace-warn "bemenu-run" "${bemenu}/bin/bemenu-run" \
      --replace-warn "foot" "${kitty}/bin/kitty" \
      --replace-warn "pamixer" "${pamixer}/bin/pamixer" \
      --replace-warn "brightnessctl" "${brightnessctl}/bin/brightnessctl" \
      --replace-warn "autostart.sh" "${autostart}/bin/dwl_autostart.sh";
  '';
})


