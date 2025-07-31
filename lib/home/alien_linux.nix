{config, pkgs, dwl, nixgl, ...}:
let getExe = pkgs.lib.meta.getExe;
    nixGLIntel = nixgl.packages.${pkgs.system}.nixGLIntel;
    nixglwrap = config.lib.nixGL.wrap;
    slstatus = dwl.packages.${pkgs.system}.slstatus;
in {

  nixGL.packages = {
    inherit nixGLIntel;
  };
  programs.alacritty.package = nixglwrap pkgs.alacritty;
  home.packages = [
    (pkgs.writeScriptBin "dwls" ''
        ${getExe slstatus} -s | ${nixglwrap dwl.packages.${pkgs.system}.dwl}/bin/dwl
    '')
  ];
}
