{pkgs, ...}: {
  home.packages = [
    pkgs.colima
  ];

  home.emacs.package = pkgs.emacs30;

  home.file.".gnupg/gpg-agent.conf" = {
    enable = true;
    text = ''
      pinentry-program ${pkgs.lib.getExe pkgs.pinentry_mac}
    '';
  };
};
