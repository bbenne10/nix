{pkgs, dwl}: {
  home.packages = {
    dwls = dwl.packages.${pkgs.system}.dwls;
  };

  services.gpg-agent = {
    enable = true;
    enableZshIntegration = true;
    enableScDaemon = true;
    enableSshSupport = true;
    pinentryPackage = pkgs.pinentry-bemenu;
  };

}
