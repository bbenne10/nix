{ pkgs , ... }: {
  fonts = {
    packages = [
      (pkgs.nerdfonts.override { fonts = ["Recursive"]; })
      pkgs.noto-fonts
    
    ];
  };
}
