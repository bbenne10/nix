{ pkgs, home-manager, userName, ... }: {
  system.stateVersion = "22.11";

  console.earlySetup = true;

  networking.nameservers = [ "192.168.1.142" "1.1.1.1" ];

  users.users.${userName} = {
    home = "/home/${userName}";
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" "vboxusers" "video" ];
    hashedPassword = "$6$hc672tTQXjHQV$xOGejAjJAdP3VhKMAHCZ2J8G0mj2mjrYS7l4hkq6fVRlLygWplZeem4LX0MEdGGBsGaqClLUc6Z4fkRsfROYB/";
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN05GNTpS1NpA9W52N1tNKlS8yuRzRwTqwpL25CA7CCD cardno:23_229_660"
    ];
  };
}
