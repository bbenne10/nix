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
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDQtmdA5vhoNcN14PeFS80Y++BVPSBJKajg1hlqdr5dwhr+Ug6zvUHVpJy36FZvM6VL0t/cB4GwFpv9B+tHkECTfHQgQLvQ1pQIua5ByEf3hhc5owVWA3WOQa9E92F+PFR/AjNJHaQqSAZevYobxRT03r4fCkwaODXWuttz0314hV0HJMZPXZQxHrPEpBBmm7AcetWsu4zExCwwEODK1aT7WvDUp6CvIQaAqRSkfZQhirD//E7XgChTvVcVbjVV2E6akSOPr0cAZb08P6/XjXemddV3ohJtgzGVB8zixCf34Z53etD4j6MaVWjiRmv5J2Pffc7Kzwwdjs+LFkSr328L cardno:000606534762"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIN05GNTpS1NpA9W52N1tNKlS8yuRzRwTqwpL25CA7CCD cardno:23_229_660"
    ];
  };
}
