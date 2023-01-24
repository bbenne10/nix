{ pkgs, home-manager, userName, ... }: {
  system.stateVersion = "22.11";
  users.users.${userName}.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDQtmdA5vhoNcN14PeFS80Y++BVPSBJKajg1hlqdr5dwhr+Ug6zvUHVpJy36FZvM6VL0t/cB4GwFpv9B+tHkECTfHQgQLvQ1pQIua5ByEf3hhc5owVWA3WOQa9E92F+PFR/AjNJHaQqSAZevYobxRT03r4fCkwaODXWuttz0314hV0HJMZPXZQxHrPEpBBmm7AcetWsu4zExCwwEODK1aT7WvDUp6CvIQaAqRSkfZQhirD//E7XgChTvVcVbjVV2E6akSOPr0cAZb08P6/XjXemddV3ohJtgzGVB8zixCf34Z53etD4j6MaVWjiRmv5J2Pffc7Kzwwdjs+LFkSr328L cardno:000606534762"
  ];
}
