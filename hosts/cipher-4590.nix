{ home-manager, userName, pkgs, ... }:
let
  notmuchInputFile = pkgs.writeText "notmuch-tag-batch" ''

    # mailing lists that I don't want to see
    -new -inbox "subject:/^Academic and Research Update/"
    -new -inbox -unread +deleted "subject:/^\[Ms-coc-announce\]/"

    # mark trashed files as deleted
    -new -unread +deleted "folder:work/Trash"

    # remove the "new" tag so we don't keep processing everything missed here
    -new tag:new
  '';
  notmuchPreNewHook = pkgs.writeScriptBin "notmuch-prenew-hook" ''
    # actually kill deleted files
    # notmuch search --output=files --format=text0 tag:deleted | xargs -r0 rm

    # Can this be done in a more generic way?
    notmuch search --output=files --format=text0 "tag:archive and not folder:work/Archive" |
      xargs -r0 -I{} mv {} /Users/${userName}/Maildir/work/Archive

    notmuch search --output=files --format=text0 "tag:deleted and not folder:work/Trash" |
      xargs -r0 -I{} mv {} /Users/${userName}/Maildir/work/Trash

    # and now fetch files from the server
    ${pkgs.isync}/bin/mbsync --all
  '';
  notmuchPostNewHook = pkgs.writeScriptBin "notmuch-postnew-hook" ''
    notmuch tag --input=${notmuchInputFile}
    numNew=$(notmuch count "tag:inbox and tag:unread")
    if [ $numNew -gt 0 ]; then
      osascript -e "display notification \"$numNew unread emails\" with title \"Notmuch\" sound name \"Submarine\""
    fi
  '';
  acsaml = pkgs.callPackage ../derivations/acsaml.nix { };
in
{
  services.nix-daemon.enable = true;

  home-manager.users.${userName} = {
    home.packages = [ acsaml ];
    programs.mbsync.enable = true;
    programs.msmtp.enable = true;
    programs.notmuch = {
      enable = true;
      hooks = {
        postNew = "${notmuchPostNewHook}/bin/notmuch-postnew-hook";
        preNew = "${notmuchPreNewHook}/bin/notmuch-prenew-hook";
      };
    };
    accounts.email.accounts.work = {
      primary = true;
      realName = "Bryan Bennett";
      address = "Bryan.Bennett@gtri.gatech.edu";
      userName = "bbennett37";
      passwordCommand = "echo R1ghtSock0perator";
      imap.host = "localhost";
      imap.port = 1143;
      imap.tls.enable = false;
      smtp.host = "localhost";
      smtp.port = 1025;
      mbsync = {
        enable = true;
        #create = "both";
        #expunge = "both";
        extraConfig.account = {
          AuthMechs = "LOGIN";
        };
      };
      notmuch = {
        enable = true;
      };
      msmtp = {
        enable = true;
      };
    };

    # Relies on preNew hook for actually doing the fetching!
    launchd.agents.receiveMail = {
      enable = true;
      config = ({
        ProgramArguments = [ "${pkgs.notmuch}/bin/notmuch" "new" ];
        ProcessType = "Adaptive";
        RunAtLoad = true;
        StartCalendarInterval = [
          {
            Hour = 7;
            Minute = 55;
          }
          {
            Hour = 11;
            Minute = 55;
          }
          {
            Hour = 15;
            Minute = 55;
          }
        ];
      });
    };
  };
}
