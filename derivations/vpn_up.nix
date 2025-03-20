{ writeShellApplication, openconnect, fetchFromGitLab, libnotify }:

let updated_openconnect = openconnect.overrideAttrs {
  version = "9.12+";
  src = fetchFromGitLab {
    owner = "openconnect";
    repo  = "openconnect";
    rev = "f17fe20d337b400b476a73326de642a9f63b59c8"; # head 1/21/25
    hash = "sha256-OBEojqOf7cmGtDa9ToPaJUHrmBhq19/CyZ5agbP7WUw=";
  };
}; in writeShellApplication {
  name = "vpn_up";
  runtimeInputs = [ updated_openconnect libnotify];
  text = ''
    set -x
    # This script integrates with a system-wide installation
    # of openconnect in obscure ways when used with networkmanager.
    # They won't give me a NixOS machine so I can only do so much
    # ¯\_(ツ)_/¯

    # The arguments are subtly different between invocation "kinds":
    #
    # If using NetworkManager, the first argument *must* be the VPN name.
    # The rest are passed to the initial authenticating openconnect call.
    #
    # If using standalone, the arguments are all proxied directly to openconnect.
    # Additionally, you must pass the vpn gateway to the standalone invocation
    # where this would be inferred from the NM VPN in the NM case.
    # Standalone invocations may need to be invoked as root as well.

    # NM case:
    # $ vpn_up Work --auth-group="Foo"
    #
    # Standalone case (note root invocation!):
    # # vpn_up vpn1.work.com --auth-group "Foo"

    browser="$(command -v xdg-open || command -v open)"

    function standalone() {
        COOKIE=
        sudo -v
        # Sets $COOKIE, $FINGERPRINT, $CONNECT_URL and optionally $RESOLVE
        eval "$(${updated_openconnect}/bin/openconnect  \
                "$@" \
                --authenticate \
                --external-browser "$browser" \
                --useragent "AnyConnect*")"

        if [ -z "$COOKIE" ]; then
            echo "OpenConnect didn't set the expected variables!" 1>&2
            exit 1
        fi

        sudo ${updated_openconnect}/bin/openconnect  \
            --servercert "$FINGERPRINT" \
            "$CONNECT_URL" \
            --useragent "AnyConnect*" \
            --cookie-on-stdin \
            ''${RESOLVE:+--resolve "$RESOLVE"} <<< "$COOKIE"
    }

    function networkmanager() {
        vpn_name="$1"
        shift

        # sets $protocol and $gateway
        eval "$(nmcli -t -g vpn.data connection show "$vpn_name" \
                | grep -Po '(^|[ ,])(protocol|gateway)\ =\ [^,]*' \
                | sed 's/ //g')"

        if [ -z "$protocol" ] || [ -z "$gateway" ]; then
            echo "Couldn't find VPN $vpn_name?"
            exit 1
        fi

        # sets $COOKIE, $FINGERPRINT, $CONNECT_URL, and optionally $RESOLVE
        eval "$(openconnect \
            --authenticate \
            --useragent "AnyConnect" \
            --external-browser "$browser" \
            --protocol="$protocol" \
            "$gateway" "$@")"

        # Used as a canary here. All of the prior 4 variables should be unset if this is unset.
        if [ -z "$COOKIE" ]; then
            echo "OpenConnect didn't set the expected variables!" 1>&2
            exit 1
        fi

        secrets=$(mktemp "''${TMPDIR:-/tmp}/vpn_up.XXXXX")
        trap 'rm "$secrets"' EXIT

        cat <<EOF >"$secrets"
            vpn.secrets.cookie:$COOKIE
            vpn.secrets.gwcert:$FINGERPRINT
            vpn.secrets.gateway:$CONNECT_URL
            vpn.secrets.resolve:$RESOLVE
    EOF

        if nmcli con up "$vpn_name" passwd-file "$secrets"; then
          notify-send --icon=network-workgroup "Network \"$vpn_name\" up"
        fi
    }

    if command -v systemctl >/dev/null && systemctl is-active NetworkManager >/dev/null; then
        networkmanager "$@"
    else
        standalone "$@"
    fi
  '';
}
