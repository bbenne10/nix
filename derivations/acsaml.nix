{ writeShellApplication, openconnect }: writeShellApplication {
  name = "acsaml";
  runtimeInputs = [ openconnect ];
  text = ''
    sudo -v
    browser="$(command -v xdg-open || command -v open)"
    COOKIE=

    eval "$(
        openconnect \
            "$@" \
            --authenticate \
            --external-browser "$browser" \
            --useragent "AnyConnect Linux_64")"

    if [ -z "$COOKIE" ]; then
        echo "OpenConnect didn't set the expected variables!" 1>&2
        exit 1
    fi

    sudo openconnect \
         --servercert "$FINGERPRINT" \
         "$CONNECT_URL" \
         --useragent "AnyConnect Linux_64" \
         --cookie-on-stdin \
         ''${RESOLVE:+--resolve "$RESOLVE"} <<< "$COOKIE"
  '';
}
