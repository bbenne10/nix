{stdenv, fetchFromGitHub, ...}: stdenv.mkDerivation {
  name = "theme.sh";
  version = "1.1.5";
  srcs = [
    (fetchFromGitHub {
      owner = "lemnos";
      repo = "theme.sh";
      rev = "v1.1.5";
      sha256="zDw8WGBzO4/HRCgN7yoUxT49ibTz+QkRa5WpBQbl1nI=";
      name = "themesh";
    })
    (fetchFromGitHub {
      owner = "earl-grey-theme";
      repo = "earl-grey";
      rev = "5dc431c8479966d50086a1e46a077eaa5cc3e728";
      sha256 = "sqt8F1snBkUASKWXoQrq2TMN/XSdvr0HphJH53hYly8=";
      name = "earl-grey";
    })
  ];
  sourceRoot = ".";

  buildPhase = ''
    mv earl-grey/themes/kitty/earl-grey-theme.conf ./earl_grey.conf
    pushd themesh
    make
    bin/theme.sh -a ../earl_grey.conf
    popd
  '';

  installPhase = ''
    mkdir $out
    mv themesh/bin/ $out/
  '';

  doTest = false;
}
