{ stdenv, fetchFromGitHub, ... }: stdenv.mkDerivation {
  name = "theme.sh";
  version = "1.1.6";
  srcs = [
    (fetchFromGitHub {
      owner = "lemnos";
      repo = "theme.sh";
      rev = "326a43a215bd5d1e7e8a56939e48cc1a3acfe271";
      sha256 = "lSJ2Gp/rdYbZRh9Ecn/cm26ual9SKXbN+UVIrjfCXNc=";
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
