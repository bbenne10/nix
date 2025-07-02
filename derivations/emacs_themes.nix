{ emacsPackages, emacs_themes_src }:
emacsPackages.trivialBuild {
  pname = "bennett-themes";
  version = "1.0";
  src = emacs_themes_src;
  postBuild = ''
    emacs -L . --batch -f batch-byte-compile themes/*.el
  '';
  postInstall = ''
    install themes/*.el themes/*.elc $out/share/emacs/site-lisp
  '';
}
