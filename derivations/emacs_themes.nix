{ emacsPackages, lib, fetchFromGitHub, emacs_themes }: emacsPackages.trivialBuild {
  pname = "bennett-themes";
  version = "1.0";
  src = emacs_themes;
  postBuild = ''
    emacs -L . --batch -f batch-byte-compile themes/*.el
  '';
  postInstall = ''
    install themes/*.el themes/*.elc $out/share/emacs/site-lisp
  '';
}
