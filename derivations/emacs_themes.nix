{ emacsPackages, lib, fetchFromGitHub }: emacsPackages.trivialBuild {
  pname = "bennett-themes";
  src = fetchFromGitHub {
    owner = "bbenne10";
    repo = "emacs_themes";
    rev = "02221a63f0d8c28695b063ecc4c3244be72b2683";
    sha256 = "bN1FbXsNorC0zmW6AAQEfdn94XsOf6rlKbeXPC24318=";
  };
  postBuild = ''
    emacs -L . --batch -f batch-byte-compile themes/*.el
  '';
  postInstall = ''
    install themes/*.el themes/*.elc $out/share/emacs/site-lisp
  '';
}
