{ emacsPackages, lib, fetchFromGitHub }: emacsPackages.trivialBuild {
  pname = "everforest";
  src = fetchFromGitHub {
    owner = "Theory-of-Everything";
    repo = "everforest-emacs";
    rev = "703b16b742b753f6ad077b5c7f51947d1926c530";
    sha256 = "3dZ2LMv0esbzJvfrtWWbO9SFotXj3UeizjMxO6vs73M=";
  };
  preBuild = ''
    ls -lah .
    false
    # rm everforest.el
    # cp editors/emacs-WIP/*.el .
  '';
}
