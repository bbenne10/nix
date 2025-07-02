{ emacsPackages, fetchFromGitHub }:
emacsPackages.trivialBuild {
  pname = "modern-tab-bar";
  version = "2025-03-26";
  src = fetchFromGitHub {
    owner = "aaronjensen";
    repo = "emacs-modern-tab-bar";
    rev = "aa01bed4ae7b00cfaa21e044062703d5cad7a065";
    hash = "sha256-tcnG1WTod9FqKrLnL+yINjS6sRwp83iFe8ZzApXlxQI=";
  };

  postBuild = ''
    emacs -L . --batch -f batch-byte-compile modern-tab-bar.el
  '';

  postInstall = ''
    install modern-tab-bar.el modern-tab-bar.elc $out/share/emacs/site-lisp
  '';
}
