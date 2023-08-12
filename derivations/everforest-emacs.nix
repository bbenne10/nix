{ emacsPackages, lib, fetchFromGitHub }: emacsPackages.trivialBuild {
  pname = "everforest";
  src = fetchFromGitHub {
    owner = "bbenne10";
    repo = "everforest-emacs";
    rev = "232fb60a61fbb7897a98df5619c0086b83da82ed";
    sha256 = "fKsjgAg5naN+t6Fx6XxDBlm2Uhy5oc6XavvlfZDbP5w=";
  };
  preBuild = ''
    rm everforest.el
    mkdir themes/
    mv *.el themes/
    cat << EOF > everforest.el
        ;;;###autoload
        (when (and (boundp 'custom-theme-load-path) load-file-name)
        (let* ((base (file-name-directory load-file-name))
                (dir (expand-file-name "themes/" base)))
            (add-to-list 'custom-theme-load-path
                        (or (and (file-directory-p dir) dir)
                            base))))
        (provide 'everforest)
    EOF
  '';
  postBuild = ''
    emacs -L . --batch -f batch-byte-compile themes/*.el
  '';
  postInstall = ''
    install themes/*.el themes/*.elc $out/share/emacs/site-lisp
  '';
}
