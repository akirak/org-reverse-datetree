{ pkgs ? import <nixpkgs> {},
  emacs ? (import (builtins.fetchTarball "https://github.com/purcell/nix-emacs-ci/archive/master.tar.gz")).emacs-26-1
}:
let
  check-package = import (builtins.fetchTarball "https://github.com/akirak/emacs-package-checker/archive/v1.tar.gz");
in
{
  org-reverse-datetree = check-package {
    inherit emacs pkgs;
    name = "org-reverse-datetree";
    src = ./.;
    targetFiles = ["org-reverse-datetree.el"];
    emacsPackages = epkgs: (with epkgs.melpaPackages; [
      dash
    ]);
  };
}
