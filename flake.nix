{
  description = "org-reverse-datetree package for Emacs";

  inputs = {
    gnu-elpa = {
      url = "git+https://git.savannah.gnu.org/git/emacs/elpa.git?ref=main";
      flake = false;
    };
    melpa = {
      url = "github:melpa/melpa";
      flake = false;
    };

    elinter = {
      url = "github:akirak/elinter/v5";
      inputs.gnu-elpa.follows = "gnu-elpa";
      inputs.melpa.follows = "melpa";
    };
  };

  outputs =
    { self
    , elinter
    , ...
    } @ inputs:
    elinter.lib.mkFlake {
      src = ./.;
      lockDirName = "lock";
      localPackages = [
        "org-reverse-datetree"
      ];
      extraPackages = [
        "buttercup"
        # Emacs 26.2 or before requires installation of a newer version of Org
        "org"
      ];
      scripts = {
        test = {
          description = "Run buttercup tests";
          compile = true;
          text = ''
            emacs -batch -l buttercup -f buttercup-run-discover "$PWD"
          '';
        };
      };
    };
}
