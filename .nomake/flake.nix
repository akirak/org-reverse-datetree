{
  description =
    "THIS IS AN AUTO-GENERATED FILE. PLEASE DON'T EDIT IT MANUALLY.";
  inputs = {
    buttercup = {
      flake = false;
      owner = "jorgenschaefer";
      repo = "emacs-buttercup";
      type = "github";
    };
    dash = {
      flake = false;
      owner = "magnars";
      repo = "dash.el";
      type = "github";
    };
    org = {
      flake = false;
      ref = "bugfix";
      type = "git";
      url = "git://git.sv.gnu.org/emacs/org-mode.git";
    };
    org-reverse-datetree = {
      flake = false;
      owner = "akirak";
      repo = "org-reverse-datetree";
      type = "github";
    };
  };
  outputs = { ... }: { };
}
