{
  outputs = {...}: {
    elisp-rice = {
      packages = [
        "org-reverse-datetree"
      ];
      tests = {
        buttercup.enable = true;
      };
    };
  };
}
