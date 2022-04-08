let 
  pkgs = import <nixpkgs> { };
in 
  pkgs.haskellPackages.developPackage {
    root = builtins.path { path = ./.; name = "pandoc-logic-proof"; };
  }

