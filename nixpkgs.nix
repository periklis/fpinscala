let
  _pkgs = import <nixpkgs> {};

  hcNixpkgs = _pkgs.fetchgit {
    url = "https://898c7d674e1fa6c31e2c0a530e040c191dea9101@github.hc.ag/hc-nix/hc-nixpkgs.git";
    rev = "027c157d55bb73b454013fee94e80589bebfdaf3";
    sha256 = "06qzccgd8kmwvgl7qqkb1i9sgaa0a2nvcz1g64q74i9kyaa2fxl8";
  };

  hcNixpkgsOverlays = import "${hcNixpkgs}/overlays.nix" {};

  pinVersionsOverlay = self: super: {
    elasticsearch = super.elasticsearch5;
  };
in
  import (_pkgs.fetchFromGitHub {
    owner = "nixos";
    repo = "nixpkgs";
    rev = "1a8a95e87962bc8ff8514b28e026fc987fbdb010";
    sha256 = "1rx14g8wlw6vdjalsv6rnznmfsazwf218rv75z9ac2vdgwihclxh";
}) { overlays = [ pinVersionsOverlay hcNixpkgsOverlays ]; }
