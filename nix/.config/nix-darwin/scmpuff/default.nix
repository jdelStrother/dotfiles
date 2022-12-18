{ config, lib, pkgs, ... }:

pkgs.buildGoModule {
  pname = "scmpuff";
  version = "0.5.0";

  vendorSha256 = "sha256-7WHVSEz3y1nxWfbxkzkfHhINLC8+snmWknHyUUpNy7c=";
  src = pkgs.fetchFromGitHub {
    owner = "mroth";
    repo = "scmpuff";
    rev = "8a7a6514486e64c4a94473da91f783b57e6d775d";
    hash="sha256-+L0W+M8sZdUSCWj9Ftft1gkRRfWMHdxon2xNnotx8Xs";
  };
  checkPhase = "";
}
