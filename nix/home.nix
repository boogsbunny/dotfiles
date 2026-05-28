{ config, pkgs, ... }: {
  home.username = "root";
  home.homeDirectory = "/root";

  home.packages = import ./home-packages.nix { inherit pkgs; };

  programs.home-manager.enable = true;

  programs.git.enable = true;
  programs.neovim.enable = true;
  programs.fzf.enable = true;
  programs.zellij.enable = true;

  # Silence neovim warnings
  programs.neovim.withRuby = false;
  programs.neovim.withPython3 = false;

  programs.bash.enable = false;

  home.stateVersion = "24.11";
}
