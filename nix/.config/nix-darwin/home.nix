{ pkgs, ... }:

{
  imports = [./home-manager-apps.nix];
  home.stateVersion = "22.11";
  home.packages = [
    pkgs.ruby_3_1
    pkgs.nodejs-16_x
    pkgs.php # for Alfred devdocs workflow

    pkgs.awscli2
    pkgs.nixUnstable
    # pkgs.awslogs
    pkgs.aws-vault
    pkgs.clang
    pkgs.coreutils
    pkgs.fzf
    pkgs.gist
    pkgs.gnugrep #macos grep is weird
    pkgs.gnused # macos sed is weird
    pkgs.graphviz # dot for emacs/roam

    pkgs.gnupg pkgs.pinentry

    pkgs.jq
    pkgs.niv

    pkgs.parallel
    pkgs.pssh
    pkgs.ripgrep
    # common dependencies for gem installs (nokogiri)
    # You'll need `gem install nokogiri -- --use-system-libraries` and/or `bundle config build.nokogiri --use-system-libraries`
    pkgs.zlib pkgs.libiconv pkgs.libxml2
    pkgs.rdbtools
    pkgs.go
    pkgs.tmux
    pkgs.oathToolkit
    pkgs.shellcheck
    pkgs.nixfmt
    pkgs.cmake

    pkgs.nodePackages.typescript-language-server # for emacs lsp

    (pkgs.callPackage ./pkgs/macos-trash {})
    (pkgs.callPackage ./pkgs/pngpaste {})
    (pkgs.callPackage ./pkgs/scmpuff {})

    pkgs.emacs
  ];
  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  programs.fish.enable = true;
  programs.fish.plugins = [
    {
      name = "z";
      src = pkgs.fetchFromGitHub {
        owner = "jethrokuan";
        repo = "z";
        rev = "e0e1b9dfdba362f8ab1ae8c1afc7ccf62b89f7eb";
        sha256 = "0dbnir6jbwjpjalz14snzd3cgdysgcs3raznsijd6savad3qhijc";
      };
    }
    {
      name = "iterm2_shell_integration";
      src = ./fish/iterm2_shell_integration;
    }
  ];

  programs.fish.shellInit = ''
      if test -d /opt/homebrew
        # Apple Silicon homebrew
        /opt/homebrew/bin/brew shellenv | source
      else
        # Intel homebrew
        /usr/local/bin/brew shellenv | source
      end

      # Force these paths to take precedence over homebrew
      fish_add_path --prepend --global ~/bin ~/go/bin ~/.npm/bin

      ### Add nix binary paths to the PATH
      # Perhaps someday will be fixed in nix or nix-darwin itself
      # https://github.com/LnL7/nix-darwin/issues/122
      if test (uname) = Darwin
        fish_add_path --prepend --global \
          "$HOME/.nix-profile/bin" \
          "/etc/profiles/per-user/$USER/bin" \
          /nix/var/nix/profiles/default/bin \
          /run/current-system/sw/bin
      end
    '';

  programs.fish.interactiveShellInit = ''
    iterm2_shell_integration

    # completion for git-lg
    complete --no-files -c git -a '(__fish_git_branches)' -n '__fish_git_using_command lg'

    # Allow gpg signing via the terminal if we connect over ssh
    # (Otherwise pinentry-mac will pop up a GUI window)
    if test -n "$SSH_CONNECTION"
      set GPG_TTY (tty)
      set PINENTRY_USER_DATA "USE_CURSES=1"
    end

    # I use 24 bit color in iterm, but that's not going to work on most ssh hosts...
    alias ssh="TERM=xterm-256color command ssh"

    set -g theme_display_aws_vault_profile yes

    if which scmpuff > /dev/null
      scmpuff init -s --shell=fish | source
    end
  '';
}
