{
  config,
  pkgs,
  unstable,
  ...
}:

let
  # emacs = pkgs.emacs-unstable; # build from latest tag
  emacs = pkgs.emacs-git; # build from latest master
  emacsWithPackages = (
    (pkgs.emacsPackagesFor emacs).emacsWithPackages (epkgs: [
      epkgs.vterm
      epkgs.treesit-grammars.with-all-grammars
    ])
  );
  # edit a dir/file in emacs, geared towards browsing third-party code
  # so opens in a temp workspace and sets up projectile to isolate just that directory.
  # (As opposed to opening node_modules/bootstrap and finding that, eg, `SPC SPC` tries to browse
  # the top-level project folder.
  emacsLauncher = pkgs.writeShellScriptBin "edit" (builtins.readFile ./bin/edit);
  git-recent = pkgs.writeScriptBin "git-recent" (builtins.readFile ./bin/git-recent);
  jj-pr = pkgs.writeScriptBin "jj-pr" (builtins.readFile ./bin/jj-pr);
  ruby = pkgs.ruby_3_4;
  dotfiles = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles";

in
{
  imports = [
    ./home-manager-apps.nix
    ./fish
  ];

  home.username = "jon";
  home.homeDirectory = "/Users/jon";
  home.stateVersion = "22.11";

  home.sessionVariables =
    let
      gemHome = "$HOME/.gem/ruby/${builtins.baseNameOf ruby}";
    in
    {
      EDITOR = "emacsclient --tty --alternate-editor=''";
      # BUNDLER_EDITOR = "${emacsLauncher}/bin/edit";

      GEM_HOME = gemHome;
      GEM_PATH = gemHome;
      PATH = "${gemHome}/bin:$PATH";
    };

  home.packages = [
    emacsLauncher
    git-recent
    jj-pr
    ruby
    unstable.fzf
    pkgs.nodejs
    pkgs.php # for Alfred devdocs workflow

    pkgs.nix
    pkgs.home-manager
    unstable.devenv

    pkgs.awscli2
    # pkgs.awslogs
    pkgs.aws-vault
    pkgs.clang
    pkgs.clang-tools # for clangd lsp
    pkgs.coreutils
    pkgs.gist
    pkgs.git-absorb
    pkgs.gnugrep # macos grep is weird
    pkgs.gnused # macos sed is weird

    # emacs deps. Could maybe be siloed into emacs, but YOLO
    pkgs.zstd # doom-emacs uses zstd for some optimizations
    pkgs.codespell # for flymake-codespell
    pkgs.typescript-language-server

    unstable.jujutsu
    unstable.meld
    unstable.mergiraf
    pkgs.gg-jj
    pkgs.difftastic

    pkgs.gh
    pkgs.gnupg
    pkgs.pinentry_mac
    pkgs.jq
    (unstable.llm.override { enable-llm-anthropic = true; })
    pkgs.niv
    pkgs.parallel
    pkgs.pssh
    pkgs.ripgrep
    # common dependencies for gem installs (nokogiri)
    # You'll need `gem install nokogiri -- --use-system-libraries` and/or `bundle config build.nokogiri --use-system-libraries`
    pkgs.zlib
    pkgs.libiconv
    pkgs.libxml2
    pkgs.rdbtools
    pkgs.go
    pkgs.tmux
    pkgs.shellcheck
    pkgs.nixfmt-rfc-style
    pkgs.nil # nix lsp
    pkgs.cmake
    pkgs._1password-cli

    (pkgs.callPackage ./pkgs/macos-trash { })
    (pkgs.callPackage ./pkgs/pngpaste { })
    (pkgs.callPackage ./pkgs/scmpuff { })
  ];

  programs.emacs.enable = true;
  programs.emacs.package = emacsWithPackages;

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  programs.git = {
    enable = true;
    userName = "Jonathan del Strother";
    userEmail = "me@delstrother.com";

    aliases = {
      amend = "commit --amend -C HEAD";
    };
    ignores = [
      ".claude"
      ".devenv"
      ".DS_Store"
      ".env"
      ".projectile"
      ".dir-locals.el"
    ];
    extraConfig = {
      core.editor = "vim";
      github.user = "jdelStrother";
      init.defaultBranch = "main";
      pull.ff = "only";
      # I keep getting http-408 errors on pushing to Github. Supposedly this fixes it.
      http.postBuffer = 524288000;

      # seems problematic with `doom sync`
      # core.untrackedCache = true;
      rebase.updateRefs = true;

      # make it explicit so that jj picks it up
      credential.helper = "osxkeychain";

      commit.gpgsign = true;
      gpg.format = "ssh";
      gpg.ssh.allowedSignersFile = "~/.config/git/allowed_signers";
      user.signingkey = "~/.ssh/id_ed25519.pub";
    };
  };

  xdg.configFile."git/allowed_signers".text =
    "* ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL9JCPvve0m6vPjbO25OGkqk3w4kEqBNmg1dJ3kCj4zR";

  xdg.configFile."jj/config.toml".source = "${dotfiles}/jj.toml";
}
