{
  config,
  pkgs,
  unstable,
  ...
}:

let
  emacsSrc = pkgs.emacs-unstable; # build from latest tag
  # emacsSrc = pkgs.emacs-git; # build from latest master
  emacs = emacsSrc.overrideAttrs (old: rec {
    # fix building in sandbox https://github.com/NixOS/nixpkgs/issues/520441#issuecomment-5085239665
    postPatch = old.postPatch + ''
      substituteInPlace lisp/gnus/smime.el --replace-fail '(car (gnutls-trustfiles))' '"/etc/ssl/cert.pem"'
    '';
    # eglot+ruby-lsp makes it very easy to blow past the 1024 open file limit,
    # since it sets up watchers for every subdirectory of the project, for each of ruby-lsp, rubocop, and workspace-watcher
    # We can redefine FD_SETSIZE to allow more open files
    configureFlags = old.configureFlags ++ [ "CFLAGS=-DFD_SETSIZE=20000" ];
    # Avoid squircle jail
    postInstall = old.postInstall + ''
      cp ${./icons/Emacs.icns} $out/Applications/Emacs.app/Contents/Resources/Emacs.icns
    '';
  });
  emacsWithPackages = (pkgs.emacsPackagesFor emacs).emacsWithPackages (epkgs: [
    epkgs.treesit-grammars.with-all-grammars
  ]);
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
    ./fish
  ];

  home.username = "jon";
  home.homeDirectory = "/Users/jon";
  home.stateVersion = "26.05";

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

      DOCKER_CLI_HINTS = "false";
    };

  home.packages = [
    emacsLauncher
    git-recent
    jj-pr
    ruby
    pkgs.nodejs
    pkgs.php # for Alfred devdocs workflow

    pkgs.home-manager
    unstable.devenv

    pkgs.awscli2
    unstable.aws-vault # unstable has the Byteness fork, which is the newer 'official' one
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
    pkgs.nixfmt
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
    ignores = [
      ".agent-shell"
      ".claude"
      ".devenv"
      ".devenv*"
      ".direnv"
      "devenv.local.nix"
      ".DS_Store"
      ".env"
      ".projectile"
      ".dir-locals.el"
    ];
    settings = {
      user.name = "Jonathan del Strother";
      user.email = "me@delstrother.com";
      user.signingkey = "~/.ssh/id_ed25519.pub";
      alias = {
        amend = "commit --amend -C HEAD";
      };
      # core.editor = "vim";
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
    };
  };

  xdg.configFile."git/allowed_signers".text =
    "* ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL9JCPvve0m6vPjbO25OGkqk3w4kEqBNmg1dJ3kCj4zR";

  xdg.configFile."jj/config.toml".source = "${dotfiles}/jj.toml";
}
