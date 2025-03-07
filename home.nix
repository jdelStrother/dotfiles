{ pkgs, unstable, ... }:

let
  emacs = pkgs.emacs-unstable-nox;
  emacsWithPackages = ((pkgs.emacsPackagesFor emacs).emacsWithPackages
    (epkgs: [ epkgs.vterm epkgs.treesit-grammars.with-all-grammars ]));
  # edit a dir/file in emacs, geared towards browsing third-party code
  # so opens in a temp workspace and sets up projectile to isolate just that directory.
  # (As opposed to opening node_modules/bootstrap and finding that, eg, `SPC SPC` tries to browse
  # the top-level project folder.
  emacsLauncher =
    pkgs.writeShellScriptBin "edit" (builtins.readFile ./bin/edit);
  git-recent =
    pkgs.writeScriptBin "git-recent" (builtins.readFile ./bin/git-recent);
  ruby = pkgs.ruby_3_4;
  fzf = pkgs.fzf.overrideAttrs (prev: {
    # Prevent shell integrations from installing automatically.
    # Otherwise fzf-key-bindings.fish gets sourced before we have opportunity to set FZF_CTRL_T_COMMAND=''
    postInstall = prev.postInstall + ''
      rm $out/share/fish/vendor_conf.d/load-fzf-key-bindings.fish
    '';
  });

in {
  imports = [ ./home-manager-apps.nix ];

  home.username = "jon";
  home.homeDirectory = "/Users/jon";
  home.stateVersion = "22.11";

  home.sessionVariables =
    let gemHome = "$HOME/.gem/ruby/${builtins.baseNameOf ruby}";
    in {
      EDITOR = "emacsclient --tty --alternate-editor=''";
      # BUNDLER_EDITOR = "${emacsLauncher}/bin/edit";

      GEM_HOME = gemHome;
      GEM_PATH = gemHome;
      PATH = "${gemHome}/bin:$PATH";
    };

  home.packages = [
    emacsLauncher
    git-recent
    ruby
    fzf
    pkgs.nodejs
    pkgs.php # for Alfred devdocs workflow

    pkgs.nix
    pkgs.home-manager
    unstable.devenv

    pkgs.awscli2
    # pkgs.awslogs
    pkgs.aws-vault
    pkgs.clang
    pkgs.coreutils
    pkgs.gist
    pkgs.git-absorb
    pkgs.gnugrep # macos grep is weird
    pkgs.gnused # macos sed is weird
    pkgs.zstd # doom-emacs uses zstd for some optimizations
    pkgs.codespell # for flymake-codespell

    unstable.jujutsu
    unstable.meld # at least until magit gets jujutsu support
    unstable.gg-jj

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
    pkgs.nixfmt-classic
    pkgs.cmake
    pkgs._1password-cli

    (pkgs.callPackage ./pkgs/macos-trash { })
    (pkgs.callPackage ./pkgs/pngpaste { })
    (pkgs.callPackage ./pkgs/scmpuff { })

    (pkgs.symlinkJoin {
      name = "emacs-and-deps";
      paths = [
        emacsWithPackages
        pkgs.nodePackages.typescript-language-server
        pkgs.graphviz
        pkgs.multimarkdown
      ];
    })
  ];

  programs.direnv.enable = true;
  programs.direnv.nix-direnv.enable = true;

  programs.git = {
    enable = true;
    userName = "Jonathan del Strother";
    userEmail = "me@delstrother.com";

    aliases = { amend = "commit --amend -C HEAD"; };
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
      name = "theme-bobthefish";
      src = pkgs.fetchFromGitHub {
        owner = "oh-my-fish";
        repo = "theme-bobthefish";
        rev = "2dcfcab653ae69ae95ab57217fe64c97ae05d8de";
        sha256 = "118hj100c4bb8hyhr22mrsjhg97pyd24cwb1l39bhryd0k9yc5lc";
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
    if test "$TERM_PROGRAM" = iTerm.app
      iterm2_shell_integration
    end

    # completion for git-lg
    complete --no-files -c git -a '(__fish_git_branches)' -n '__fish_git_using_command lg'

    # Allow gpg signing via the terminal if we connect over ssh
    # (Otherwise pinentry-mac will pop up a GUI window)
    if test -n "$SSH_CONNECTION"
      set GPG_TTY (tty)
      set PINENTRY_USER_DATA "USE_CURSES=1"
    end

    # iterm/ghostty provide 24 bit color, but that's not going to work on most ssh hosts...
    alias ssh="TERM=xterm-256color command ssh"

    set -g theme_color_scheme base16
    set -g theme_display_aws_vault_profile yes
    set -g theme_display_git_untracked no
    set -g theme_display_ruby no
    set -g theme_nerd_fonts yes

    set --erase fish_greeting

    if which scmpuff > /dev/null
      scmpuff init -s --shell=fish | source
    end

    # I want ctrl-t to transpose characters, not invoke fzf's file-finder
    set -gx FZF_CTRL_T_COMMAND ""
    fzf_key_bindings

    # load fish_prompt from bobthefish, then rewrite it to support jj
    fish_prompt
    source ${./fish/bobthefish_hacks_for_jj.fish};
  '';

  programs.fish.functions = {
    abssh = ''
      if test "$argv[1]" = "staging"
        set --erase argv[1]
        if test "$argv[1]" = "--web"
          set argv $argv[2..-1]
          set hosts staging1-1 staging1-2
        else if test "$argv[1]" = "--dj"
          set argv $argv[2..-1]
          set hosts staging1-3
        else if string match -- '--*' "$argv[1]"
          echo "unrecognized host option" 1>&2
          return 1
        else
          set hosts staging1-1 staging1-2 staging1-3
        end
      else
        if test "$argv[1]" = "production"
          set --erase argv[1]
        end
        if test "$argv[1]" = "--web"
          set argv $argv[2..-1]
          set hosts app1-1 app1-2 app1-3 app1-4 app1-5 dj1-1 dj1-2
        else if test "$argv[1]" = "--dj"
          set argv $argv[2..-1]
          set hosts app1-4 app1-5 dj1-1 dj1-2
        else if test "$argv[1]" = "--redis"
          set argv $argv[2..-1]
          set hosts redis1-1 redis1-2 redis1-3 ubuntu@app1-1 ubuntu@app1-2 ubuntu@app1-3
        else if string match -- '--*' "$argv[1]"
          echo "unrecognized host option" 1>&2
          return 1
        else
          set hosts dj1-1 dj1-2 app1-1 app1-2 app1-3 app1-4 app1-5 redis1-1 redis1-2 redis1-3
        end
      end

      pssh -H "$hosts" $argv
    '';
    aws-mfa = ''
      op read 'op://Employee/Amazon AWS/mfacode?attribute=otp'
    '';

    aws-credentials = ''
      set -e AWS_ACCESS_KEY_ID
      set -e AWS_SECRET_ACCESS_KEY
      set -e AWS_SESSION_TOKEN
      set token (aws-mfa); or return $status
      set temporary_credentials (aws sts get-session-token --token-code $token --serial-number arn:aws:iam::466056351294:mfa/jdelStrother)
      if test -n "$temporary_credentials"
        set -x -g AWS_ACCESS_KEY_ID (echo $temporary_credentials | jq -re '.Credentials.AccessKeyId')
        set -x -g AWS_SECRET_ACCESS_KEY (echo $temporary_credentials | jq -re '.Credentials.SecretAccessKey')
        set -x -g AWS_SESSION_TOKEN (echo $temporary_credentials | jq -re '.Credentials.SessionToken')
      end
    '';
    aws-login = ''
      # by default aws-vault generates session names with timestamps,
      # which is annoying for Quicksight which then sees every login as a fresh user
      export AWS_ROLE_SESSION_NAME=jdelStrother

      # url-encode the login url and use https://addons.mozilla.org/en-GB/firefox/addon/open-url-in-container/
      # to open it in a specific container
      set -l loginurl (aws-vault login --duration=8h --stdout "$argv[1]" | jq -sRr @uri)
      if test -z $loginurl
        return 1
      end

      set url $(printf 'ext+container:name=%s&url=%s' "aws-$argv[1]" "$loginurl")
      open -a Firefox.app "$url"
    '';
    aws-openvpn-permit = ''
      set groupid (aws ec2 describe-security-groups --filters 'Name=group-name,Values="OpenVPN Access Server"' --query 'SecurityGroups[0].GroupId' --output text)
      set myip (curl --silent ifconfig.me)
      aws ec2 authorize-security-group-ingress --group-id "$groupid" --protocol tcp --port 22 --cidr "$myip/32"
      echo "You should run this when done:"
      echo aws ec2 revoke-security-group-ingress --group-id "$groupid" --protocol tcp --port 22 --cidr "$myip/32"
    '';
    e = ''
      emacsclient -nw $argv
    '';
    webpack-analyze = ''
      if test -z "$argv[1]"
        echo "filename missing"
        return 1
      end

      env NODE_ENV=production node_modules/.bin/webpack --bail --config webpack.production.config.js  --profile --json > "/tmp/$argv[1]"
      and webpack-bundle-analyzer "/tmp/$argv[1]" ~/Developer/web/app/assets/javascripts/packages
    '';

    # eg `gemgrep 'google*' | xargs bundle update`
    gemgrep = ''
      ruby -rbundler -e "puts Bundler::LockfileParser.new(Bundler.read_file('Gemfile.lock')).specs.map(&:name).select{File.fnmatch(ARGV[0], _1)}" "$argv[1]"
    '';

    icat = ''
      if ! count $argv > /dev/null ;
        set argv "-"
      end
      for f in $argv
        ${pkgs.imagemagick}/bin/magick "$f" -resize '300x300>' - | ${unstable.kitty}/bin/kitten icat --align left
      end
    '';
  };

  home.file = {
    ".config/fish/completions/aws.fish".text = ''
      complete --command aws --no-files --arguments '(begin; set --local --export COMP_SHELL fish; set --local --export COMP_LINE (commandline); aws_completer | sed \'s/ $//\'; end)';
    '';

    ".config/fish/completions/git-lg.fish".text = ''
      complete --no-files -c git -a '(__fish_git_branches)' -n '__fish_git_using_command lg'
    '';

    ".config/fish/completions/rake.fish".text = ''
      function __get_rake_completions -d "Get rake completions"
        set tool rake
        if test -f bin/rake
          set tool bin/rake
        end
        $tool -T 2>&1 | sed -e "s/^rake \([a-z:_0-9!\-]*\).*#\(.*\)/\1	\2/"
      end
      complete -c rake --no-files -a "(__get_rake_completions)"
    '';

    ".config/fish/completions/just.fish".text = ''
      source (just --completions fish | psub)
    '';

    ".config/git/allowed_signers".text =
      "* ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL9JCPvve0m6vPjbO25OGkqk3w4kEqBNmg1dJ3kCj4zR";
  };
}
