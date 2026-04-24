{
  description = "org-mcp development environment";

  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1";
    flake-parts.url = "https://flakehub.com/f/hercules-ci/flake-parts/0.1";
    git-hooks-nix.url = "github:cachix/git-hooks.nix";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [inputs.git-hooks-nix.flakeModule];

      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem = {
        pkgs,
        config,
        ...
      }: {
        pre-commit.settings.hooks = {
          # Single combined hook so pre-commit only emits one status line
          # (`check.....Passed`) on success.  The recipe runs lint first,
          # then tests, and aborts on the first failure.
          check = {
            enable = true;
            name = "check";
            entry = "just check";
            pass_filenames = false;
          };
        };

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            # Emacs / eask
            emacs
            eask-cli

            # CI runner
            act

            # Task runner
            just

            # Shell checks
            shellcheck
            shfmt

            # GitHub Actions checks
            # checkov
            zizmor
          ];
          shellHook = ''
            ${config.pre-commit.installationScript}
          '';
        };
      };
    };
}
