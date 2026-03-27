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

      perSystem = {pkgs, config, ...}: {
        pre-commit.settings.hooks = {
          lint = {
            enable = true;
            name = "lint";
            entry = "just lint";
            pass_filenames = false;
          };
          tests = {
            enable = true;
            name = "tests";
            entry = "just tests";
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

            # Markdown / text
            mdl
            prettier

            # JSON / JS
            biome
            nodejs
          ];
          shellHook = ''
            ${config.pre-commit.installationScript}
          '';
        };
      };
    };
}
