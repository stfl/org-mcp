{
  description = "org-mcp development environment";

  inputs = {
    nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1";
    flake-parts.url = "https://flakehub.com/f/hercules-ci/flake-parts/0.1";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem = {pkgs, ...}: {
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
            textlint
            textlint-rule-terminology

            # JSON / JS
            biome
            nodejs
          ];
        };
      };
    };
}
