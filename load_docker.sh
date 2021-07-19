docker load < $(nix-build ./nix/docker.nix -A kbtzim --option binary-caches "s3://ee-nixcache?region=ap-southeast-1" --option require-sigs false)

docker load < $(nix-build ./nix/docker.nix -A server --option binary-caches "s3://ee-nixcache?region=ap-southeast-1" --option require-sigs false)

