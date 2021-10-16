echo "Building Client..."
nix-build ./nix/snowman.nix -A build --arg isJS true -o ui --option binary-caches "s3://ee-nixcache?region=ap-southeast-1" --option require-sigs false & C=$!

echo "Building Server..."
nix-build -A chopaan.server -o server --option binary-caches "s3://ee-nixcache?region=ap-southeast-1" --option require-sigs false & S=$!

wait $C $S

echo "Running Server with Client"

./server/bin/server --assets ./ui/bin/ui.jsexe --tinkerHost "localhost" --tinkerPort 8182
