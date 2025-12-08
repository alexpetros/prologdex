build:
  wasm-pack build --target web --no-default-features

serve:
  python3 -m http.server -d www

dex:
  scryer-prolog ./dex/prologdex.pl

generate-dex:
  node ./scripts/create-dex.js > ./dex/dex.pl
