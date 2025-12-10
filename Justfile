db:
  scryer-prolog ./db/prologdex.pl

build:
  wasm-pack build --target web --no-default-features

serve:
  python3 -m http.server -d www

generate-dex:
  node ./scripts/generate-dex.js
