build:
  wasm-pack build --target web --no-default-features

serve:
  python3 -m http.server -d www

db:
  scryer-prolog ./db/prologdex.pl

generate-dex:
  node ./scripts/generate-dex.js
