build:
  wasm-pack build --target web --no-default-features

serve:
  python3 -m http.server

dex:
  scryer-prolog ./dex/prologdex.pro

generate-dex:
  ./scripts/create-dex.js > ./dex/dex.pro
