PROLOGDEX_FILE := "./db/prologdex.pl"
TEST_FILE := "./test.pl"

db:
  scryer-prolog {{ PROLOGDEX_FILE }}

# Running this currently requires building from source, because "-t" is unreleased
# https://github.com/mthom/scryer-prolog/pull/3147
test:
  scryer-prolog {{ TEST_FILE }} -g 'run' -t halt

build:
  wasm-pack build --target web --no-default-features

serve:
  python3 -m http.server -d www

generate-dex:
  node ./scripts/generate-dex.js
