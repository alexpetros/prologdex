PROLOGDEX_FILE := "./src/prologdex.pl"
AUTOPSY_FILE := "./src/autopsy.pl"
TEST_FILE := "./test.pl"

db:
  scryer-prolog {{ PROLOGDEX_FILE }}

autopsy:
  scryer-prolog {{ AUTOPSY_FILE }}

# Running this currently requires building from source, because "-t" is unreleased
# https://github.com/mthom/scryer-prolog/pull/3147
test:
  scryer-prolog {{ TEST_FILE }} -g 'run' -t halt

# build:
#   wasm-pack build --target web --no-default-features

serve:
  python3 -m http.server -d www

generate-dex:
  node ./generator/generate-dex.js

download:
  scryer-prolog ./src/autopsy/download-logs.pl -g run -t halt

unknowns:
  find ./logs -name '*.log' | xargs scryer-prolog autopsy.pl -g unknown -- | cut -d '|' -f 1 | sort | uniq -c | sort -nr

print-all:
  scryer-prolog {{ AUTOPSY_FILE }} -g print -- ./logs/*.log

test-autopsy:
  ./src/autopsy/test.sh
