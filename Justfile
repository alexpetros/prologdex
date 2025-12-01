run:
  scryer-prolog ./db/prologdex.pro

generate-dex:
  ./scripts/create-dex.js > ./db/dex.pro
