#!/bin/bash
set -euo pipefail

for file in ./logs/*.log; do
  scryer-prolog ./autopsy.pl -g battle -t halt -- "$file" > /dev/null
  if [ "$?" -ne 0 ]; then
    echo $file failed;
  else
    echo $file succeed;
  fi;
done
