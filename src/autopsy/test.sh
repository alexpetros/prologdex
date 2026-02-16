#!/bin/bash
set -uo pipefail

for file in ./logs/*.log; do
  scryer-prolog ./autopsy.pl -g print -- "$file" > /dev/null
  if [ "$?" -ne 0 ]; then
    echo $file failed;
  else
    echo $file succeed;
  fi;
done
