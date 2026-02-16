/\|move\|/ {
  move[$4] += 1
}

/\[miss\]/ {
  misses[$4] += 1
}

END {
  for (missedMove in misses) {
    print missedMove " " ((misses[missedMove] / move[missedMove])*100) "%"
  }
}
