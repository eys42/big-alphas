Valid CLI input displays both boards based on the placeholder board created in main.ml

  $ ../bin/main.exe --players 2 --difficulty easy | grep -E "^(Your board:|Opponent's board:)"
  Your board:
  Opponent's board:

Check that both boards display their coordinate headers.

  $ ../bin/main.exe --players 2 --difficulty easy | grep -c '^      0'
  2

Check that both 5x5 boards display all five rows.

  $ ../bin/main.exe --players 2 --difficulty easy | grep -c '^  [0-4] '
  10
