# Team 9: big ['a]s

## Team members

Ethan Song (eys42)

Stephanie Chen (sc2927)

Carissa Zheng (caz39)

Kanat Schoeberlein (kts59)

Charles Xue (cx272)

# Testing with coverage

If not already installed, run:

`opam install bisect_ppx` and
`opam install batteries`

Run the test suite with the command:

`dune exec --instrument-with bisect_ppx test/test_big_alphas.exe`

This will generate files that look like `bisect*.coverage`.

Run `bisect-ppx-report html` to generate an HTML report of the coverage.

Run `open _coverage/index.html` to open the HTML report in the browser (macOS only).

