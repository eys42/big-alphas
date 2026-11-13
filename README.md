# Team 9: big ['a]s

## Team members

Ethan Song (eys42)

Stephanie Chen (sc2927)

Carissa Zheng (caz39)

Kanat Schoeberlein (kts59)

Charles Xue (cx272)

# Testing

## Including a test suite

To include your test suite, add your test suite filename in the `names` stanza of `test/dune`:

`(tests (names test_board test_cli_display INSERT_TEST_FILENAME) ...)`

Then, `dune test` will execute all test files in the `test/` directory.

## Testing with coverage

If not already installed, run:

`opam install bisect_ppx` and
`opam install batteries`

Run an individual test suite with the command:

`dune exec --instrument-with bisect_ppx test/INSERT_TEST_FILENAME.exe`

This will generate files that look like `bisect*.coverage`.

You can run multiple test files with the bisect instrumentation to generate coverage files.

Run `bisect-ppx-report html` to generate an HTML report of the coverage of all test suites for which you generated `bisect*.coverage` files.

Run `open _coverage/index.html` to open the HTML report in the browser (macOS only).

