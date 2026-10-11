# Copilot Instructions

Small Common Lisp library (ASDF system `jsonx`) extending `cl-json`. Dependencies: alexandria, cl-json, closer-mop, quri. Tests use FiveAM (system `jsonx-test`); no linter or build script exists.

## Load / try it

From a REPL with the repo on the ASDF path (e.g. under `~/quicklisp/local-projects`):

```lisp
(ql:quickload "jsonx")   ; or (asdf:load-system "jsonx")
(jsonx:with-decoder-jrm-semantics (json:decode-json-from-string "{\"a\":[true,null]}"))
```

## Tests

```lisp
(asdf:test-system "jsonx-test")            ; full suite; errors on failure
(ql:quickload "jsonx-test")
(fiveam:run! 'jsonx-test::decode-literals) ; single test (or 'jsonx-test:jsonx-suite)
```

Headless SBCL (repo dir pushed onto `asdf:*central-registry*` if not in local-projects):

```
sbcl --noinform --non-interactive --load ~/quicklisp/setup.lisp --eval '(push (truename ".") asdf:*central-registry*)' --eval '(asdf:test-system "jsonx-test")'
```

Tests live in `jsonx-test.lisp` (package `JSONX-TEST`, suite `JSONX-SUITE`). cl-json escapes `/` as `\/` when encoding, so compare strings after decoding rather than against literal encoded text.

## Architecture

- `package.lisp` defines package `JSONX` (uses `ALEXANDRIA` and `CL`) and the export list. Any new public symbol must be added to `:export` there.
- `jsonx.lisp` holds everything else:
  - `json-literal` class with singletons `+json-true+`, `+json-false+`, `+json-null+`, plus `+json-empty-list+` (empty vector) and `+json-empty-object+` (empty hash table). `json:encode-json` is specialized so literals print as `true`/`false`/`null`.
  - `set-decoder-jrm-semantics` / `with-decoder-jrm-semantics` reconfigure cl-json's decoder: true/false/null decode to the literal singletons, arrays to vectors, objects to hash tables (empty object → `+json-empty-object+`). This relies on cl-json internals (`cl-json::` symbols), so changes need to be checked against the cl-json version in use.
  - Also adds `cl-json:encode-json` methods for functions, packages, pathnames, `quri:uri` (all as strings) and structure objects (as JSON objects), plus `dehashify` (hash tables → alists, recursively).
  - `jsonx.lisp` runs a load-time smoke test (`assert`s inside `eval-when`); a load-time failure means the decoder semantics broke.
- `jsonx.asd` lists components; the `package` file must precede `jsonx`. `jsonx-test.asd` defines the test system.

## Conventions

- `json-literal`'s `VALUE` slot deliberately has no `:reader`/`:accessor`; literals are atomic and immutable. Access it with `slot-value` internally only.
- Files begin with `;;; -*- Lisp -*-`; source lives in `(in-package "JSONX")` with uppercase string package designators.
- Editor backup files (`*~`) are git-ignored; ignore `package.lisp~` and `jsonx.asd~`.
