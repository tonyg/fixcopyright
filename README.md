# fixcopyright.rkt

A small command-line tool for adding and/or updating [SPDX](https://spdx.org/) summaries in
source files. You could use it as a git hook (perhaps with `-n`/`--dry-run`) to lint files
before commit.

For example, in one of my Racket projects, I have

    fixcopyright.rkt -n --preset-racket LGPL-3.0-or-later

in a `pre-commit` git hook, and

    fixcopyright.rkt --preset-racket LGPL-3.0-or-later

as a script in the `Makefile`; and in a TypeScript project, I have

    fixcopyright.rkt --preset-typescript --file-pattern 'packages/**.ts'
    fixcopyright.rkt --preset-javascript --file-pattern 'packages/**.js'

as an available script.
