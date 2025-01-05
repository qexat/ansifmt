# 0.1.3

## Fixes

- Fixed a bug where unwanted `m` characters would appear in the output before every colored token

# 0.1.2

## Fixes

- Moved `Formatting.tokenize` and `Formatting.format` to a dedicated `Formatting.Util` submodule - it is common for users who wish to implement `TOKENIZABLE` to `open Formatting`, and `Formatting.tokenize` would then shadow the user's own function, making it cumbersome if the latter was recursive.
  This does not affect the `Ansifmt` prelude - `format` stays available.

# 0.1.1

## Features

- Added `print_formatted` and the `IO` submodule.

# 0.1.0

First pre-release.
