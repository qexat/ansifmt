# 0.3.0

## Features

- Add `Custom` token type variant which takes a styling, for tokens without particular semantics.
- Add `Formatting.Element` that supersedes `Formatting.Tree`.
- Add `Formatting.Interfaces.TO_ELEMENT` interface which establishes the contract to convert to a formatting element that is used by formatting and printing utilitary functions such as `format` and `IO.print_formatted`.
- Expose the `Int8` module that is used by `Color`.

## Removed

- Remove `Formatting.Tree`, `Formatting.TOKENIZABLE` and its associated functions. Use `Formatting.Element` instead.
- Remove the `Prelude` module. It has merged with the core `Ansifmt` module.
- Remove `print_formatted` alias from the prelude. The function can still be found as `IO.print_formatted`.
- Remove `make_styling` alias from the prelude. The function can still be found as `Styling.create`.

## Internal

- Renamed `Utils` to `Internal`.
- `Formatting` is now a directory instead of a file containing all its submodules.
- Added `List.intersperse` and `List.singleton` (used in `Formatting.Element`).
- Added `Bool.tautology` (used in `Formatting.Element`).

# 0.2.0

## Features

- Add color functions `luminance` and `best_for_contrast`. (by @chshersh in #5)
- Add styling functions `fg`, `bg`, `bold`, `dim`, `italic`, `underlined`. (by @chshersh in #6)
- Add styling composition with `&`. (by @chshersh in #6)
- Add function `make_rgb_hex` to parse hexadecimal codes into RGB colors. (by @chshersh in #7)

## Breaking Changes

- `Color.t` and `Color.Ground.t` are now polymorphic variants. (by @chshersh in #5)

## Removed

- `Util.parenthesize_if` which is deemed unuseful and is kind of a duplicate of `Tree.parenthesize_if` in `Formatting`.

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
