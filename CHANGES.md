# 1.0.0

**1.0.0 is a complete rewrite of ansifmt**.

At this stage, ansifmt has been fully rewritten. It now includes a test suite. This version is entirely incompatible with ansifmt 0.3.0 and older. Notably, some of its features were purposefully lost in this rewrite.
Users should expect a new library on top of this one to replace them in the future.

## Changes

- `ansifmt` is now split in two modules: `Ansi`, which provides APIs for escape sequences (in `Ansi`), attributes as a low-level representation (in `Ansi.Attribute`) and terminal-compatible colors (in `Ansi.Color`), and `Fmt`, which provides a pretty-printable string API built on top of `Ansi`.
- The `Color` API has been simplified. Colors can now be constructed using built-in integers ; they will be normalized-on-demand. `Advanced` is now called `Basic`. `Minimal` has been dropped as it is already covered by `Basic`, assuming that modern terminal emulators support 8-bit colors.
- The `Styling` API (now called `Ansi`) has been tremendously simplified and is now based on a choice type instead of a record type.
- `parse_hex` is now `of_hex_repr`.
- Luminance calculation now linearizes properly the color.

## Features

- A new `Fmt` API is available, which allows to construct strings with specific parts stylized using `Ansi`. This includes a convenient function, `Fmt.print`, to print in the terminal, stripping escape sequences when the output channel is not a TTY (by default -- this is configurable).
- Color parsing now supports deserialization of strings of the form `rgb(r, g, b)`.
- `Ansi`, `Attributes`, `Color` and `Fmt` objects are serializable.
- `Ansi` objects are now deserializable.

## Removed

- The `Formatting` API has been fully dropped. We estimate that `ansifmt` is not the right place to put this in. Instead, we are working on another library, built on top of `ansifmt`, that will provide these features in an improved manner.
- The `IO` API has been removed following `Formatting`.

## Performance

No benchmark has been done, so this should be taken with a grain of salt, but it is likely that the new version exhibits better performance, due to simplification of implementation and lightening of the library overall.

# 0.3.0

## Features

- Add `Custom` token type variant which takes a styling, for tokens without particular semantics.
- Add `Formatting.Element` that supersedes `Formatting.Tree`.
- Add `Formatting.Interfaces.TO_ELEMENT` interface which establishes the contract to convert to a formatting element that is used by formatting and printing utilitary functions such as `format` and `IO.print_formatted`.
- Expose the `Int8` module that is used by `Color`.
- Add `Token.number` to easily construct a number literal token.

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
