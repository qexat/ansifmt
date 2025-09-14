
# ansifmt

ansifmt is a simple, lightweight library for ANSI formatting and styling.

It provides a convenient API for [SGR escape sequences](https://en.wikipedia.org/wiki/ANSI_escape_code#Select_Graphic_Rendition_parameters), as well as a string-like data type where parts of it can be stylized in different ways.

## Structure

The library is comprised of two modules: `Ansi` ([documentation](https://qexat.github.io/ansifmt/Ansifmt/Ansi/)), which provides the aforementioned interface over escape sequences, and `Fmt` ([documentation](https://qexat.github.io/ansifmt/Ansifmt/Ansi/)) for dealing with that pretty-printable stylized string data type.

> [!TIP]
> For simple usage, you probably want to stick to `Fmt`.
> `Ansi` exists for more granular control, or if you want to build your own library using it.
