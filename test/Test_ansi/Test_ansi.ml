open Ansifmt

module Fixtures = struct
  open Ansi

  let simple_styling : t = `Blink

  (* orang *)
  let simple_color : t = `Background (`Rgb (255, 127, 0))
  let styling_and_color = `Bold & `Foreground Color.red

  let complex_composition =
    `Italic
    & `Reverse
    & `Foreground (`Basic 42)
    & `Background (`Rgb (0, 0, 0))
  ;;
end

module Test_serialize = struct
  let%test "serialize simple styling" =
    Ansi.serialize Fixtures.simple_styling = "blink"
  ;;

  let%test "serialize simple color" =
    Ansi.serialize Fixtures.simple_color
    = "background(rgb(255, 127, 0))"
  ;;

  let%test "serialize styling and color" =
    Ansi.serialize Fixtures.styling_and_color
    = "bold & foreground(basic(1))"
  ;;

  let%test "serialize complex composition" =
    Ansi.serialize Fixtures.complex_composition
    = "italic & reverse & foreground(basic(42)) & \
       background(rgb(0, 0, 0))"
  ;;
end

module Test_deserialize = struct
  let%test "deserialize simple styling" =
    Ansi.deserialize "blink" = Some Fixtures.simple_styling
  ;;

  let%test "deserialize simple color" =
    Ansi.deserialize "background(rgb(255, 127, 0))"
    = Some Fixtures.simple_color
  ;;

  let%test "deserialize styling and color" =
    Ansi.deserialize "bold & foreground(basic(1))"
    = Some Fixtures.styling_and_color
  ;;

  let%test "deserialize complex composition" =
    Ansi.deserialize
      "italic & REVERSE& foreground ( basic(42))  &background  \
       (rgb(0, 0, 0)  )"
    = Some Fixtures.complex_composition
  ;;
end

module Test_show = struct
  let%test "show simple styling" =
    Ansi.show Fixtures.simple_styling = "\x1b[5m"
  ;;

  let%test "show simple color" =
    Ansi.show Fixtures.simple_color = "\x1b[48;2;255;127;0m"
  ;;

  let%test "show styling and color" =
    Ansi.show Fixtures.styling_and_color = "\x1b[1;38;5;1m"
  ;;

  let%test "show complex composition" =
    Ansi.show Fixtures.complex_composition
    = "\x1b[3;7;38;5;42;48;2;0;0;0m"
  ;;
end

module Test_unshow = struct
  let%test "unshow simple styling" =
    Ansi.unshow Fixtures.simple_styling = "\x1b[25m"
  ;;

  let%test "unshow simple color" =
    Ansi.unshow Fixtures.simple_color = "\x1b[49m"
  ;;

  let%test "unshow styling and color" =
    Ansi.unshow Fixtures.styling_and_color = "\x1b[22;39m"
  ;;

  let%test "unshow complex composition" =
    Ansi.unshow Fixtures.complex_composition
    = "\x1b[23;27;39;49m"
  ;;
end

module Test_wrap = struct
  let%test "wrap empty string" =
    Ansi.wrap Fixtures.complex_composition ""
    = "\x1b[23;27;39;49m"
  ;;

  let%test "wrap contentful string" =
    Ansi.wrap Fixtures.complex_composition "I love beans 🫘"
    = "\x1b[3;7;38;5;42;48;2;0;0;0mI love beans \
       🫘\x1b[23;27;39;49m"
  ;;
end
