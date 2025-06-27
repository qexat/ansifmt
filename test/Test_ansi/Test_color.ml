open Ansifmt.Ansi.Color

module Fixtures = struct
  let bright_magenta = bright_magenta
  let salmon = `Basic 173
  let middle_gray = `Basic 244
  let blue_by_modulo = `Basic 260
  let bright_red_by_abs = `Basic ~-9
  let rgb_pink = `Rgb (255, 140, 185)
  let rgb_brown = `Rgb (133, 66, 33)
  let rgb_green_by_normalization = `Rgb (~-127, 511, ~-1024)
end

module Test_basic = struct
  let%test "basic valid integer" =
    basic 244 = Some Fixtures.middle_gray
  ;;

  let%test "basic integer too large" = basic 432 = None
  let%test "basic negative integer" = basic ~-17 = None
end

module Test_rgb = struct
  let%test "rgb valid integers" =
    rgb (255, 140, 185) = Some Fixtures.rgb_pink
  ;;

  let%test "rgb one integer is not valid" =
    rgb (47, ~-5, 210) = None
  ;;

  let%test "rgb all integers are invalid" =
    rgb (~-21, 476, 298) = None
  ;;
end

module Test_of_hex_repr = struct
  let%test
      "of hex repr valid hexadecimal, 6 characters, with hash"
    =
    of_hex_repr "#FF8CB9" = Some Fixtures.rgb_pink
  ;;

  let%test
      "of hex repr valid hexadecimal, 3 characters, with hash"
    =
    of_hex_repr "#888" = Some (`Rgb (136, 136, 136))
  ;;

  let%test
      "of hex repr valid hexadecimal, 6 characters, no hash"
    =
    of_hex_repr "854221" = Some Fixtures.rgb_brown
  ;;

  let%test
      "of hex repr valid hexadecimal, 3 characters, no hash"
    =
    of_hex_repr "fff" = Some (`Rgb (255, 255, 255))
  ;;

  let%test "of hex repr invalid hexadecimal (4 characters)" =
    of_hex_repr "#d0d0" = None
  ;;

  let%test
      "of hex repr invalid hexadecimal (characters outside of \
       [0-9A-F])"
    =
    of_hex_repr "#3g4zME" = None
  ;;

  let%test "of_hex_repr invalid hexadecimal (empty)" =
    of_hex_repr "" = None
  ;;
end

module Test_parse_basic = struct
  let%test "parse valid basic in bounds 0-255" =
    parse_basic "basic(173)" = Some Fixtures.salmon
  ;;

  let%test
      "parse valid basic in bounds 0-255, different formatting"
    =
    parse_basic " BaSiC (  244) " = Some Fixtures.middle_gray
  ;;

  let%test "parse valid basic out of bounds >255" =
    parse_basic "basic(260)" = Some Fixtures.blue_by_modulo
  ;;

  let%test "parse invalid basic out of bounds <0" =
    parse_basic "basic(-9)" = None
  ;;

  let%test "parse invalid basic starting with #" =
    parse_basic "#basic(42)" = None
  ;;

  let%test "parse invalid basic with 3 arguments" =
    parse_basic "basic(255, 140, 185)" = None
  ;;

  let%test "parse invalid empty string" = parse_basic "" = None
end

module Test_parse_rgb = struct
  let%test "parse valid rgb in bounds 0-255" =
    parse_rgb "rgb(255, 140, 185)" = Some Fixtures.rgb_pink
  ;;

  let%test
      "parse valid rgb in bounds 0-255, different formatting"
    =
    parse_rgb " RGb ( 133 ,66  ,33)   "
    = Some Fixtures.rgb_brown
  ;;

  let%test "parse valid rgb out of bounds >255" =
    parse_rgb "rgb(511, 383, 256)" = Some (`Rgb (511, 383, 256))
  ;;

  let%test "parse invalid rgb out of bounds <0" =
    parse_rgb "rgb(-21, 476, 298)" = None
  ;;

  let%test "parse invalid rgb starting with #" =
    parse_rgb "#rgb(57, 189, 43)" = None
  ;;

  let%test "parse invalid rgba with alpha channel" =
    parse_rgb "rgba(100, 50, 0, 0.5)" = None
  ;;

  let%test "parse invalid rgb with ratios" =
    parse_rgb "rgb(0.6, 0.3, 0.47)" = None
  ;;

  let%test "parse invalid rgb with one argument" =
    parse_rgb "rgb(23)" = None
  ;;

  let%test "parse invalid empty string" = parse_rgb "" = None
end

module Test_parse = struct
  (* NOTE: These tests are copies of Test_parse_basic and
     Test_parse_rgb, because we want to be sure that their
     behaviors are matched without introducing functor logic *)

  let%test "parse valid basic in bounds 0-255" =
    parse "basic(173)" = Some Fixtures.salmon
  ;;

  let%test
      "parse valid basic in bounds 0-255, different formatting"
    =
    parse " BaSiC (  244) " = Some Fixtures.middle_gray
  ;;

  let%test "parse valid basic out of bounds >255" =
    parse "basic(260)" = Some Fixtures.blue_by_modulo
  ;;

  let%test "parse invalid basic out of bounds <0" =
    parse "basic(-9)" = None
  ;;

  let%test "parse invalid basic starting with #" =
    parse "#basic(42)" = None
  ;;

  let%test "parse invalid basic with 3 arguments" =
    parse "basic(255, 140, 185)" = None
  ;;

  let%test "parse valid rgb in bounds 0-255" =
    parse "rgb(255, 140, 185)" = Some Fixtures.rgb_pink
  ;;

  let%test
      "parse valid rgb in bounds 0-255, different formatting"
    =
    parse " RGb ( 133 ,66  ,33)   " = Some Fixtures.rgb_brown
  ;;

  let%test "parse valid rgb out of bounds >255" =
    parse "rgb(511, 383, 256)" = Some (`Rgb (511, 383, 256))
  ;;

  let%test "parse invalid rgb out of bounds <0" =
    parse "rgb(-21, 476, 298)" = None
  ;;

  let%test "parse invalid rgb starting with #" =
    parse "#rgb(57, 189, 43)" = None
  ;;

  let%test "parse invalid rgba with alpha channel" =
    parse "rgba(100, 50, 0, 0.5)" = None
  ;;

  let%test "parse invalid rgb with ratios" =
    parse "rgb(0.6, 0.3, 0.47)" = None
  ;;

  let%test "parse invalid rgb with one argument" =
    parse "rgb(23)" = None
  ;;

  let%test "parse invalid empty string" = parse "" = None
end

module Test_luminance = struct
  (* TODO *)
end

module Test_perceived_lightness = struct
  (* TODO *)
end

module Test_best_for_contrast = struct
  let%test "best for contrast pink" =
    best_for_contrast Fixtures.rgb_pink = `Dark
  ;;

  let%test "best for contrast brown" =
    best_for_contrast Fixtures.rgb_brown = `Light
  ;;
end

module Test_serialize = struct
  let%test "serialize basic bright magenta" =
    serialize Fixtures.bright_magenta = "basic(13)"
  ;;

  let%test "serialize basic salmon" =
    serialize Fixtures.salmon = "basic(173)"
  ;;

  let%test "serialize basic blue by modulo" =
    serialize Fixtures.blue_by_modulo = "basic(4)"
  ;;

  let%test "serialize basic bright red by abs" =
    serialize Fixtures.bright_red_by_abs = "basic(9)"
  ;;

  let%test "serialize rgb pink" =
    serialize Fixtures.rgb_pink = "rgb(255, 140, 185)"
  ;;

  let%test "serialize rgb green by normalization" =
    serialize Fixtures.rgb_green_by_normalization
    = "rgb(127, 255, 0)"
  ;;
end
