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

module Test_parse = struct
  let%test "parse valid hexadecimal, 6 characters, with hash" =
    parse "#FF8CB9" = Some Fixtures.rgb_pink
  ;;

  let%test "parse valid hexadecimal, 3 characters, with hash" =
    parse "#888" = Some (`Rgb (136, 136, 136))
  ;;

  let%test "parse valid hexadecimal, 6 characters, no hash" =
    parse "854221" = Some Fixtures.rgb_brown
  ;;

  let%test "parse valid hexadecimal, 3 characters, no hash" =
    parse "fff" = Some (`Rgb (255, 255, 255))
  ;;

  let%test "parse invalid hexadecimal (4 characters)" =
    parse "#d0d0" = None
  ;;

  let%test
      "parse invalid hexadecimal (characters outside of \
       [0-9A-F])"
    =
    parse "#3g4zME" = None
  ;;

  let%test "parse invalid hexadecimal (empty)" = parse "" = None

  let%test "parse valid rgb in bounds 0-255" =
    parse "rgb(255, 140, 185)" = Some Fixtures.rgb_pink
  ;;

  let%test
      "parse valid rgb in bounds 0-255, different formatting"
    =
    parse "RGb ( 133 ,66  ,33)" = Some Fixtures.rgb_brown
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
end

module Test_luminance = struct
  (* TODO *)
end

module Test_best_for_contrast = struct
  let%test "best_for_contrast pink, default threshold" =
    best_for_contrast Fixtures.rgb_pink = `Dark
  ;;

  let%test "best_for_contrast dark brown, default threshold" =
    best_for_contrast Fixtures.rgb_brown = `Light
  ;;

  let%test "best_for_contrast pink, high threshold" =
    best_for_contrast ~threshold:191 Fixtures.rgb_pink = `Light
  ;;

  let%test "best_for_contrast dark brown, high threshold" =
    best_for_contrast ~threshold:191 Fixtures.rgb_brown = `Light
  ;;

  let%test "best_for_contrast pink, low threshold" =
    best_for_contrast ~threshold:63 Fixtures.rgb_pink = `Dark
  ;;

  let%test "best_for_contrast dark brown, low threshold" =
    best_for_contrast ~threshold:63 Fixtures.rgb_brown = `Dark
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
