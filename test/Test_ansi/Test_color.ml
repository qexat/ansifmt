open Ansifmt.Ansi

module Fixtures = struct
  open Color

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
  let ( = ) = Option.equal Color.( = )

  let%test "basic valid integer" =
    (Color.basic 244 :> Color.t option)
    = Some Fixtures.middle_gray
  ;;

  let%test "basic integer too large" =
    (Color.basic 432 :> Color.t option) = None
  ;;

  let%test "basic negative integer" =
    (Color.basic ~-17 :> Color.t option) = None
  ;;
end

module Test_rgb = struct
  let ( = ) = Option.equal Color.( = )

  let%test "rgb valid integers" =
    (Color.rgb (255, 140, 185) :> Color.t option)
    = Some Fixtures.rgb_pink
  ;;

  let%test "rgb one integer is not valid" =
    (Color.rgb (47, ~-5, 210) :> Color.t option) = None
  ;;

  let%test "rgb all integers are invalid" =
    (Color.rgb (~-21, 476, 298) :> Color.t option) = None
  ;;
end

module Test_of_hex_repr = struct
  let ( = ) = Option.equal Color.( = )

  let%test
      "of hex repr valid hexadecimal, 6 characters, with hash"
    =
    (Color.of_hex_repr "#FF8CB9" :> Color.t option)
    = Some Fixtures.rgb_pink
  ;;

  let%test
      "of hex repr valid hexadecimal, 3 characters, with hash"
    =
    (Color.of_hex_repr "#888" :> Color.t option)
    = Some (`Rgb (136, 136, 136))
  ;;

  let%test
      "of hex repr valid hexadecimal, 6 characters, no hash"
    =
    (Color.of_hex_repr "854221" :> Color.t option)
    = Some Fixtures.rgb_brown
  ;;

  let%test
      "of hex repr valid hexadecimal, 3 characters, no hash"
    =
    (Color.of_hex_repr "fff" :> Color.t option)
    = Some (`Rgb (255, 255, 255))
  ;;

  let%test "of hex repr invalid hexadecimal (4 characters)" =
    (Color.of_hex_repr "#d0d0" :> Color.t option) = None
  ;;

  let%test
      "of hex repr invalid hexadecimal (characters outside of \
       [0-9A-F])"
    =
    (Color.of_hex_repr "#3g4zME" :> Color.t option) = None
  ;;

  let%test "of_hex_repr invalid hexadecimal (empty)" =
    (Color.of_hex_repr "" :> Color.t option) = None
  ;;
end

module Test_basic_parse = struct
  let ( = ) = Option.equal Color.( = )

  let%test "parse valid basic in bounds 0-255" =
    Color.Basic.parse "basic(173)" = Some Fixtures.salmon
  ;;

  let%test
      "parse valid basic in bounds 0-255, different formatting"
    =
    Color.Basic.parse " BaSiC (  244) "
    = Some Fixtures.middle_gray
  ;;

  let%test "parse valid basic out of bounds >255" =
    Color.Basic.parse "basic(260)"
    = Some Fixtures.blue_by_modulo
  ;;

  let%test "parse invalid basic out of bounds <0" =
    Color.Basic.parse "basic(-9)" = None
  ;;

  let%test "parse invalid basic starting with #" =
    Color.Basic.parse "#basic(42)" = None
  ;;

  let%test "parse invalid basic with 3 arguments" =
    Color.Basic.parse "basic(255, 140, 185)" = None
  ;;

  let%test "parse invalid empty string" =
    Color.Basic.parse "" = None
  ;;
end

module Test_rgb_parse = struct
  let ( = ) = Option.equal Color.( = )

  let%test "parse valid rgb in bounds 0-255" =
    Color.Rgb.parse "rgb(255, 140, 185)"
    = Some Fixtures.rgb_pink
  ;;

  let%test
      "parse valid rgb in bounds 0-255, different formatting"
    =
    Color.Rgb.parse " RGb ( 133 ,66  ,33)   "
    = Some Fixtures.rgb_brown
  ;;

  let%test "parse valid rgb out of bounds >255" =
    Color.Rgb.parse "rgb(511, 383, 256)"
    = Some (`Rgb (511, 383, 256))
  ;;

  let%test "parse invalid rgb out of bounds <0" =
    Color.Rgb.parse "rgb(-21, 476, 298)" = None
  ;;

  let%test "parse invalid rgb starting with #" =
    Color.Rgb.parse "#rgb(57, 189, 43)" = None
  ;;

  let%test "parse invalid rgba with alpha channel" =
    Color.Rgb.parse "rgba(100, 50, 0, 0.5)" = None
  ;;

  let%test "parse invalid rgb with ratios" =
    Color.Rgb.parse "rgb(0.6, 0.3, 0.47)" = None
  ;;

  let%test "parse invalid rgb with one argument" =
    Color.Rgb.parse "rgb(23)" = None
  ;;

  let%test "parse invalid empty string" =
    Color.Rgb.parse "" = None
  ;;
end

module Test_parse = struct
  let ( = ) = Option.equal Color.( = )

  (* NOTE: These tests are copies of Test_parse_basic and
     Test_parse_rgb, because we want to be sure that their
     behaviors are matched without introducing functor logic *)

  let%test "parse valid basic in bounds 0-255" =
    Color.parse "basic(173)" = Some Fixtures.salmon
  ;;

  let%test
      "parse valid basic in bounds 0-255, different formatting"
    =
    Color.parse " BaSiC (  244) " = Some Fixtures.middle_gray
  ;;

  let%test "parse valid basic out of bounds >255" =
    Color.parse "basic(260)" = Some Fixtures.blue_by_modulo
  ;;

  let%test "parse invalid basic out of bounds <0" =
    Color.parse "basic(-9)" = None
  ;;

  let%test "parse invalid basic starting with #" =
    Color.parse "#basic(42)" = None
  ;;

  let%test "parse invalid basic with 3 arguments" =
    Color.parse "basic(255, 140, 185)" = None
  ;;

  let%test "parse valid rgb in bounds 0-255" =
    Color.parse "rgb(255, 140, 185)" = Some Fixtures.rgb_pink
  ;;

  let%test
      "parse valid rgb in bounds 0-255, different formatting"
    =
    Color.parse " RGb ( 133 ,66  ,33)   "
    = Some Fixtures.rgb_brown
  ;;

  let%test "parse valid rgb out of bounds >255" =
    Color.parse "rgb(511, 383, 256)"
    = Some (`Rgb (511, 383, 256))
  ;;

  let%test "parse invalid rgb out of bounds <0" =
    Color.parse "rgb(-21, 476, 298)" = None
  ;;

  let%test "parse invalid rgb starting with #" =
    Color.parse "#rgb(57, 189, 43)" = None
  ;;

  let%test "parse invalid rgba with alpha channel" =
    Color.parse "rgba(100, 50, 0, 0.5)" = None
  ;;

  let%test "parse invalid rgb with ratios" =
    Color.parse "rgb(0.6, 0.3, 0.47)" = None
  ;;

  let%test "parse invalid rgb with one argument" =
    Color.parse "rgb(23)" = None
  ;;

  let%test "parse invalid empty string" = Color.parse "" = None
end

module Test_luminance = struct
  (* IDEA: property testing that values always fall between 0
     and 1? *)

  let ( =~ ) float1 float2 =
    Float.(abs (sub float1 float2)) <= 0.0001
  ;;

  module Expected = struct
    (* FROZEN 2025-10-30 *)

    let pink = 0.4351894959372870
    let brown = 0.0899278022203988
    let green_by_normalization = 0.7603202590262281
  end

  let%test "luminance pink" =
    Color.luminance Fixtures.rgb_pink =~ Expected.pink
  ;;

  let%test "luminance brown" =
    Color.luminance Fixtures.rgb_brown =~ Expected.brown
  ;;

  let%test "luminance green by normalization" =
    Color.luminance Fixtures.rgb_green_by_normalization
    =~ Expected.green_by_normalization
  ;;
end

module Test_perceived_lightness = struct
  let ( = ) = Int.equal

  (* IDEA: property testing that values always fall between 0
     and 100? *)

  module Expected = struct
    (* FROZEN 2025-06-27 *)

    let pink = 71
    let brown = 35
    let green_by_normalization = 89
  end

  let%test "perceived lightness pink" =
    Color.perceived_lightness Fixtures.rgb_pink = Expected.pink
  ;;

  let%test "perceived lightness brown" =
    Color.perceived_lightness Fixtures.rgb_brown
    = Expected.brown
  ;;

  let%test "perceived lightness green by normalization" =
    Color.perceived_lightness
      Fixtures.rgb_green_by_normalization
    = Expected.green_by_normalization
  ;;
end

module Test_best_for_contrast = struct
  (* the compared type is just two tags so it's not too
     horrible to use [=] here. *)
  let ( = ) = Stdlib.( = )

  let%test "best for contrast pink" =
    Color.best_for_contrast Fixtures.rgb_pink = `Dark
  ;;

  let%test "best for contrast brown" =
    Color.best_for_contrast Fixtures.rgb_brown = `Light
  ;;
end

module Test_serialize = struct
  let ( = ) = String.equal

  let%test "serialize basic bright magenta" =
    Color.serialize Fixtures.bright_magenta = "basic(13)"
  ;;

  let%test "serialize basic salmon" =
    Color.serialize Fixtures.salmon = "basic(173)"
  ;;

  let%test "serialize basic blue by modulo" =
    Color.serialize Fixtures.blue_by_modulo = "basic(4)"
  ;;

  let%test "serialize basic bright red by abs" =
    Color.serialize Fixtures.bright_red_by_abs = "basic(9)"
  ;;

  let%test "serialize rgb pink" =
    Color.serialize Fixtures.rgb_pink = "rgb(255, 140, 185)"
  ;;

  let%test "serialize rgb green by normalization" =
    Color.serialize Fixtures.rgb_green_by_normalization
    = "rgb(127, 255, 0)"
  ;;
end
