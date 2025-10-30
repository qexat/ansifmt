open Ansifmt

module Fixtures = struct
  open Fmt

  let raw = string "hello world"

  let stylized =
    stylize
      Ansi.(`Bold & `Foreground Color.green)
      (string "beans")
  ;;

  let composed =
    stylize
      (`Foreground Ansi.Color.blue)
      (string "blue" ++ stylize `Italic (string "+italic"))
    ++ stylize `Italic (string "-blue")
  ;;
end

module Test_render = struct
  let ( = ) = String.equal

  let%test "render raw with styling" =
    Fmt.render ~with_styles:true Fixtures.raw = "hello world"
  ;;

  let%test "render raw without styling" =
    Fmt.render ~with_styles:false Fixtures.raw = "hello world"
  ;;

  let%test "render stylized with styling" =
    Fmt.render ~with_styles:true Fixtures.stylized
    = "\x1b[1;38;5;2mbeans\x1b[22;39m"
  ;;

  let%test "render stylized without styling" =
    Fmt.render ~with_styles:false Fixtures.stylized = "beans"
  ;;

  let%test "render composed with styling" =
    Fmt.render ~with_styles:true Fixtures.composed
    = "\x1b[38;5;4mblue\x1b[3m+italic\x1b[23m\x1b[39m\x1b[3m-blue\x1b[23m"
  ;;

  let%test "render composed without styling" =
    Fmt.render ~with_styles:false Fixtures.composed
    = "blue+italic-blue"
  ;;
end

module Test_print = struct
  let%expect_test
      "print composed with always styling, default ending, \
       default out"
    =
    Fmt.print ~with_styles:`Always Fixtures.composed;
    [%expect_exact
      "\x1b[38;5;4mblue\x1b[3m+italic\x1b[23m\x1b[39m\x1b[3m-blue\x1b[23m\n"]
  ;;

  let%expect_test
      "print composed with never styling, default ending, \
       default out"
    =
    Fmt.print ~with_styles:`Never Fixtures.composed;
    [%expect_exact "blue+italic-blue\n"]
  ;;

  let%expect_test
      "print stylized with always styling, no ending, default \
       out"
    =
    Fmt.print
      ~with_styles:`Always
      ~ending:None
      Fixtures.stylized;
    [%expect_exact "\x1b[1;38;5;2mbeans\x1b[22;39m"]
  ;;

  let%expect_test
      "print stylized with never styling, custom ending, \
       default out"
    =
    Fmt.(
      print
        ~with_styles:`Never
        ~ending:(Some (string "arecool"))
        Fixtures.stylized);
    [%expect_exact "beansarecool"]
  ;;
end
