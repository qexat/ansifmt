open Ansifmt

module Fixtures = struct
  open Fmt

  let raw = Fmt.raw "hello world"

  let stylized =
    Fmt.stylize
      (Raw "beans")
      Ansi.(`Bold & `Foreground Color.green)
  ;;

  let composed =
    Fmt.stylize
      Fmt.(raw "blue" ++ stylize (raw "+italic") `Italic)
      (`Foreground Ansi.Color.blue)
    ++ Fmt.stylize (Fmt.raw "-blue") `Italic
  ;;
end

module Test_join = struct
  let%test "join empty list of fmts" =
    Fmt.join ~on:(Raw "+") [] = Raw "+"
  ;;

  let%test "join non-empty list of fmts" =
    Fmt.join ~on:(Raw "-") [ Raw "a"; Raw "b"; Raw "c" ]
    = Composed
        ( Composed
            ( Composed (Composed (Raw "a", Raw "-"), Raw "b")
            , Raw "-" )
        , Raw "c" )
  ;;
end

module Test_render = struct
  let%test "render raw with styling" =
    Fmt.render ~with_styling:true Fixtures.raw = "hello world"
  ;;

  let%test "render raw without styling" =
    Fmt.render ~with_styling:false Fixtures.raw = "hello world"
  ;;

  let%test "render stylized with styling" =
    Fmt.render ~with_styling:true Fixtures.stylized
    = "\x1b[1;38;5;2mbeans\x1b[22;39m"
  ;;

  let%test "render stylized without styling" =
    Fmt.render ~with_styling:false Fixtures.stylized = "beans"
  ;;

  let%test "render composed with styling" =
    Fmt.render ~with_styling:true Fixtures.composed
    = "\x1b[38;5;4mblue\x1b[3m+italic\x1b[23m\x1b[39m\x1b[3m-blue\x1b[23m"
  ;;

  let%test "render composed without styling" =
    Fmt.render ~with_styling:false Fixtures.composed
    = "blue+italic-blue"
  ;;
end

module Test_print = struct
  (* TODO: test print with different [out]s *)

  let%expect_test
      "print composed with always styling, default ending, \
       default out"
    =
    Fmt.print ~with_styling:`Always Fixtures.composed;
    [%expect_exact
      "\x1b[38;5;4mblue\x1b[3m+italic\x1b[23m\x1b[39m\x1b[3m-blue\x1b[23m\n"]
  ;;

  let%expect_test
      "print composed with never styling, default ending, \
       default out"
    =
    Fmt.print ~with_styling:`Never Fixtures.composed;
    [%expect_exact "blue+italic-blue\n"]
  ;;

  let%expect_test
      "print stylized with always styling, no ending, default \
       out"
    =
    Fmt.print
      ~with_styling:`Always
      ~ending:None
      Fixtures.stylized;
    [%expect_exact "\x1b[1;38;5;2mbeans\x1b[22;39m"]
  ;;

  let%expect_test
      "print stylized with never styling, custom ending, \
       default out"
    =
    Fmt.print
      ~with_styling:`Never
      ~ending:(Some "arecool")
      Fixtures.stylized;
    [%expect_exact "beansarecool"]
  ;;
end

module Test_serialize = struct
  let%test "serialize raw" =
    Fmt.serialize Fixtures.raw = "raw(\"hello world\")"
  ;;

  let%test "serialize stylized" =
    Fmt.serialize Fixtures.stylized
    = "stylized(raw(\"beans\"), bold & foreground(basic(2)))"
  ;;

  let%test "serialize composed" =
    Fmt.serialize Fixtures.composed
    = "stylized(raw(\"blue\") ++ stylized(raw(\"+italic\"), \
       italic), foreground(basic(4))) ++ \
       stylized(raw(\"-blue\"), italic)"
  ;;
end

module Test_show = struct
  let%test "show raw" = Fmt.show Fixtures.raw = Fixtures.raw

  let%test "show stylized" =
    Fmt.show Fixtures.stylized = Fixtures.stylized
  ;;

  let%test "show composed" =
    Fmt.show Fixtures.composed = Fixtures.composed
  ;;
end
