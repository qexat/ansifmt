open Ansifmt.Ansi.Attributes

module Fixtures = struct
  let bold = create 1
  let fg_yellow = create ~parameters:[ 5; 3 ] 38
  let italic_bg_blue = create ~parameters:[ 48; 2; 0; 0; 255 ] 3
end

module Test_serialize = struct
  let%test "serialize bold" =
    serialize Fixtures.bold = "{ code = 1 ; parameters = [] }"
  ;;

  let%test "serialize fg_yellow" =
    serialize Fixtures.fg_yellow
    = "{ code = 38 ; parameters = [5; 3] }"
  ;;

  let%test "serialize italic_bg_blue" =
    serialize Fixtures.italic_bg_blue
    = "{ code = 3 ; parameters = [48; 2; 0; 0; 255] }"
  ;;
end
