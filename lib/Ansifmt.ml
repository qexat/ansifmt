module Color = Color
module IO = IO
module Formatting = Formatting
module Styling = Styling

type color = Color.t
type styling = Styling.t
type stylizer = Formatting.Stylizer.t

let make_styling = Styling.create
let format = Formatting.Util.format
let print_formatted = IO.print_formatted
