type color = Color.t
type styling = Styling.t
type stylizer = Formatting.Stylizer.t

let make_styling = Styling.create
let format = Formatting.Util.format
let parenthesize_if = Formatting.Util.parenthesize_if
let print_formatted = IO.print_formatted
