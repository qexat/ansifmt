type color = Color.t
type styling = Styling.t
type stylizer = Formatting.Stylizer.t

let make_styling = Styling.create
let format = Formatting.Util.format
let print_formatted = IO.print_formatted
