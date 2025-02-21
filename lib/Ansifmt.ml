module Color = Color
module IO = IO
module Formatting = Formatting
module Styling = Styling

type color = Color.t
type styling = Styling.t
type stylizer = Formatting.Stylizer.t

let format = Formatting.Util.format
