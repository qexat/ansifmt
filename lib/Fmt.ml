include Rich_string.Make (Ansi)

let stylize = enrich

let rec prune_styled rs =
  match rs with
  | Empty | String _ -> rs
  | Enriched (_, rs) -> prune_styled rs
  | Join (sep, rss) ->
    Join (prune_styled sep, List.map prune_styled rss)
;;

let render ~with_styles rs =
  let rs' = if with_styles then rs else prune_styled rs in
  render rs'
;;

let print
      ?(out = stdout)
      ?(ending = Some (String "\n"))
      ?with_styles:(color_strategy = `Auto)
      rs
  =
  let with_styles =
    match color_strategy with
    | `Always -> true
    | `Never -> false
    | `Auto -> Out_channel.isatty out
  in
  let rs' = if with_styles then rs else prune_styled rs in
  print ~out ~ending rs'
;;
