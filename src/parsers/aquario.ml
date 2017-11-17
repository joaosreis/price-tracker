open Soup

let get_price html =
  let soup = parse html in
  match soup $? "div.mensagem-sucesso" with
    None -> Price.NoStock
  | Some _ -> let regex = Str.regexp "\\([0-9]+\\),\\([0-9]+\\)" in
      let price_string = soup $ "span.pvp" |> R.leaf_text in
      let _ = Str.string_match regex price_string 0 in
      Price.Stock (Str.matched_string price_string |> Str.replace_first regex "\\1.\\2" |> float_of_string)

let get_name html =
  parse html $ "h1" |> R.leaf_text