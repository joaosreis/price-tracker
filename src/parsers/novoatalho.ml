open Soup

let get_price html =
  let soup = parse html in
  let regex = Str.regexp "\\([0-9]+\\)[,\\|\\.]\\([0-9]+\\).+" in
  Price.Stock (soup $ "span.product-price" |> R.leaf_text |> Str.replace_first regex "\\1.\\2" |> float_of_string)

let get_name html =
  let soup = parse html in
  soup $ "h3.product-name" |> R.leaf_text