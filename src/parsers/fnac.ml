let get_price html =
  let open Soup in
  let soup = parse html in
  let div = soup $ "div#hl_1" in
  if div |> R.attribute "data-in-stock" |> int_of_string = 0 then
    Price.NoStock
  else
    Price.Stock (div |> R.attribute "data-price" |> float_of_string)

let get_name html =
  let open Soup in
  let soup = parse html in
  soup $ "h1" |> R.leaf_text |> String.trim