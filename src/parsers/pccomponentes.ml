open Soup

let get_price html =
  let soup = parse html in
  if (soup $ "meta[itemprop='availability']" |> R.attribute "content" = "OutOfStock") then
    Price.NoStock
  else
    Price.Stock (soup $ "div[data-price]" |> R.attribute "data-price" |> float_of_string)

let get_name html =
  let soup = parse html in
  soup $ "h1[itemprop='name']" |> R.leaf_text