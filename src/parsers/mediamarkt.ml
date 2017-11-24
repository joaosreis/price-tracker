open Soup

let get_price html =
  let soup = parse html in
  if (soup $ "meta[itemprop='availability']" |> R.attribute "content" = "http://schema.org/OutOfStock") then
    Price.NoStock
  else
    Price.Stock (soup $ "div.item-price" |> R.attribute "content" |> float_of_string)

let get_name html =
  let soup = parse html in
  soup $ "h1" |> R.leaf_text