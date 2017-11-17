let get_price html =
  let open Soup in
  let soup = parse html in
  if soup $ "meta[property='og:availability']" |> R.attribute "content" = "oos" then
    Price.NoStock
  else
    Price.Stock (soup $ "meta[property='product:price:amount']" |> R.attribute "content" |> float_of_string)

let get_name html =
  let open Soup in
  let soup = parse html in
  soup $ "meta[property='og:title']" |> R.attribute "content"