open Soup

let get_price html =
  let soup = parse html in
  (* TODO check stock*)
  Price.Stock (soup $ "meta[itemprop='price']" |> R.attribute "content" |> float_of_string)

let get_name html =
  let soup = parse html in
  soup $ "meta[property='og:title']" |> R.attribute "content"