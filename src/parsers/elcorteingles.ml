open Soup

let get_price html =
  let soup = parse html in
  Price.Stock (soup $ "span[itemprop='price']" |> R.attribute "content" |> float_of_string)
  (* TODO stock *)

let get_name html =
  let soup = parse html in
  soup $ "meta[property='og:title']" |> R.attribute "content"