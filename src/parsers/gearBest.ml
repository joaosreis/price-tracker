let get_price html =
  let open Soup in
  let soup = parse html in
  match (soup $? "a.no_addToCartBtn") with
    None -> Price.Stock (soup $ "meta[property='og:price:amount']" |> R.attribute "content" |> float_of_string)
  | Some _ -> Price.NoStock

let get_name html =
  let open Soup in
  let soup = parse html in
  soup $ "h1" |> R.leaf_text