let get_price html =
  let open Soup in
  let soup = parse html in
  match (soup $? "div.col-xs-12.col-sm-12.col-md-4 > form > div.stock-available > i.outStock") with
    Some _ -> Price.NoStock
  | None -> Price.Stock (soup $ "meta[property='product:price:amount']" |> R.attribute "content" |> float_of_string)

let get_name html =
  let open Soup in
  let soup = parse html in
  soup $ "meta[property='og:title']" |> R.attribute "content"