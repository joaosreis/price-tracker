let get_price html =
  let open Soup in
  let soup = parse html in
  try Price.Stock (soup $ "div.ProductPriceBox-item" |> R.attribute "data-price" |> float_of_string) with _ -> Price.NoStock