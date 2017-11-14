let get_price html =
  let open Soup in
  let soup = parse html in
  try Price.Stock (soup $ "div.ProductPriceBox-item" |> R.attribute "data-price" |> float_of_string) with _ -> Price.NoStock

let get_name html =
  let open Soup in
  let soup = parse html in
  with_stop (fun stop ->
    soup $$ "span" |> iter (fun e ->
      match (attribute "itemprop" e) with
        Some v when v = "name" -> R.leaf_text e |> String.trim |> stop.throw
      | Some _ | None -> ()); "Não foi possível obter o nome")