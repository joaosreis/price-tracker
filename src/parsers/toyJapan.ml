open Batteries

let get_price html =
  let open Soup in
  let soup = parse html in
  let availabilitySpan = soup $ "span#availability_value" in
  if (String.exists (availabilitySpan |> R.attribute "class") "label-success") then
    Price.Stock (soup $ "meta[property='product:price:amount']" |> R.attribute "content" |> float_of_string)
  else
    Price.NoStock

let get_name html =
  let open Soup in
  let soup = parse html in
  with_stop (fun stop ->
    soup $$ "h1" |> iter (fun e ->
      match (attribute "itemprop" e) with
        Some v when v = "name" -> R.leaf_text e |> String.trim |> stop.throw
      | Some _ | None -> ()); "Não foi possível obter o nome")