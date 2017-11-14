let get_price html =
  let open Soup in
  let soup = parse html in
  match (soup $? "span.out_of_stock") with
    None -> begin
      try Price.Stock (soup $ "span.w-product__price__current" |> R.attribute "content" |> String.map (fun c -> if c = ',' then '.' else c) |> float_of_string)
      with _ -> Price.NoStock
    end
    | Some _ -> Price.NoStock

let get_name html =
  let open Soup in
  let soup = parse html in
  soup $ "span.w-product__name" |> R.leaf_text |> String.trim