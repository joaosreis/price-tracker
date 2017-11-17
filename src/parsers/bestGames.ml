open Batteries

let get_price html =
  let open Soup in
  let soup = parse html in
  let availabilitySpan = soup $ "span#availability_value" in
  match attribute "class" availabilitySpan with
    Some _ -> Price.NoStock
  | None -> begin
      let regex = Str.regexp "\\([0-9]+\\)[,\\|\\.]\\([0-9]+\\).*" in
      let price_text = soup $ "span.our_price_display" |> R.leaf_text |> Str.replace_first regex "\\1.\\2" in
      Price.Stock (price_text |> float_of_string)
    end

let get_name html =
  let open Soup in
  let soup = parse html in
  soup $ "h1[itemprop='name']" |> R.leaf_text |> String.trim