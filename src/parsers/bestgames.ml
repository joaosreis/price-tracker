open Batteries

let get_price html =
  let open Soup in
  let soup = parse html in
  let availabilitySpan = soup $ "span#availability_value" in
  match attribute "class" availabilitySpan with
    None -> begin
      let regex = Str.regexp "\\([0-9]+\\)[,\\|\\.]\\([0-9]+\\).*" in
      let price_text = soup $ "span.our_price_display" |> R.leaf_text |> Str.replace_first regex "\\1.\\2" in
      price_text |> print_endline;
      Utils.Stock (price_text |> float_of_string)
    end
  | Some _ -> Utils.NoStock