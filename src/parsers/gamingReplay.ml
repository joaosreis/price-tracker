open Batteries

let get_price html =
  let open Soup in
  let soup = parse html in
  let availability = soup $? "link[itemprop='availability']" in
  match availability with
    Some l when l |> R.attribute "href" = "http://schema.org/InStock" ->
      let regex = Str.regexp "\\([0-9]+\\)[,\\|\\.]\\([0-9]+\\).*" in
      let price_text = soup $ "span#our_price_display" |> R.leaf_text |> Str.replace_first regex "\\1.\\2" in
      Price.Stock (price_text |> float_of_string)
  | _ -> Price.NoStock

let get_name html =
  let open Soup in
  let soup = parse html in
  soup $ "title" |> R.leaf_text