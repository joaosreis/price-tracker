open Batteries

let get_price html =
  let open Soup in
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  let soup = parse html in
  let json = soup $ "script[type='application/ld+json']" |> R.leaf_text |> from_string |> member "offers" in
  let availability = json |> member "availability" |> to_string in
  if String.exists availability "OutOfStock" then
    Price.NoStock
  else
    Price.Stock (json |> member "price" |> to_string |> float_of_string)

let get_name html =
  let open Soup in
  let open Yojson.Basic in
  let open Yojson.Basic.Util in
  let soup = parse html in
  soup $ "script[type='application/ld+json']" |> R.leaf_text |> from_string |> member "name" |> to_string