open Batteries

let get_price html =
  let open Soup in
  let soup = parse html in
  let thirdPartyDiv = soup $? "div#soldByThirdParty" in
  match thirdPartyDiv with
    None -> begin
      let availabilityDiv = soup $ "div#availability" in
      if (String.exists (availabilityDiv $ "span" |> R.attribute "class") "a-color-success") then
        let regex = Str.regexp "EUR \\([0-9]+\\)[,\\|\\.]\\([0-9]+\\)" in
        let price_text = soup $ "span#priceblock_ourprice" |> R.leaf_text |> Str.replace_first regex "\\1.\\2" in
        price_text |> print_endline;
        Price.Stock (price_text |> float_of_string)
      else
        Price.NoStock
    end
  | Some _ -> Price.NoStock

let get_name html =
  let open Soup in
  let soup = parse html in
  soup $ "span#productTitle" |> R.leaf_text |> String.trim