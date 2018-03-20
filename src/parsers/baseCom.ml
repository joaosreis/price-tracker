open Batteries

let get_price html =
  let open Soup in
  let soup = parse html in
  let oc = open_out "in.html" in
  Printf.fprintf oc "%s" html;
  close_out oc;
  let availability = soup $ "p.stock-message" |> leaf_text in
  match availability with
    Some s when String.exists s "unavailable" ->
      Price.NoStock
  | Some _ | None ->
      let div = soup $ "div[itemprop='offers']" in
      let price = div $ "span.price" |> R.leaf_text |> String.lchop ~n:2 in
      Log.debug "%s\n" price;
      Price.Stock (price |> float_of_string)

let get_name html =
  let open Soup in
  let soup = parse html in
  soup $ "meta[property='og:title']" |> R.attribute "content"