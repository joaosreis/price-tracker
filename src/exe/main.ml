open Batteries

let () =
  let db = Database.open_db () in
  let _ = Database.initialize db in
  let open Sqlite3 in
  let _ = exec db Database.select_items_query ~cb:(fun row _ ->
    match row.(0), row.(1), row.(2), row.(3), row.(4), row.(5) with
      Some id, Some chat_id, Some url, Some stock, p, Some interval ->
        let price = if (Bool.of_string stock) then Some (Price.Stock (Option.get p |> float_of_string))
          else Some Price.NoStock in
        let thread = Watcher.get_thread (Int64.of_string id) (int_of_string chat_id) url price (int_of_string interval) in
        Lwt.async thread;
    | Some id, Some chat_id, Some url, _, _, Some interval ->
        let thread = Watcher.get_thread (Int64.of_string id) (int_of_string chat_id) url None (int_of_string interval) in
        Lwt.async thread;
    | _ -> ()) in
  let _ = Sqlite3.db_close db in
  Bot.run ()