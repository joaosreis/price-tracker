open Batteries

let () =
  Log.set_log_level Log.DEBUG;
  Log.color_on();

  let db = Db.open_db () in
  let _ = Db.initialize db in
  let open Sqlite3 in
  let _ = exec db Db.select_items_query ~cb:(fun row _ ->
    match row.(0), row.(1), row.(2), row.(3), row.(4), row.(5) with
      Some id, Some chat_id, Some url, Some stock, p, Some interval ->
        let price = if (Bool.of_string stock) then Some (Price.Stock (Option.get p |> float_of_string))
          else Some Price.NoStock in
        let site = Watcher.get_site url in
        let thread = Watcher.get_thread (Int64.of_string id) (int_of_string chat_id) url site price (int_of_string interval) in
        Lwt.async thread;
    | Some id, Some chat_id, Some url, _, _, Some interval ->
        let site = Watcher.get_site url in
        let thread = Watcher.get_thread (Int64.of_string id) (int_of_string chat_id) url site None (int_of_string interval) in
        Lwt.async thread;
    | _ -> ()) in
  let _ = Sqlite3.db_close db in
  Bot.run ()