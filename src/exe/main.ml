open Batteries

let () =
  let db = Database.open_db () in
  let _ = Database.initialize db in
  let open Sqlite3 in
  let _ = exec db Database.select_items_query ~cb:(fun row _ ->
    match row.(0), row.(1), row.(2), row.(3) with
      Some id, Some chat_id, Some url, Some interval ->
        let thread = Watcher.get_thread (Int64.of_string id) (int_of_string chat_id) url (int_of_string interval) in
        Lwt.async thread;
    | _ -> ()) in
  let _ = Sqlite3.db_close db in
  Bot.run ()