open Sqlite3

let open_db () = db_open "pricetracker.db"

let begin_transaction db = Sqlite3.exec db "BEGIN TRANSACTION;"

let commit db = Sqlite3.exec db "COMMIT;"

let rollback db = Sqlite3.exec db "ROLLBACK;"

let try_operation f =
  try
    let db = open_db () in
    let _ = begin_transaction db in
    try
      let success, result = f db in
      if (success) then
        let _ = commit db in
        let _ = db_close db in
        result
      else
        let _ = rollback db in
        let _ = db_close db in
        result
    with
      Error s | InternalError s ->
        let _ = rollback db in
        let _ = db_close db in
        Log.error "%s" s;
        None
    | e ->
        let _ = rollback db in
        let _ = db_close db in
        Printexc.to_string e |> Log.error "%s";
        None
  with Error s | InternalError s ->
    Log.error "%s" s;
    None

let initialize db =
  let sql =
    "CREATE TABLE IF NOT EXISTS chats (
        chat_id integer PRIMARY KEY,
        interval int NOT NULL
      );

    CREATE TABLE IF NOT EXISTS items (
      id integer PRIMARY KEY AUTOINCREMENT,
      chat_id integer,
      url text NOT NULL,
      stock integer,
      price float,
      FOREIGN KEY (chat_id) REFERENCES chats(chat_id)
      ON DELETE CASCADE ON UPDATE CASCADE
    );" in
  exec db sql

let select_items_query = "SELECT id, items.chat_id, url, stock, price, interval FROM items LEFT JOIN chats ON items.chat_id = chats.chat_id"

let insert_chat_stmt db chat_id interval = 
  let stmt = prepare db "INSERT INTO chats (chat_id,interval) VALUES(?,?)" in
  let _ = bind stmt 1 (Data.INT (Int64.of_int chat_id)) in
  let _ = bind stmt 2 (Data.INT (Int64.of_int interval)) in
  stmt

let insert_item_stmt db chat_id url =
  let stmt = prepare db "INSERT INTO items (chat_id,url) VALUES(?,?)" in
  let _ = bind stmt 1 (Data.INT (Int64.of_int chat_id)) in
  let _ = bind stmt 2 (Data.TEXT url) in
  stmt

let update_item_url_stmt db id url =
  let stmt = prepare db "UPDATE items SET url=? WHERE id=?" in
  let _ = bind stmt 1 (Data.TEXT url) in
  let _ = bind stmt 2 (Data.INT id) in
  stmt

let update_item_price_stmt db id price =
  let stmt = prepare db "UPDATE items SET stock=?, price=? WHERE id=?" in
  let _ = bind stmt 3 (Data.INT id) in
  match price with
    Price.NoStock ->
      let _ = bind stmt 1 (Data.INT 0L) in
      let _ = bind stmt 2 (Data.NULL) in
      stmt
  | Price.Stock p ->
      let _ = bind stmt 1 (Data.INT 1L) in
      let _ = bind stmt 2 (Data.FLOAT p) in
      stmt