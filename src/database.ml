open Sqlite3

let open_db () = db_open "pricetracker.db"

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
    FOREIGN KEY (chat_id) REFERENCES chats(chat_id)
    ON DELETE CASCADE ON UPDATE CASCADE
  );" in
  exec db sql

let select_items_query = "SELECT id, items.chat_id, url, interval FROM items LEFT JOIN chats ON items.chat_id = chats.chat_id"

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

let update_item_stmt db id chat_id url =
  let stmt = prepare db "UPDATE items SET chat_id=?, url=? WHERE id=?" in
  let _ = bind stmt 1 (Data.INT (Int64.of_int chat_id)) in
  let _ = bind stmt 2 (Data.TEXT url) in
  let _ = bind stmt 3 (Data.INT id) in
  stmt