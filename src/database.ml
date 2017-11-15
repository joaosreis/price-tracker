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

let select_items_query = "SELECT items.chat_id, url, interval FROM items LEFT JOIN chats ON items.chat_id = chats.chat_id"