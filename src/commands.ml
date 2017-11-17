open Batteries
open Telegram.Api
open Telegram.Actions

let start message =
  let open Sqlite3 in
  let open Message in
  let open Command in
  try
    let db = Database.open_db () in
    let stmt = Database.insert_chat_stmt db message.chat.id 5 in
    let _ = step stmt in
    let _ = finalize stmt in
    let _ = db_close db in
    SendMessage (message.chat.id, "Para fazer tracking de um produto envie `/track url_do_produto`", Some Telegram.Api.ParseMode.Markdown, false, false, None, None)
  with InternalError s | Error s -> print_endline s; Nothing

let track message  =
  let open Message in
  let open Command in
  let text = Option.get message.text in
  let regex = Str.regexp "/track\\(.+\\)? \\(.+\\)" in
  try
    let _ = Str.string_match regex text 0 in
    let url = Str.matched_group 2 text in
    let open Sqlite3 in
    let db = Database.open_db () in
    let stmt = Database.insert_item_stmt db message.chat.id url in
    let _ = step stmt in
    let _ = finalize stmt in
    let id = last_insert_rowid db in
    let _ = db_close db in
    let interval = 5 in
    let thread = Watcher.get_thread id message.chat.id url interval in
    Lwt.async thread;
    Nothing
  with Not_found -> SendMessage (message.chat.id, "URL required", Some Telegram.Api.ParseMode.Markdown, false, false, None, None)
     | Sqlite3.InternalError s | Sqlite3.Error s -> print_endline s; Nothing