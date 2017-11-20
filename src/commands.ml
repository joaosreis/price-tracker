open Batteries
open Telegram.Api
open Telegram.Actions

let start message =
  let open Sqlite3 in
  let open Message in
  let open Command in
  try
    let db = Database.open_db () in
    let stmt = Database.insert_chat_stmt db message.chat.id 60 in
    let _ = step stmt in
    let _ = finalize stmt in
    let _ = db_close db in
    SendMessage (message.chat.id, "Para fazer tracking de um produto envie `/track url_do_produto`", Some Telegram.Api.ParseMode.Markdown, false, false, None, None)
  with InternalError s | Error s -> print_endline s; Nothing

let track message  =
  let open Message in
  let open Command in
  let open Sqlite3 in
  let text = Option.get message.text in
  let regex = Str.regexp "/track\\(.+\\)? \\(.+\\)" in
  try
    let db = Database.open_db () in
    let _ = Database.begin_transaction db in
    begin try
        let _ = Str.string_match regex text 0 in
        let url = Str.matched_group 2 text in
        let stmt = Database.insert_item_stmt db message.chat.id url in
        let _ = step stmt in
        let _ = finalize stmt in
        let id = last_insert_rowid db in
        let _ = db_close db in
        let interval = 5 in
        let thread = Watcher.get_thread id message.chat.id url None interval in
        Lwt.async thread;
        let _ = Database.commit db in
        Nothing
      with
        Not_found ->
          let _ = Database.rollback db in
          SendMessage (message.chat.id, "URL necessário", Some Telegram.Api.ParseMode.Markdown, false, false, None, None)
      | Watcher.Not_supported s ->
          let _ = Database.rollback db in
          SendMessage (message.chat.id, "Site não suportado", Some Telegram.Api.ParseMode.Markdown, false, false, None, None)
      | _ ->
          let _ = Database.rollback db in
          print_endline "Error";
          SendMessage (message.chat.id, "Ocorreu um erro, tente novamente", Some Telegram.Api.ParseMode.Markdown, false, false, None, None)
    end
  with _ -> print_endline "Error"; SendMessage (message.chat.id, "Ocorreu um erro, tente novamente", Some Telegram.Api.ParseMode.Markdown, false, false, None, None)
