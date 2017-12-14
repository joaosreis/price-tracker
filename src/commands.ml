open Batteries
open Telegram.Api
open Telegram.Actions

let interval = 90 (* TODO remove *)

let start message =
  let open Sqlite3 in
  let open Message in
  let open Command in
  Db.try_operation (fun db ->
    let stmt = Db.insert_chat_stmt db message.chat.id interval in
    let _ = step stmt in
    let _ = finalize stmt in
    Log.info "New chat: %d" message.chat.id;
    true, Some (SendMessage (message.chat.id, "Para fazer tracking de um produto envie `/track url_do_produto`", Some Telegram.Api.ParseMode.Markdown, false, false, None, None)))
  |> Option.default (SendMessage (message.chat.id, "Ocorreu um erro, tente novamente", Some Telegram.Api.ParseMode.Markdown, false, false, None, None))


let track message  =
  let open Message in
  let open Command in
  let open Sqlite3 in
  let text = Option.get message.text in
  let regex = Str.regexp "/track\\(.+\\)? \\(.+\\)" in
  Db.try_operation (fun db ->
    let found = Str.string_match regex text 0 in
    if not found then begin
      Log.warn "URL not found: %s" text;
      false, Some (SendMessage (message.chat.id, "URL não encontrado", Some Telegram.Api.ParseMode.Markdown, false, false, None, None))
    end
    else
      let url = Str.matched_group 2 text in
      let site = Watcher.get_site url in
      match site with
        `NotSupported -> false, Some (SendMessage (message.chat.id, "Site não suportado", Some Telegram.Api.ParseMode.Markdown, false, false, None, None))
      | _ ->  
          try
            let stmt = Db.insert_item_stmt db message.chat.id url in
            let _ = step stmt in
            let _ = finalize stmt in
            let id = last_insert_rowid db in
            let thread = Watcher.get_thread id message.chat.id url site None interval in
            Lwt.async thread;
            Log.info "Tracking: URL - %s | Interval - %d | Chat id - %d" url interval message.chat.id;
            true, Some Nothing
          with
            Not_found ->
              Log.warn "Unsupported URL - %s" text;
              false, Some (SendMessage (message.chat.id, "URL necessário", Some Telegram.Api.ParseMode.Markdown, false, false, None, None))
          | Watcher.Not_supported s ->
              Log.error "Not_supported, shouldn't have gotten here - %s" text;
              false, Some (SendMessage (message.chat.id, "Ocorreu um erro, tente novamente", Some Telegram.Api.ParseMode.Markdown, false, false, None, None))
          | e ->
              Printexc.to_string e |> Log.error "%s";
              false, Some (SendMessage (message.chat.id, "Ocorreu um erro, tente novamente", Some Telegram.Api.ParseMode.Markdown, false, false, None, None)))
  |> Option.default (SendMessage (message.chat.id, "Ocorreu um erro, tente novamente", Some Telegram.Api.ParseMode.Markdown, false, false, None, None))
