open Batteries

include Telegram.Api.Mk(struct
    include Telegram.BotDefaults
    let token = Configuration.token

    let commands = let open Telegram.Api.Command in
      [{name = "start"; description = "Registar"; enabled = true; run = Commands.start};
       {name = "track"; description = "Track this product"; enabled = true; run = Commands.track}]
  end)

let run ?(log=true) () =
  let open Lwt in

  let open Telegram.Api in
  let open Command in

  let process = function
    | Result.Success _ -> return ()
    | Result.Failure e ->
        if log && e <> "Could not get head" then (* Ignore spam *)
          Lwt_io.printl e
        else return () in
  let rec evaluator = let open Telegram.Api.Command in
    let dispose x = x >>= fun _ -> return ()
    and eval f x = x >>= fun y -> evaluator (f y)
    and identify : 'a. (?chat_id:string option -> ?message_id:int option -> ?inline_message_id:string option -> 'a) -> [`ChatMessageId of string * int | `InlineMessageId of string] -> 'a =
      fun f id -> match id with
        | `ChatMessageId (c, m) -> f ~chat_id:(Some c) ~message_id:(Some m) ~inline_message_id:None
        | `InlineMessageId i -> f ~chat_id:None ~message_id:None ~inline_message_id:(Some i) in
    function
    | Nothing -> return ()
    | GetMe f -> get_me |> eval f
    | SendMessage (chat_id, text, parse_mode, disable_web_page_preview, disable_notification, reply_to, reply_markup) -> send_message ~chat_id ~text ~parse_mode ~disable_web_page_preview ~disable_notification ~reply_to ~reply_markup |> dispose
    | ForwardMessage (chat_id, from_chat_id, disable_notification, message_id) -> forward_message ~chat_id ~from_chat_id ~disable_notification ~message_id |> dispose
    | SendChatAction (chat_id, action) -> send_chat_action ~chat_id ~action |> dispose
    | SendPhoto (chat_id, photo, caption, disable_notification, reply_to, reply_markup, f) -> send_photo ~chat_id ~photo ~caption ~disable_notification ~reply_to ~reply_markup |> eval f
    | ResendPhoto (chat_id, photo, caption, disable_notification, reply_to, reply_markup) -> resend_photo ~chat_id ~photo ~caption ~disable_notification ~reply_to ~reply_markup |> dispose
    | SendAudio (chat_id, audio, performer, title, disable_notification, reply_to, reply_markup, f) -> send_audio ~chat_id ~audio ~performer ~title ~disable_notification ~reply_to ~reply_markup |> eval f
    | ResendAudio (chat_id, audio, performer, title, disable_notification, reply_to, reply_markup) -> resend_audio ~chat_id ~audio ~performer ~title ~disable_notification ~reply_to ~reply_markup |> dispose
    | SendDocument (chat_id, document, disable_notification, reply_to, reply_markup, f) -> send_document ~chat_id ~document ~disable_notification ~reply_to ~reply_markup |> eval f
    | ResendDocument (chat_id, document, disable_notification, reply_to, reply_markup) -> resend_document ~chat_id ~document ~disable_notification ~reply_to ~reply_markup |> dispose
    | SendSticker (chat_id, sticker, disable_notification, reply_to, reply_markup, f) -> send_sticker ~chat_id ~sticker ~disable_notification ~reply_to ~reply_markup |> eval f
    | ResendSticker (chat_id, sticker, disable_notification, reply_to, reply_markup) -> resend_sticker ~chat_id ~sticker ~disable_notification ~reply_to ~reply_markup |> dispose
    | SendVideo (chat_id, video, duration, caption, disable_notification, reply_to, reply_markup, f) -> send_video ~chat_id ~video ~duration ~caption ~disable_notification ~reply_to ~reply_markup |> eval f
    | ResendVideo (chat_id, video, duration, caption, disable_notification, reply_to, reply_markup) -> resend_video ~chat_id ~video ~duration ~caption ~disable_notification ~reply_to ~reply_markup |> dispose
    | SendVoice (chat_id, voice, disable_notification, reply_to, reply_markup, f) -> send_voice ~chat_id ~voice ~disable_notification ~reply_to ~reply_markup |> eval f
    | ResendVoice (chat_id, voice, disable_notification, reply_to, reply_markup) -> resend_voice ~chat_id ~voice ~disable_notification ~reply_to ~reply_markup |> dispose
    | SendLocation (chat_id, latitude, longitude, disable_notification, reply_to, reply_markup) -> send_location ~chat_id ~latitude ~longitude ~disable_notification ~reply_to ~reply_markup |> dispose
    | SendVenue (chat_id, latitude, longitude, title, address, foursquare_id, disable_notification, reply_to, reply_markup) -> send_venue ~chat_id ~latitude ~longitude ~title ~address ~foursquare_id ~disable_notification ~reply_to ~reply_markup |> dispose
    | SendContact (chat_id, phone_number, first_name, last_name, disable_notification, reply_to, reply_markup) -> send_contact ~chat_id ~phone_number ~first_name ~last_name ~disable_notification ~reply_to ~reply_markup |> dispose
    | GetUserProfilePhotos (user_id, offset, limit, f) -> get_user_profile_photos ~user_id ~offset ~limit |> eval f
    | GetFile (file_id, f) -> get_file ~file_id |> eval f
    | GetFile' (file_id, f) -> get_file' ~file_id |> eval f
    | DownloadFile (file, f) -> download_file ~file |> eval f
    | KickChatMember (chat_id, user_id) -> kick_chat_member ~chat_id ~user_id |> dispose
    | LeaveChat chat_id -> leave_chat ~chat_id |> dispose
    | UnbanChatMember (chat_id, user_id) -> unban_chat_member ~chat_id ~user_id |> dispose
    | GetChat (chat_id, f) -> get_chat ~chat_id |> eval f
    | GetChatAdministrators (chat_id, f) -> get_chat_administrators ~chat_id |> eval f
    | GetChatMembersCount (chat_id, f) -> get_chat_members_count ~chat_id |> eval f
    | GetChatMember (chat_id, user_id, f) -> get_chat_member ~chat_id ~user_id |> eval f
    | AnswerCallbackQuery (callback_query_id, text, show_alert) -> answer_callback_query ~callback_query_id ~text ~show_alert () |> dispose
    | AnswerInlineQuery (inline_query_id, results, cache_time, is_personal, next_offset) -> answer_inline_query ~inline_query_id ~results ~cache_time ~is_personal ~next_offset () |> dispose
    | EditMessageText (id, text, parse_mode, disable_web_page_preview, reply_markup) ->
        (identify edit_message_text id) ~text ~parse_mode ~disable_web_page_preview ~reply_markup () |> dispose
    | EditMessageCaption (id, caption, reply_markup) ->
        (identify edit_message_caption id) ~caption ~reply_markup () |> dispose
    | EditMessageReplyMarkup (id, reply_markup) ->
        (identify edit_message_reply_markup id) ~reply_markup () |> dispose
    | GetUpdates f -> get_updates |> eval f
    | PeekUpdate f -> peek_update |> eval f
    | PopUpdate (run_cmds, f) -> pop_update ~run_cmds () |> eval f
    | Chain (first, second) -> evaluator first >> evaluator second in
  let rec loop () =
    let%lwt all = Actions_queue.get_all () in
    let%lwt () = evaluator all in
    pop_update ~run_cmds:true () >>= process >>= loop in
  while true do (* Recover from errors if an exception is thrown *)
    try Lwt_main.run @@ loop ()
    with _ -> ()
  done