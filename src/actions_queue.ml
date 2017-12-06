open Batteries
open Telegram

let mutex = Lwt_mutex.create ()

let q : (Telegram.Api.Command.action) Queue.t = Queue.create ()

let get_all () =
  let%lwt _ = Lwt_mutex.lock mutex in
  let all = Queue.enum q |> List.of_enum |> Actions.sequence in
  let _ = Lwt_mutex.unlock mutex in
  Lwt.return all

let push action =
  let%lwt _ = Lwt_mutex.lock mutex in
  let () = Queue.push action q in
  let _ = Lwt_mutex.unlock mutex in
  Lwt.return_unit