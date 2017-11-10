type price = NoStock | Stock of float

exception Http_error

let get_html url =
  let open Lwt in
  let open Cohttp in
  let open Cohttp_lwt_unix in
  Client.get (Uri.of_string url) >>= fun (resp, body) ->
  let code = resp |> Response.status |> Code.code_of_status in
  if (code = 200) then begin
    body |> Cohttp_lwt.Body.to_string
  end
  else begin
    raise Http_error
  end