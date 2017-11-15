include Telegram.Api.Mk(struct
    include Telegram.BotDefaults
    let token = Configuration.token

    let commands = let open Telegram.Api.Command in
      [{name = "start"; description = "Registar"; enabled = true; run = Commands.start};
       {name = "track"; description = "Track this product"; enabled = true; run = Commands.track}]
  end)