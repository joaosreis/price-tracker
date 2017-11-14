include Telegram.Api.Mk(struct
    include Telegram.BotDefaults
    let token = "464437218:AAHFBQawqGEVSgg2ieg5k-SFE1kpXEe4ehI"

    let commands = let open Telegram.Api.Command in
      [{name = "track"; description = "Track this product"; enabled = true; run = Commands.track}]
  end)