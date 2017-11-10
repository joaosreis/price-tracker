let () =
  let module F1 = Watcher.Make(Fnac)
      (struct
        let url = "https://www.fnac.pt/Doom-Nintendo-Switch-Jogo-Nintendo-Switch/a1360369"
        let interval = 5000
      end) in
  let module F2 = Watcher.Make(Fnac)
      (struct
        let url = "https://www.fnac.pt/Pokemon-Ultra-Sun-3DS-Jogo-Nintendo-3DS/a1263331"
        let interval = 5000
      end) in
  let _ = Lwt_main.run F1.run in
  let _ = Lwt_main.run F2.run in
  ()