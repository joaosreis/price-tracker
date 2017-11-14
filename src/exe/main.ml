let () =
  let url1 = "https://www.fnac.pt/Doom-Nintendo-Switch-Jogo-Nintendo-Switch/a1360369" in
  let interval1 = 5 in
  let thread1 = Watcher.get_thread 87784254 url1 interval1 in
  let url2 = "https://www.fnac.pt/Pokemon-Ultra-Sun-3DS-Jogo-Nintendo-3DS/a1263331" in
  let interval2 = 5 in
  let thread2 = Watcher.get_thread 87784254 url2 interval2 in
  Lwt.async thread1;
  Lwt.async thread2;
  Bot.run ()
  (*let never_terminate = fst (Lwt.wait ()) in
  Lwt_main.run never_terminate*)