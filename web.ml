module H = Dom_html

let js = Js.string

let main () =
  let document = H.document in
  let game =
    Js.Opt.get (document##getElementById(js"game"))
      (fun () -> assert false)
  in
  let canvas = H.createCanvas document in
  canvas##width <- Lookandfeel.win_w;
  canvas##height <- Lookandfeel.win_h;
  Dom.appendChild game canvas

let _ = main ()
