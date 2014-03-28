module H = Dom_html

let js = Js.string

let color c =
  let open Lookandfeel in
  js(Printf.sprintf "rgb(%d,%d,%d)" (redof c) (greenof c) (blueof c))

let draw_empty ctx i j =
  let (x, y, w, h) = Lookandfeel.rect_at i j in
  ctx##fillStyle <- color Lookandfeel.emptycolor;
  ctx##fillRect (float x, float y, float w, float h)

let draw_tile ctx i j t =
  let (x, y, w, h) = Lookandfeel.rect_at i j in
  ctx##fillStyle <- color (Lookandfeel.tilecolor t);
  ctx##fillRect (float x, float y, float w, float h)

let draw_board ctx board =
  for i = 0 to 3 do
    for j = 0 to 3 do
      match board.(i).(j) with
      | None -> draw_empty ctx i j
      | Some t -> draw_tile ctx i j t
    done
  done

let ev_to_dir _ = Some (Dmqh.D)

let main () =
  let document = H.document in
  let game =
    Js.Opt.get (document##getElementById(js"game"))
      (fun () -> assert false)
  in
  let canvas = H.createCanvas document in
  canvas##width <- Lookandfeel.win_w;
  canvas##height <- Lookandfeel.win_h;
  Dom.appendChild game canvas;
  let ctx = canvas##getContext (H._2d_) in
  let b = Dmqh.empty_board () in
  Dmqh.add_random b;
  draw_board ctx b;
  document##onkeydown <- H.handler (fun e ->
    begin match ev_to_dir e with
    | None -> ()
    | Some d -> Dmqh.move b d; Dmqh.add_random b; draw_board ctx b
    end; Js._true)

let _ = main ()
