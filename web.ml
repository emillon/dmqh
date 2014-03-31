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
  ctx##fillRect (float x, float y, float w, float h);
  ctx##fillStyle <- js"black";
  ctx##fillText (js(Dmqh.pr_tile t),
    float x +. float Lookandfeel.cell_w /. 2.,
    float y +. float Lookandfeel.cell_h /. 2.)

let draw_board ctx board =
  ctx##fillStyle <- color (Lookandfeel.bgcolor);
  ctx##fillRect (0., 0., float Lookandfeel.win_w, float Lookandfeel.win_h);
  for i = 0 to 3 do
    for j = 0 to 3 do
      match board.(i).(j) with
      | None -> draw_empty ctx i j
      | Some t -> draw_tile ctx i j t
    done
  done

let parse_ev e =
  let open Dmqh in
  match e##keyCode with
  | 37 -> Some (Move L)
  | 38 -> Some (Move U)
  | 39 -> Some (Move R)
  | 40 -> Some (Move D)
  | 82 -> Some New_game
  | _ -> None

let rec play_game ctx =
  let b = Dmqh.empty_board () in
  Dmqh.add_random b;
  draw_board ctx b;
  H.document##onkeydown <- H.handler (fun e ->
    let handle_lose () =
      ctx##fillStyle <- js"black";
      ctx##fillText (js"You lose!",
        float Lookandfeel.win_w /. 2.,
        float Lookandfeel.win_h /. 2.);
      H.document##onkeydown <- H.handler (fun e ->
        begin match parse_ev e with
          | Some Dmqh.New_game -> play_game ctx
          | _ -> ();
        end;
        Js._true
      )
    in
    let handle_move d =
      try
        Dmqh.move b d;
        Dmqh.add_random b;
        draw_board ctx b
      with Dmqh.Lose -> handle_lose ()
    in
    begin match parse_ev e with
    | None -> ()
    | Some (Dmqh.Move d) -> handle_move d
    | Some Dmqh.New_game -> play_game ctx
    end; Js._true)

let main () =
  let game =
    Js.Opt.get (H.document##getElementById(js"game"))
      (fun () -> assert false)
  in
  let canvas = H.createCanvas H.document in
  canvas##width <- Lookandfeel.win_w;
  canvas##height <- Lookandfeel.win_h;
  Dom.appendChild game canvas;
  let ctx = canvas##getContext (H._2d_) in
  play_game ctx

let _ = main ()
