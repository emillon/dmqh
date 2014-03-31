open Dmqh
open Lookandfeel

let sdl_rect_at i j =
  let (x, y, w, h) = rect_at i j in
  Sdlvideo.rect ~x ~y ~w ~h

let draw_text =
  let open Sdlttf in
  init ();
  let font = open_font "/usr/share/fonts/truetype/ttf-dejavu/DejaVuSansMono.ttf" 60 in
  fun win dst_rect msg fg ->
    let text = render_text_solid font msg ~fg in
    Sdlvideo.blit_surface ~src:text ~dst:win ~dst_rect ()

let draw_tile win i j t =
  let rect = sdl_rect_at i j in
  Sdlvideo.fill_rect ~rect win (tilecolor t);
  let msg = pr_tile t in
  draw_text win rect msg fontcolor

let clear win =
  Sdlvideo.fill_rect win bgcolor

let draw_empty win i j =
  let rect = sdl_rect_at i j in
  Sdlvideo.fill_rect ~rect win emptycolor

let draw_board win board =
  for i = 0 to 3 do
    for j = 0 to 3 do
      match board.(i).(j) with
      | None -> draw_empty win i j
      | Some t -> draw_tile win i j t
    done
  done

let handle_events win =
  let open Sdlkey in
  let open Sdlevent in
  pump();
  match wait_event () with
  | KEYDOWN { keysym = KEY_LEFT } -> Some (Move L)
  | KEYDOWN { keysym = KEY_RIGHT } -> Some (Move R)
  | KEYDOWN { keysym = KEY_UP } -> Some (Move U)
  | KEYDOWN { keysym = KEY_DOWN } -> Some (Move D)
  | KEYDOWN { keysym = KEY_r } -> Some New_game
  | KEYDOWN { keysym = KEY_q } -> raise Exit
  | _ -> None

let rec play_game win =
  let b = empty_board () in
  add_random b;
  while true do
    clear win;
    draw_board win b;
    Sdlvideo.flip win;
    begin match handle_events win with
    | Some (Move d) -> (move b d; add_random b)
    | Some New_game -> play_game win
    | None -> ()
    end;
  done

let main () =
  Sdl.init ~auto_clean:true [`VIDEO];
  let win = Sdlvideo.set_video_mode ~w:win_w ~h:win_h [] in
  while true do
    try play_game win with
    | Lose -> begin
      clear win;
      let rect = Sdlvideo.rect ~x:10 ~y:10 ~w:0 ~h:0 in
      draw_text win rect "You lose!" Sdlvideo.black;
      Sdlvideo.flip win;
      let rec wait_r () =
        match handle_events win with
        | Some New_game -> ()
        | _ -> wait_r ()
      in
      wait_r ()
    end
  done

let _ = main ()
