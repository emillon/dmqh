open Dmqh

let border = 10
let cell_w = 80
let cell_h = 80
let inter_cell = 10
let win_w = 2 * border + 4 * cell_w + 3 * inter_cell
let win_h = 2 * border + 4 * cell_h + 3 * inter_cell

(* http://www.colourlovers.com/palette/3298310/lonely_hands. *)

let c1 = 0xEDEAEAl
let c2 = 0xF5CCBEl
let c3 = 0xE7A19El
let c4 = 0xBD8996l
let c5 = 0x98829Al

let redof x = (Int32.to_int x land 0xff0000) lsr 16
let greenof x = (Int32.to_int x land 0xff00) lsr 8
let blueof x = (Int32.to_int x land 0xff)

let fontcolor = (redof c1, greenof c1, blueof c1)

let bgcolor = c1
let emptycolor = c2
let tilecolor = function
  | T2 | T4 | T8 | T16 -> c3
  | T32 | T64 | T128 -> c4
  | T256 | T512 | T1024 | T2048 -> c5

let rect_at i j =
  let x = border + i * (cell_w + inter_cell) in
  let y = border + j * (cell_h + inter_cell) in
  let w = cell_w in
  let h = cell_h in
  Sdlvideo.rect ~x ~y ~w ~h

let draw_tile =
  let open Sdlttf in
  init ();
  let font = open_font "/usr/share/fonts/truetype/ttf-dejavu/DejaVuSansMono.ttf" 60 in
  fun win i j t ->
    let rect = rect_at i j in
    Sdlvideo.fill_rect ~rect win (tilecolor t);
    let msg = pr_tile t in
    let text = render_text_solid font msg ~fg:fontcolor in
    Sdlvideo.blit_surface ~src:text ~dst:win ~dst_rect:rect ()

let clear win =
  Sdlvideo.fill_rect win bgcolor

let draw_empty win i j =
  let rect = rect_at i j in
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
  | KEYDOWN { keysym = KEY_q } -> raise Exit
  | _ -> None


let main () =
  Sdl.init ~auto_clean:true [`VIDEO];
  let win = Sdlvideo.set_video_mode ~w:win_w ~h:win_h [] in
  let b = empty_board () in
  add_random b;
  while true do
    clear win;
    draw_board win b;
    Sdlvideo.flip win;
    begin match handle_events win with
    | Some (Move d) -> (move b d; add_random b)
    | None -> ()
    end;
  done

let _ = main ()
