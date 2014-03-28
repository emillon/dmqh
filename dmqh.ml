type tile = T2 | T4 | T8 | T16 | T32 | T64 | T128 | T256 | T512 | T1024 | T2048

type board = tile option array array

let empty_board () =
  Array.make_matrix 4 4 None

let add_random b =
  assert false

let clear b =
  assert false

let draw_empty win i j =
  assert false

let draw_tile win i j t =
  assert false

let draw_board win board =
  for i = 1 to 4 do
    for j = 1 to 4 do
      match board.(i).(j) with
      | None -> draw_empty win i j
      | Some t -> draw_tile win i j t
    done
  done

type direction = U | D | L | R

type event =
  | Move of direction

let wait_event win =
  assert false

let move d =
  assert false

let border = 10
let cell_w = 80
let cell_h = 80
let inter_cell = 10
let win_w = 2 * border + 4 * cell_w + 3 * inter_cell
let win_h = 2 * border + 4 * cell_h + 3 * inter_cell

let main () =
  Sdl.init ~auto_clean:true [`VIDEO];
  let win = Sdlvideo.set_video_mode ~w:win_w ~h:win_h [] in
  let b = empty_board () in
  while true do
    add_random b;
    clear win;
    draw_board win b;
    begin match wait_event win with
    | Move d -> move d
    end
  done

let _ = main ()
