let border = 10
let cell_w = 80
let cell_h = 80
let inter_cell = 10
let win_w = 2 * border + 4 * cell_w + 3 * inter_cell
let win_h = 2 * border + 4 * cell_h + 3 * inter_cell
let bgcolor = 0xff0000l
let emptycolor = 0x00ff00l

type tile = T2 | T4 | T8 | T16 | T32 | T64 | T128 | T256 | T512 | T1024 | T2048

let all_tiles =
  [T2 ; T4 ; T8 ; T16 ; T32 ; T64 ; T128 ; T256 ; T512 ; T1024 ; T2048]

type board = tile option array array

let empty_board () =
  Array.make_matrix 4 4 None

let random_list l =
  let len = List.length l in
  List.nth l (Random.int len)

let random_tile () =
  random_list all_tiles

let random_avail b =
  let (>>=) l f = List.concat (List.map f l) in
  let all_idx = [0;1;2;3] in
  let all_idx2 =
    all_idx >>= fun i ->
    all_idx >>= fun j ->
    [(i, j)]
  in
  let avail = List.filter (fun (i, j) -> b.(i).(j) = None) all_idx2 in
  random_list avail

let add_random b =
  let (i, j) = random_avail b in
  b.(i).(j) <- Some (random_tile ())

let clear win =
  Sdlvideo.fill_rect win bgcolor

let rect_at i j =
  let x = border + i * (cell_w + inter_cell) in
  let y = border + j * (cell_h + inter_cell) in
  let w = cell_w in
  let h = cell_h in
  Sdlvideo.rect ~x ~y ~w ~h

let draw_empty win i j =
  let rect = rect_at i j in
  Sdlvideo.fill_rect ~rect win emptycolor

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
