let border = 10
let cell_w = 80
let cell_h = 80
let inter_cell = 10
let win_w = 2 * border + 4 * cell_w + 3 * inter_cell
let win_h = 2 * border + 4 * cell_h + 3 * inter_cell
let bgcolor = 0xff0000l
let emptycolor = 0x00ff00l
let tilecolor = 0x0000ffl

type tile = T2 | T4 | T8 | T16 | T32 | T64 | T128 | T256 | T512 | T1024 | T2048

let pr_tile = function
  | T2 -> "2"
  | T4 -> "4"
  | T8 -> "8"
  | T16 -> "16"
  | T32 -> "32"
  | T64 -> "64"
  | T128 -> "128"
  | T256 -> "256"
  | T512 -> "512"
  | T1024 -> "1024"
  | T2048 -> "2048"

exception Win

let next_tile = function
  | T2 -> T4
  | T4 -> T8
  | T8 -> T16
  | T16 -> T32
  | T32 -> T64
  | T64 -> T128
  | T128 -> T256
  | T256 -> T512
  | T512 -> T1024
  | T1024 -> T2048
  | T2048 -> raise Win

let pr_tileo = function
  | None -> "."
  | Some t -> pr_tile t

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

let (>>=) l f = List.concat (List.map f l)

let all_idx = [0;1;2;3]

let random_avail b =
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
  let rect = rect_at i j in
  Sdlvideo.fill_rect ~rect win tilecolor

let draw_board win board =
  for i = 0 to 3 do
    for j = 0 to 3 do
      match board.(i).(j) with
      | None -> draw_empty win i j
      | Some t -> draw_tile win i j t
    done
  done

type direction = U | D | L | R

type event =
  | Move of direction

let wait_event win =
  let open Sdlkey in
  Sdlevent.pump();
  match Sdlevent.wait_event () with
  | Sdlevent.KEYDOWN { Sdlevent.keysym = KEY_LEFT } -> Some (Move L)
  | _ -> None

let rec span p = function
  | [] -> ([], [])
  | x::xs when p x -> let (t, f) = span p xs in (x::t, f)
  | xs -> ([], xs)

let gravity l =
  let (n, o) = span (fun x -> x = None) l
  in
  o @ n

let rec merge_tiles = function
  | (Some x)::(Some x')::xs when x = x' -> Some (next_tile x):: (merge_tiles xs) @ [None]
  | x::xs -> x::merge_tiles xs
  | [] -> []

let move_row l =
  merge_tiles (gravity l)

let move_list b xs =
  let elems = List.map (fun (i, j) -> b.(i).(j)) xs in
  let elems' = move_row elems in
  List.iter2 (fun (i, j) t -> b.(i).(j) <- t) xs elems'

let row x = List.map (fun i -> (i, x)) all_idx
let col x = List.map (fun j -> (x, j)) all_idx

let move b d =
  let all_cols = List.map col all_idx in
  let all_rows = List.map row all_idx in
  let lists = match d with
    | U -> all_cols
    | L -> all_rows
    | D -> List.map List.rev all_cols
    | R -> List.map List.rev all_rows
  in
  List.iter (move_list b) lists

let run_tests () =
  let p_couple prn (x, y) =
    "(" ^ prn x ^ ", " ^ prn y ^ ")"
  in
  let p_list prn l =
    "[" ^ String.concat " " (List.map prn l) ^ "]"
  in
  let tc (input, exp) =
    let printer l =
      p_list pr_tileo l
    in
    OUnit.assert_equal ~printer exp (move_row input)
  in
  let printer = p_couple (p_list string_of_int) in
  OUnit.assert_equal ~printer ([1;2;3], [4;5]) (span (fun x -> x <= 3) [1;2;3;4;5]);
  List.iter tc
    [ [None;None;None;None], [None;None;None;None]
    ; [None;Some T2;None;None], [Some T2;None;None;None]
    ; [Some T2;Some T2;None;None], [Some T4;None;None;None]
    ; [None;Some T2;Some T2;None], [Some T4;None;None;None]
    ; [Some T16;Some T2;Some T2;None], [Some T16;Some T4;None;None]
    ; [Some T2;Some T2;Some T4;Some T4], [Some T4;Some T8;None;None]
    ; [Some T2;Some T2;Some T2;None], [Some T4;Some T2;None;None]
    ];
  print_endline "OK"

let main () =
  Sdl.init ~auto_clean:true [`VIDEO];
  let win = Sdlvideo.set_video_mode ~w:win_w ~h:win_h [] in
  let b = empty_board () in
  add_random b;
  while true do
    clear win;
    draw_board win b;
    Sdlvideo.flip win;
    begin match wait_event win with
    | Some (Move d) -> (move b d; add_random b)
    | None -> ()
    end;
    print_endline "tick"
  done

let _ =
  match Sys.argv with
  | [| _ ; "-t" |] -> run_tests ()
  | _ -> main ()
