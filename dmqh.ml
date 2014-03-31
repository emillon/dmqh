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
exception Lose

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

type board = tile option array array

let empty_board () =
  Array.make_matrix 4 4 None

let random_list l =
  let len = List.length l in
  List.nth l (Random.int len)

let (>>=) l f = List.concat (List.map f l)

let all_idx = [0;1;2;3]

let random_avail b =
  let all_idx2 =
    all_idx >>= fun i ->
    all_idx >>= fun j ->
    [(i, j)]
  in
  let avail = List.filter (fun (i, j) -> b.(i).(j) = None) all_idx2 in
  if avail = [] then
    raise Lose
  else
    random_list avail

let add_random b =
  let (i, j) = random_avail b in
  b.(i).(j) <- Some T2

type direction = U | D | L | R

type event =
  | Move of direction
  | New_game

let rec partition p = function
  | [] -> ([], [])
  | x::xs ->
      let (t, f) = partition p xs in
      if p x then
        (x::t, f)
      else
        (t, x::f)

let gravity l =
  let (n, o) = partition (fun x -> x = None) l
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
