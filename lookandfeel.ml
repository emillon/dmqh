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
  (x, y, w, h)
