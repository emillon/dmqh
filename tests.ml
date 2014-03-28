open Dmqh

let run_tests () =
  let p_list prn l =
    "[" ^ String.concat " " (List.map prn l) ^ "]"
  in
  let tc (input, exp) =
    let printer l =
      p_list pr_tileo l
    in
    (printer input, `Quick, fun () ->
      OUnit.assert_equal ~printer exp (move_row input)
    )
  in
  Alcotest.run "DMQH" [("move", List.map tc
      [ [None;None;None;None], [None;None;None;None]
      ; [None;Some T2;None;None], [Some T2;None;None;None]
      ; [Some T2;Some T2;None;None], [Some T4;None;None;None]
      ; [None;Some T2;Some T2;None], [Some T4;None;None;None]
      ; [Some T16;Some T2;Some T2;None], [Some T16;Some T4;None;None]
      ; [Some T2;Some T2;Some T4;Some T4], [Some T4;Some T8;None;None]
      ; [Some T2;Some T2;Some T2;None], [Some T4;Some T2;None;None]
      ; [Some T16;None;Some T2;Some T2], [Some T16;Some T4;None;None]
      ]
  )]

let _ = run_tests ()
