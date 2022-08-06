let () =
  let tests = D01.collect_tests () @ D02.collect_tests () in

  let open Alcotest in
  run "advent" tests

(* D01.run_tests (); *)
(* D02.run_tests (); *)
