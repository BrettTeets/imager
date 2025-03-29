(* open OUnit2
open Imagine.Mappy
open Imagine.Process

let _guassian7F = [0.;  0.;  1.;   2.;  1.;  0.; 0.;
0.;  3.; 13.;  22.; 13.;  3.; 0.;
1.; 13.; 59.;  97.; 59.; 13.; 1.;
2.; 22.; 97.; 159.; 97.; 22.; 2.;
1.; 13.; 59.;  97.; 59.; 13.; 1.;
0.;  3.; 13.;  22.; 13.;  3.; 0.;
0.;  0.;  1.;   2.;  1.;  0.; 0.;]  *)

(* let test1 _ = 
  try (
  let a = Mappy.create_gray 10 10 in
  let b = Mappy.create_gray 10 10 in
  ignore @@ Process.kernel_7 a b _guassian7F 1003.);
  assert_bool "unreachable" true with
  | Invalid_argument _ -> assert_bool "Test 1: out of bounds failure" false
  | _ -> assert_bool "Unknown error." false
;; *)

(* let suite =
  "suite">:::
   ["test1">:: test1;]
;; *)

(* let () =
  run_test_tt_main suite
;; *)