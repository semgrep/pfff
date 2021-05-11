(*
   Test suite for this folder.
*)

open Printf
open OUnit

let test_parallel_invoke_res () =
  assert_equal true ((Parallel.invoke (fun x -> x + x) 1) () = 2)

let test_parallel_invoke_exn () =
  try
    (Parallel.invoke (fun _ -> if true then raise Exit) ()) ();
    assert false
  with _unmatchable_exception ->
    ()

let test_parallel_invoke_crash () =
  assert_equal true (
    try
      (Parallel.invoke
         (fun _ ->
            (*
               Killing self with sigsegv appears to not work in native code,
               but other signals cause process termination as expected.
               See discussion started at
               https://discuss.ocaml.org/t/delivering-sigsegv-to-self-in-native-code/7837
            *)
            Unix.kill (Unix.getpid ()) Sys.sigterm
         ) ()) ();
      eprintf "Parallel.invoke should have raised an exception.\n%!";
      false
    with
    | Failure errmsg ->
        (try
           Scanf.sscanf errmsg
             "process %i was killed by signal sigterm: Termination"
             (fun _pid -> ());
           true
         with _ ->
           eprintf "Unexpected error message: %s\n%!" errmsg;
           false
        )
    | e ->
        eprintf "Not the exception we were expecting: %s\n%!"
          (Printexc.to_string e);
        false
  )

let unittest =
  "commons_core" >::: [
    "parallel" >::: [
      "invoke res" >:: test_parallel_invoke_res;
      "invoke exn" >:: test_parallel_invoke_exn;
      "invoke crash" >:: test_parallel_invoke_crash;
    ]
  ]
