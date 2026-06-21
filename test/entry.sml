(* entry.sml

   Defines `main : unit -> unit`, the entry point used by both compilers.
   It runs the suite, prints the harness summary, and exits with a status
   code reflecting success. *)

fun main () =
  let val ok = Tests.run ()
  in if ok then OS.Process.exit OS.Process.success
     else OS.Process.exit OS.Process.failure
  end
