(* buffer.sig

   A growable, mutable byte/char buffer for assembling strings without the
   O(n^2) cost of repeated `^` concatenation. Backed by a doubling char
   array; `contents` materializes the accumulated bytes as a string.

   This is the workhorse for building HTTP responses, serialized messages,
   and any other place a server emits a stream of small fragments. *)

signature BUFFER =
sig
  type buffer

  (* Create an empty buffer. The hint is an initial capacity in bytes; it is
     only an optimization (a non-positive hint is clamped to a small default). *)
  val new       : int -> buffer
  val empty     : unit -> buffer

  (* Current number of bytes held. *)
  val length    : buffer -> int
  val isEmpty   : buffer -> bool

  (* Append operations (all mutate the buffer in place). *)
  val addChar   : buffer -> char -> unit
  val addString : buffer -> string -> unit
  val addSubstring : buffer -> substring -> unit
  (* Append the contents of another buffer (the source is not modified). *)
  val addBuffer : buffer -> buffer -> unit

  (* Materialize. `contents` returns all accumulated bytes; `sub` reads a
     single byte (raises Subscript if out of range). *)
  val contents  : buffer -> string
  val sub       : buffer -> int -> char

  (* Reset to empty, keeping the allocated capacity for reuse. *)
  val clear     : buffer -> unit

  (* Build a string by running a function that appends into a fresh buffer.
     `build (fn b => ...)` is the idiomatic entry point. *)
  val build     : (buffer -> unit) -> string

  (* Concatenate a list of strings via a single buffer pass. *)
  val concat    : string list -> string
  val concatWith : string -> string list -> string
end
