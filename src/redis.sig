(* redis.sig

   A pure RESP (REdis Serialization Protocol) codec plus Redis command
   builders. No sockets, no FFI, no I/O -- just bytes in, bytes out -- so the
   same code runs identically under MLton and Poly/ML and is trivial to test.

   RESP (the protocol Redis speaks on the wire) has five value kinds, modelled
   by the `resp` datatype:

     - Simple of string         "+OK\r\n"            a status line
     - Error  of string         "-ERR bad\r\n"       an error line
     - Int    of int            ":1000\r\n"          a 64-bit signed integer
     - Bulk   of string option  "$5\r\nhello\r\n"    a length-prefixed blob;
                                                     NONE is the null bulk "$-1\r\n"
     - Array  of resp list option
                                "*2\r\n...\r\n..."    an array of values;
                                                     NONE is the null array "*-1\r\n"

   The byte container is `string`: RESP bulk payloads are arbitrary bytes and
   SML `string` is a byte vector, so it interoperates cleanly with the rest of
   the ecosystem (sml-buffer, sml-codec, ...). All line terminators are CRLF. *)

signature REDIS =
sig
  datatype resp =
      Simple of string
    | Error  of string
    | Int    of int
    | Bulk   of string option
    | Array  of resp list option

  (* `encode v` serializes a RESP value to its on-wire byte string. Negative
     integers use a leading '-' (RESP convention), never SML's '~'. *)
  val encode : resp -> string

  (* `decode bytes` parses a single RESP value from the FRONT of `bytes`,
     returning the value together with the number of bytes it consumed (so the
     caller can advance a stream cursor). It returns NONE when the input is
     truncated (not enough bytes have arrived yet) or malformed. Trailing bytes
     beyond the first complete value are ignored and reflected in the unused
     remainder (size bytes - consumed). *)
  val decode : string -> (resp * int) option

  (* `cmd args` frames a client command as a RESP array of bulk strings, e.g.
     cmd ["SET", "k", "v"] = "*3\r\n$3\r\nSET\r\n$1\r\nk\r\n$1\r\nv\r\n". *)
  val cmd : string list -> string

  (* ---- RESP3 (additive) -------------------------------------------------

     RESP3 extends RESP2 with several new value kinds. Because the original
     `resp` datatype (and its exhaustive `encode`/`decode`) must stay intact,
     RESP3 is modelled by a SEPARATE datatype `value3` with its own
     `encode3`/`decode3`. `value3` also carries the RESP2 kinds it needs for
     nesting (a Map value, Set element or Push payload can be any value3).

       - Null                       "_\r\n"
       - Boolean of bool            "#t\r\n" / "#f\r\n"
       - Double of real             ",3\r\n", ",3.25\r\n", ",inf\r\n", ",-inf\r\n"
       - BigNumber of string        "(3492890328409238509324850943850943825024385\r\n"
                                    (kept as a string -- magnitudes exceed `int`)
       - Verbatim of (fmt, content) "=15\r\ntxt:Some string\r\n"
                                    (the on-wire length counts the "fmt:" prefix)
       - Map of (value3*value3) list  "%2\r\n+first\r\n:1\r\n+second\r\n:2\r\n"
       - Set of value3 list           "~2\r\n:1\r\n:2\r\n"
       - Push of value3 list          ">3\r\n+message\r\n+channel\r\n+payload\r\n"
       (carryover RESP2 kinds, for nesting / standalone use)
       - SimpleString of string     "+OK\r\n"
       - SimpleError of string      "-ERR bad\r\n"
       - Integer of int             ":1000\r\n"
       - BlobString of string option "$5\r\nhello\r\n" / null "$-1\r\n"
       - Array3 of value3 list option "*2\r\n...\r\n..." / null "*-1\r\n" *)
  datatype value3 =
      Null
    | Boolean of bool
    | Double of real
    | BigNumber of string
    | Verbatim of string * string
    | Map of (value3 * value3) list
    | Set of value3 list
    | Push of value3 list
    | SimpleString of string
    | SimpleError of string
    | Integer of int
    | BlobString of string option
    | Array3 of value3 list option

  (* `doubleToString r` renders a RESP3 Double payload (the part after ',').
     The format is fixed so it is byte-identical under MLton and Poly/ML:
       - non-finite: "inf", "-inf", "nan";
       - integral values: no decimal point ("3", "-3");
       - otherwise: a fixed-precision decimal with trailing zeros stripped
         ("3.25"). Negatives use a leading '-', never SML's '~'. *)
  val doubleToString : real -> string

  (* `encode3 v` serializes a RESP3 value to its on-wire byte string;
     `decode3 bytes` parses one value3 from the front, returning the value and
     the number of bytes consumed (NONE when truncated or malformed) -- exactly
     the contract of `encode`/`decode`. *)
  val encode3 : value3 -> string
  val decode3 : string -> (value3 * int) option

  (* Typed command builders. Each frames the standard RESP2 array-of-bulk-
     strings request a client sends (same byte type as `encode`/`cmd`).
       set ("key","val") = "*3\r\n$3\r\nSET\r\n$3\r\nkey\r\n$3\r\nval\r\n"
     `pipeline` concatenates a list of already-encoded commands. *)
  val set      : string * string -> string
  val get      : string -> string
  val hget     : string * string -> string
  val hset     : string * string * string -> string
  val pipeline : string list -> string
end
