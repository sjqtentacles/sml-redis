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
end
