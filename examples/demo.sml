(* demo.sml

   A tiny tour of `Redis`: build two client commands (SET then GET), then parse
   the byte stream a server would send back, all in pure SML -- no sockets.
   Build and run with `make example`.

   We print each frame with its CRLFs made visible ("\r\n") so the on-wire
   bytes are legible, alongside the decoded value. *)

structure R = Redis

fun line s = print (s ^ "\n")

(* Render raw protocol bytes with CR/LF shown as escapes for readability. *)
fun visible s =
  String.translate
    (fn #"\r" => "\\r" | #"\n" => "\\n" | c => String.str c)
    s

fun show (label, raw) =
  line ("  " ^ label ^ ": " ^ visible raw)

(* Pretty-print a decoded RESP value (one line). *)
fun respStr (R.Simple s)        = "Simple \"" ^ s ^ "\""
  | respStr (R.Error s)         = "Error \"" ^ s ^ "\""
  | respStr (R.Int i)           = "Int " ^ Int.toString i
  | respStr (R.Bulk NONE)       = "Bulk NONE"
  | respStr (R.Bulk (SOME s))   = "Bulk (SOME \"" ^ s ^ "\")"
  | respStr (R.Array NONE)      = "Array NONE"
  | respStr (R.Array (SOME xs)) =
      "Array [" ^ String.concatWith ", " (List.map respStr xs) ^ "]"

fun parseAndShow (label, raw) =
  (show (label, raw);
   case R.decode raw of
       SOME (v, n) =>
         line ("           -> decoded: " ^ respStr v
               ^ "  (" ^ Int.toString n ^ " bytes)")
     | NONE => line "           -> decoded: <incomplete / malformed>")

val () = line "=== sml-redis: a SET / GET exchange (pure RESP, no sockets) ==="
val () = line ""

(* --- Client -> server: SET greeting "hello, redis" --- *)
val () = line "Client sends:"
val setCmd = R.cmd ["SET", "greeting", "hello, redis"]
val () = show ("SET", setCmd)

(* --- Server -> client: +OK --- *)
val () = line ""
val () = line "Server replies:"
val () = parseAndShow ("reply", R.encode (R.Simple "OK"))

(* --- Client -> server: GET greeting --- *)
val () = line ""
val () = line "Client sends:"
val getCmd = R.cmd ["GET", "greeting"]
val () = show ("GET", getCmd)

(* --- Server -> client: $12\r\nhello, redis\r\n --- *)
val () = line ""
val () = line "Server replies:"
val () = parseAndShow ("reply", R.encode (R.Bulk (SOME "hello, redis")))

(* --- A pipelined reply stream: parse values back-to-back from one buffer --- *)
val () = line ""
val () = line "Pipelined reply stream (decode one frame at a time):"
val stream =
  String.concat
    [ R.encode (R.Int 1)
    , R.encode (R.Simple "PONG")
    , R.encode (R.Array (SOME [R.Bulk (SOME "a"), R.Bulk (SOME "b")]))
    , R.encode (R.Bulk NONE) ]

fun drain (buf, idx) =
  if buf = "" then ()
  else
    case R.decode buf of
        NONE => line ("  <waiting for more bytes at frame " ^ Int.toString idx ^ ">")
      | SOME (v, n) =>
          (line ("  frame " ^ Int.toString idx ^ ": " ^ respStr v);
           drain (String.extract (buf, n, NONE), idx + 1))

val () = drain (stream, 0)

val () = line ""
val () = line "OK -- built and parsed a full exchange with the RESP codec."
