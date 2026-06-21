(* redis.sml

   Implementation of REDIS: a pure RESP (REdis Serialization Protocol) codec
   and command builders. No sockets and no I/O -- `encode`/`decode` move bytes
   between `string` and the `resp` datatype, so the whole thing is
   deterministic and identical under MLton and Poly/ML.

   Encoding assembles bytes through the vendored sml-buffer (`Buffer.build`),
   which avoids the O(n^2) cost of repeated `^` when serializing deep arrays.

   Decoding is a recursive-descent parser over the input string indexed by an
   absolute cursor. Each helper returns `(value, nextIndex) option`, where NONE
   uniformly signals "truncated or malformed". The public `decode` parses one
   value from the front and reports the number of bytes it consumed. *)

structure Redis :> REDIS =
struct
  datatype resp =
      Simple of string
    | Error  of string
    | Int    of int
    | Bulk   of string option
    | Array  of resp list option

  (* ---- encoding ---- *)

  (* RESP integers use a leading '-' for negatives, unlike SML's Int.toString
     which prints '~'. Reformat by hand. *)
  fun intStr i =
    if i < 0 then "-" ^ Int.toString (~i) else Int.toString i

  fun crlf b = Buffer.addString b "\r\n"

  fun encodeInto b r =
    case r of
        Simple s => (Buffer.addChar b #"+"; Buffer.addString b s; crlf b)
      | Error s  => (Buffer.addChar b #"-"; Buffer.addString b s; crlf b)
      | Int i    => (Buffer.addChar b #":"; Buffer.addString b (intStr i); crlf b)
      | Bulk NONE => Buffer.addString b "$-1\r\n"
      | Bulk (SOME s) =>
          (Buffer.addChar b #"$"; Buffer.addString b (intStr (size s)); crlf b;
           Buffer.addString b s; crlf b)
      | Array NONE => Buffer.addString b "*-1\r\n"
      | Array (SOME xs) =>
          (Buffer.addChar b #"*"; Buffer.addString b (intStr (length xs)); crlf b;
           List.app (encodeInto b) xs)

  fun encode r = Buffer.build (fn b => encodeInto b r)

  fun cmd args =
    encode (Array (SOME (List.map (fn s => Bulk (SOME s)) args)))

  (* ---- decoding ---- *)

  (* Locate the CRLF at or after index `i`; returns the index of the '\r'.
     NONE means no complete line terminator is present yet (truncated). *)
  fun findCrlf (s, i) =
    let
      val n = size s
      fun loop k =
        if k + 1 >= n then NONE
        else if String.sub (s, k) = #"\r" andalso String.sub (s, k + 1) = #"\n"
        then SOME k
        else loop (k + 1)
    in
      loop i
    end

  (* Read the line starting at `i` (up to the next CRLF). Returns the line
     contents and the index just past the CRLF. *)
  fun readLine (s, i) =
    case findCrlf (s, i) of
        NONE => NONE
      | SOME j => SOME (String.substring (s, i, j - i), j + 2)

  (* Parse a RESP integer line strictly: the whole line must be a signed
     decimal numeral (Int.scan would otherwise accept a numeric prefix). *)
  fun parseIntLine line =
    let
      val n = size line
      fun digits k = k >= n orelse (Char.isDigit (String.sub (line, k)) andalso digits (k + 1))
      val ok =
        n > 0 andalso
        (case String.sub (line, 0) of
             #"-" => n >= 2 andalso digits 1
           | #"+" => n >= 2 andalso digits 1
           | _    => digits 0)
    in
      if ok then Int.fromString line else NONE
    end

  (* Parse one value starting at absolute index `i`; returns (value, next). *)
  fun parseAt s i =
    if i >= size s then NONE
    else
      (case String.sub (s, i) of
           #"+" =>
             (case readLine (s, i + 1) of
                  SOME (line, j) => SOME (Simple line, j)
                | NONE => NONE)
         | #"-" =>
             (case readLine (s, i + 1) of
                  SOME (line, j) => SOME (Error line, j)
                | NONE => NONE)
         | #":" =>
             (case readLine (s, i + 1) of
                  SOME (line, j) =>
                    (case parseIntLine line of
                         SOME v => SOME (Int v, j)
                       | NONE => NONE)
                | NONE => NONE)
         | #"$" => parseBulk s (i + 1)
         | #"*" => parseArray s (i + 1)
         | _ => NONE)

  and parseBulk s i =
    (case readLine (s, i) of
         NONE => NONE
       | SOME (line, j) =>
           (case parseIntLine line of
                NONE => NONE
              | SOME len =>
                  if len < 0 then SOME (Bulk NONE, j)
                  else
                    (* need `len` payload bytes then a trailing CRLF *)
                    if j + len + 2 > size s then NONE
                    else if String.sub (s, j + len) = #"\r"
                         andalso String.sub (s, j + len + 1) = #"\n"
                    then SOME (Bulk (SOME (String.substring (s, j, len))), j + len + 2)
                    else NONE))

  and parseArray s i =
    (case readLine (s, i) of
         NONE => NONE
       | SOME (line, j) =>
           (case parseIntLine line of
                NONE => NONE
              | SOME count =>
                  if count < 0 then SOME (Array NONE, j)
                  else
                    let
                      fun loop (k, 0, acc) = SOME (Array (SOME (rev acc)), k)
                        | loop (k, m, acc) =
                            (case parseAt s k of
                                 NONE => NONE
                               | SOME (v, k') => loop (k', m - 1, v :: acc))
                    in
                      loop (j, count, [])
                    end))

  fun decode s =
    case parseAt s 0 of
        NONE => NONE
      | SOME (v, n) => SOME (v, n)
end
