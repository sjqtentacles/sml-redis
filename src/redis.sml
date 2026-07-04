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
    | Int    of IntInf.int
    | Bulk   of string option
    | Array  of resp list option

  (* ---- encoding ---- *)

  (* RESP integers use a leading '-' for negatives, unlike SML's Int.toString
     which prints '~'. Reformat by hand.

     `intStr` is for machine-int length/count fields (bounded by the input
     size, so it never overflows). `intStrInf` renders an arbitrary-precision
     integer reply value, which can legitimately exceed 2^31 on the wire. *)
  fun intStr i =
    if i < 0 then "-" ^ Int.toString (~i) else Int.toString i
  fun intStrInf (i : IntInf.int) =
    if i < 0 then "-" ^ IntInf.toString (~i) else IntInf.toString i

  fun crlf b = Buffer.addString b "\r\n"

  fun encodeInto b r =
    case r of
        Simple s => (Buffer.addChar b #"+"; Buffer.addString b s; crlf b)
      | Error s  => (Buffer.addChar b #"-"; Buffer.addString b s; crlf b)
      | Int i    => (Buffer.addChar b #":"; Buffer.addString b (intStrInf i); crlf b)
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

  (* Is `line` a strict signed decimal numeral? (Int.scan would otherwise
     accept a numeric prefix such as "12x".) RESP writes the sign as '-';
     IntInf.fromString / Int.fromString want SML's '~', so we translate. *)
  fun numeralOk line =
    let
      val n = size line
      fun digits k = k >= n orelse (Char.isDigit (String.sub (line, k)) andalso digits (k + 1))
    in
      n > 0 andalso
      (case String.sub (line, 0) of
           #"-" => n >= 2 andalso digits 1
         | #"+" => n >= 2 andalso digits 1
         | _    => digits 0)
    end

  (* Rewrite a RESP-signed numeral into SML syntax ('-' -> '~', drop '+'). *)
  fun toSmlNumeral line =
    if String.isPrefix "-" line then "~" ^ String.extract (line, 1, NONE)
    else if String.isPrefix "+" line then String.extract (line, 1, NONE)
    else line

  (* Parse a RESP integer reply value. These are 64-bit (and occasionally
     larger) on the wire, so the result is an arbitrary-precision `IntInf.int`:
     `IntInf.fromString` never overflows, so decoding is lossless and identical
     under MLton (32-bit `int`) and Poly/ML (63-bit `int`). *)
  fun parseIntLineInf line =
    if numeralOk line then IntInf.fromString (toSmlNumeral line) else NONE

  (* Parse a RESP length / element-count field. Unlike a reply value this must
     be a machine `int` (it indexes into the byte string), but it is bounded by
     the buffer size in practice. We still parse through `IntInf` and range-check
     against the FIXED 32-bit signed range -- the width that MLton's default
     `int` is guaranteed to hold -- so an absurd length fails gracefully as
     "malformed" (NONE) on every compiler rather than raising Overflow on
     MLton. (We use the fixed literals, not `Int.maxInt`, so the bound is the
     same regardless of the host compiler's actual `int` width.) *)
  val lenMin : IntInf.int = ~2147483648
  val lenMax : IntInf.int =  2147483647
  fun parseIntLine line =
    case parseIntLineInf line of
        NONE => NONE
      | SOME n => if n >= lenMin andalso n <= lenMax then SOME (IntInf.toInt n) else NONE

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
                    (case parseIntLineInf line of
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

  (* ==================================================================
     RESP3 (additive): a separate datatype plus encode3/decode3, sharing
     the line helpers above. The original `resp` codec is untouched.
     ================================================================== *)

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
    | Integer of IntInf.int
    | BlobString of string option
    | Array3 of value3 list option

  (* ---- RESP3 Double formatting (deterministic across compilers) ----

     `Real.fmt (StringCvt.FIX n)` is byte-identical under MLton and Poly/ML,
     so it is the basis for a fixed format. RESP3 emits integral doubles with
     no decimal point and uses 'inf'/'-inf'/'nan' for non-finite values. *)

  fun dashSign s =
    if String.isPrefix "~" s then "-" ^ String.extract (s, 1, NONE) else s

  (* Drop trailing '0' digits, then a now-dangling '.', from a decimal text. *)
  fun stripTrailingZeros s =
    let
      val n = size s
      fun lastNonZero k =
        if k >= 0 andalso String.sub (s, k) = #"0" then lastNonZero (k - 1) else k
      val k = lastNonZero (n - 1)
      val k = if k >= 0 andalso String.sub (s, k) = #"." then k - 1 else k
    in
      String.substring (s, 0, k + 1)
    end

  fun doubleToString r =
    if Real.isNan r then "nan"
    else if Real.== (r, Real.posInf) then "inf"
    else if Real.== (r, Real.negInf) then "-inf"
    else if Real.== (r, Real.realFloor r) then
      (* integral value -> no decimal point *)
      dashSign (Real.fmt (StringCvt.FIX (SOME 0)) r)
    else
      stripTrailingZeros (dashSign (Real.fmt (StringCvt.FIX (SOME 17)) r))

  (* ---- RESP3 encoding ---- *)

  fun encode3Into b v =
    case v of
        Null            => Buffer.addString b "_\r\n"
      | Boolean true    => Buffer.addString b "#t\r\n"
      | Boolean false   => Buffer.addString b "#f\r\n"
      | Double r        =>
          (Buffer.addChar b #","; Buffer.addString b (doubleToString r); crlf b)
      | BigNumber s     => (Buffer.addChar b #"("; Buffer.addString b s; crlf b)
      | Verbatim (fmt, content) =>
          let val body = fmt ^ ":" ^ content
          in Buffer.addChar b #"="; Buffer.addString b (intStr (size body)); crlf b;
             Buffer.addString b body; crlf b
          end
      | Map pairs =>
          (Buffer.addChar b #"%"; Buffer.addString b (intStr (length pairs)); crlf b;
           List.app (fn (k, v) => (encode3Into b k; encode3Into b v)) pairs)
      | Set xs =>
          (Buffer.addChar b #"~"; Buffer.addString b (intStr (length xs)); crlf b;
           List.app (encode3Into b) xs)
      | Push xs =>
          (Buffer.addChar b #">"; Buffer.addString b (intStr (length xs)); crlf b;
           List.app (encode3Into b) xs)
      | SimpleString s => (Buffer.addChar b #"+"; Buffer.addString b s; crlf b)
      | SimpleError s  => (Buffer.addChar b #"-"; Buffer.addString b s; crlf b)
      | Integer i      => (Buffer.addChar b #":"; Buffer.addString b (intStrInf i); crlf b)
      | BlobString NONE => Buffer.addString b "$-1\r\n"
      | BlobString (SOME s) =>
          (Buffer.addChar b #"$"; Buffer.addString b (intStr (size s)); crlf b;
           Buffer.addString b s; crlf b)
      | Array3 NONE => Buffer.addString b "*-1\r\n"
      | Array3 (SOME xs) =>
          (Buffer.addChar b #"*"; Buffer.addString b (intStr (length xs)); crlf b;
           List.app (encode3Into b) xs)

  fun encode3 v = Buffer.build (fn b => encode3Into b v)

  (* ---- RESP3 decoding ---- *)

  (* A big number is a (possibly signed) run of decimal digits; kept as text so
     magnitudes beyond `int` survive. *)
  fun isBigNum line =
    let
      val n = size line
      fun digits k = k >= n orelse (Char.isDigit (String.sub (line, k)) andalso digits (k + 1))
    in
      n > 0 andalso
      (case String.sub (line, 0) of
           #"-" => n >= 2 andalso digits 1
         | #"+" => n >= 2 andalso digits 1
         | _    => digits 0)
    end

  (* Parse a RESP3 Double payload line. Accepts 'inf'/'-inf'/'+inf'/'nan' and
     decimal numerals (RESP uses '-' for the sign; SML's reader wants '~'). *)
  fun parseDouble line =
    if line = "inf" orelse line = "+inf" then SOME Real.posInf
    else if line = "-inf" then SOME Real.negInf
    else if line = "nan" then SOME (Real.posInf - Real.posInf)
    else
      let val t = if String.isPrefix "-" line then "~" ^ String.extract (line, 1, NONE) else line
      in Real.fromString t end

  (* Split a verbatim body "fmt:content" at its first ':'. *)
  fun splitColon body =
    let
      val n = size body
      fun loop k =
        if k >= n then NONE
        else if String.sub (body, k) = #":"
        then SOME (String.substring (body, 0, k), String.extract (body, k + 1, NONE))
        else loop (k + 1)
    in
      loop 0
    end

  fun parseV3 s i =
    if i >= size s then NONE
    else
      (case String.sub (s, i) of
           #"_" => (case readLine (s, i + 1) of
                        SOME ("", j) => SOME (Null, j)
                      | _ => NONE)
         | #"#" => (case readLine (s, i + 1) of
                        SOME ("t", j) => SOME (Boolean true, j)
                      | SOME ("f", j) => SOME (Boolean false, j)
                      | _ => NONE)
         | #"," => (case readLine (s, i + 1) of
                        NONE => NONE
                      | SOME (line, j) =>
                          (case parseDouble line of
                               SOME r => SOME (Double r, j)
                             | NONE => NONE))
         | #"(" => (case readLine (s, i + 1) of
                        NONE => NONE
                      | SOME (line, j) =>
                          if isBigNum line then SOME (BigNumber line, j) else NONE)
         | #"=" => parseVerbatim s (i + 1)
         | #"%" => parseMap s (i + 1)
         | #"~" => parseSeq s (i + 1) Set
         | #">" => parseSeq s (i + 1) Push
         | #"+" => (case readLine (s, i + 1) of
                        SOME (line, j) => SOME (SimpleString line, j)
                      | NONE => NONE)
         | #"-" => (case readLine (s, i + 1) of
                        SOME (line, j) => SOME (SimpleError line, j)
                      | NONE => NONE)
         | #":" => (case readLine (s, i + 1) of
                        SOME (line, j) =>
                          (case parseIntLineInf line of
                               SOME v => SOME (Integer v, j)
                             | NONE => NONE)
                      | NONE => NONE)
         | #"$" => parseBlob s (i + 1)
         | #"*" => parseArr3 s (i + 1)
         | _ => NONE)

  and parseVerbatim s i =
    (case readLine (s, i) of
         NONE => NONE
       | SOME (line, j) =>
           (case parseIntLine line of
                NONE => NONE
              | SOME len =>
                  if len < 0 then NONE
                  else if j + len + 2 > size s then NONE
                  else if String.sub (s, j + len) = #"\r"
                       andalso String.sub (s, j + len + 1) = #"\n"
                  then (case splitColon (String.substring (s, j, len)) of
                            SOME (fmt, content) => SOME (Verbatim (fmt, content), j + len + 2)
                          | NONE => NONE)
                  else NONE))

  and parseMap s i =
    (case readLine (s, i) of
         NONE => NONE
       | SOME (line, j) =>
           (case parseIntLine line of
                NONE => NONE
              | SOME count =>
                  if count < 0 then NONE
                  else
                    let
                      fun loop (k, 0, acc) = SOME (Map (rev acc), k)
                        | loop (k, m, acc) =
                            (case parseV3 s k of
                                 NONE => NONE
                               | SOME (key, k1) =>
                                   (case parseV3 s k1 of
                                        NONE => NONE
                                      | SOME (v, k2) => loop (k2, m - 1, (key, v) :: acc)))
                    in
                      loop (j, count, [])
                    end))

  and parseSeq s i con =
    (case readLine (s, i) of
         NONE => NONE
       | SOME (line, j) =>
           (case parseIntLine line of
                NONE => NONE
              | SOME count =>
                  if count < 0 then NONE
                  else
                    let
                      fun loop (k, 0, acc) = SOME (con (rev acc), k)
                        | loop (k, m, acc) =
                            (case parseV3 s k of
                                 NONE => NONE
                               | SOME (v, k') => loop (k', m - 1, v :: acc))
                    in
                      loop (j, count, [])
                    end))

  and parseBlob s i =
    (case readLine (s, i) of
         NONE => NONE
       | SOME (line, j) =>
           (case parseIntLine line of
                NONE => NONE
              | SOME len =>
                  if len < 0 then SOME (BlobString NONE, j)
                  else if j + len + 2 > size s then NONE
                  else if String.sub (s, j + len) = #"\r"
                       andalso String.sub (s, j + len + 1) = #"\n"
                  then SOME (BlobString (SOME (String.substring (s, j, len))), j + len + 2)
                  else NONE))

  and parseArr3 s i =
    (case readLine (s, i) of
         NONE => NONE
       | SOME (line, j) =>
           (case parseIntLine line of
                NONE => NONE
              | SOME count =>
                  if count < 0 then SOME (Array3 NONE, j)
                  else
                    let
                      fun loop (k, 0, acc) = SOME (Array3 (SOME (rev acc)), k)
                        | loop (k, m, acc) =
                            (case parseV3 s k of
                                 NONE => NONE
                               | SOME (v, k') => loop (k', m - 1, v :: acc))
                    in
                      loop (j, count, [])
                    end))

  fun decode3 s = parseV3 s 0

  (* ---- typed command builders (standard RESP2 array-of-bulk requests) ---- *)

  fun set (key, value)       = cmd ["SET", key, value]
  fun get key                = cmd ["GET", key]
  fun hget (key, field)      = cmd ["HGET", key, field]
  fun hset (key, field, value) = cmd ["HSET", key, field, value]
  fun pipeline cmds          = Buffer.concat cmds
end
