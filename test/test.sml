(* test.sml

   Strict-TDD suite for `Redis`, the RESP codec. RESP is a byte protocol, so
   every value is compared with exact string/structural equality (no epsilon):
   the same bytes must come out of both MLton and Poly/ML.

   Coverage:
     - encode goldens for every `resp` variant (incl. the null bulk/array and
       negative integers, which must use '-' not SML's '~');
     - encode -> decode round-trips for every variant, including nesting and
       empty payloads;
     - the "bytes consumed" contract: decode reports exactly how many bytes the
       first complete value used, and stops there even with trailing data;
     - partial-buffer handling: every prefix of a complete frame decodes to
       NONE (insufficient), and malformed frames decode to NONE;
     - command framing: `cmd` produces the canonical RESP array of bulk strings.

   The oracle is the protocol spec itself, written out as literal byte strings. *)

structure Tests =
struct
  structure R = Redis

  (* ---- sample values, one per variant (plus edge cases) ---- *)
  val vSimple   = R.Simple "OK"
  val vSimpleE  = R.Simple ""                 (* empty status line          *)
  val vError    = R.Error "ERR unknown command"
  val vIntPos   = R.Int 1000
  val vIntZero  = R.Int 0
  val vIntNeg   = R.Int ~42                    (* must encode as ":-42\r\n"  *)
  val vBulk     = R.Bulk (SOME "hello")
  val vBulkE    = R.Bulk (SOME "")             (* empty bulk "$0\r\n\r\n"     *)
  val vBulkBin  = R.Bulk (SOME "a\r\nb")       (* CRLF inside the payload     *)
  val vBulkNull = R.Bulk NONE                  (* null bulk "$-1\r\n"         *)
  val vArr      = R.Array (SOME [R.Int 1, R.Int 2, R.Int 3])
  val vArrEmpty = R.Array (SOME [])            (* empty array "*0\r\n"        *)
  val vArrNull  = R.Array NONE                 (* null array "*-1\r\n"        *)
  val vArrMixed = R.Array (SOME [vSimple, vError, vIntNeg, vBulk, vBulkNull])
  val vArrNest  = R.Array (SOME [R.Array (SOME [R.Bulk (SOME "x")]),
                                 R.Array NONE,
                                 R.Int ~7])

  val allValues =
    [vSimple, vSimpleE, vError, vIntPos, vIntZero, vIntNeg, vBulk, vBulkE,
     vBulkBin, vBulkNull, vArr, vArrEmpty, vArrNull, vArrMixed, vArrNest]

  (* round-trips: decode (encode v) recovers v and consumes the whole string *)
  fun roundTrips v =
    let val s = R.encode v
    in case R.decode s of
           SOME (v', n) => v' = v andalso n = size s
         | NONE => false
    end

  (* ---- RESP3 helpers ----

     `value3` carries `Double of real`, so it is NOT an equality type; compare
     structurally with an epsilon on Doubles. *)
  val eps = 1E~9
  fun reqv (a, b) = Real.== (a, b) orelse Real.abs (a - b) < eps

  fun eq3 (R.Null, R.Null) = true
    | eq3 (R.Boolean a, R.Boolean b) = a = b
    | eq3 (R.Double a, R.Double b) = reqv (a, b)
    | eq3 (R.BigNumber a, R.BigNumber b) = a = b
    | eq3 (R.Verbatim a, R.Verbatim b) = a = b
    | eq3 (R.Map a, R.Map b) = eqPairs (a, b)
    | eq3 (R.Set a, R.Set b) = eqList (a, b)
    | eq3 (R.Push a, R.Push b) = eqList (a, b)
    | eq3 (R.SimpleString a, R.SimpleString b) = a = b
    | eq3 (R.SimpleError a, R.SimpleError b) = a = b
    | eq3 (R.Integer a, R.Integer b) = a = b
    | eq3 (R.BlobString a, R.BlobString b) = a = b
    | eq3 (R.Array3 a, R.Array3 b) = eqOpt (a, b)
    | eq3 _ = false
  and eqList ([], []) = true
    | eqList (x :: xs, y :: ys) = eq3 (x, y) andalso eqList (xs, ys)
    | eqList _ = false
  and eqPairs ([], []) = true
    | eqPairs ((k1, v1) :: xs, (k2, v2) :: ys) =
        eq3 (k1, k2) andalso eq3 (v1, v2) andalso eqPairs (xs, ys)
    | eqPairs _ = false
  and eqOpt (NONE, NONE) = true
    | eqOpt (SOME a, SOME b) = eqList (a, b)
    | eqOpt _ = false

  (* A RESP3 value round-trips iff decode3 (encode3 v) recovers v (structurally,
     epsilon on Doubles) and consumes exactly the encoded bytes. *)
  fun roundTrips3 v =
    let val s = R.encode3 v
    in case R.decode3 s of
           SOME (v', n) => eq3 (v', v) andalso n = size s
         | NONE => false
    end

  val v3samples =
    [ R.Null
    , R.Boolean true
    , R.Boolean false
    , R.Double 3.0
    , R.Double 3.25
    , R.Double ~2.5
    , R.Double 0.5
    , R.Double 100.0
    , R.Double Real.posInf
    , R.Double Real.negInf
    , R.BigNumber "3492890328409238509324850943850943825024385"
    , R.BigNumber "-42"
    , R.Verbatim ("txt", "Some string")
    , R.Map [(R.SimpleString "first", R.Integer 1),
             (R.SimpleString "second", R.Integer 2)]
    , R.Set [R.Integer 1, R.Integer 2]
    , R.Push [R.SimpleString "message", R.SimpleString "channel",
              R.SimpleString "payload"]
    , R.SimpleString "OK"
    , R.SimpleError "ERR boom"
    , R.Integer ~7
    , R.BlobString (SOME "hello")
    , R.BlobString NONE
    , R.Array3 (SOME [R.Integer 1, R.Boolean true, R.Null])
    , R.Array3 NONE
    , R.Array3 (SOME [])
    (* nesting: a map whose values are a set and an array of doubles *)
    , R.Map [(R.SimpleString "s", R.Set [R.Integer 9]),
             (R.SimpleString "a", R.Array3 (SOME [R.Double 1.5, R.Double 2.0]))] ]

  fun runAll () =
    let
      (* ---- encode goldens ---- *)
      val () = Harness.section "encode goldens (per variant)"
      val () = Harness.checkString "Simple" ("+OK\r\n", R.encode vSimple)
      val () = Harness.checkString "Simple (empty)" ("+\r\n", R.encode vSimpleE)
      val () = Harness.checkString "Error"
                 ("-ERR unknown command\r\n", R.encode vError)
      val () = Harness.checkString "Int (positive)" (":1000\r\n", R.encode vIntPos)
      val () = Harness.checkString "Int (zero)" (":0\r\n", R.encode vIntZero)
      val () = Harness.checkString "Int (negative uses '-')"
                 (":-42\r\n", R.encode vIntNeg)
      val () = Harness.checkString "Bulk" ("$5\r\nhello\r\n", R.encode vBulk)
      val () = Harness.checkString "Bulk (empty)" ("$0\r\n\r\n", R.encode vBulkE)
      val () = Harness.checkString "Bulk (binary CRLF payload)"
                 ("$4\r\na\r\nb\r\n", R.encode vBulkBin)
      val () = Harness.checkString "Bulk (null)" ("$-1\r\n", R.encode vBulkNull)
      val () = Harness.checkString "Array"
                 ("*3\r\n:1\r\n:2\r\n:3\r\n", R.encode vArr)
      val () = Harness.checkString "Array (empty)" ("*0\r\n", R.encode vArrEmpty)
      val () = Harness.checkString "Array (null)" ("*-1\r\n", R.encode vArrNull)
      val () = Harness.checkString "Array (nested)"
                 ("*3\r\n*1\r\n$1\r\nx\r\n*-1\r\n:-7\r\n", R.encode vArrNest)

      (* ---- round-trips for every variant ---- *)
      val () = Harness.section "encode -> decode round-trips"
      val () =
        ignore (List.foldl
          (fn (v, i) =>
            (Harness.check ("round-trip #" ^ Int.toString i) (roundTrips v); i + 1))
          0 allValues)

      (* ---- bytes-consumed contract ---- *)
      val () = Harness.section "decode reports bytes consumed"
      val () =
        (case R.decode "+OK\r\n" of
             SOME (v, n) =>
               (Harness.check "value" (v = R.Simple "OK");
                Harness.checkInt "consumed" (5, n))
           | NONE => Harness.check "decode +OK" false)
      val () =
        (* trailing bytes beyond the first value are not consumed *)
        (case R.decode "$5\r\nhello\r\n+EXTRA\r\n" of
             SOME (v, n) =>
               (Harness.check "first value only" (v = R.Bulk (SOME "hello"));
                Harness.checkInt "consumed stops at frame end" (11, n))
           | NONE => Harness.check "decode bulk+extra" false)
      val () =
        (* consuming the first value then the second from the remainder *)
        (case R.decode "*2\r\n:1\r\n:2\r\n:3\r\n" of
             SOME (v, n) =>
               (Harness.check "array of two" (v = R.Array (SOME [R.Int 1, R.Int 2]));
                Harness.checkInt "consumed only the 2-array" (12, n))
           | NONE => Harness.check "decode array+extra" false)

      (* ---- partial-buffer handling: every proper prefix is NONE ---- *)
      val () = Harness.section "partial buffers decode to NONE"
      val () =
        let
          (* For a representative complete frame, every strictly-shorter prefix
             must be insufficient (NONE). The full string itself must decode. *)
          fun allPrefixesNone label full =
            let
              val n = size full
              fun loop k =
                if k >= n then true
                else not (Option.isSome (R.decode (String.substring (full, 0, k))))
                     andalso loop (k + 1)
            in
              Harness.check (label ^ ": all prefixes NONE") (loop 0);
              Harness.check (label ^ ": full decodes") (Option.isSome (R.decode full))
            end
        in
          allPrefixesNone "Simple"  "+OK\r\n";
          allPrefixesNone "Int"     ":-42\r\n";
          allPrefixesNone "Bulk"    "$5\r\nhello\r\n";
          allPrefixesNone "Array"   "*2\r\n$3\r\nfoo\r\n$3\r\nbar\r\n"
        end
      val () = Harness.check "empty input is NONE" (R.decode "" = NONE)
      val () = Harness.check "bare CR without LF is NONE" (R.decode "+OK\r" = NONE)
      val () = Harness.check "missing terminator is NONE" (R.decode "+OK" = NONE)
      val () = Harness.check "bulk data short is NONE"
                 (R.decode "$5\r\nhel" = NONE)
      val () = Harness.check "bulk missing trailing CRLF is NONE"
                 (R.decode "$5\r\nhello" = NONE)
      val () = Harness.check "array missing element is NONE"
                 (R.decode "*2\r\n$3\r\nfoo\r\n" = NONE)

      (* ---- malformed input ---- *)
      val () = Harness.section "malformed input decodes to NONE"
      val () = Harness.check "unknown type byte" (R.decode "?oops\r\n" = NONE)
      val () = Harness.check "non-numeric integer" (R.decode ":abc\r\n" = NONE)
      val () = Harness.check "non-numeric bulk length" (R.decode "$xx\r\n" = NONE)

      (* ---- null round-trips explicitly ---- *)
      val () = Harness.section "null bulk / null array"
      val () = Harness.check "null bulk round-trips" (roundTrips vBulkNull)
      val () = Harness.check "null array round-trips" (roundTrips vArrNull)
      val () =
        (case R.decode "$-1\r\n" of
             SOME (v, n) =>
               (Harness.check "null bulk value" (v = R.Bulk NONE);
                Harness.checkInt "null bulk consumed" (5, n))
           | NONE => Harness.check "decode null bulk" false)

      (* ---- command framing ---- *)
      val () = Harness.section "command framing (cmd)"
      val () = Harness.checkString "cmd [SET,k,v]"
                 ("*3\r\n$3\r\nSET\r\n$1\r\nk\r\n$1\r\nv\r\n",
                  R.cmd ["SET", "k", "v"])
      val () = Harness.checkString "cmd [PING]"
                 ("*1\r\n$4\r\nPING\r\n", R.cmd ["PING"])
      val () = Harness.checkString "cmd [] (empty array of bulks)"
                 ("*0\r\n", R.cmd [])
      val () =
        (* cmd is an array of bulk strings -> decode mirrors the arguments *)
        (case R.decode (R.cmd ["GET", "greeting"]) of
             SOME (v, _) =>
               Harness.check "cmd decodes to array of bulks"
                 (v = R.Array (SOME [R.Bulk (SOME "GET"), R.Bulk (SOME "greeting")]))
           | NONE => Harness.check "decode cmd" false)

      (* ---- RESP3 encode goldens (authoritative wire vectors) ---- *)
      val () = Harness.section "RESP3 encode goldens"
      val () = Harness.checkString "Null" ("_\r\n", R.encode3 R.Null)
      val () = Harness.checkString "Boolean true" ("#t\r\n", R.encode3 (R.Boolean true))
      val () = Harness.checkString "Boolean false" ("#f\r\n", R.encode3 (R.Boolean false))
      val () = Harness.checkString "Double 3.0 (integral, no point)"
                 (",3\r\n", R.encode3 (R.Double 3.0))
      val () = Harness.checkString "Double 3.25"
                 (",3.25\r\n", R.encode3 (R.Double 3.25))
      val () = Harness.checkString "Double ~3.25 (leading '-')"
                 (",-3.25\r\n", R.encode3 (R.Double ~3.25))
      val () = Harness.checkString "Double +inf"
                 (",inf\r\n", R.encode3 (R.Double Real.posInf))
      val () = Harness.checkString "Double -inf"
                 (",-inf\r\n", R.encode3 (R.Double Real.negInf))
      val () = Harness.checkString "BigNumber"
                 ("(3492890328409238509324850943850943825024385\r\n",
                  R.encode3 (R.BigNumber "3492890328409238509324850943850943825024385"))
      val () = Harness.checkString "Verbatim (txt:Some string)"
                 ("=15\r\ntxt:Some string\r\n",
                  R.encode3 (R.Verbatim ("txt", "Some string")))
      val () = Harness.checkString "Map {first:1, second:2}"
                 ("%2\r\n+first\r\n:1\r\n+second\r\n:2\r\n",
                  R.encode3 (R.Map [(R.SimpleString "first", R.Integer 1),
                                    (R.SimpleString "second", R.Integer 2)]))
      val () = Harness.checkString "Set {1,2}"
                 ("~2\r\n:1\r\n:2\r\n",
                  R.encode3 (R.Set [R.Integer 1, R.Integer 2]))
      val () = Harness.checkString "Push [message,channel,payload]"
                 (">3\r\n+message\r\n+channel\r\n+payload\r\n",
                  R.encode3 (R.Push [R.SimpleString "message",
                                     R.SimpleString "channel",
                                     R.SimpleString "payload"]))

      (* ---- RESP3 Double formatting (documented, deterministic) ---- *)
      val () = Harness.section "RESP3 doubleToString format"
      val () = Harness.checkString "3.0 -> 3"   ("3",   R.doubleToString 3.0)
      val () = Harness.checkString "3.25"        ("3.25", R.doubleToString 3.25)
      val () = Harness.checkString "~3.0 -> -3" ("-3",  R.doubleToString ~3.0)
      val () = Harness.checkString "0.5"         ("0.5", R.doubleToString 0.5)
      val () = Harness.checkString "100.0 -> 100" ("100", R.doubleToString 100.0)
      val () = Harness.checkString "+inf"        ("inf", R.doubleToString Real.posInf)
      val () = Harness.checkString "-inf"        ("-inf", R.doubleToString Real.negInf)

      (* ---- RESP3 round-trips (epsilon on Doubles) ---- *)
      val () = Harness.section "RESP3 encode3 -> decode3 round-trips"
      val () =
        ignore (List.foldl
          (fn (v, i) =>
            (Harness.check ("v3 round-trip #" ^ Int.toString i) (roundTrips3 v); i + 1))
          0 v3samples)

      (* ---- RESP3 decode value/bytes-consumed spot checks ---- *)
      val () = Harness.section "RESP3 decode3 details"
      val () =
        (case R.decode3 "_\r\n" of
             SOME (v, n) =>
               (Harness.check "Null value" (eq3 (v, R.Null));
                Harness.checkInt "Null consumed" (3, n))
           | NONE => Harness.check "decode3 Null" false)
      val () =
        (case R.decode3 ",3\r\n" of
             SOME (R.Double r, n) =>
               (Harness.check "Double 3.0 value" (reqv (r, 3.0));
                Harness.checkInt "Double consumed" (4, n))
           | _ => Harness.check "decode3 Double" false)
      val () =
        (case R.decode3 "=15\r\ntxt:Some string\r\n" of
             SOME (v, _) =>
               Harness.check "Verbatim value"
                 (eq3 (v, R.Verbatim ("txt", "Some string")))
           | NONE => Harness.check "decode3 Verbatim" false)
      val () =
        (* trailing bytes beyond the first value are not consumed *)
        (case R.decode3 "#t\r\n#f\r\n" of
             SOME (v, n) =>
               (Harness.check "first bool only" (eq3 (v, R.Boolean true));
                Harness.checkInt "bool consumed stops at frame" (4, n))
           | NONE => Harness.check "decode3 bool+extra" false)

      (* ---- RESP3 partial / malformed input ---- *)
      val () = Harness.section "RESP3 partial / malformed decode3 = NONE"
      fun none3 s = not (Option.isSome (R.decode3 s))
      val () = Harness.check "empty is NONE" (none3 "")
      val () = Harness.check "truncated Null is NONE" (none3 "_\r")
      val () = Harness.check "bad boolean is NONE" (none3 "#x\r\n")
      val () = Harness.check "non-numeric double is NONE" (none3 ",abc\r\n")
      val () = Harness.check "non-digit bignum is NONE" (none3 "(12x\r\n")
      val () = Harness.check "map missing pair is NONE" (none3 "%2\r\n+first\r\n:1\r\n")
      val () = Harness.check "set short is NONE" (none3 "~2\r\n:1\r\n")

      (* ---- typed command builders (authoritative wire vectors) ---- *)
      val () = Harness.section "typed command builders"
      val () = Harness.checkString "set"
                 ("*3\r\n$3\r\nSET\r\n$3\r\nkey\r\n$3\r\nval\r\n",
                  R.set ("key", "val"))
      val () = Harness.checkString "get"
                 ("*2\r\n$3\r\nGET\r\n$5\r\nmykey\r\n", R.get "mykey")
      val () = Harness.checkString "hget"
                 ("*3\r\n$4\r\nHGET\r\n$1\r\nh\r\n$1\r\nf\r\n", R.hget ("h", "f"))
      val () = Harness.checkString "hset"
                 ("*4\r\n$4\r\nHSET\r\n$1\r\nh\r\n$1\r\nf\r\n$1\r\nv\r\n",
                  R.hset ("h", "f", "v"))
      val () = Harness.checkString "pipeline [get a, get b]"
                 ("*2\r\n$3\r\nGET\r\n$1\r\na\r\n*2\r\n$3\r\nGET\r\n$1\r\nb\r\n",
                  R.pipeline [R.get "a", R.get "b"])
      val () = Harness.checkString "pipeline [] (empty)" ("", R.pipeline [])
      val () =
        (* a builder's output decodes back to the RESP2 array of bulk args *)
        (case R.decode (R.set ("key", "val")) of
             SOME (v, _) =>
               Harness.check "set decodes to array of bulks"
                 (v = R.Array (SOME [R.Bulk (SOME "SET"),
                                     R.Bulk (SOME "key"),
                                     R.Bulk (SOME "val")]))
           | NONE => Harness.check "decode set" false)

      (* ---- large integer replies (cross-compiler overflow safety) ----

         RESP integer replies (`:<n>\r\n`) are 64-bit on the Redis wire, and
         some commands (e.g. bit counts, memory stats) legitimately return
         values beyond 2^31. A machine `int` is only 32-bit under MLton's
         default, so `Int.fromString` on such a numeral raises Overflow (a
         crash) there while Poly/ML (63-bit) silently accepts it -- a
         cross-compiler divergence. The integer reply value is therefore an
         `IntInf.int`, so every magnitude round-trips losslessly and identically
         on both compilers, and decoding NEVER raises. *)
      val () = Harness.section "large integer replies (IntInf)"
      val big31 : IntInf.int = 3000000000            (* > 2^31, crashes 32-bit Int.fromString *)
      val big63 : IntInf.int = 12345678901234567890  (* > 2^63, a 20-digit reply *)
      val () = Harness.check "decode never raises on 20-digit int"
                 (let val _ = R.decode ":12345678901234567890\r\n" in true end
                  handle _ => false)
      val () =
        (case R.decode ":3000000000\r\n" of
             SOME (R.Int v, n) =>
               (Harness.check "value past 2^31 decodes exactly" (v = big31);
                Harness.checkInt "consumed" (13, n))
           | _ => Harness.check "decode 3000000000" false)
      val () =
        (case R.decode ":12345678901234567890\r\n" of
             SOME (R.Int v, _) =>
               Harness.check "20-digit int decodes exactly" (v = big63)
           | _ => Harness.check "decode 20-digit int" false)
      val () = Harness.checkString "encode round 20-digit int"
                 (":12345678901234567890\r\n", R.encode (R.Int big63))
      val () = Harness.check "RESP2 large int round-trips" (roundTrips (R.Int big63))
      val () = Harness.check "RESP2 large negative int round-trips"
                 (roundTrips (R.Int (~big63)))
      val () =
        (* RESP3 Integer shares the same 64-bit-and-beyond domain *)
        (case R.decode3 ":12345678901234567890\r\n" of
             SOME (R.Integer v, _) =>
               Harness.check "RESP3 20-digit Integer decodes exactly" (v = big63)
           | _ => Harness.check "decode3 20-digit Integer" false)
      val () = Harness.check "RESP3 large Integer round-trips"
                 (roundTrips3 (R.Integer big63))
    in
      ()
    end

  fun run () = (Harness.reset (); runAll (); Harness.run ())
end
