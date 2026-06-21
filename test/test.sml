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
    in
      ()
    end

  fun run () = (Harness.reset (); runAll (); Harness.run ())
end
