# sml-redis

[![CI](https://github.com/sjqtentacles/sml-redis/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-redis/actions/workflows/ci.yml)

A pure Standard ML codec for **RESP** — both **RESP2** and **RESP3**, the wire
protocols Redis speaks — plus **typed Redis command builders**. `encode`/`decode`
(RESP2) and `encode3`/`decode3` (RESP3) move bytes between a value and its
on-wire `string` form — **no sockets, no FFI, no I/O** — so the codec is
trivially testable and runs byte-identically under
[MLton](http://mlton.org/) and [Poly/ML](https://www.polyml.org/). Bytes are
assembled through [`sml-buffer`](https://github.com/sjqtentacles/sml-buffer)
(vendored, Layout B), so the repo builds standalone.

The decoder is **streaming-friendly**: it parses a single value from the front
of a buffer and reports exactly how many bytes that value consumed, returning
`NONE` when the buffer is truncated (the rest of the frame hasn't arrived yet)
or malformed — the discipline you need to drive a real socket loop.

## Highlights

- **RESP2 codec** — `Simple`, `Error`, `Int`, `Bulk`, `Array`, with null
  bulk/array, embedded-CRLF payloads, and a streaming `decode`.
- **RESP3 codec** — `Null`, `Boolean`, `Double`, `BigNumber`, `VerbatimString`,
  `Map`, `Set`, `Push` plus the carryover RESP2 kinds for nesting, via a
  separate `value3` type and `encode3`/`decode3` (the RESP2 API is untouched).
- **Typed command builders** — `set`, `get`, `hget`, `hset`, and `pipeline`
  emit the standard RESP2 array-of-bulk-strings requests a client sends.
- **Deterministic Doubles** — RESP3 `Double` formatting is fixed and
  byte-identical across compilers (see [Double formatting](#double-formatting)).

## Status

- 135 assertions, green on MLton and Poly/ML, both printing `135 passed, 0 failed`.
- Basis-library only; deterministic across compilers.
- Vendors `sml-buffer` (Layout B), byte-identical to upstream.
- Integer replies use `IntInf.int` so 64-bit (and larger) wire integers decode
  losslessly and identically on both compilers — a plain `Int.fromString` would
  raise `Overflow` on MLton's 32-bit `int` for a value past 2^31 while Poly/ML
  accepted it. See the breaking-change note under **API**.

## Install

With [`smlpkg`](https://github.com/diku-dk/smlpkg):

```
smlpkg add github.com/sjqtentacles/sml-redis
smlpkg sync
```

Include the MLB from your own (it pulls in the vendored `sml-buffer`):

```
local
  $(SML_LIB)/basis/basis.mlb
  lib/github.com/sjqtentacles/sml-redis/... (via smlpkg)
in
  ...
end
```

This brings `structure Redis` (and the vendored `Buffer`) into scope.

## Quick start

```sml
(* Frame a client command: a RESP array of bulk strings. *)
val req = Redis.cmd ["SET", "greeting", "hello"]
(* "*3\r\n$3\r\nSET\r\n$8\r\ngreeting\r\n$5\r\nhello\r\n" *)

(* Parse a server reply from a byte buffer. *)
val SOME (v, n) = Redis.decode "+OK\r\n"      (* v = Redis.Simple "OK", n = 5 *)

(* A bulk string. *)
val SOME (Redis.Bulk (SOME s), _) = Redis.decode "$5\r\nhello\r\n"   (* s = "hello" *)

(* Truncated input: not enough bytes yet. *)
val NONE = Redis.decode "$5\r\nhel"
```

## API (`signature REDIS`)

```sml
datatype resp =
    Simple of string          (* "+OK\r\n"            *)
  | Error  of string          (* "-ERR bad\r\n"       *)
  | Int    of IntInf.int      (* ":1000\r\n"          *)
  | Bulk   of string option   (* "$5\r\nhello\r\n" / null "$-1\r\n"  *)
  | Array  of resp list option (* "*2\r\n...\r\n..." / null "*-1\r\n" *)

val encode : resp -> string
val decode : string -> (resp * int) option   (* value + bytes consumed *)
val cmd    : string list -> string           (* RESP array of bulk strings *)
```

**Breaking change:** integer replies (`Int`, and the RESP3 `Integer`) carry an
`IntInf.int`, not a machine `int`. RESP wire integers are 64-bit and some
replies exceed 2^31; MLton's default `int` is 32-bit, so decoding such a value
with `Int.fromString` raises `Overflow` there while Poly/ML's 63-bit `int`
accepts it. `IntInf.int` (arbitrary precision) makes every magnitude decode
losslessly and byte-identically on both compilers. Integer *literals* you pass
to `Int`/`Integer` still work unchanged; if you match an integer reply and need
a machine `int`, convert with `IntInf.toInt` (or `Int.fromLarge`).

### RESP3 (additive)

RESP3 is modelled by a **separate** `value3` datatype with its own
`encode3`/`decode3`, so the original RESP2 `resp` type and codec stay intact.
`value3` also carries the RESP2 kinds it needs for nesting (a `Map` value, `Set`
element, or `Push` payload can be any `value3`):

```sml
datatype value3 =
    Null                            (* "_\r\n"                                  *)
  | Boolean of bool                 (* "#t\r\n" / "#f\r\n"                       *)
  | Double of real                  (* ",3\r\n", ",3.25\r\n", ",inf\r\n", ",-inf\r\n" *)
  | BigNumber of string             (* "(3492890328409238509324...\r\n"          *)
  | Verbatim of string * string     (* (fmt, content): "=15\r\ntxt:Some string\r\n" *)
  | Map of (value3 * value3) list   (* "%2\r\n+first\r\n:1\r\n+second\r\n:2\r\n"  *)
  | Set of value3 list              (* "~2\r\n:1\r\n:2\r\n"                       *)
  | Push of value3 list             (* ">3\r\n+message\r\n+channel\r\n+payload\r\n" *)
  (* carryover RESP2 kinds, for nesting / standalone use *)
  | SimpleString of string          (* "+OK\r\n"                                 *)
  | SimpleError of string           (* "-ERR bad\r\n"                            *)
  | Integer of IntInf.int           (* ":1000\r\n"                               *)
  | BlobString of string option     (* "$5\r\nhello\r\n" / null "$-1\r\n"        *)
  | Array3 of value3 list option    (* "*2\r\n...\r\n..." / null "*-1\r\n"        *)

val doubleToString : real -> string             (* the ',' Double payload  *)
val encode3 : value3 -> string
val decode3 : string -> (value3 * int) option   (* value + bytes consumed  *)
```

### Typed command builders

These produce the standard RESP2 array-of-bulk-strings request a client sends
(the same byte type as `encode`/`cmd`):

```sml
val set      : string * string -> string          (* set ("key","val")  *)
val get      : string -> string                   (* get "mykey"        *)
val hget     : string * string -> string          (* hget ("h","f")     *)
val hset     : string * string * string -> string (* hset ("h","f","v") *)
val pipeline : string list -> string              (* concat encoded cmds *)
```

```sml
Redis.set ("key", "val")
(* "*3\r\n$3\r\nSET\r\n$3\r\nkey\r\n$3\r\nval\r\n" *)

Redis.pipeline [Redis.get "a", Redis.get "b"]
(* "*2\r\n$3\r\nGET\r\n$1\r\na\r\n*2\r\n$3\r\nGET\r\n$1\r\nb\r\n" *)
```

### Semantics

- `encode v` produces the canonical RESP byte string. Negative integers use a
  leading `-` (RESP convention), never SML's `~`.
- `decode bytes` parses **one** value from the front of `bytes`:
  - on success returns `SOME (v, n)` where `n` is the number of bytes the value
    occupied — advance your cursor by `n` and decode the remainder for the next
    frame;
  - returns `NONE` when the input is **truncated** (a complete frame hasn't
    arrived) or **malformed** (bad type byte, non-numeric length, etc.).
- Bulk payloads are raw bytes: a `Bulk` may contain embedded `\r\n`, and its
  declared length is authoritative (the codec never scans the payload for a
  terminator).
- `cmd args` is `encode (Array (SOME (map (Bulk o SOME) args)))` — the form
  Redis clients use to send commands.
- `encode3`/`decode3` follow the identical contract for RESP3 `value3` values.
  `decode3` reports bytes consumed and returns `NONE` on truncated/malformed
  input, just like `decode`.

<a name="double-formatting"></a>
### Double formatting

RESP3 `Double` payloads (`doubleToString`) use a **fixed, deterministic** format
so the bytes are identical under MLton and Poly/ML:

- non-finite values render as `inf`, `-inf`, `nan`;
- integral values carry **no** decimal point — `3.0` → `,3\r\n`;
- other values use a fixed-precision decimal with trailing zeros stripped —
  `3.25` → `,3.25\r\n`;
- negatives use a leading `-`, never SML's `~` — `~3.25` → `,-3.25\r\n`.

It builds on `Real.fmt (StringCvt.FIX _)`, which is byte-identical across both
compilers. Compare RESP3 `Double`s with an epsilon (never `=`).

## Example

`make example` builds and parses a full SET/GET exchange (and a pipelined reply
stream) entirely in memory:

```
=== sml-redis: a SET / GET exchange (pure RESP, no sockets) ===

Client sends:
  SET: *3\r\n$3\r\nSET\r\n$8\r\ngreeting\r\n$12\r\nhello, redis\r\n

Server replies:
  reply: +OK\r\n
           -> decoded: Simple "OK"  (5 bytes)

Client sends:
  GET: *2\r\n$3\r\nGET\r\n$8\r\ngreeting\r\n

Server replies:
  reply: $12\r\nhello, redis\r\n
           -> decoded: Bulk (SOME "hello, redis")  (19 bytes)

Pipelined reply stream (decode one frame at a time):
  frame 0: Int 1
  frame 1: Simple "PONG"
  frame 2: Array [Bulk (SOME "a"), Bulk (SOME "b")]
  frame 3: Bulk NONE

Typed command builders:
  set: *3\r\n$3\r\nSET\r\n$8\r\ngreeting\r\n$5\r\nhello\r\n
  get: *2\r\n$3\r\nGET\r\n$8\r\ngreeting\r\n
  hset: *4\r\n$4\r\nHSET\r\n$1\r\nh\r\n$1\r\nf\r\n$1\r\nv\r\n
  pipeline: *2\r\n$3\r\nGET\r\n$1\r\na\r\n*2\r\n$3\r\nGET\r\n$1\r\nb\r\n

RESP3 values (encode3):
  Null: _\r\n
  Boolean: #t\r\n
  Double 3.0: ,3\r\n
  Double 3.25: ,3.25\r\n
  Double -inf: ,-inf\r\n
  Verbatim: =15\r\ntxt:Some string\r\n
  Map: %2\r\n+first\r\n:1\r\n+second\r\n:2\r\n
  Set: ~2\r\n:1\r\n:2\r\n
  Push: >3\r\n+message\r\n+channel\r\n+payload\r\n

OK -- built and parsed a full exchange with the RESP codec.
```

## Build & test

```
make test        # MLton
make test-poly   # Poly/ML
make all-tests   # both
make example     # build + run examples/demo.sml
make clean
```

Both compilers run the same strict-TDD suite (`test/test.sml`), whose oracle is
the protocol spec written out as literal byte strings. Highlights:

- **Encode goldens** for every variant, including the null bulk (`$-1\r\n`),
  null array (`*-1\r\n`), empty payloads, and negative integers (`:-42\r\n`).
- **RESP3 wire vectors** for every `value3` kind (`_\r\n`, `#t\r\n`, `,3\r\n`,
  `,3.25\r\n`, `,-inf\r\n`, `(...\r\n`, `=15\r\ntxt:...`, `%`/`~`/`>` maps, sets
  and pushes), plus the `doubleToString` format checks.
- **Typed builders** assert the exact `set`/`get`/`hget`/`hset`/`pipeline`
  request bytes.
- **Round-trips:** `decode (encode v) = (v, size (encode v))` for every variant,
  including nested arrays and bulks with embedded CRLF; `decode3 (encode3 v)`
  recovers every `value3` (Doubles compared with an epsilon).
- **Bytes-consumed contract:** `decode`/`decode3` stop at the end of the first
  value even when more bytes follow, so a buffer can be drained one frame at a
  time.
- **Partial buffers:** every strict prefix of a complete frame decodes to
  `NONE`, and so do malformed frames (unknown type byte, non-numeric length).

## License

MIT — see [LICENSE](LICENSE).
