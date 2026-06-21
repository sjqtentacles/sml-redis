# sml-redis

A pure Standard ML codec for **RESP** (the REdis Serialization Protocol) plus
Redis client command builders. `encode`/`decode` move bytes between a `resp`
value and its on-wire `string` form — **no sockets, no FFI, no I/O** — so the
codec is trivially testable and runs byte-identically under
[MLton](http://mlton.org/) and [Poly/ML](https://www.polyml.org/). Bytes are
assembled through [`sml-buffer`](https://github.com/sjqtentacles/sml-buffer)
(vendored, Layout B), so the repo builds standalone.

The decoder is **streaming-friendly**: it parses a single value from the front
of a buffer and reports exactly how many bytes that value consumed, returning
`NONE` when the buffer is truncated (the rest of the frame hasn't arrived yet)
or malformed — the discipline you need to drive a real socket loop.

## Status

- 60 assertions, green on MLton and Poly/ML, both printing `60 passed, 0 failed`.
- Basis-library only; deterministic across compilers.
- Vendors `sml-buffer` (Layout B), byte-identical to upstream.

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
  | Int    of int             (* ":1000\r\n"          *)
  | Bulk   of string option   (* "$5\r\nhello\r\n" / null "$-1\r\n"  *)
  | Array  of resp list option (* "*2\r\n...\r\n..." / null "*-1\r\n" *)

val encode : resp -> string
val decode : string -> (resp * int) option   (* value + bytes consumed *)
val cmd    : string list -> string           (* RESP array of bulk strings *)
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
- **Round-trips:** `decode (encode v) = (v, size (encode v))` for every variant,
  including nested arrays and bulks with embedded CRLF.
- **Bytes-consumed contract:** `decode` stops at the end of the first value even
  when more bytes follow, so a buffer can be drained one frame at a time.
- **Partial buffers:** every strict prefix of a complete frame decodes to
  `NONE`, and so do malformed frames (unknown type byte, non-numeric length).

## License

MIT — see [LICENSE](LICENSE).
