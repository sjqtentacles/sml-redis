(* buffer.sml

   Implementation of BUFFER backed by a CharArray that doubles on demand.
   The mutable state is the backing array plus the live length; the array
   may be larger than `len` (the slack is reused on `clear`). *)

structure Buffer :> BUFFER =
struct
  type buffer = { arr : CharArray.array ref, len : int ref }

  val minCap = 16

  fun new hint =
    let val cap = if hint < minCap then minCap else hint
    in { arr = ref (CharArray.array (cap, #"\000")), len = ref 0 } end

  fun empty () = new minCap

  fun length ({ len, ... } : buffer) = !len
  fun isEmpty b = length b = 0

  (* Ensure the backing array can hold at least `need` total bytes. *)
  fun ensure ({ arr, len } : buffer) need =
    let
      val cap = CharArray.length (!arr)
    in
      if need <= cap then ()
      else
        let
          fun grow c = if c >= need then c else grow (c * 2)
          val newCap = grow (if cap < minCap then minCap else cap)
          val dst = CharArray.array (newCap, #"\000")
          fun copy i =
            if i >= !len then ()
            else (CharArray.update (dst, i, CharArray.sub (!arr, i)); copy (i + 1))
        in
          copy 0;
          arr := dst
        end
    end

  fun addChar (b as { arr, len } : buffer) c =
    (ensure b (!len + 1);
     CharArray.update (!arr, !len, c);
     len := !len + 1)

  fun addSubstring (b as { arr, len } : buffer) ss =
    let
      val n = Substring.size ss
    in
      if n = 0 then ()
      else
        let
          val () = ensure b (!len + n)
          fun loop i =
            if i >= n then ()
            else (CharArray.update (!arr, !len + i, Substring.sub (ss, i));
                  loop (i + 1))
        in
          loop 0;
          len := !len + n
        end
    end

  fun addString b s = addSubstring b (Substring.full s)

  fun addBuffer dst (src as { len, ... } : buffer) =
    let
      fun loop i =
        if i >= !len then () else (addChar dst (sub src i); loop (i + 1))
    in
      loop 0
    end

  and sub ({ arr, len } : buffer) i =
    if i < 0 orelse i >= !len then raise Subscript
    else CharArray.sub (!arr, i)

  fun contents ({ arr, len } : buffer) =
    CharArraySlice.vector (CharArraySlice.slice (!arr, 0, SOME (!len)))

  fun clear ({ len, ... } : buffer) = len := 0

  fun build f =
    let val b = empty () in f b; contents b end

  fun concat strs =
    build (fn b => List.app (addString b) strs)

  fun concatWith sep strs =
    case strs of
        [] => ""
      | first :: rest =>
          build (fn b =>
            (addString b first;
             List.app (fn s => (addString b sep; addString b s)) rest))
end
