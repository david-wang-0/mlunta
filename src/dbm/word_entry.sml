functor SignedWordDBMEntry(W : SIGNED_WORD)
        : PROTO_ENTRY where type t = W.t =
struct
open W
type t = W.t

(* Static limits: *)
val zero = W.one
val inf = W.maximum

(* Minimum, Maximum, Inf values for Integers as words *)
val MIN_INT = W.minimum |> shiftr 1
val MAX_INT = W.|+| (W.|~| W.one, W.maximum |> shiftr 1)

(* Comparison: *)
val (op |<=|) = W.|<=|
val (op |>=|) = W.|>=|
val (op |<|) = W.|<|
val (op |>|) = W.|>|
val (op ==) = W.==

fun cmp x y =
    if x |<| y then Lt
    else if x == y then Eq
    else Gt

val min = W.min
val max = W.max

fun is_inf x = x == inf

fun max_ceil c c' =
    case (is_inf c, is_inf c') of
        (true, _) => c' |
        (_, true) => c |
        _ => max c c'

(* Modifying the LSB: *)
fun set_lsb w = w \/ W.one
fun unset_lsb w = W.not W.one |&| w
fun toggle_lsb w =
    if W.mod2 w == W.one then unset_lsb w
    else set_lsb w

(* Everything connected to negation and Two's complement: *)
val check_neg = W.check_neg
fun |~| w = (W.|~| (shiftr 1 w) |> shiftl 1) \/  (w |&| W.one)

fun inner_not_bound b =
    let
      val tightness = if W.mod2 b == W.one then W.zero
                      else W.one
      val res = tightness \/ (unset_lsb b |> W.|~|)
    in
      res
    end

(* Unfortunately we must test whether the value of the bound b is *)
(* b <=  MIN_INT + 1, since we otherwise could generate an out of *)
(* bounds value *)
fun not_bound b =
    let
      val is_inf = is_inf b
      val not_too_small = shiftr 1 b |>| MIN_INT |+| W.one
    in
      case (is_inf, not_too_small) of
          (false, true) => inner_not_bound b
        | (true, _) => inf
        | _ => raise W.Overflow
    end

(* Addition on bounds: *)
fun add x y =
    let
      fun lsb_set w = w |&| W.one
    in
      case (x == inf, y == inf) of
          (true, _) => inf
        | (_, true) => inf
        | _ => (x |+| y)
                |+| (W.|~| ((lsb_set x) \/ (lsb_set y)))
    end

fun (x |+| y) = add x y

(* Creating bounds: *)
fun aux_from_int n =
    let
      val entry = W.from_int n
      (* val neg = W.check_neg entry *)
      (* val res = entry |> W.shiftl 1 *)
      (* val neg_res = W.check_neg res *)
    in
      case (W.|>| (entry, MAX_INT), W.|<| (entry, MIN_INT)) of
          (true, _) => raise W.Overflow
        | (_, true) => raise W.Underflow
        | _ => entry |> W.shiftl 1
    end

fun from_int (IntRep.LTE 0) = W.one
  | from_int (IntRep.LT 0) = W.zero
  | from_int IntRep.Inf = inf
  | from_int (IntRep.LTE n) = set_lsb (aux_from_int n)
  | from_int (IntRep.LT n) = aux_from_int n

fun =< x = from_int (IntRep.LT x)
fun ==< x = from_int (IntRep.LTE x)

(* Printing: *)
fun to_string w =
    let fun aux cmp = "(" ^ (W.shiftr 1 w |> W.to_string) ^ ", " ^ cmp ^ ")" in
      if w == inf then "oo"
      else if w |&| W.one ==  W.zero then aux "<" else aux "<="
    end

fun int_of_inner w =
    W.shiftr 1 w

val inner_to_string' =
    int_of_inner #> W.to_string

fun inner_to_string w =
    if w == inf then "oo" else inner_to_string' w

fun mk_string x y w =
    let fun aux cmp = case (x, y) of
        (NONE, NONE) => cmp ^ " " ^ inner_to_string' w
      | (SOME x, NONE) => x ^ " " ^ cmp ^ " " ^ inner_to_string' w
      | (NONE, SOME y) => "- " ^ y ^ " " ^ cmp ^ " -" ^ inner_to_string' w
      | (SOME x, SOME y) => x ^ " - " ^ y ^ " " ^ cmp ^ " " ^ inner_to_string' w
    in
      if w == inf then "oo"
      else if w |&| W.one ==  W.zero then aux "<" else aux "<="
    end
end

functor DBMEntryWordFn(structure W : SIGNED_WORD
                       structure B : BINARY where type from = W.t) : DBM_ENTRY =
struct
structure Entry = SignedWordDBMEntry(W)
open Entry
type from = Entry.t
type to = B.to
val serialize = B.serialize
end

structure Entry8Bit = DBMEntryWordFn(structure W = Int8
                                     structure B = SerWord8)
structure Entry32Bit = DBMEntryWordFn(structure W = Int32
                                     structure B = SerWord32)
structure Entry64Bit = DBMEntryWordFn(structure W = Int64
                                     structure B = SerWord64)
