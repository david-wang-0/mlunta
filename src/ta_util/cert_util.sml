(* Hash Maps *)
fun hm_sat P m k =
  case Isa_Map.hm_lookup1 k m of
    NONE => false
  | SOME x => P x