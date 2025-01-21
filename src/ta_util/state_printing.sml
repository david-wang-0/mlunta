signature STATE_PRINTING =
sig
    type dbm

    val print_state: Network.renaming_dicts -> int array * int array -> string
    val print_state_dbm: Network.renaming_dicts -> (int array * int array) * dbm -> string
end

functor State_Printing(D : DBM) : STATE_PRINTING =
struct
type dbm = D.t

fun print_state (renamings: Network.renaming_dicts) (locs, vars) =
    let
        open IndexDict
        fun loc_to_string i x = find x (find i (#locations renamings))
        val loc_string = mapi_array (fn (i, x) => loc_to_string i x) locs
        val loc_string = separate ", " loc_string |> implode
        fun var_to_string (i, x) = find i (#vars renamings) ^ "=" ^ Int.toString x
        val var_string = mapi_array var_to_string vars
        val var_string = separate ", " var_string |> implode
    in
        "<" ^ loc_string ^ ">, <" ^ var_string ^ ">"
    end

fun print_state_dbm renamings (loc_vars, dbm) =
    let
      val loc_vars_string = print_state renamings loc_vars
      val dbm_string = D.to_string3 (fn x => IndexDict.find x (#clocks renamings)) dbm
    in
      loc_vars_string ^ ", <" ^ dbm_string ^ ">"
    end

end
