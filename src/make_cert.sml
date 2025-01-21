structure Key_Int : KEY =
struct
    type key = int
    fun ord (a, b) = if a < b then LESS else if a > b then GREATER else EQUAL
end

structure Key_State : KEY =
struct
    type key = int array * int array
    val array_to_list = Array.foldr (op ::) []
    fun cmp_int_list [] [] = EQUAL
      | cmp_int_list [] _  = LESS
      | cmp_int_list _ []  = GREATER
      | cmp_int_list (x :: xs) (y :: ys) =
        case Key_Int.ord (x, y) of
            EQUAL => cmp_int_list xs ys
          | x  => x
    fun cmp_int_arrays xs ys =
        cmp_int_list (array_to_list xs) (array_to_list ys)
    fun ord ((a, b), (c, d)) =
        case cmp_int_arrays a c of
            EQUAL => cmp_int_arrays b d
          | x  => x
end

structure Tab_Int = Table(Key_Int)

structure Tab_State = Table(Key_State)

functor MakeCert(Setup : CHECKING_SETUP) = struct

structure Passed = Setup.Passed
structure D = Setup.D
structure Basic = BasicSetup(D)
structure Printing_Util = State_Printing(D)

fun log_state_dot (renamings : Network.renaming_dicts) ((locs, vars), (dbm, scc_num, n, succs)) =
    let
        val state_string = Printing_Util.print_state_dbm renamings ((locs, vars), dbm)
        val succs_string =
            List.map (fn i => Int.toString n ^ " -> " ^ Int.toString i) succs
            |> separate ";\n    " |> implode
        val label_string =
            Int.toString n ^ ":" ^ Int.toString scc_num ^ ": (" ^ state_string ^ ")"
    in
        Int.toString n ^ "[label = \"" ^ label_string ^ "\"]" ^ ";\n    " ^
        succs_string ^ ";"
    end

fun log_state_ml (renamings : Network.renaming_dicts) ((locs, vars), (dbm, scc_num, n, succs)) =
    let
        val state_string = Printing_Util.print_state_dbm renamings ((locs, vars), dbm)
        val succs_string =
            List.map Int.toString succs |> separate ", " |> implode
        val label_string =
            Int.toString n ^ ":" ^ Int.toString scc_num ^ ":(" ^ state_string ^ ")"
    in
        "  (" ^ Int.toString n ^ ", \"" ^ label_string ^ "\", [" ^
        succs_string ^ "]),"
    end

fun log_certificate log_state renamings kv_list =
    let
        fun print_loc (loc, zones) =
            List.map (fn dbm => log_state renamings (loc, dbm)) zones
            |> separate "\n" |> implode
    in
        List.map print_loc kv_list |> separate "\n" |> implode
    end

fun log_certificate_as_dot renamings kv_list =
    (
        println "digraph test {";
        log_certificate log_state_dot renamings kv_list |> println;
        println "}"
    )

fun log_certificate_as_ml renamings kv_list =
    (
        println "[";
        log_certificate log_state_ml renamings kv_list |> println;
        println "]"
    )

fun sem_equal x y = D.subsumption x y andalso D.subsumption y x

fun make_graph succs initial kv_list =
    let
        open Unsynchronized
        val state_to_index = ref Tab_State.empty
        val index_to_state = ref Tab_Int.empty
        val counter = ref 0
        fun insert_state loc kv_pairs =
            let
                val tab = !state_to_index
                val tab = Tab_State.update_new (loc, kv_pairs) tab
            in state_to_index := tab end
        fun insert_index index state =
            let
                val tab = !index_to_state
                val tab = Tab_Int.update_new (index, state) tab
            in index_to_state := tab end
        fun insert loc zones =
            let
                val pairs =
                    List.foldl (fn (zone, pairs) =>
                        let
                            val n = inc counter - 1
                            val _ = insert_index n (loc, zone)
                        in
                            (zone, n) :: pairs
                        end
                    ) [] zones
            in insert_state loc pairs end
        val _ = map (fn (loc, zones) => insert loc zones) kv_list
        fun lookup_state loc zone =
            let
                val zones = the (Tab_State.lookup (!state_to_index) loc)
            in
                case List.find (fn (zone', _) => sem_equal zone zone') zones of
                    SOME (zone, index) => SOME index
                    | NONE => NONE
            end
        fun find_subsumption loc zone =
            let
                val zones = the (Tab_State.lookup (!state_to_index) loc)
            in
                case List.find (fn (zone', _) => D.subsumption zone zone') zones of
                    SOME (zone, index) => SOME index
                    | NONE => NONE
            end
        fun get_succ loc zone =
            case lookup_state loc zone of
                SOME x => x
              | NONE => the (find_subsumption loc zone)
        fun lookup_index index =
            the (Tab_Int.lookup (!index_to_state) index)
        fun edges index =
            let
                val (loc, zone) = lookup_index index
                val the_succs = succs (loc, zone)
            in
                map (fn (loc, zone) => get_succ loc zone) the_succs
            end
        val (l0, z0) = initial
        val start = the (lookup_state l0 z0)
        val bound = !counter
        val graph =
            {
                bound = bound,
                start = start,
                edges = edges
            }
        val kv_list = Tab_State.dest (!state_to_index)
    in
        (graph, kv_list)
    end

fun make_cert (system : D.t Network.system) renamings passed =
    let
        val setup =
            Basic.initial_setup system
        val (P, norm, initial) = (
            Basic.P setup,
            Basic.norm setup,
            Basic.initial setup)
        val succf = #trans system #> map norm #> flat
        val kv_list1 = Passed.kv_list passed
        val (graph, kv_list2) = make_graph succf initial kv_list1
        val nums = SCC.gabow graph
        val kv_list3 =
            List.map (fn (loc, zones) =>
                List.map (fn (zone, n) =>
                    (zone, Array.sub (nums, n))
                ) zones
                |> (fn pairs => (loc, pairs))
            ) kv_list2
        (* val kv_all = *)
        (*     List.map (fn (loc, zones) => *)
        (*         List.map (fn (zone, n) => *)
        (*             (zone, Array.sub (nums, n), n, #edges graph n) *)
        (*         ) zones *)
        (*         |> (fn pairs => (loc, pairs)) *)
        (*     ) kv_list2 *)
        (* val _ = log_certificate_as_dot renamings kv_all *)
    in kv_list3 end

fun serialize_union dbms =
    map (fn (dbm, i) => Word8Vector.concat [D.serialize dbm, SerInt.serialize i]) dbms

fun one_union ((l, dbms), acc) =
    let
      val length = dbms |> length |> SerInt.serialize
      val loc = Location.serialize l
    in
      dbms
      |> serialize_union
      |> cons length
      |> cons loc
      |> Word8Vector.concat
      |> flip cons acc
    end

fun serialize passed =
    let
      val n_buckets = passed |> length
      val n_buckets_ser = n_buckets |> SerInt.serialize
      val states = List.foldl one_union [] passed
    in
      states |> cons n_buckets_ser |> Word8Vector.concat
    end

end
