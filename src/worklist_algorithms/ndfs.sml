structure BuechiData = struct
val explored = Unsynchronized.ref 0
val subsumed = Unsynchronized.ref 0
val accepting = Unsynchronized.ref 0
end

functor BuechiChecker(Setup : CHECKING_SETUP) = struct
structure Passed = Setup.Passed
structure D = Setup.D
structure Basic = BasicSetup(D)

local
    open Unsynchronized
in
fun inc_explored () = inc BuechiData.explored
fun inc_subsumed () = inc BuechiData.subsumed
fun reset_subsumed () = BuechiData.subsumed := 0
fun inc_accepting () = inc BuechiData.accepting
fun reset_accepting () = BuechiData.accepting := 0
end

val table_add = Passed.insert_p'

fun sem_eq x y = D.subsumption x y andalso D.subsumption y x

fun table_rem table (key, zone) = Passed.remove_p (sem_eq zone) table key

(* state <= table *)
fun setsubsumes table state =
    if Passed.exists D.subsumption table state then
        (inc_subsumed (); true)
    else false

(* table <= state *)
fun subsumesset state table =
    Passed.exists (flip D.subsumption) table state

(* state in table *)
(* XXX Can this be more efficient? *)
fun table_test state table =
    Passed.exists sem_eq state table

fun reorderqueue succs = succs


(* General scheme of a DFS *)
fun rundfs succs enterdfs predfs lookahead cyclefound filterdfs testaltdfs alternativedfs
    testrecursivedfs postdfs thestate =
    (
        predfs thestate;
        let 
            val successors = reorderqueue (succs thestate)
            fun process_sucs suclist = case suclist of
              [] => ()
            | suc::body => (
                if filterdfs thestate suc then
                    if testaltdfs thestate suc then alternativedfs suc
                    else if testrecursivedfs suc then
                        rundfs succs enterdfs predfs lookahead cyclefound filterdfs testaltdfs alternativedfs testrecursivedfs postdfs suc
                    else ()
                else ();
                process_sucs body)
        in
        case lookahead successors of
           SOME cyclestate => cyclefound thestate cyclestate
         | NONE => process_sucs successors; postdfs thestate
        end
    )


fun ndfs succs is_accepting start  =
    let
    exception CYCLE_FOUND
    val cyan = Passed.mkTable 32
    val blue = Passed.mkTable 32
    val red  = Passed.mkTable 32
    fun enterdfs state = false (* XXX  Dummy remove *)
    fun predfs state =
        table_add cyan state
    fun cyclefound the_state state =
        (
            print "Cycle found\n";
            raise CYCLE_FOUND
        )
    fun filterdfs the_state state =
        not (table_test blue state orelse table_test cyan state orelse setsubsumes red state)
    fun testaltdfs _ _ = false
    fun alternativedfs state = ()
    fun testrecursivedfs state = true
    val withlookahead = List.find (fn x => is_accepting x andalso table_test cyan x)
    fun nolookahead succs = NONE
    fun postdfs state =
        let
            fun enterdfs state = true
            fun predfs state = table_add red state
            val cyclefound = cyclefound
            fun filterdfs the_state state = true
            (* post-cycle found *)
            fun testaltdfs the_state state = subsumesset state cyan (* Cyan <= t *)
            fun alternativedfs state = cyclefound state state
            fun testrecursivedfs state = not (setsubsumes red state) (* ~ t <= Red *)
            fun postdfs state = ()
        in
            if is_accepting state
            then (
                inc_accepting ();
                rundfs succs enterdfs predfs nolookahead cyclefound filterdfs testaltdfs alternativedfs testrecursivedfs postdfs state
                )
            else ();
            table_add blue state;
            table_rem cyan state;
            inc_explored ();
            ()
        end
    in
        (
            rundfs succs enterdfs predfs withlookahead cyclefound filterdfs testaltdfs alternativedfs testrecursivedfs postdfs start;
            ((cyan, blue, red), false)
        )
        handle CYCLE_FOUND => ((cyan, blue, red), true)
    end

fun check (system : D.t Network.system) =
    let
        val setup =
            Basic.initial_setup system
        val (P, norm, initial) = (
            Basic.P setup,
            Basic.norm setup,
            Basic.initial setup)
        val succf = #trans system #> map norm #> flat
        val _ = reset_subsumed ()
        val _ = reset_accepting ()
        val result = ndfs succf P initial
        open Unsynchronized
        val _ = Log.int "States accepting" (!BuechiData.accepting)
        val _ = Log.int "States subsumed" (!BuechiData.subsumed)
    in result end

end