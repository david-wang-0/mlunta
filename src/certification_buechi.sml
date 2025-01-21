functor Certification_Buechi(
  Setup : CHECKING_SETUP
) = struct

structure D = Setup.D
structure Basic = BasicSetup(D)
structure Printing_Util = State_Printing(D)
structure Passed = Setup.Passed
structure PWList = PassedToPWList(structure P = Passed)

val subsumes = D.subsumption

local
  open Either
in

fun all _ [] = Right true
  | all P (x :: xs) =
    case P x of
      Left y => Left (x, y)
    | Right _ => all P xs

fun lift_pred P x =
  if P x then Right true else Left x

fun check_invariant4 succs check_accepting pairs =
  let
    fun le is_accepting x i (y, j) =
      (if is_accepting then i < j else i <= j) andalso subsumes x y
    fun map_pair (L, s) = (array_to_list L, array_to_list s)
    val m = Isa_Map.hashmap_of_list1 (map (fn (l, xs) => (map_pair l, xs)) pairs)
    fun is_subsumed i (l, x) =
      hm_sat (exists (le (check_accepting (l, x)) x i)) m (map_pair l)
    fun all_subsumed l (x, i) = all (lift_pred (is_subsumed i)) (succs (l, x))
      |> mapL fst
    fun check_pair (l, xs) = all (all_subsumed l) xs
    val result = all check_pair pairs |> mapL (fn ((l, xs), y) => (l, y))
  in
    result
  end

end

fun check version discrete_succs system renamings cert =
  let
    val setup = Basic.initial_setup system
    val (P, norm, initial) = (
      Basic.P setup,
      Basic.norm setup,
      Basic.initial setup)
    val succs = #trans system #> map norm #> flat
    fun with_debug checker cert =
      case checker cert of
        Either.Left (l, ((x, i), (l', y))) => (
          "> Certificate check failed for state\n>  " ^
          Printing_Util.print_state_dbm renamings (l, x) ^
          "\n> on successor\n>  " ^
          Printing_Util.print_state_dbm renamings (l', y)
          |> println; false)
      | Either.Right _ => true
    val checker =
      case version of
        4 => (fn succs => fn P => with_debug (check_invariant4 succs P))
      | _ => K false |> K |> K
    val checker = fn cert =>
      let
        val r = Benchmark.time_it (checker succs P) cert
        val _ = Log.time "Time for certifiate checking" (#time r)
      in #result r end
  in
    if version > 0 then
      cert
      |> (fn x => (print "Starting certificate checking\n"; x))
      |> checker
      |> (fn x => (print "Finished certificate checking\n"; x))
    else false
  end

end
