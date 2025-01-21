functor MluntaFn(E : DBM_ENTRY) = struct
structure Construction = Construction(E)
structure D = MakeLinDBM(E)
structure Passed = PolyToMonoPassed(structure Zone = D
                                    structure Key = Location)
structure Printing_Util = State_Printing(D)

structure Setup : CHECKING_SETUP = struct
structure D = D
structure Passed = Passed
end

structure Reachability = ReachabilityChecker(Setup)
structure AEventually = AlwaysEventually(Setup)
structure Leads = LeadstoChecker(Setup)
structure Buechi = GloballyEventually(Setup)

structure Comp = Compression(Setup)

structure Certify = Certification(Setup)
structure Certify_Buechi = Certification_Buechi(Setup)
structure Cert = MakeCert(Setup)

structure Diagnostic = Diagnostic(Passed)

fun construct extra_lu json_str =
    json_str
    |> Construction.parse_construct extra_lu Construction.print_log

fun choose_algo reach cycle leadsto buechi (n : D.t Network.system) =
    let
        open Network
        open Formula
    in
        case #formula n of
            Ex _ => Ex (reach n) |
            Eg _ => Eg (cycle n) |
            Ax _ => Ax (cycle n) |
            Ag _ => Ag (reach n) |
            Leadsto _ => Ag (leadsto n) |
            GF _ => GF (buechi n)
    end

local
  open Formula
  open Property
in

fun log_state_space renamings passed =
    let
        val kv_list = Passed.kv_list passed
        val log_state = Printing_Util.print_state_dbm renamings
        fun print_loc (loc, zones) =
            List.map (fn dbm => log_state (loc, dbm) |> println) zones
    in
        List.map print_loc kv_list
    end

fun cert certify time renamings info result =
    let
        val prop =
            result |> the_formula |> just_prop
        fun certs version ht =
            Diagnostic.make_with_cert time info (certify ht) version prop
    in
        case result of
            Ex (Unsatisfied ht) => certs 0 ht |
            Ag (Satisfied ht)   => certs 0 ht |
            GF (Unsatisfied ht) => certs 1 ht |
            _                   =>
                Diagnostic.make_without_cert time prop
                |> tap (K (Log.info "* No certificate extraction possible"))
    end
end

fun check_network net =
    net
    |> choose_algo Reachability.check AEventually.check Leads.check Buechi.check
    |> Formula.result Property.convert
    |> tap (Formula.the_formula #> Property.to_string #> Log.log "Property is")

fun construct_and_check extra_lu json_str =
    json_str
    |> construct extra_lu
    |> Either.mapR (fn (_, net) => (Network.renaming_dicts net, Network.info net, net))
    |> Either.mapR (fn (r, info, net) =>  (r, info, check_network net))

fun use_time f b =
    Benchmark.add_time (fn (time, result) => Either.mapR (f time) result) b

fun check_and_then extra_lu post json_str =
    json_str
    |> Benchmark.time_it (construct_and_check extra_lu)
    |> use_time (fn time => fn (renamings, info, prop) => post (renamings, info) time prop)

fun no_cert_cc time formula =
    formula
    |> Formula.the_formula
    |> Property.just_prop
    |> Diagnostic.make_without_cert time

fun just_check extra_lu json_str =
    json_str
    |> check_and_then extra_lu (Diagnostic.property ooo (K no_cert_cc))


val log_model_file = Log.log "Model"

fun checking extra_lu model json_str =
    (
      log_model_file model;
      json_str
      |> check_and_then extra_lu ((Diagnostic.finish NONE) ooo (K no_cert_cc))
    )

fun cert_buechi version (net, trans) renamings passed =
    let
        val cert = Cert.make_cert trans renamings passed
        val _ =
            if version > 0 then
                if Certify_Buechi.check version net trans renamings cert then
                    print "Certificate check passed.\n"
                else
                    print "Certificate check failed.\n"
            else ()
        val binary = Cert.serialize cert
    in binary end

fun cert_and_compress compression certification (net, trans) passed =
    let
        val _ =
            if certification > 0 then
                if Certify.check certification net trans passed then
                    print "Certificate check passed.\n"
                else
                    print "Certificate check failed.\n"
            else ()
        val compressed =
            if compression > 0 then
                Comp.compress compression net trans passed
            else passed
        val binary = Passed.serialize compressed
    in binary end

fun certcc extra_lu compression certification json_str renaming_path cert_path (renamings, info) time =
    let
        val (net, trans) = json_str |> construct extra_lu |> Either.the_right
        val is_buechi =
            case #formula trans of
              Formula.GF _ => true
            | _ => false
        val do_it =
            if is_buechi then
                cert_buechi certification (net, trans) renamings
            else
                cert_and_compress compression certification (net, trans)
        fun do_log passed = ()
            (* let *)
            (*   val result = log_state_space renamings passed *)
            (* in *)
            (*   do_it passed *)
            (* end *)
    in
        cert do_it time renamings info
        #> Diagnostic.finish (SOME (renaming_path, cert_path))
    end

fun check_and_cert extra_lu renaming_path cert_path json_str compression certification =
      json_str
      |> check_and_then extra_lu (
          certcc
            extra_lu compression certification json_str renaming_path cert_path
         )

val just_check_lu = just_check true
val just_check = just_check false

val checking_lu = checking true
val checking = checking false

val check_and_cert_lu = check_and_cert true
val check_and_cert = check_and_cert false

(* Das ist die Funktion für die LU-Approx wie oben extra_lu=true ansonsten extra_lu=false*)
(* log ist dann die Funktion mit den Argumenten log renamings passed_set*)
fun check_and_log_passed (extra_lu : bool) log json_str =
    let
      fun post_processing (renamings, _) _ res =
          res |> Formula.the_formula |> Property.result |> log renamings
    in
      check_and_then extra_lu post_processing json_str
    end
end

structure MluntaInteger = MluntaFn(Entry)
structure Mlunta32 = MluntaFn(Entry32Bit)
structure Mlunta64 = MluntaFn(Entry64Bit)
structure Mlunta = MluntaInteger
