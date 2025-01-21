functor GloballyEventually(Setup : CHECKING_SETUP) = struct
structure Buechi = BuechiChecker(Setup)

fun check system =
    case Buechi.check system of
        ((cyan, blue, red), true) => Property.Satisfied blue |
        ((cyan, blue, red), _) => Property.Unsatisfied blue
end