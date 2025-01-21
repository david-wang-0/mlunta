fun enumerate _ [] = []
  | enumerate i (x :: xs) = (i, x) :: enumerate (i + 1) xs

fun sort key xs =
    let
        fun merge xs [] = xs
          | merge [] ys = ys
          | merge (x::xs) (y::ys) = if key x < key y then x :: merge xs (y::ys) else y :: merge (x::xs) ys
        fun merge_sort [] = []
          | merge_sort [x] = [x]
          | merge_sort xs = (
            let
                val n = List.length xs div 2
                val ls = List.take (xs, n)
                val rs = List.drop (xs, n)
            in
                merge (merge_sort ls) (merge_sort rs)
            end)
    in
        merge_sort xs
    end

structure Test_Graph =
struct

val test_graph0 =
[
    (0, [1, 2]),
    (1, [3]),
    (2, [0]),
    (3, [4]),
    (4, [3])
]

val test_graph1 =
[
    (0, [14, 22]),
    (1, [14, 24]),
    (2, [23, 18]),
    (3, [13, 15]),
    (4, [13, 15]),
    (5, [9, 16]),
    (6, [9, 16]),
    (7, [16]),
    (8, [20]),
    (9, [8]),
    (10,[8]),
    (11, [17, 10, 26]),
    (12, [17, 10, 28]),
    (13, [5, 12]),
    (14, [4]),
    (15, [4]),
    (16, [7]),
    (17, [2]),
    (18, [10]),
    (19, [10]),
    (20, [6, 19]),
    (21, [3, 27, 9, 0]),
    (22, [4, 27, 0]),
    (23, [3, 29, 9, 1]),
    (24, [4, 29, 1]),
    (25, [21, 11]),
    (26, [23, 12]),
    (27, [23, 12]),
    (28, [23, 12]),
    (29, [23, 12])
]

val test_graph2 =
[
    (0, [14, 22]),
    (1, [14, 24]),
    (2, [23, 18]),
    (3, [13, 15]),
    (4, [13, 15]),
    (5, [8, 16]),
    (6, []),
    (7, [16]),
    (8, [12]),
    (9, []),
    (10, []),
    (11, [17, 8, 26]),
    (12, [17, 8, 28]),
    (13, [5, 12]),
    (14, [4]),
    (15, [4]),
    (16, [7]),
    (17, [2]),
    (18, [8]),
    (19, []),
    (20, []),
    (21, [3, 27, 8, 0]),
    (22, [4, 27, 0]),
    (23, [3, 29, 8, 1]),
    (24, [4, 29, 1]),
    (25, [21, 11]),
    (26, [23, 12]),
    (27, [23, 12]),
    (28, [23, 12]),
    (29, [23, 12])
]

fun print_graph_as_dot graph =
    let
        fun succs_string n succs =
            List.map (fn i => Int.toString n ^ " -> " ^ Int.toString i) succs
            |> separate ";\n    " |> implode
        val s = map (fn (n, i, succs) =>
            Int.toString n ^
            "[label = \"" ^ Int.toString n ^ ":" ^ Int.toString i ^ "\"]" ^
            ";\n    " ^
            succs_string n succs ^ ";"
            ) graph |> separate "\n" |> implode
    in print s; print "\n" end

fun compute_scc start test_graph =
    let
        val edge_list = test_graph |> sort (fn (x, _) => x) |> map snd
        val graph = edge_list |> SCC.graph_from_list start
        val nums = SCC.gabow graph
        val nums = Array.foldr (fn (x, xs) => x :: xs) [] nums
        val num_map = enumerate 0 nums
        val edge_list = test_graph |> sort (fn (x, _) => x)
          |> map (fn (x, z) => (x, the (AList.lookup (=) num_map x), z))
    in edge_list end

fun check_forward graph =
    let
        val graph = map (fn (x, y, z) => (x, (y, z))) graph
        fun check_inner i y =
            case AList.lookup (=) graph y of
                SOME (j, _) => i <= j
              | _ => false
    in
        all (fn (x, (i, succs)) => all (check_inner i) succs) graph
    end

end

structure Gabow_SCC_Test :> TESTSUITE = struct
open SMLUnit
open Test_Graph

val test0 = "Graph 0" >+ (
    test_graph0
    |> compute_scc 0
    |> check_forward
    |> assert_true)

val test1 = "Graph 1" >+ (
    test_graph1
    |> compute_scc 25
    |> check_forward
    |> assert_true)

val test2 = "Graph 2" >+ (
    test_graph2
    |> compute_scc 25
    |> check_forward
    |> assert_true)

fun tests name =
    name >++
        [
           test0, test1, test2
        ]

fun check name =
    run_test (tests name)

end
