fun implode xs = rev xs |> List.foldl (op ^) ""

fun map_array f arr = Array.appi (fn (i, x) => Array.update (arr, i, f x)) arr

structure SCC =
struct

type v = int

type graph = {
    edges : v -> v list,
    bound : v,
    start : v
}

fun pop_until_p p acc [] = (acc, [])
  | pop_until_p p acc (x :: xs) =
    if p x then
        (acc, x :: xs)
    else
        pop_until_p p (x :: acc) xs

fun gabow (graph : graph) = let
    val s = []
    val p = []
    val pre = Array.array (#bound graph, 0)
    val nums = Array.array (#bound graph, 0)
    (* XXX for debugging, remove *)
    fun show_list show xs = map show xs |> separate "," |> implode
    fun array_to_list xs = Array.foldr (op ::) [] xs
    val show_array = show_list Int.toString o array_to_list
    val show_list = show_list Int.toString
    fun println s = (print s; print "\n")
    fun println _ = ()
    
    fun pop_until_pre s v =
        let
            val _ = println "Popping p"
            val _ = "Before: " ^ show_list s |> println
            val n = Array.sub (pre, v)
            val s = case pop_until_p (fn w => Array.sub (pre, w) <= n) [] s of
                (acc, s) => s
            val _ = "After: " ^ show_list s |> println
        in s end
    fun pop_until s v =
        let
            val _ = println "Popping s"
            val _ = "Before: " ^ show_list s |> println
            val (component, s) =
            case pop_until_p (fn w => v = w) [] s of
                (acc, _ :: s) => (v :: acc, s)
            val _ = "After: " ^ show_list s |> println
        in (component, s) end
    val count = ref 0;
    val num_clusters = ref 0;
    fun dfs s p v = let
        val _ = count := !count + 1
        val _ = Array.update (pre, v, !count)
        val _ = "Exploring " ^ Int.toString v |> println
        val s = v :: s
        val p = v :: p
        val (s, p) =
            List.foldl (fn (w, (s, p)) =>
                if Array.sub (pre, w) = 0 then
                    dfs s p w
                else if Array.sub (nums, w) = 0 then
                (* cylce found -> pop all nodes that have to be on the cycle *)
                    (s, pop_until_pre p w)
                else (s, p)
            )
            (s, p)
            (#edges graph v)
        val (s, v' :: p) = (s, p)
        val (s, p) =
            (* initial node of an SCC found -> remove SCC and assign SCC number *)
            if v = v' then
                let
                    val _ = "Found component " ^ Int.toString v |> println
                    val (component, s) = pop_until s v
                    val n = !num_clusters
                    val _ = num_clusters := n + 1
                    val _ = map (fn w => Array.update (nums, w, n)) component
                    val _ = "Assigning " ^ Int.toString n ^ " to component " ^ show_list component
                        |> println
                in (s, p) end
            else (s, v' :: p)
        in (s, p) end
    in (
        dfs s p (#start graph);
        map_array (fn x => !num_clusters - x) nums;
        nums
    )
    end

fun graph_from_list start edge_list =
    let
        val edge_array = Array.fromList edge_list
        fun E v = Array.sub (edge_array, v)
        val bound = List.length edge_list
    in
        {
            bound = bound,
            edges = E,
            start = start
        }
    end

end