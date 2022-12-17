use std::{
    collections::{BTreeMap, HashMap},
    fmt::Display,
};

pub fn tasks() {
    let input = super::load_input(16);
    let data = parse_input(&input);

    let graph = transform(data);
    let graph = PruneGraph::from_graph(graph);

    // println!(
    //     "Part 1 lazy:   {:0>4}",
    //     dfs::<false>(&mut HashMap::new(), &graph, 0, 0, 30, 0)
    // );
    // println!(
    //     "Part 1 greedy: {:0>4}",
    //     dfs::<true>(&mut HashMap::new(), &graph, 0, 0, 30, 0)
    // );

    // println!(
    //     "Part 2 lazy:   {:0>4}",
    //     dfs_2::<false>(&mut HashMap::new(), &graph, 0, 0, 0, 26)
    // );
    // println!(
    //     "Part 2 greedy: {:0>4}",
    //     dfs_2::<true>(&mut HashMap::new(), &graph, 0, 0, 0, 26)
    // );

    let rec = rec(&graph, &mut HashMap::new(), (0, 26), (0, 26), 0);
    println!("rec {}", rec);

    // 2501 too low
    // 3000 too high
}

fn rec(
    graph: &dyn Graph,
    cache: &mut HashMap<((Node, Minute), (Node, Minute), ValveMask), Score>,
    h: (Node, Minute),
    e: (Node, Minute),
    open: ValveMask,
) -> Score {
    let sort_best = |node: Node, opened: ValveMask| -> Vec<&(Node, u8)> {
        let mut next = graph
            .get_next(&node)
            .iter()
            .filter(|n| *graph.get_valve(&n.0) & opened == 0)
            .collect::<Vec<_>>();
        next.sort_by_key(|n| {
            let this = graph.get_valve(&n.0);
            let rhs = n.1 as u64;
            let d = this / rhs;
            let r = this % rhs;
            if r > 0 && rhs > 0 {
                d + 1
            } else {
                d
            }
        });
        next
    };

    if let Some(cached) = cache.get(&(h, e, open)) {
        return *cached;
    }

    let mut best = Score::MIN;
    for turn_h in sort_best(h.0, open) {
        if h.1 < turn_h.1 as Minute {
            continue;
        }

        let new_h = (turn_h.0, h.1.saturating_sub(1 + turn_h.1 as Minute));
        let score_h = new_h.1 * graph.get_flow(&turn_h.0) as Score;
        let open_h = open | graph.get_valve(&turn_h.0);

        let mut best_inner = Score::MIN;

        for turn_e in sort_best(e.0, open) {
            if e.1 < turn_e.1 as Minute {
                continue;
            }
            let new_e = (turn_e.0, e.1.saturating_sub(1 + turn_e.1 as Minute));
            let score_e = new_e.1 * graph.get_flow(&turn_e.0) as Minute;
            let mut open_e = open | graph.get_valve(&turn_e.0);
            if h.1 < new_e.1 {
                open_e |= open_h;
            }
            let res = rec(graph, cache, new_h, new_e, open_e);
            best_inner = best_inner.max(res + score_e);
        }
        best = best.max(best_inner + score_h);
    }

    cache.insert((h, e, open), best);
    best
}

type Node = usize;
type Valve = u64;
type ValveMask = Valve;
type Score = u16;
type Minute = u16;

fn dfs_2<const GREEDY: bool>(
    cache: &mut HashMap<(Node, Node, ValveMask, Minute), Score>,
    graph: &dyn Graph,
    h: Node,
    e: Node,
    opened: ValveMask,
    minute: Minute,
) -> Score {
    if minute == 0 || graph.all_open(&opened) {
        return 0;
    }

    if let Some(cached) = cache.get(&(h, e, opened, minute)) {
        return *cached;
    }

    let mut best = Score::MIN;

    let h_valve = *graph.get_valve(&h);
    let h_flow = graph.get_flow(&h) as Score;

    let e_valve = *graph.get_valve(&e);
    let e_flow = graph.get_flow(&e) as Score;

    // can valve be opened? pos flow and currently closed
    let h_open = h_valve & opened == 0 && h_flow > 0;
    let e_open = h != e && e_valve & opened == 0 && e_flow > 0;

    match (h_open, e_open) {
        (true, true) => {
            let res = dfs_2::<GREEDY>(
                cache,
                graph,
                h,
                e,
                opened | h_valve | e_valve,
                minute.saturating_sub(1),
            );
            best = best.max(res + (minute as Score - 1) * (h_flow + e_flow));

            if !GREEDY {
                {
                    for &(e_child, e_cost) in graph.get_next(&e) {
                        let res = dfs_2::<GREEDY>(
                            cache,
                            graph,
                            h,
                            e_child,
                            opened | h_valve,
                            minute.saturating_sub(e_cost as Minute),
                        );
                        best = best.max(res + (h_flow * (minute.saturating_sub(1))));
                    }
                }
                {
                    for &(h_child, h_cost) in graph.get_next(&h) {
                        let sub = dfs_2::<GREEDY>(
                            cache,
                            graph,
                            h_child,
                            e,
                            opened | e_valve,
                            minute.saturating_sub(h_cost as Minute),
                        );
                        best = best.max(sub + (minute.saturating_sub(1)) * e_flow);
                    }
                }
            }
        }
        (true, false) => {
            for &(e_child, e_cost) in graph.get_next(&e) {
                let res = dfs_2::<GREEDY>(
                    cache,
                    graph,
                    h,
                    e_child,
                    opened | h_valve,
                    minute.saturating_sub(e_cost as Minute),
                );
                best = best.max(res + minute.saturating_sub(1) * h_flow);
            }
        }
        (false, true) => {
            for &(h_child, h_cost) in graph.get_next(&h) {
                let res = dfs_2::<GREEDY>(
                    cache,
                    graph,
                    h_child,
                    e,
                    opened | e_valve,
                    minute.saturating_sub(h_cost as Minute),
                );
                best = best.max(res + minute.saturating_sub(1) * e_flow);
            }
        }
        (false, false) => {
            if GREEDY {
                for &(h_child, h_cost) in graph.get_next(&h) {
                    for &(e_child, e_cost) in graph.get_next(&e) {
                        let res = dfs_2::<GREEDY>(
                            cache,
                            graph,
                            h_child,
                            e_child,
                            opened,
                            minute.saturating_sub((h_cost + e_cost) as Minute),
                        );
                        best = best.max(res);
                    }
                }
            }
        }
    }

    if !GREEDY {
        for &(h_child, h_cost) in graph.get_next(&h) {
            for &(e_child, e_cost) in graph.get_next(&e) {
                let res = dfs_2::<GREEDY>(
                    cache,
                    graph,
                    h_child,
                    e_child,
                    opened,
                    minute.saturating_sub((h_cost + e_cost) as Minute),
                );
                best = best.max(res);
            }
        }
    }

    cache.insert((h, e, opened, minute), best);
    best
}

fn dfs<const GREEDY: bool>(
    cache: &mut HashMap<(Node, ValveMask, Minute), Score>,
    graph: &dyn Graph,
    node: Node,
    opened: ValveMask,
    minute: Minute,
    score: Score,
) -> Score {
    if minute == 0 || graph.all_open(&opened) {
        return 0;
    }

    if let Some(cached) = cache.get(&(node, opened, minute)) {
        return *cached;
    }

    let mut best = Score::MIN;

    let n_valve = *graph.get_valve(&node);
    let n_flow = graph.get_flow(&node) as Score;
    let mut just_opened = false;
    if n_valve & opened == 0 && n_flow > 0 {
        just_opened = true;

        let valve_score = n_flow * minute.saturating_sub(1) as Score;
        let new_opened = opened | n_valve;
        let new_minute = minute.saturating_sub(1);
        let new_score = score + valve_score;
        for (child, cost) in graph.get_next(&node) {
            let sub = dfs::<GREEDY>(
                cache,
                graph,
                *child,
                new_opened,
                new_minute.saturating_sub(*cost as Minute),
                new_score,
            );
            best = best.max(sub + valve_score);
        }
    }

    if !(GREEDY && just_opened) {
        for (child, cost) in graph.get_next(&node) {
            let sub = dfs::<GREEDY>(
                cache,
                graph,
                *child,
                opened,
                minute.saturating_sub(*cost as Minute),
                score,
            );
            best = best.max(sub);
        }
    }

    cache.insert((node, opened, minute), best);
    best
}

trait Graph {
    fn get_next(&self, node: &Node) -> &[(Node, u8)];
    fn get_flow(&self, node: &Node) -> u8;
    fn get_valve(&self, node: &Node) -> &Valve;

    fn all_open(&self, mask: &ValveMask) -> bool;
}

#[derive(Debug)]
struct PruneGraph {
    valves: HashMap<Node, Valve>,
    nodes: HashMap<Node, Vec<(Node, u8)>>,
    flows: HashMap<Valve, u8>,

    _all_valves_mask: ValveMask,
}

impl PruneGraph {
    fn from_graph(graph: GraphRaw) -> Self {
        let mut map = HashMap::new();

        fn dive(
            graph: &GraphRaw,
            source: Node,
            from: Node,
            nodes: &[(Node, u8)],
            cost: u8,
        ) -> Vec<(Node, u8)> {
            let mut res = Vec::new();
            for (node, _) in nodes {
                if graph.get_flow(node) == 0 {
                    let mut next = graph.get_next(node).to_vec();
                    next.retain(|n| n.0 != source && n.0 != from);
                    if next.is_empty() {
                        break;
                    }
                    res.extend(dive(graph, source, *node, &next, cost + 1));
                } else {
                    res.push((*node, cost));
                }
            }
            res
        }

        for (node, nodes) in &graph.nodes {
            map.insert(*node, dive(&graph, *node, *node, nodes, 1));
        }

        Self {
            valves: graph.valves,
            nodes: map,
            flows: graph.flows,
            _all_valves_mask: graph._all_valves_mask,
        }
    }
}

impl Graph for PruneGraph {
    fn get_next(&self, node: &Node) -> &[(Node, u8)] {
        &self.nodes.get(node).unwrap()[..]
    }

    fn get_flow(&self, node: &Node) -> u8 {
        let valve = self.get_valve(node);
        *self.flows.get(valve).unwrap_or(&0)
    }
    fn get_valve(&self, node: &Node) -> &Valve {
        self.valves.get(node).unwrap()
    }

    fn all_open(&self, mask: &ValveMask) -> bool {
        self._all_valves_mask == *mask & self._all_valves_mask
    }
}

struct GraphRaw {
    valves: HashMap<Node, Valve>,
    /// Maps to the indices of `valves`
    nodes: HashMap<Node, Vec<(Node, u8)>>,
    /// Flows
    flows: HashMap<Valve, u8>,

    _all_valves_mask: ValveMask,
}

impl GraphRaw {
    fn new(valves: Vec<Valve>, nodes: HashMap<Node, Vec<Node>>, flows: HashMap<Valve, u8>) -> Self {
        let mut all_mask = 0;
        for (valve, _ppm) in flows.iter().filter(|(_v, p)| **p > 0) {
            all_mask |= valve;
        }
        GraphRaw {
            valves: valves
                .into_iter()
                .enumerate()
                .collect::<HashMap<Node, Valve>>(),
            nodes: nodes
                .into_iter()
                .map(|(k, v)| (k, v.into_iter().map(|n| (n, 1)).collect::<Vec<_>>()))
                .collect(),
            flows,
            _all_valves_mask: all_mask,
        }
    }
}

impl Graph for GraphRaw {
    fn get_next(&self, node: &Node) -> &[(Node, u8)] {
        &self.nodes.get(node).unwrap()[..]
    }

    fn get_flow(&self, node: &Node) -> u8 {
        let valve = self.get_valve(node);
        *self.flows.get(valve).unwrap_or(&0)
    }
    fn get_valve(&self, node: &Node) -> &Valve {
        self.valves.get(node).unwrap()
    }

    fn all_open(&self, mask: &ValveMask) -> bool {
        self._all_valves_mask == *mask & self._all_valves_mask
    }
}

impl Display for GraphRaw {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let valves = self
            .valves
            .iter()
            .map(|(k, v)| format!("\n\t\t{k} = {v:0>64b}"))
            .collect::<String>();
        let nodes = self
            .nodes
            .iter()
            .map(|(k, v)| format!("\n\t\t{k} = {v:?}"))
            .collect::<String>();
        let flows = self
            .flows
            .iter()
            .map(|(k, v)| format!("\n\t\t{k:0>64b} => {v}"))
            .collect::<String>();

        write!(
            f,
            "Graph {{\n\tValves{}\n\tNodes:{}\n\tFlows:{},\n\tA-Open:\t{:0>64b}\n}}",
            valves, nodes, flows, self._all_valves_mask
        )
    }
}

fn transform((graph, valves): (HashMap<&str, Vec<&str>>, HashMap<&str, u8>)) -> GraphRaw {
    let mut names = valves.keys().collect::<Vec<_>>();
    names.sort();

    let mut map_name_index = HashMap::new();
    let mut vals = Vec::new();
    let mut flows = HashMap::new();
    for (index, &name) in names.iter().enumerate() {
        let ppm = *valves.get(name).unwrap();
        let valv = name_to_ident(name);
        vals.push(valv);
        flows.insert(valv, ppm);

        map_name_index.insert(name, index);
    }

    let mut map = HashMap::new();
    for (name, connections) in graph {
        let name_index = map_name_index.get(&name).unwrap();

        map.insert(
            *name_index,
            connections
                .into_iter()
                .map(|n| *map_name_index.get(&n).unwrap())
                .collect::<Vec<_>>(),
        );
    }

    GraphRaw::new(vals, map, flows)
}

fn name_to_ident(name: &str) -> Valve {
    1 << name.bytes().take(2).map(|b| b - b'A').sum::<u8>() as u16
}

fn parse_input(input: &str) -> (HashMap<&str, Vec<&str>>, HashMap<&str, u8>) {
    use regex::Regex;

    let r_name = Regex::new(r"[A-Z]{2}").unwrap();
    let r_value = Regex::new(r"\d+").unwrap();

    let mut graph = HashMap::new();
    let mut valves = HashMap::new();

    for line in input.lines() {
        let names = r_name
            .find_iter(line)
            .map(|r| r.as_str())
            .collect::<Vec<_>>();
        let value = r_value
            .find_iter(line)
            .map(|r| r.as_str().parse::<u8>().unwrap())
            .collect::<Vec<_>>()[0];

        graph.insert(names[0], names[1..].to_vec());
        valves.insert(names[0], value);
    }

    (graph, valves)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_day16_part1() {
        let (graph, valves) = parse_input(TEST);
        let graph = transform((graph, valves));
        let graph = PruneGraph::from_graph(graph);

        let mut cache = HashMap::new();
        let best = dfs::<false>(&mut cache, &graph, 0, 0, 30, 0);

        assert_eq!(1651, best);
    }

    #[test]
    fn test_day16_part2() {
        let (graph, valves) = parse_input(TEST);
        let graph = transform((graph, valves));
        let graph = PruneGraph::from_graph(graph);

        let best = dfs_2::<false>(&mut HashMap::new(), &graph, 0, 0, 0, 26);
        let bestg = dfs_2::<true>(&mut HashMap::new(), &graph, 0, 0, 0, 26);
        println!("best lazy {best}, best greedy {bestg}");

        assert_eq!(1707, best);
    }

    const TEST: &str = "\
Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II";
}
