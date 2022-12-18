use std::collections::HashMap;

pub fn tasks() {
    let input = super::load_input(16);
    let data = parse_input(&input);

    let graph = transform(data);
    let initial = PruneGraph::prune_from(&graph, 0, 0, graph.get_next(&0), 1);
    let graph = PruneGraph::from_graph(graph);

    let mut best_greed = Score::MIN;
    let mut best = Score::MIN;
    for (start, cost) in &initial {
        let res_greed = dfs::<true>(
            &mut HashMap::new(),
            &graph,
            *start,
            0,
            30 - *cost as Minute,
            0,
        );
        let res = dfs::<false>(
            &mut HashMap::new(),
            &graph,
            *start,
            0,
            30 - *cost as Minute,
            0,
        );
        println!("Part 1:        {:0>4}", res);
        println!("Part 1 greedy: {:0>4}", res_greed);
        best = best.max(res);
        best_greed = best.max(res_greed);
    }
    println!("Part 1 best (greedy) = {best} ({best_greed})");

    let mut best = Score::MIN;
    for i in 0..initial.len() {
        let h = initial[i];
        let e = initial[(i + 1) % initial.len()];
        let h = (h.0, 26 - h.1 as Minute);
        let e = (e.0, 26 - e.1 as Minute);
        let res = dfs_2(&mut HashMap::new(), &graph, h, e, 0, 26);
        println!("With start {h:?}, {e:?} => {res}");
        best = best.max(res);
    }

    println!("Part 2 best = {best}");

    // 2501 too low
    // 3000 too high
}

type Node = usize;
type Valve = u64;
type ValveMask = Valve;
type Score = u16;
type Minute = u16;

fn dfs_2(
    cache: &mut HashMap<((Node, Minute), (Node, Minute), ValveMask, Minute), Score>,
    graph: &dyn Graph,
    h: (Node, Minute),
    e: (Node, Minute),
    opened: ValveMask,
    minute: Minute,
) -> Score {
    if let Some(cached) = cache.get(&(h, e, opened, minute)) {
        return *cached;
    }

    if (h.1 == 0 && e.1 == 0) || minute == 0 || graph.all_open(&opened) {
        return 0;
    }

    let h_valve = *graph.get_valve(&h.0);
    let h_flow = graph.get_flow(&h.0) as Score;

    let e_valve = *graph.get_valve(&e.0);
    let e_flow = graph.get_flow(&e.0) as Score;

    let mut opened = opened;
    let mut score = 0;

    let mut h = h;
    let mut e = e;

    if h.1 == minute && h_valve & opened == 0 {
        opened |= h_valve;
        h.1 -= 1;
        score += h.1 * h_flow;
    }
    if e.1 == minute && e_valve & opened == 0 {
        opened |= e_valve;
        e.1 -= 1;
        score += e.1 * e_flow;
    }

    // Both walking to a valve...
    if h.1 < minute && e.1 < minute {
        let min_min = h.1.abs_diff(minute).min(e.1.abs_diff(minute));
        let new_min = minute - (min_min);
        return score + dfs_2(cache, graph, h, e, opened, new_min);
    }

    let get_valid = |node: (Node, Minute)| -> Vec<(Node, Minute)> {
        // Is at designates target
        if node.1 == minute {
            graph
                .nodes()
                .iter()
                .filter(|n| graph.get_valve(n) & opened == 0 && **n != node.0)
                .flat_map(|n| {
                    let time = node.1.checked_sub(graph.distance(&node.0, n) as Minute);
                    time.map(|c| (*n, c))
                })
                .filter(|n| node.1 > n.1)
                .collect::<Vec<_>>()
        } else {
            // Still walking to its target node... wait
            vec![(node.0.clone(), node.1.clone())]
        }
    };

    let h_next = get_valid(h);
    let e_next = get_valid(e);

    // if h_next.is_empty() && e_next.is_empty() {
    //     return score;
    // }

    let mut best = Score::MIN;
    for next_h in &h_next {
        for next_e in &e_next {
            let res = dfs_2(
                cache,
                graph,
                *next_h,
                *next_e,
                opened,
                // A Minute passes...
                minute.saturating_sub(1),
            );
            best = best.max(res + score);
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
    // fn from_empty(&self) -> &[(Node, u8)];
    fn nodes(&self) -> Vec<Node>;

    fn get_next(&self, node: &Node) -> &[(Node, u8)];
    fn get_flow(&self, node: &Node) -> u8;
    fn get_valve(&self, node: &Node) -> &Valve;

    fn all_open(&self, mask: &ValveMask) -> bool;

    fn dist(&self, f: &Node, t: &Node, path: &mut Vec<Node>) -> u16 {
        path.push(*f);
        let mut res = 0;
        for (node, d) in self.get_next(f) {
            if node == t {
                return *d as u16;
            }
            if path.contains(node) {
                continue;
            }
            path.push(*node);
            res = self.dist(node, t, path).saturating_add(*d as u16);
            path.pop();
        }
        res
    }
    fn distance(&self, from: &Node, to: &Node) -> u16 {
        self.dist(from, to, &mut Vec::new())
    }
}

#[derive(Debug)]
struct PruneGraph {
    valves: HashMap<Node, Valve>,
    nodes: HashMap<Node, Vec<(Node, u8)>>,
    flows: HashMap<Valve, u8>,

    _all_valves_mask: ValveMask,
}

impl PruneGraph {
    fn prune_from(
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
                res.extend(PruneGraph::prune_from(
                    graph,
                    source,
                    *node,
                    &next,
                    cost + 1,
                ));
            } else {
                res.push((*node, cost));
            }
        }
        res
    }

    fn from_graph(graph: GraphRaw) -> Self {
        let mut map = HashMap::new();

        let non_zero = graph.nodes.iter().filter(|n| graph.get_flow(n.0) > 0);
        for (node, nodes) in non_zero {
            map.insert(
                *node,
                PruneGraph::prune_from(&graph, *node, *node, nodes, 1),
            );
        }

        let valves = graph
            .valves
            .into_iter()
            .filter(|e| map.keys().any(|k| *k == e.0))
            .collect::<HashMap<_, _>>();
        let flows = graph
            .flows
            .into_iter()
            .filter(|(_, f)| *f > 0)
            .collect::<HashMap<_, _>>();

        Self {
            valves,
            nodes: map,
            flows,
            _all_valves_mask: graph._all_valves_mask,
        }
    }
}

impl Graph for PruneGraph {
    fn nodes(&self) -> Vec<Node> {
        self.nodes.keys().cloned().collect::<Vec<_>>()
    }

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
    fn nodes(&self) -> Vec<Node> {
        self.nodes.keys().cloned().collect::<Vec<_>>()
    }

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
        let init = PruneGraph::prune_from(&graph, 0, 0, graph.get_next(&0), 1);
        let graph = PruneGraph::from_graph(graph);

        let mut best = Score::MIN;
        for (start, cost) in init {
            let res = dfs::<false>(
                &mut HashMap::new(),
                &graph,
                start,
                0,
                30 - cost as Minute,
                0,
            );
            best = best.max(res);
        }

        assert_eq!(1651, best);
    }

    #[test]
    fn test_day16_part2() {
        let (graph, valves) = parse_input(TEST);
        let graph = transform((graph, valves));

        let initial = PruneGraph::prune_from(&graph, 0, 0, graph.get_next(&0), 1);

        let graph = PruneGraph::from_graph(graph);

        let mut best = Score::MIN;
        for i in 0..initial.len() {
            let h = initial[i];
            let e = initial[(i + 1) % initial.len()];
            let res = dfs_2(
                &mut HashMap::new(),
                &graph,
                (h.0, 26 - h.1 as Minute),
                (e.0, 26 - e.1 as Minute),
                0,
                26,
            );
            println!(
                "Result at (initial) ({}, {}) => {} cmp {best}",
                initial[i].0,
                initial[(i + 1) % initial.len()].0,
                res
            );
            best = best.max(res);
        }

        // let best = dfs_3::<false>(&mut HashMap::new(), &graph, (0, 26), (0, 26), 0, 26);
        // let bestg = dfs_3::<true>(&mut HashMap::new(), &graph, (0, 26), (0, 26), 0, 26);
        // println!("best lazy {best}, best greedy {bestg}");

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
