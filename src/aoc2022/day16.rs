use std::collections::{HashMap, HashSet};

pub fn tasks() {
    let input = super::load_input(16);
    let (graph, valves) = parse_input(&input);

    // let mut path = Vec::new();
    // let mut cache = HashMap::new();
    // let res = search("AA", 30, &mut path, &graph, &valves, &mut cache);

    // println!("Result: {res}");

    let mut p = Vec::new();
    let mut c = HashMap::new();
    let players = vec!["AA", "AA"];
    let res2 = search_n(&players, 26, &mut p, &graph, &valves, &mut c);

    println!("Result: {res2}");
}

fn search(
    node: &str,
    minutes: i32,
    path: &mut Vec<String>,
    graph: &HashMap<&str, Vec<&str>>,
    valves: &HashMap<&str, i32>,
    cache: &mut HashMap<(String, Vec<String>, i32), i32>,
) -> i32 {
    if minutes < 1 {
        return 0;
    }

    if let Some(answer) = cache.get(&(node.to_string(), path.clone(), minutes)) {
        return *answer;
    }

    let mut best = i32::MIN;
    if *valves.get(node).unwrap() > 0 && !path.contains(&node.to_string()) {
        for &child in graph.get(&node).unwrap() {
            path.push(node.to_string());
            let sub = search(child, minutes - 2, path, graph, valves, cache);
            best = best.max(sub + valves.get(&node).unwrap() * (minutes - 1));
            path.pop();
        }
    }

    for &child in graph.get(&node).unwrap() {
        let sub = search(child, minutes - 1, path, graph, valves, cache);
        best = best.max(sub);
    }
    cache.insert((node.to_string(), path.clone(), minutes), best);

    best
}

// fn dfs(
//     cache: &mut HashMap<(u64, u16, u16, u16), u16>,
//     graph: &[(u16, Vec<usize>)],
//     h: usize,
//     e: usize,
//     opened: u64,
//     rate: u16,
//     steps: u16,
// ) -> u16 {
//     if steps <= 0 {
//         return 0;
//     }

//     if let Some(answer) = cache.get(&(opened, steps, h as u16, e as u16)) {
//         return *answer;
//     }

//     let mut anser = 0;

//     let h_open = opened & (1 << h) == 0 && graph[h].0 > 0;
//     let e_open = opened & (1 << e) == 0 && graph[e].0 > 0 && e != h;

//     match (h_open, e_open) {
//         (true, true) => {
//             let next_opened = opened | (1 << h) | (1 << e);
//             let next_rate = rate + graph[h].0 + graph[e].0;

//             let rel = dfs(cache,graph,h,e,next_opened,next_rate,steps-1);
//             anser = anser.max(rel);
//         }
//         (true,false) => {
//             let next_open = opened | (1<<h);
//             let next_rate = rate + graph[h];
//             for next_e in graph[e].1.iter().copied() {
//                 let rel = dfs(cache, graph, h,next_e,next_open,next_rate,steps-1);
//                 anser = anser.max(rel);
//             }
//         }
//         (false, true) => {
//             let next_open= opened | (1<<e);
//             let next_rate = rate+graph[e];
//             for next_h in graph[h].1.iter().copied() {
//                 let rel =dfs(cache, graph, next_h, e, next_open, next_rate, steps-1);
//                 anser = anser.max(rel);
//             }
//         }
//         (false,false) => {
//             for next_h in graph[h].1.iter().copied() {
//                 for next_e in graph[e].1.iter().copied() {
//                     let rel = dfs(cache, graph,next_h,next_e, opened,rate,steps-1);
//                     anser = anser.max(rel);
//                 }
//             }
//         }

//         anser += rate;
//     cache.insert((opened, steps, h, e), anser);
//     anser
// }

fn search_n(
    nodes: &HashSet<&str>,
    minutes: i32,
    path: &mut Vec<String>,
    graph: &HashMap<&str, Vec<&str>>,
    valves: &HashMap<&str, i32>,
    cache: &mut HashMap<(String, Vec<String>, i32), i32>,
) -> i32 {
    let mut best = i32::MIN;
    for n in nodes {
        if minutes < 0 {
            return 0;
        }

        if let Some(answer) = cache.get(&(n.to_string(), path.clone(), minutes)) {
            return *answer;
        }

        let mut best_in = i32::MIN;
        if *valves.get(n).unwrap() > 0 && !path.contains(&n.to_string()) {
            for &child in graph.get(n).unwrap() {
                path.push(n.to_string());
                let sub = search_n(&nodes, minutes - 2, path, graph, valves, cache)
                    + valves.get(n).unwrap() * (minutes - 1);
                best_in = best_in.max(sub);
                path.pop();
            }
        } else {
            for &child in graph.get(n).unwrap() {
                let sub = search_n(nodes, minutes - 1, path, graph, valves, cache);
                best_in = best_in.max(sub);
            }
        }
        cache.insert((n.to_string(), path.clone(), minutes), best_in);
        best += best_in;
        // let b = search(n, minutes, path, graph, valves, cache);
        // best += b;
    }

    best
}

fn dfs(
    cache: &mut HashSet<(u16, Vec<String>, u16, u16), u16>,
    graph: &HashMap<&str, Vec<&str>>,
    valves: &HashMap<&str, u16>,
    h: &str,
    e: &str,
    path: Vec<String>,
    score: u16,
    minutes: u16,
) -> u16 {
    if minutes == 0 {
        return 0;
    }

    if let Some(answer) = cache.get(&(score, path.clone(), h, e)) {
        return *answer;
    }

    let h_open = *valves.get(&h).unwrap() > 0 && !path.contains(&h.to_string());
    let e_open = *valves.get(&e).unwrap() > 0 && !path.contains(&e.to_string()) && e != h;

    let mut best = 0;
    match (h_open, e_open) {
        (true, true) => {
            // Both will open
            let mut next_path = path.clone();
            next_path.extend(vec![h.to_string(), e.to_string()]);

            let next_score = score + valves.get(&h).unwrap() + valves.get(&e).unwrap();
            let res = dfs(
                cache,
                graph,
                valves,
                h,
                e,
                next_path,
                next_score,
                minutes - 1,
            );
            best = best.max(res);
        }
        (true, false) => {
            let mut next_path = path.clone();
            next_path.extend(vec![h.to_string()]);
            let next_score = score + valves.get(&h).unwrap();

            let res = dfs(
                cache,
                graph,
                valves,
                h,
                e,
                next_path,
                next_score,
                minutes - 1,
            );
            best = best.max(res);
        }
        (false, true) => {
            let mut next_path = path.clone();
            next_path.push(e.to_string());
            let next_score = score + valves.get(&e).unwrap();

            let res = dfs(
                cache,
                graph,
                valves,
                h,
                e,
                next_path,
                next_score,
                minutes - 1,
            );
            best = best.max(res);
        }
        (false, false) => {
            for n_h in graph.get(&h).unwrap() {
                for n_e in graph.get(&e).unwrap() {
                    let res = dfs(cache, graph, valves, h, e, path, score, minutes - 1);
                    best = best.max(res);
                }
            }
        }
    }
    best += score;
    cache.insert((score, path.clone(), h, e), best);
    best
}

fn parse_input(input: &str) -> (HashMap<&str, Vec<&str>>, HashMap<&str, i32>) {
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
            .map(|r| r.as_str().parse::<i32>().unwrap())
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

        println!("{graph:?}");
        println!("{valves:?}");

        let mut path = Vec::new();
        let mut cache = HashMap::new();
        let best = search("AA", 30, &mut path, &graph, &valves, &mut cache);

        assert_eq!(1651, best);
    }

    #[test]
    fn test_day16_part2() {
        let (graph, valves) = parse_input(TEST);

        let mut path = Vec::new();
        let mut cache = HashMap::new();
        let players = vec!["AA"];
        let best = search_n(&players, 26, &mut path, &graph, &valves, &mut cache);

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
