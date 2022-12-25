use std::collections::{HashMap, VecDeque};

type WorryCount = u64;

pub fn tasks() {
    let input = super::load_input(11);
    let p1 = calculate_monkey_business(&input, 20, true);
    let p2 = calculate_monkey_business(&input, 10000, false);

    println!("Monkey buisness Part1: {}", p1);
    println!("Monkey buisness Part2: {}", p2);
}

fn calculate_monkey_business(input: &str, rounds: usize, recovering: bool) -> WorryCount {
    let mut monkeys = read_monkeys(input);
    let handling = monkey_handling(&mut monkeys, rounds, recovering);

    count_monkey_business(handling, 2)
}

fn monkey_handling(
    monkeys: &mut [Monkey],
    rounds: usize,
    recovering: bool,
) -> HashMap<usize, WorryCount> {
    let mut rounds = Round::new(rounds);
    let mut vars = Vars::new();

    let common_divisor = monkeys
        .iter()
        .map(|monkey| {
            monkey
                .test
                .symbols
                .iter()
                .flat_map(|symbol| {
                    if let Symbol::Val(Value::Literal(div)) = symbol {
                        return Some(*div);
                    }
                    None
                })
                .filter(|div| *div != 0)
                .collect::<Vec<_>>()[0]
        })
        .product::<WorryCount>();

    //for each Monkey
    //    Monkey - inspects
    //    Worry level decreases FLOOR(Worry /= 3) because NOT DAMAGED
    //    Monkey tests worry level
    //    Monkey decides how to handle with test
    //    Item-Handled goes to end of monkey list

    let mut monkey_inspects: HashMap<usize, WorryCount> = HashMap::with_capacity(monkeys.len());

    while !rounds.complete() {
        for current_monkey in 0..monkeys.len() {
            monkey_inspects.entry(current_monkey).or_insert(0);
            let monkey_inspections = monkey_inspects.get_mut(&current_monkey).unwrap();

            let mut passed_items = Vec::new();
            {
                let monkey = &mut monkeys[current_monkey];
                while !monkey.items.is_empty() {
                    *monkey_inspections += 1;

                    let mut worry = monkey.inspect(&mut vars);
                    if recovering {
                        worry.recover();
                    } else {
                        worry.level %= common_divisor;
                    }
                    vars.set("new", worry.level);
                    vars.set("old", worry.level);
                    let passed_to = if monkey.test(&worry, &mut vars) {
                        monkey.throw_true
                    } else {
                        monkey.throw_false
                    };
                    // println!("Monkey {current_monkey} inspected item and passes it to {passed_to} with {}", worry.level);
                    passed_items.push((passed_to, worry));
                }
            }

            for (to, item) in passed_items {
                let monkey_to = &mut monkeys[to];
                monkey_to.items.push_back(item);
            }
        }
        // println!("Round {}", rounds.current);
        // for m in 0..monkeys.len() {
        //     let monke = &monkeys[m];
        //     let inspects = monkey_inspects.get(&m).unwrap_or(&0);
        //     println!("Monkey {m} (Inspects {inspects}): {:?}", monke.items)
        // }
        // println!("{}", "-".repeat(25));
        rounds.next();
    }
    monkey_inspects
}

fn count_monkey_business(map: HashMap<usize, WorryCount>, top: usize) -> WorryCount {
    let mut values = map.values().collect::<Vec<_>>();
    values.sort_by(|a, b| b.cmp(a));

    values.iter().take(top).map(|c| **c).product()
}

#[derive(Debug)]
struct Round {
    current: usize,
    until: usize,
}

impl Round {
    fn new(until: usize) -> Self {
        Self { current: 0, until }
    }

    fn next(&mut self) {
        self.current += 1;
    }

    fn complete(&self) -> bool {
        self.current >= self.until
    }
}

/// Worry level about Items handled
#[derive(Debug)]
struct Worry {
    level: WorryCount,
}

impl Worry {
    const OLD: &str = "old";
    // const NEW: &str = "new";

    fn new(level: WorryCount) -> Self {
        Worry { level }
    }

    /// Recover to a third of current level
    fn recover(&mut self) {
        self.level /= 3;
    }
}

#[derive(Debug)]
struct Monkey {
    /// Lists 'worry level' for each item the monkey is holding
    items: VecDeque<Worry>,
    /// 'Worry level' change when monkey inspects item
    inspection: SymbolTree,
    /// Describes how to monkey uses 'worry level' to handle
    test: SymbolTree,
    throw_true: usize,
    throw_false: usize,
}

impl Monkey {
    fn inspect(&mut self, vars: &mut Vars) -> Worry {
        let mut worry = self.items.pop_front().unwrap();
        vars.set(Worry::OLD, worry.level);
        let new_level = self.inspection.eval(vars);
        worry.level = new_level;
        worry
    }

    fn test(&self, _worry: &Worry, vars: &mut Vars) -> bool {
        match self.test.eval(vars) {
            0 => false,
            1 => true,
            _ => panic!("Wrong eval in test"),
        }
    }
}

#[derive(Debug, Clone)]
struct SymbolTree {
    symbols: Vec<Symbol>,
}

impl SymbolTree {
    fn eval(&self, vars: &mut Vars) -> WorryCount {
        let is_assign = self.symbols[1] == Symbol::ASSIGN || self.symbols[1] == Symbol::CHECK;
        let mut temp = if is_assign {
            &self.symbols[2..]
        } else {
            &self.symbols[..]
        }
        .iter()
        .cloned()
        .collect::<VecDeque<_>>();

        while temp.len() > 1 {
            let left = match temp.pop_front().unwrap() {
                Symbol::Val(val) => val,
                Symbol::Op(_) => panic!("Invalid execution order"),
            };
            let op = match temp.pop_front().unwrap() {
                Symbol::Val(_) => panic!("Invalid execution order"),
                Symbol::Op(op) => op,
            };
            let right = match temp.pop_front().unwrap() {
                Symbol::Val(val) => val,
                Symbol::Op(_) => panic!("Invalid execution order"),
            };

            let new_value = op.apply(left, right, vars);
            temp.insert(0, Symbol::Val(new_value));
        }

        if is_assign {
            let assign_into = match self.symbols[0].clone() {
                Symbol::Val(val) => val,
                Symbol::Op(_) => panic!("Invalid execution order"),
            };
            let eq_op = match self.symbols[1].clone() {
                Symbol::Val(_) => panic!("Invalid execution order"),
                Symbol::Op(op) => op,
            };
            let assign_value = match temp.pop_front().unwrap() {
                Symbol::Val(val) => val,
                Symbol::Op(_) => panic!("Invalid execution order"),
            };

            let res = eq_op.apply(assign_into, assign_value, vars);
            temp.push_front(Symbol::Val(res));
        }

        let result = match temp.pop_front().unwrap() {
            Symbol::Val(val) => val,
            Symbol::Op(_) => panic!("Invalid execution order"),
        };
        result.eval(vars)
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Symbol {
    Val(Value),
    Op(Operand),
}

impl Symbol {
    const ASSIGN: Self = Self::Op(Operand::Eq);
    const CHECK: Self = Self::Op(Operand::EqEq);
}

struct Vars {
    vars: Vec<(String, WorryCount)>,
}

impl Vars {
    fn new() -> Self {
        Self {
            vars: vec![("old".to_owned(), 0), ("new".to_owned(), 0)],
        }
    }

    fn set(&mut self, name: &str, value: WorryCount) {
        self.vars
            .iter_mut()
            .find(|(n, _v)| n == name)
            .unwrap_or_else(|| panic!("Variable named '{name}' does not exist"))
            .1 = value;
    }

    fn get(&self, name: &str) -> WorryCount {
        self.vars
            .iter()
            .find(|(n, _v)| n == name)
            .unwrap_or_else(|| panic!("Variable named '{name}' does not exist"))
            .1
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Operand {
    Eq,
    EqEq,
    Add,
    Mul,
    Div,
    Sub,
    Mod,
}

impl Operand {
    fn prio(&self) -> usize {
        match self {
            Operand::Eq => 0,
            Operand::EqEq => 0,
            Operand::Add => 1,
            Operand::Mul => 1,
            Operand::Div => 2,
            Operand::Sub => 2,
            Operand::Mod => 1,
        }
    }

    fn apply(&self, mut left: Value, right: Value, vars: &mut Vars) -> Value {
        let r = match right {
            Value::Var(ref name) => vars.get(name),
            Value::Literal(ref val) => *val,
        };
        let l = match left {
            Value::Var(ref name) => vars.get(name),
            Value::Literal(ref val) => *val,
        };

        match self {
            Operand::Eq => {
                match left {
                    Value::Var(ref name) => {
                        vars.set(name, r);
                    }
                    Value::Literal(val) => {
                        left = Value::Literal(val);
                    }
                };
                left
            }
            Operand::EqEq => {
                let value = u64::from(left.eval(vars) == right.eval(vars));
                Value::Literal(value)
            }
            Operand::Add => Value::Literal(l + r),
            Operand::Mul => Value::Literal(l * r),
            Operand::Div => Value::Literal(l / r),
            Operand::Sub => Value::Literal(l - r),
            Operand::Mod => Value::Literal(l % r),
        }
    }
}
impl PartialOrd for Operand {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.prio().cmp(&other.prio()))
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Var(String),
    Literal(WorryCount),
}

impl Value {
    fn eval(&self, vars: &Vars) -> WorryCount {
        match self {
            Value::Var(name) => vars.get(name),
            Value::Literal(val) => *val,
        }
    }
}

fn read_operation(operation: &str) -> SymbolTree {
    let mut symbols = Vec::new();

    for s in operation.split_whitespace() {
        let symbol = match s.trim() {
            "+" => Symbol::Op(Operand::Add),
            "-" => Symbol::Op(Operand::Sub),
            "*" => Symbol::Op(Operand::Mul),
            "/" => Symbol::Op(Operand::Div),
            "=" => Symbol::Op(Operand::Eq),
            s => {
                let val = if let Ok(v) = s.parse::<WorryCount>() {
                    Value::Literal(v)
                } else {
                    Value::Var(s.to_owned())
                };
                Symbol::Val(val)
            }
        };
        symbols.push(symbol);
    }
    SymbolTree { symbols }
}

fn read_test(test: &str) -> SymbolTree {
    const DIV_BY: &str = "divisible by";
    let mut symbols = vec![
        Symbol::Val(Value::Literal(0)),
        Symbol::Op(Operand::EqEq),
        Symbol::Val(Value::Var(Worry::OLD.to_owned())),
    ];

    let mut working = test.trim_start();
    while !working.is_empty() {
        let (symbol, rest) = match working {
            _mod if working.starts_with(DIV_BY) => {
                (Symbol::Op(Operand::Mod), &working[DIV_BY.len()..])
            }
            _ => {
                let symbol = if let Ok(val) = working.parse::<WorryCount>() {
                    Symbol::Val(Value::Literal(val))
                } else {
                    Symbol::Val(Value::Var(working.to_owned()))
                };
                symbols.push(symbol);
                break;
            }
        };
        symbols.push(symbol);
        working = rest.trim_start();
    }
    SymbolTree { symbols }
}

fn read_monkeys(input: &str) -> Vec<Monkey> {
    fn ac(str: &str) -> &str {
        str.split_once(':').unwrap().1
    }

    let mut lines = input.lines().collect::<Vec<_>>();
    let mut monkeys = Vec::new();

    while !lines.is_empty() {
        let six = &lines[..6];
        let _monkey_name = &six[0];

        let items = ac(six[1]);
        let items = items
            .split(',')
            .map(|item| item.trim().parse::<WorryCount>().unwrap())
            .map(Worry::new)
            .collect();

        let op = read_operation(ac(six[2]));
        let test = read_test(ac(six[3]));

        let on_true = ac(six[4]).split_whitespace().last().unwrap();
        let on_true = on_true
            .parse::<usize>()
            .expect("Test doesnt end with expected value");

        let on_false = ac(six[5]).split_whitespace().last().unwrap();
        let on_false = on_false
            .parse::<usize>()
            .expect("Test doesnt end with expected value");

        monkeys.push(Monkey {
            items,
            inspection: op,
            test,
            throw_true: on_true,
            throw_false: on_false,
        });
        if lines.len() > 6 {
            lines = lines[7..].to_vec();
        } else {
            break;
        }
    }

    monkeys
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST: &str = "\
Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1";

    #[test]
    pub fn test_part1() {
        let monkey_business = calculate_monkey_business(TEST, 20, true);

        assert_eq!(10605, monkey_business);
    }

    #[test]
    pub fn test_part2() {
        let monkey_business = calculate_monkey_business(TEST, 10000, false);

        assert_eq!(2713310158, monkey_business);
    }
}
