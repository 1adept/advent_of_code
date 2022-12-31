use std::{
    collections::{BTreeMap, VecDeque},
    fmt::Display,
    str::FromStr,
};

pub fn tasks() {
    let input = &super::load_input(21);
    let mt = parse_monkeys(input);

    let result = mt.call();
    println!("Root will call: {result}");

    let mut mt = mt;
    mt.monkey.change_task_operation(Operator::Equals);
    let humn_call = mt.ask_value_of("humn");
    println!("To equal the other root value humn (human) has to call: {humn_call}");
}

fn parse_monkeys(input: &str) -> MonkeyTree {
    use regex::Regex;
    let (pattern_name, pattern_number) = {
        let monkey_name = r"([a-z]{4})";
        let pattern_name = Regex::new(&format!(
            r"{monkey_name}: {monkey_name} ([+*/-]) {monkey_name}"
        ))
        .unwrap();
        let pattern_number = Regex::new(&format!("{monkey_name}: (\\d+)")).unwrap();
        (pattern_name, pattern_number)
    };

    let mut monkeys = BTreeMap::new();
    for line in input.lines() {
        if let Some(captures) = pattern_name.captures(line) {
            let name = captures.get(1).unwrap().as_str().to_owned();
            monkeys.insert(
                name.clone(),
                Monkey {
                    name,
                    task: Task::Call(
                        captures.get(2).unwrap().as_str().to_owned(),
                        Operator::from_str(captures.get(3).unwrap().as_str())
                            .ok()
                            .unwrap(),
                        captures.get(4).unwrap().as_str().to_owned(),
                    ),
                },
            );
        } else if let Some(captures) = pattern_number.captures(line) {
            let name = captures.get(1).unwrap().as_str().to_owned();
            monkeys.insert(
                name.clone(),
                Monkey {
                    name,
                    task: Task::Number(captures.get(2).unwrap().as_str().parse::<_>().unwrap()),
                },
            );
        } else {
            panic!("Invalid pattern for {line}");
        }
    }

    let root_monkey = monkeys.remove("root").unwrap();

    let mut root = MonkeyTree::new(root_monkey);
    //Todo contains empty (call-monkey) nodes which have to be done next
    let mut todo_branches = Vec::new();
    todo_branches.push(&mut root);

    while let Some(branch) = todo_branches.pop() {
        if let Task::Call(left, _, right) = &branch.monkey.task {
            let left_monkey = monkeys.remove(left).unwrap();
            let right_monkey = monkeys.remove(right).unwrap();

            branch.left_branch = Some(Box::new(MonkeyTree::new(left_monkey)));
            branch.right_branch = Some(Box::new(MonkeyTree::new(right_monkey)));

            let left = branch.left_branch.as_mut().unwrap().as_mut();
            let right = branch.right_branch.as_mut().unwrap().as_mut();

            // Add to `todo` to (maybe) add more branches
            todo_branches.extend([left, right]);
        } else {
            // Number already added to tree and removed from 'monkeys'
        }
    }

    assert!(monkeys.is_empty());
    root
}

#[derive(Debug)]
struct MonkeyTree {
    monkey: Monkey,
    left_branch: Option<Box<MonkeyTree>>,
    right_branch: Option<Box<MonkeyTree>>,
}

impl MonkeyTree {
    fn new(root: Monkey) -> Self {
        Self {
            monkey: root,
            left_branch: None,
            right_branch: None,
        }
    }

    fn ask_value_of(&self, name: &str) -> MonkeyNumber {
        let is_humn = move |m: &Monkey| m.name == name;
        let mm = self.call_rev(&Box::new(is_humn));

        let should_equal = match mm {
            MonkeyMath::Number(n) => n,
            MonkeyMath::Formula(ref f) => match f.pairs.front() {
                Some(formular_type) => match formular_type {
                    FormulaType::Post(Some(n), _) => *n,
                    FormulaType::Pre(_, Some(n)) => *n,
                    _ => todo!(),
                },
                None => todo!(),
            },
        };

        match mm {
            MonkeyMath::Number(n) => n,
            MonkeyMath::Formula(f) => {
                let mut acc = should_equal;
                //Skip equals and reverse Operations to backtrack from root to humn
                for f in f.pairs.iter().skip(1).map(FormulaType::reverse) {
                    acc = f.apply(acc);
                }
                acc
            }
        }
    }

    fn call_rev<P>(&self, call_upwards: &P) -> MonkeyMath
    where
        P: Fn(&Monkey) -> bool,
    {
        if call_upwards(&self.monkey) {
            let formular = Formula {
                pairs: VecDeque::from(vec![FormulaType::Base]),
            };
            return MonkeyMath::Formula(formular);
        }

        match &self.monkey.task {
            Task::Call(_, op, _) => {
                let math1 = self.left_branch.as_ref().unwrap().call_rev(call_upwards);
                let math2 = self.right_branch.as_ref().unwrap().call_rev(call_upwards);

                MonkeyMath::apply(op, math1, math2)
            }
            Task::Number(n) => MonkeyMath::Number(*n),
        }
    }

    fn call(&self) -> MonkeyNumber {
        match &self.monkey.task {
            Task::Number(num) => *num,
            Task::Call(_, op, _) => {
                let left = self.left_branch.as_ref().unwrap().call();
                let right = self.right_branch.as_ref().unwrap().call();
                op.apply(left, right)
            }
        }
    }
}

#[derive(Debug)]
enum MonkeyMath {
    Number(MonkeyNumber),
    Formula(Formula),
}

#[derive(Debug)]
struct Formula {
    pairs: VecDeque<FormulaType>,
}

#[derive(Debug)]
enum FormulaType {
    Post(Option<MonkeyNumber>, Operator),
    Pre(Operator, Option<MonkeyNumber>),
    Base,
}

impl Display for FormulaType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            FormulaType::Post(v, op) => format!("{:?} {:?}", v, op),
            FormulaType::Pre(op, v) => format!("{:?} {:?}", op, v),
            FormulaType::Base => "base".to_owned(),
        };
        write!(f, "{str}")
    }
}

impl FormulaType {
    fn reverse(&self) -> Self {
        match self {
            FormulaType::Post(n, op) => match op {
                Operator::Add => FormulaType::Pre(Operator::Sub, *n),
                Operator::Mult => FormulaType::Pre(Operator::Div, *n),
                // Sub stays because => 50 - 20 = 30 ... 50 - 30 = 20
                // Div stays because => 50 / 20 = 2.5 ... 50 / 2.5 = 20
                _ => FormulaType::Post(*n, op.clone()),
            },
            FormulaType::Pre(op, n) => FormulaType::Pre(op.reverse(), *n),
            FormulaType::Base => FormulaType::Base,
        }
    }

    fn apply(&self, value: MonkeyNumber) -> MonkeyNumber {
        match self {
            FormulaType::Post(Some(n), op) => op.apply(*n, value),
            FormulaType::Pre(op, Some(n)) => op.apply(value, *n),
            _ => value,
        }
    }
}

impl MonkeyMath {
    fn apply(operator: &Operator, left: MonkeyMath, right: MonkeyMath) -> MonkeyMath {
        match (left, right) {
            (MonkeyMath::Number(n1), MonkeyMath::Number(n2)) => {
                MonkeyMath::Number(operator.apply(n1, n2))
            }
            (MonkeyMath::Number(n), MonkeyMath::Formula(mut f)) => {
                f.pairs
                    .push_front(FormulaType::Post(Some(n), operator.clone()));
                MonkeyMath::Formula(f)
            }
            (MonkeyMath::Formula(mut f), MonkeyMath::Number(n)) => {
                f.pairs
                    .push_front(FormulaType::Pre(operator.clone(), Some(n)));
                MonkeyMath::Formula(f)
            }
            _ => unreachable!("Cant reach"),
        }
    }
}

type MonkeyNumber = i128;
#[derive(Debug)]
struct Monkey {
    name: String,
    task: Task,
}

impl Monkey {
    fn change_task_operation(&mut self, task_op: Operator) {
        if let Task::Call(_, op, _) = &mut self.task {
            *op = task_op;
        }
    }
}

#[derive(Debug, Clone)]
enum Operator {
    Add,
    Sub,
    Mult,
    Div,
    Equals,
}

impl Operator {
    fn apply(&self, left: MonkeyNumber, right: MonkeyNumber) -> MonkeyNumber {
        match self {
            Operator::Add => left + right,
            Operator::Sub => left - right,
            Operator::Mult => left * right,
            Operator::Div => left / right,
            Operator::Equals => i128::from(left != right),
        }
    }

    fn reverse(&self) -> Self {
        match self {
            Operator::Add => Self::Sub,
            Operator::Sub => Self::Add,
            Operator::Mult => Self::Div,
            Operator::Div => Self::Mult,
            Operator::Equals => Self::Equals,
        }
    }
}

impl FromStr for Operator {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "+" => Ok(Operator::Add),
            "-" => Ok(Operator::Sub),
            "*" => Ok(Operator::Mult),
            "/" => Ok(Operator::Div),
            "=" => Ok(Operator::Equals),
            _ => anyhow::bail!("Cannot parse to Operator"),
        }
    }
}

#[derive(Debug)]
enum Task {
    Number(MonkeyNumber),
    Call(String, Operator, String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_day21_part1() {
        let monkey_tree = parse_monkeys(TEST);

        assert_eq!(152, monkey_tree.call());
    }

    #[test]
    fn test_day21_part2() {
        let mut monkey_tree = parse_monkeys(TEST);
        monkey_tree.monkey.change_task_operation(Operator::Equals);

        assert_eq!(301, monkey_tree.ask_value_of("humn"));
    }

    const TEST: &str = "\
root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32";
}
