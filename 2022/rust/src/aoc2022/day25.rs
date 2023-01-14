pub fn tasks() {
    let input = super::load_input(25);

    let sum_snafu = solve(&input, parse, |snafus| {
        let sum = snafus.iter().map(|s| s.decimal).sum::<u64>();
        SNAFU::from(sum)
    });
    println!("SNAFU number of total fuel: {}", sum_snafu.snafu);
}

fn solve<P, S, R>(input: &str, parse: P, solver: S) -> R
where
    P: Fn(&str) -> Vec<SNAFU>,
    S: Fn(&[SNAFU]) -> R,
{
    let snafus = parse(input);
    solver(&snafus)
}

// Heat output + flow rate => sum fuel equirements
fn parse(input: &str) -> Vec<SNAFU> {
    input.lines().into_iter().map(SNAFU::from).collect()
}

type SnafNum = u64;

/// Special Numeral Analogue Fuel Units
/// 5-based Numbers, 0-2, minus (`-`) -> -1, double minus (`=`) -> -2
/// e.g. base 10: `10` = base 5: `20` (2 5s, +0 1s)
///      base 10: `08` = base 5: `2=` (2 5s, -2 1s)
#[derive(Debug, PartialEq, Eq)]
#[allow(clippy::upper_case_acronyms)]
struct SNAFU {
    snafu: String,
    decimal: SnafNum,
}

impl From<&str> for SNAFU {
    fn from(value: &str) -> Self {
        fn map_single_snafu((i, char): (usize, char)) -> i64 {
            5i64.pow(i as u32)
                * match char {
                    '=' => -2,
                    '-' => -1,
                    '0' => 0,
                    '1' => 1,
                    '2' => 2,
                    _ => unreachable!("{char} is not a SNAFU-Symbol"),
                }
        }

        let snafu_decimal: i64 = value.chars().rev().enumerate().map(map_single_snafu).sum();
        SNAFU {
            snafu: value.into(),
            decimal: snafu_decimal as SnafNum,
        }
    }
}

impl From<u64> for SNAFU {
    fn from(value: u64) -> Self {
        let mut power = 1;
        let mut work = value;

        let mut snafu = String::new();
        while work > 0 {
            let pow = 5u64.pow(power - 1);
            let rem = work % 5u64.pow(power);

            let str = if rem == 0 {
                '0'
            } else {
                let diff = rem / pow;
                let rel: i64 = {
                    let diff = diff as i64;
                    if diff < 3 {
                        diff
                    } else if diff < 5 {
                        diff - 5
                    } else {
                        rem as i64
                    }
                };
                let pow = pow as i64;
                let minwork = -rel * pow;
                work = (work as i64 + minwork) as u64;
                match rel {
                    0 | 1 => '1',
                    2 => '2',
                    -1 => '-',
                    -2 => '=',
                    _ => panic!("WHAT {rel}"),
                }
            };
            snafu.push(str);

            power += 1;
        }

        SNAFU {
            snafu: snafu.chars().rev().collect(),
            decimal: value,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_day25_parse() {
        let snafu = parse(TEST);

        assert_eq!(4890, snafu.iter().map(|s| s.decimal).sum::<u64>());
    }

    #[test]
    fn test_day25_parse_back() {
        let decimals = parse(TEST);
        decimals
            .into_iter()
            .map(|s| (SNAFU::from(s.decimal), s))
            .for_each(|(s1, s2)| assert_eq!(s1, s2));
    }

    #[test]
    fn test_day25_part1() {
        let result = solve(TEST, parse, |snafus| {
            let sum = snafus.iter().map(|s| s.decimal).sum::<u64>();
            SNAFU::from(sum)
        });
        assert_eq!("2=-1=0", result.snafu);
    }

    const TEST: &str = "\
1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122";
}
