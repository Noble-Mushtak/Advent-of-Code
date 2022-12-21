use std::collections::{HashMap};
use std::error::Error;
use std::fmt::Debug;
use std::fs;
use std::ops::{Add, Sub, Mul, Div};
use num256::int256::Int256;
use num_bigint::{ToBigInt};

#[derive(Clone, Debug)]
enum Rule<T: Debug> {
    ValueR(T),
    AddR(String, String),
    SubR(String, String),
    MulR(String, String),
    DivR(String, String)
}
use crate::Rule::{ValueR, AddR, SubR, MulR, DivR};

peg::parser! {
    grammar parser() for str {
        rule i128() -> i128 = precedence! {
          n:$(['0'..='9']+) { n.parse().unwrap() }
          "-" n:$(['0'..='9']+) { -(n.parse::<i128>().unwrap()) }
        }

        rule one_rule() -> Rule<i128> = precedence! {
          name1:$(['a'..='z']+) " + " name2:$(['a'..='z']+) { AddR(name1.to_string(), name2.to_string()) }
          name1:$(['a'..='z']+) " - " name2:$(['a'..='z']+) { SubR(name1.to_string(), name2.to_string()) }
          name1:$(['a'..='z']+) " * " name2:$(['a'..='z']+) { MulR(name1.to_string(), name2.to_string()) }
          name1:$(['a'..='z']+) " / " name2:$(['a'..='z']+) { DivR(name1.to_string(), name2.to_string()) }
          num:i128() { ValueR(num) }
        }

        rule monkey() -> (String, Rule<i128>) =
          name:$(['a'..='z']+) ": " r:one_rule() { (name.to_string(), r) }

        pub(crate) rule parse() -> HashMap<String, Rule<i128>> =
          l:(monkey()**"\n") "\n"? { l.into_iter().collect() }
    }
}

fn calc_val_dfs<T: Add<Output=T> + Sub<Output=T> + Mul<Output=T> + Div<Output=T> + Clone + Debug>(rules: &HashMap<String, Rule<T>>, memo: &mut HashMap<String, T>, root_name: &str) -> T {
    match memo.get(root_name) {
        Some(val) => val.clone(),
        None => {
            let ans = match rules.get(root_name).unwrap() {
                ValueR(v) => v.clone(),
                AddR(child1, child2) => calc_val_dfs(rules, memo, child1)+calc_val_dfs(rules, memo, child2),
                SubR(child1, child2) => calc_val_dfs(rules, memo, child1)-calc_val_dfs(rules, memo, child2),
                MulR(child1, child2) => calc_val_dfs(rules, memo, child1)*calc_val_dfs(rules, memo, child2),
                DivR(child1, child2) => calc_val_dfs(rules, memo, child1)/calc_val_dfs(rules, memo, child2),
            };
            memo.insert(root_name.to_string(), ans.clone());
            ans
        }
    }
}

fn calc_val<T: Add<Output=T> + Sub<Output=T> + Mul<Output=T> + Div<Output=T> + Clone + Debug>(rules: &HashMap<String, Rule<T>>, root_name: &str) -> T {
    let mut memo: HashMap<String, T> = HashMap::new();
    calc_val_dfs(rules, &mut memo, root_name)
}

#[derive(Clone, Debug)]
struct LinearFunc {
    x_coeff: i128,
    const_term: i128,
    denom: i128
}

impl Add for LinearFunc {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        LinearFunc {
            x_coeff: self.x_coeff*other.denom+other.x_coeff*self.denom,
            const_term: self.const_term*other.denom+other.const_term*self.denom,
            denom: self.denom*other.denom
        }
    }
}

impl Sub for LinearFunc {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        LinearFunc {
            x_coeff: self.x_coeff*other.denom-other.x_coeff*self.denom,
            const_term: self.const_term*other.denom-other.const_term*self.denom,
            denom: self.denom*other.denom
        }
    }
}

impl Mul for LinearFunc {
    type Output = Self;

    //ASSUME: Either self.x_coeff is 0 or other.x_coeff is 0
    fn mul(self, other: Self) -> Self {
        LinearFunc {
            x_coeff: self.x_coeff*other.const_term+other.x_coeff*self.const_term,
            const_term: self.const_term*other.const_term,
            denom: self.denom*other.denom
        }
    }
}

impl Div for LinearFunc {
    type Output = Self;

    //ASSUME: other.x_coeff is 0
    fn div(self, other: Self) -> Self {
        LinearFunc {
            x_coeff: self.x_coeff*other.denom,
            const_term: self.const_term*other.denom,
            denom: self.denom*other.const_term
        }
    }
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let rules = parser::parse(&fs::read_to_string("in.txt")?)?;
    println!("Part 1: {}", calc_val(&rules, "root"));
    
    let new_rules: HashMap<String, Rule<LinearFunc>> = rules.iter()
    .map(|(name, r)| {
        if name == "humn" {
            (name.clone(), ValueR(LinearFunc { x_coeff: 1, const_term: 0, denom: 1 }))
        } else {
            let new_r = match r {
                ValueR(v) => ValueR(LinearFunc { x_coeff: 0, const_term: *v, denom: 1 }),
                AddR(n1, n2) => AddR(n1.to_string(), n2.to_string()),
                SubR(n1, n2) => SubR(n1.to_string(), n2.to_string()),
                MulR(n1, n2) => MulR(n1.to_string(), n2.to_string()),
                DivR(n1, n2) => DivR(n1.to_string(), n2.to_string()),
            };
            (name.clone(), new_r)
        }
    })
    .collect();
    let (child1, child2) = match rules.get("root") {
        None => unreachable!(),
        Some(ValueR(v)) => unreachable!(),
        Some(AddR(n1, n2)) => (n1, n2),
        Some(SubR(n1, n2)) => (n1, n2),
        Some(MulR(n1, n2)) => (n1, n2),
        Some(DivR(n1, n2)) => (n1, n2),
    };
    let mut new_memo: HashMap<String, LinearFunc> = HashMap::new();
    let orig_val1 = calc_val_dfs(&new_rules, &mut new_memo, child1);
    let orig_val2 = calc_val_dfs(&new_rules, &mut new_memo, child2);
    
    //Either orig_val1.x_coeff is 0 or orig_val2.x_coeff is 0
    let (val1, val2) = if orig_val1.x_coeff == 0 {
        (orig_val2, orig_val1)
    } else {
        (orig_val1, orig_val2)
    };
    //Now, val2.x_coeff is 0
    
    let x1 = Int256(val1.x_coeff.to_bigint().unwrap());
    let c1 = Int256(val1.const_term.to_bigint().unwrap());
    let d1 = Int256(val1.denom.to_bigint().unwrap());
    let c2 = Int256(val2.const_term.to_bigint().unwrap());
    let d2 = Int256(val2.denom.to_bigint().unwrap());
    // (x1 * x + c1)/d1 = c2/d2
    // -> x1*d2 * x + c1*d2 = c2*d1
    // -> x = (c2*d1-c1*d2)/(x1*d2)
    println!("Part 2: {:?}", ((c2.clone()*d1.clone()-c1.clone()*d2.clone())/(x1.clone()*d2.clone())).0);
    
    Ok(())
}