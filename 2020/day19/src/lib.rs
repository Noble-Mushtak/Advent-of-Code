use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::hash::Hash;

#[derive(PartialEq, Debug)]
struct MultiMap<K: Eq + Hash, V: Eq + Hash>(HashMap<K, HashSet<V>>);

impl<K: Eq + Hash, V: Eq + Hash> MultiMap<K, V> {
    fn new() -> MultiMap<K, V> {
        MultiMap(HashMap::new())
    }

    fn get(&self, key: &K) -> Option<&HashSet<V>> {
        self.0.get(key)
    }

    fn insert(&mut self, key: K, value: V) {
        match self.0.get_mut(&key) {
            None => {
                let mut new_val = HashSet::new();
                new_val.insert(value);
                self.0.insert(key, new_val);
            }
            Some(set) => {
                set.insert(value);
            }
        }
    }

    fn pop(&mut self) -> Option<(&K, V)>
    where
        V: Clone,
    {
        self.0
            .iter_mut()
            .find_map(|(key, set)| match set.iter().next() {
                Some(val) => {
                    let ret_val = val.clone();
                    set.remove(&ret_val);
                    Some((key, ret_val))
                }
                None => None,
            })
    }
}

#[derive(PartialEq, Debug, Clone, Eq, Hash)]
enum Product {
    Symbol(char),
    Variable(usize),
}

#[derive(PartialEq, Debug, Clone, Eq, Hash)]
struct Products(Vec<Product>);

#[derive(PartialEq, Debug)]
struct Grammar {
    start_var: usize,
    rules: HashMap<usize, Vec<Products>>,
}

peg::parser! {
    grammar parser() for str {
        rule usize() -> usize
          = n:$(['0'..='9']+) {
            n.parse().unwrap()
        }

        rule word() -> String
          = str:$(['a'..='b']*) { str.to_string() }

        rule product() -> Product
          = num:usize() { Product::Variable(num) } /
            "\"" word:$(['a' | 'b']) "\"" { Product::Symbol(word.chars().next().unwrap()) }

        rule products() -> Products
          = prods:(product()**" ") { Products(prods) }

        rule rule_line() -> (usize, Vec<Products>)
          = start_var:usize() ": " prodss:(products()**" | ") { (start_var, prodss) }

        rule grammar() -> Grammar
          = rules:(rule_line()**"\n") {
            Grammar {
                start_var: 0,
                rules: rules.into_iter().collect(),
            }
        }

        pub(crate) rule parse() -> (Grammar, Vec<String>)
          = grammar:(grammar()) "\n\n" data:(word()**"\n") { (grammar, data) }
    }
}

#[derive(PartialEq, Debug, Clone, Eq, Hash)]
struct ProductionState<'a> {
    products: &'a Products,
    variable: usize,
    cur_loc: usize,
    origin: usize,
}

impl<'a> ProductionState<'a> {
    fn new(prods: &'a Products, variable: usize, origin: usize) -> ProductionState<'a> {
        ProductionState {
            products: prods,
            variable,
            cur_loc: 0,
            origin,
        }
    }

    fn next_product(&self) -> Option<Product> {
        self.products.0.get(self.cur_loc).cloned()
    }

    fn advance_step(&self) -> ProductionState<'a> {
        ProductionState {
            products: self.products,
            variable: self.variable,
            cur_loc: self.cur_loc + 1,
            origin: self.origin,
        }
    }
}

#[derive(PartialEq, Debug)]
struct EarleySet<'a>(MultiMap<Option<Product>, ProductionState<'a>>);

impl<'a> EarleySet<'a> {
    fn insert(&mut self, prod_state: ProductionState<'a>) {
        self.0.insert(prod_state.next_product(), prod_state);
    }

    fn contains(&self, prod_state: &ProductionState<'a>) -> bool {
        match self.0.get(&prod_state.next_product()) {
            Some(set) => set.contains(prod_state),
            None => false,
        }
    }
}

impl Grammar {
    fn add_rule(&mut self, variable: usize, prods: Products) {
        match self.rules.get_mut(&variable) {
            Some(vec) => vec.push(prods),
            None => {
                self.rules.insert(variable, vec![prods]);
            }
        }
    }

    fn add_var_rules_to_earley_set<'a>(
        &'a self,
        rule_set: &mut EarleySet<'a>,
        variable: usize,
        origin: usize,
        check_before_insert: &EarleySet,
    ) {
        if let Some(vec) = self.rules.get(&variable) {
            for prods in vec {
                let new_state = ProductionState::new(prods, variable, origin);
                if !check_before_insert.contains(&new_state) {
                    rule_set.insert(new_state);
                }
            }
        }
    }

    fn parse(&self, input: &str) -> bool {
        let input_chars = input.chars().collect::<Vec<_>>();

        let mut earley_sets = Vec::with_capacity(input.len() + 1);
        for _ in 0..input.len() + 1 {
            earley_sets.push(EarleySet(MultiMap::new()));
        }

        self.add_var_rules_to_earley_set(&mut earley_sets[0], 0, 0, &EarleySet(MultiMap::new()));

        for i in 0..input.len() + 1 {
            let mut not_visited = EarleySet(MultiMap::new());
            std::mem::swap(&mut not_visited, &mut earley_sets[i]);

            while let Some((next_product, prod_state)) = not_visited.0.pop() {
                use Product::*;
                match *next_product {
                    Some(Symbol(ch)) => {
                        if input_chars.get(i) == Some(&ch) {
                            earley_sets[i + 1].insert(prod_state.advance_step());
                        }
                    }
                    Some(Variable(variable)) => self.add_var_rules_to_earley_set(
                        &mut not_visited,
                        variable,
                        i,
                        &earley_sets[i],
                    ),
                    None => {
                        if prod_state.variable == self.start_var
                            && prod_state.origin == 0
                            && i == input.len()
                        {
                            return true;
                        }

                        if let Some(vec) = earley_sets[prod_state.origin]
                            .0
                            .get(&Some(Variable(prod_state.variable)))
                        {
                            for prod_state2 in vec {
                                let new_state = prod_state2.advance_step();
                                if !earley_sets[i].contains(&new_state) {
                                    not_visited.insert(new_state);
                                }
                            }
                        }
                    }
                }
                earley_sets[i].insert(prod_state);
            }
        }

        false
    }
}

fn calc_answers(grammar: &mut Grammar, data: &[String]) -> (usize, usize) {
    let answer1 = data.iter().filter(|word| grammar.parse(word)).count();

    use Product::*;
    grammar.add_rule(8, Products(vec![Variable(42), Variable(8)]));
    grammar.add_rule(11, Products(vec![Variable(42), Variable(11), Variable(31)]));
    let answer2 = data.iter().filter(|word| grammar.parse(word)).count();

    (answer1, answer2)
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let (mut grammar, data) = parser::parse(&fs::read_to_string("in.txt")?[..])?;

    let (answer1, answer2) = calc_answers(&mut grammar, &data[..]);
    println!("Part 1: {}", answer1);
    println!("Part 2: {}", answer2);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn example_grammar() -> Grammar {
        use Product::*;
        Grammar {
            start_var: 0,
            rules: [
                (
                    0,
                    vec![Products(vec![Variable(4), Variable(1), Variable(5)])],
                ),
                (
                    1,
                    vec![
                        Products(vec![Variable(2), Variable(3)]),
                        Products(vec![Variable(3), Variable(2)]),
                    ],
                ),
                (
                    2,
                    vec![
                        Products(vec![Variable(4), Variable(4)]),
                        Products(vec![Variable(5), Variable(5)]),
                    ],
                ),
                (
                    3,
                    vec![
                        Products(vec![Variable(4), Variable(5)]),
                        Products(vec![Variable(5), Variable(4)]),
                    ],
                ),
                (4, vec![Products(vec![Symbol('a')])]),
                (5, vec![Products(vec![Symbol('b')])]),
            ]
            .iter()
            .cloned()
            .collect(),
        }
    }

    fn example_data() -> (Grammar, Vec<String>) {
        (
            example_grammar(),
            vec![
                "ababbb".to_string(),
                "bababa".to_string(),
                "abbbab".to_string(),
                "aaabbb".to_string(),
                "aaaabbb".to_string(),
            ],
        )
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            parser::parse(
                "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\"

ababbb
bababa
abbbab
aaabbb
aaaabbb"
            ),
            Ok(example_data())
        );
    }

    #[test]
    fn test_multimap() {
        let mut test = MultiMap::new();
        assert_eq!(test.get(&1), None);
        test.insert(1, 2);
        assert_eq!(test.get(&1), Some(&[2].iter().copied().collect()));
        test.insert(1, 2);
        assert_eq!(test.get(&1), Some(&[2].iter().copied().collect()));
        test.insert(1, 3);
        assert_eq!(test.get(&1), Some(&[2, 3].iter().copied().collect()));
    }

    #[test]
    fn test_earley_parser() {
        let gram = example_grammar();
        assert_eq!(gram.parse("ababbb"), true);
        assert_eq!(gram.parse("abbbab"), true);
        assert_eq!(gram.parse("bababa"), false);
        assert_eq!(gram.parse("aaabbb"), false);
        assert_eq!(gram.parse("aaaabbb"), false);
    }

    #[test]
    fn test_integration() {
        let (mut gram, data) = parser::parse(
            "42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: \"a\"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: \"b\"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba",
        )
        .unwrap();
        assert_eq!(calc_answers(&mut gram, &data[..]), (3, 12));
    }
}
