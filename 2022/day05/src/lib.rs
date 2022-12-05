use std::collections::VecDeque;
use std::error::Error;
use std::fs;

#[derive(PartialEq, Clone, Debug)]
struct Move {
    num_to_move: usize,
    from_idx: usize,
    to_idx: usize
}

peg::parser! {
    grammar parser() for str {
        rule usize() -> usize
          = n:$(['0'..='9']+) {
            n.parse().unwrap()
        }

        rule crate_() -> Option<char> = precedence!{
          "[" c:$(['A'..='Z']) "]" { c.parse().ok() }
          "   " { None }
        }

        rule crate_row() -> Vec<Option<char>>
          = l:(crate_()**" ") { l }

        rule crate_rows() -> Vec<Vec<char>>
          = rs:(crate_row()**"\n") {
          let mut res = vec![vec![]; rs[0].len()];
          for row in rs.iter().rev() {
              for (i, ch_opt) in row.iter().enumerate() {
                  if let Some(ch) = ch_opt {
                      res[i].push(*ch);
                  }
              }
          }
          res
        }

        rule ignore_line() -> ()
          = $(['1'..='9' | ' ']+) "\n" { () }

        rule move() -> Move
          = "move " n1:usize() " from " n2:usize() " to " n3:usize() {
            Move {
                num_to_move: n1,
                from_idx: n2-1,
                to_idx: n3-1
            }
          }

        rule moves() -> Vec<Move>
          = l:(move()**"\n") { l }

        pub(crate) rule parse() -> (Vec<Vec<char>>, Vec<Move>)
          = c:crate_rows() ignore_line() "\n" m:moves() "\n"? { (c, m) }
    }
}

trait Container<T: Clone> {
    fn new(elems: &[T]) -> Self;
    fn push(&mut self, elem: T);
    fn pop(&mut self) -> Option<T>;
}

fn simulate_part_1(stacks: &[Vec<char>], moves: &[Move]) -> Vec<VecDeque<char>> {
    let mut cur_stacks = vec![VecDeque::new(); stacks.len()];
    for (i, stack) in stacks.iter().enumerate() {
        for elem in stack {
            cur_stacks[i].push_back(*elem);
        }
    }
    for Move { num_to_move, from_idx, to_idx } in moves {
        for _ in 0..*num_to_move {
            let elem = cur_stacks[*from_idx].pop_back().unwrap();
            cur_stacks[*to_idx].push_back(elem);
        }
    }
    cur_stacks
}

fn simulate_part_2(stacks: &[Vec<char>], moves: &[Move]) -> Vec<VecDeque<char>> {
    let mut cur_stacks = vec![VecDeque::new(); stacks.len()];
    for (i, stack) in stacks.iter().enumerate() {
        for elem in stack {
            cur_stacks[i].push_back(*elem);
        }
    }
    for Move { num_to_move, from_idx, to_idx } in moves {
        for i in (0..*num_to_move).rev() {
            let elem = cur_stacks[*from_idx][cur_stacks[*from_idx].len()-i-1];
            cur_stacks[*to_idx].push_back(elem);
        }
        for _ in 0..*num_to_move {
            cur_stacks[*from_idx].pop_back();
        }
    }
    cur_stacks
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let (stacks, moves) = parser::parse(&fs::read_to_string("in.txt")?)?;

    println!("Part 1: {}", simulate_part_1(&stacks, &moves).iter().map(|r| r.back().unwrap()).copied().collect::<String>());
    println!("Part 2: {}", simulate_part_2(&stacks, &moves).iter().map(|r| r.back().unwrap()).copied().collect::<String>());
    Ok(())
}