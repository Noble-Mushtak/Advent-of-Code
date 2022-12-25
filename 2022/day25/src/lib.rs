use std::collections::VecDeque;
use std::error::Error;
use std::fs;

peg::parser! {
    grammar parser() for str {
        rule digit() -> isize
          = c:($(['=' | '-' | '0' | '1' | '2'])) {
          match c {
              "=" => -2,
              "-" => -1,
              "0" => 0,
              "1" => 1,
              _   => 2
          }
        }

        rule number() -> isize
          = l:(digit()+) {
          l.into_iter().fold(0, |res, d| 5*res+d)
        }

        pub(crate) rule parse() -> Vec<isize>
          = l:(number()**"\n") "\n"? { l }
    }
}

fn to_snafu(n: isize) -> String {
    let n_neg = n < 0;
    let mut abs_val = (if n_neg { -n } else { n }) as usize;
    let mut digs = VecDeque::new();
    while abs_val > 0 {
        digs.push_front((abs_val % 5) as isize);
        abs_val /= 5;
    }
    for idx in (0..digs.len()).rev() {
        if digs[idx] > 2 {
            digs[idx] -= 5;
            if idx > 0 {
                digs[idx-1] += 1;
            } else {
                digs.push_front(1);
            }
        }
    }
    if n_neg {
        for d in digs.iter_mut() {
            *d *= -1;
        }
    }
    digs.into_iter().map(|d| match d {
                             -2 => '=',
                             -1 => '-',
                             0  => '0',
                             1  => '1',
                             2  => '2',
                             _  => unreachable!(),
                         })
                    .collect()
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let nums = parser::parse(&fs::read_to_string("in.txt")?)?;
    
    println!("Part 1: {}", to_snafu(nums.iter().sum::<isize>()));
    
    Ok(())
}