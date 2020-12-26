use std::collections::HashMap;
use std::error::Error;
use std::fs;

#[derive(PartialEq, Debug, Clone)]
enum SingleBitmask {
    NoMask,
    MustBe1,
    MustBe0,
}

#[derive(PartialEq, Debug, Clone)]
struct Bitmask(Vec<SingleBitmask>);

const ALL_1S: usize = 0xFFFFFFFFF;

impl Bitmask {
    fn apply_v1(&self, num: &usize) -> usize {
        self.0
            .iter()
            .rev()
            .fold((*num, 1), |(cur_num, cur_pow2), single_bitmask| {
                use SingleBitmask::*;
                (
                    match single_bitmask {
                        NoMask => cur_num,
                        MustBe1 => cur_num | cur_pow2,
                        MustBe0 => cur_num & (ALL_1S ^ cur_pow2),
                    },
                    cur_pow2 << 1,
                )
            })
            .0
    }

    fn apply_v2(&self, num: &usize) -> Vec<usize> {
        self.0
            .iter()
            .rev()
            .fold((vec![*num], 1), |(cur_nums, cur_pow2), single_bitmask| {
                use SingleBitmask::*;
                (
                    match single_bitmask {
                        NoMask => cur_nums
                            .iter()
                            .map(|&num| num | cur_pow2)
                            .chain(cur_nums.iter().map(|&num| num & (ALL_1S ^ cur_pow2)))
                            .collect(),
                        MustBe1 => cur_nums.into_iter().map(|num| num | cur_pow2).collect(),
                        MustBe0 => cur_nums,
                    },
                    cur_pow2 << 1,
                )
            })
            .0
    }
}

#[derive(PartialEq, Debug)]
enum Instruction {
    SetBitmask(Bitmask),
    SetMemory(usize, usize),
}

#[derive(PartialEq, Debug)]
struct Program(Vec<Instruction>);

peg::parser! {
    grammar parser() for str {
        rule usize() -> usize
          = n:$(['0'..='9']+) {
            n.parse().unwrap()
        }

        rule single_bitmask() -> SingleBitmask
          = char:$(['X' | '1' | '0']) {
            use SingleBitmask::*;
            match char {
                "X" => NoMask,
                "1" => MustBe1,
                "0" => MustBe0,
                _ => unreachable!(),
            }
        }

        rule bitmask() -> Bitmask
          = l:(single_bitmask()*<36>) { Bitmask(l) }

        rule instruction() -> Instruction
          = "mask = " mask:bitmask() { Instruction::SetBitmask(mask) }
          / "mem[" loc:usize() "] = " val:usize() { Instruction::SetMemory(loc, val) }

        pub(crate) rule parse() -> Program
          = instrs:(instruction()**"\n") { Program(instrs) }
    }
}

#[derive(PartialEq, Debug)]
struct ProgramResult {
    mask: Option<Bitmask>,
    memory: HashMap<usize, usize>,
}

impl ProgramResult {
    fn update_memory_v1(&mut self, loc: usize, val: usize) -> Option<()> {
        self.memory.insert(loc, self.mask.clone()?.apply_v1(&val));
        Some(())
    }

    fn update_memory_v2(&mut self, loc: usize, val: usize) -> Option<()> {
        for new_loc in self.mask.clone()?.apply_v2(&loc) {
            self.memory.insert(new_loc, val);
        }
        Some(())
    }
}

impl Program {
    fn run<F>(&self, update_memory: &F) -> Option<ProgramResult>
    where
        F: Fn(&mut ProgramResult, usize, usize) -> Option<()>,
    {
        let mut res = ProgramResult {
            mask: None,
            memory: HashMap::new(),
        };
        for instr in self.0.iter() {
            use Instruction::*;
            match instr {
                SetBitmask(mask) => res.mask = Some(mask.clone()),
                SetMemory(loc, val) => update_memory(&mut res, *loc, *val)?,
            }
        }
        Some(res)
    }

    fn run_v1(&self) -> Option<ProgramResult> {
        self.run(&ProgramResult::update_memory_v1)
    }

    fn run_v2(&self) -> Option<ProgramResult> {
        self.run(&ProgramResult::update_memory_v2)
    }
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let instrs: Program = parser::parse(&fs::read_to_string("in.txt")?[..])?;

    let ProgramResult { mask: _, memory } = instrs.run_v1().expect("Invalid program");
    println!(
        "Part 1: {}",
        memory.iter().map(|(_, &val)| val).sum::<usize>()
    );

    let ProgramResult { mask: _, memory } = instrs.run_v2().expect("Invalid program");
    println!(
        "Part 2: {}",
        memory.iter().map(|(_, &val)| val).sum::<usize>()
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn example_data() -> Program {
        use Instruction::*;
        use SingleBitmask::*;
        Program(vec![
            SetBitmask(Bitmask(vec![
                NoMask, NoMask, NoMask, NoMask, NoMask, NoMask, NoMask, NoMask, NoMask, NoMask,
                NoMask, NoMask, NoMask, NoMask, NoMask, NoMask, NoMask, NoMask, NoMask, NoMask,
                NoMask, NoMask, NoMask, NoMask, NoMask, NoMask, NoMask, NoMask, NoMask, MustBe1,
                NoMask, NoMask, NoMask, NoMask, MustBe0, NoMask,
            ])),
            SetMemory(8, 11),
            SetMemory(7, 101),
            SetMemory(8, 0),
        ])
    }

    fn example_mask0() -> Bitmask {
        match &example_data().0[0] {
            Instruction::SetBitmask(mask) => mask.clone(),
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            parser::parse(
                "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"
            ),
            Ok(example_data())
        );
    }

    #[test]
    fn test_run() {
        assert_eq!(
            example_data().run_v1(),
            Some(ProgramResult {
                mask: Some(example_mask0()),
                memory: [(8, 64), (7, 101)].iter().copied().collect(),
            })
        );
    }

    fn example_data2() -> Program {
        parser::parse(
            "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1",
        )
        .unwrap()
    }

    fn example_mask1() -> Bitmask {
        match &example_data2().0[0] {
            Instruction::SetBitmask(mask) => mask.clone(),
            _ => unreachable!(),
        }
    }

    fn example_mask2() -> Bitmask {
        match &example_data2().0[2] {
            Instruction::SetBitmask(mask) => mask.clone(),
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_apply_v2() {
        let mask = example_mask1();
        assert_eq!(mask.apply_v2(&42), vec![59, 58, 27, 26]);
        let mask2 = example_mask2();
        assert_eq!(mask2.apply_v2(&26), vec![27, 26, 25, 24, 19, 18, 17, 16]);
    }

    #[test]
    fn test_upd_mem_v2() {
        let mut state = ProgramResult {
            mask: Some(example_mask1()),
            memory: HashMap::new(),
        };
        state.update_memory_v2(42, 100).unwrap();
        assert_eq!(
            state.memory,
            [(26, 100), (27, 100), (58, 100), (59, 100)]
                .iter()
                .cloned()
                .collect()
        );
    }

    #[test]
    fn test_run2() {
        assert_eq!(
            example_data2()
                .run_v2()
                .unwrap()
                .memory
                .iter()
                .map(|(_, &val)| val)
                .sum::<usize>(),
            208
        );
    }
}
