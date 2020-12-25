#![feature(str_split_once)]

use std::collections::HashSet;
use std::error::Error;
use std::fs;

mod program;
use crate::program::*;

fn find_loop(prog: &Program) -> Result<ProgramState, (ProgramState, ProgramError)> {
    let mut prog_state = INITIAL_STATE;
    let mut insts_visited = HashSet::new();
    
    while !insts_visited.contains(&prog_state.inst_ptr) {
        insts_visited.insert(prog_state.inst_ptr);
        if let Err(err) = prog.update_state(&mut prog_state) {
            return Err((prog_state, err));
        }
        //prog.update_state(&mut prog_state).map_err(|err| (prog_state, err))?;
    }

    Ok(prog_state)
}

#[derive(PartialEq, Debug)]
struct CorrectedProgramTermination {
    corrected_inst_ptr: usize,
    termination_acc: isize,
}

fn find_error(prog: &mut Program) -> Option<CorrectedProgramTermination> {
    use Instruction::*;

    for i in 0..prog.0.len() {
        fn modify_inst(inst: &mut Instruction) {
            match inst {
                Acc(_) => (),
                Jmp(num) => *inst = Nop(*num),
                Nop(num) => *inst = Jmp(*num),
            };
        };
        modify_inst(prog.0.get_mut(i).unwrap());

        if let Err((prog_state, ProgramError::ProgramTerminated)) = find_loop(prog) {
            return Some(CorrectedProgramTermination {
                corrected_inst_ptr: i,
                termination_acc: prog_state.acc,
            });
        }

        modify_inst(prog.0.get_mut(i).unwrap());
    }
    None
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let mut prog: Program = fs::read_to_string("in.txt")?.parse()?;
    println!(
        "Part 1: {:?}",
        find_loop(&prog).map_err(|(_, err)| err)?.acc
    );
    println!(
        "Part 2: {:?}",
        find_error(&mut prog)
            .ok_or("Could not find error")?
            .termination_acc
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn example_program() -> Program {
        "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"
            .parse()
            .unwrap()
    }

    fn example_program2() -> Program {
        "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
nop -4
acc +6"
            .parse()
            .unwrap()
    }

    #[test]
    fn test_find_loop() -> Result<(), ProgramError> {
        let prog = example_program();
        let prog_state = find_loop(&prog).map_err(|(_, err)| err)?;
        assert_eq!(prog_state.acc, 5);
        assert_eq!(prog_state.inst_ptr, 1);

        let prog2 = example_program2();
        match find_loop(&prog2) {
            Err((_, ProgramError::ProgramTerminated)) => (),
            _ => panic!("The program did not terminate!"),
        };

        Ok(())
    }

    #[test]
    fn test_find_err() {
        let mut prog = example_program();
        assert_eq!(find_error(&mut prog).unwrap().termination_acc, 8);
    }
}
