use snafu::Snafu;
use std::convert::TryInto;
use std::str::FromStr;

#[derive(PartialEq, Debug, Clone)]
pub enum Instruction {
    Acc(isize),
    Jmp(isize),
    Nop(isize),
}

#[derive(PartialEq, Debug, Snafu)]
pub enum ParseInstructionError {
    #[snafu(display("\"{}\" does not contain a space", line))]
    BadFormat { line: String },
    #[snafu(display("\"{}\" is not a valid instruction", instr))]
    BadInstruction { instr: String },
    #[snafu(display("\"{}\" does not end with an integer", line))]
    BadInteger { line: String },
}

impl FromStr for Instruction {
    type Err = ParseInstructionError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        use Instruction::*;
        use ParseInstructionError::*;

        let (instr, num_str) = input.split_once(" ").ok_or_else(|| BadFormat {
            line: input.to_string(),
        })?;
        let num: isize = num_str.parse().map_err(|_| BadInteger {
            line: input.to_string(),
        })?;
        match instr {
            "acc" => Ok(Acc(num)),
            "jmp" => Ok(Jmp(num)),
            "nop" => Ok(Nop(num)),
            _ => Err(BadInstruction {
                instr: instr.to_string(),
            }),
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct Program(pub Vec<Instruction>);

impl FromStr for Program {
    type Err = ParseInstructionError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Ok(Program(
            input
                .trim()
                .split('\n')
                .map(str::parse)
                .collect::<Result<_, _>>()?,
        ))
    }
}

#[derive(PartialEq, Debug)]
pub struct ProgramState {
    pub acc: isize,
    pub inst_ptr: usize,
}

pub const INITIAL_STATE: ProgramState = ProgramState {
    acc: 0,
    inst_ptr: 0,
};

#[derive(PartialEq, Debug, Snafu)]
pub enum ProgramError {
    ProgramTerminated,
    InstructionNotFound,
    BadJump,
}

impl Program {
    pub fn update_state(&self, prog_state: &mut ProgramState) -> Result<(), ProgramError> {
        use Instruction::*;
        use ProgramError::*;

        let cur_inst = self.0.get(prog_state.inst_ptr).ok_or_else(|| {
            if prog_state.inst_ptr == self.0.len() {
                ProgramTerminated
            } else {
                InstructionNotFound
            }
        })?;
        match cur_inst {
            Acc(num) => {
                prog_state.acc += num;
                prog_state.inst_ptr += 1;
            }
            Jmp(num) => {
                prog_state.inst_ptr = ((prog_state.inst_ptr as isize) + num)
                    .try_into()
                    .map_err(|_| BadJump)?;
            }
            Nop(_) => {
                prog_state.inst_ptr += 1;
            }
        };
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Instruction::*;

    #[test]
    fn test_parse_instr() {
        assert_eq!("nop +0".parse::<Instruction>().unwrap(), Nop(0));
        assert_eq!("acc +1".parse::<Instruction>().unwrap(), Acc(1));
        assert_eq!("jmp -3".parse::<Instruction>().unwrap(), Jmp(-3));
        assert!("jmp-3".parse::<Instruction>().is_err());
        assert!("jmpx -3".parse::<Instruction>().is_err());
        assert!("jmp -3s".parse::<Instruction>().is_err());
    }

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
    fn test_parse_prog() {
        assert_eq!(
            example_program(),
            Program(vec![
                Nop(0),
                Acc(1),
                Jmp(4),
                Acc(3),
                Jmp(-3),
                Acc(-99),
                Acc(1),
                Jmp(-4),
                Acc(6)
            ])
        );
    }

    #[test]
    fn test_exec() -> Result<(), ProgramError> {
        let prog = example_program();
        let mut prog_state = INITIAL_STATE;
        prog.update_state(&mut prog_state)?;
        assert_eq!(prog_state.inst_ptr, 1);
        prog.update_state(&mut prog_state)?;
        assert_eq!(prog_state.inst_ptr, 2);
        prog.update_state(&mut prog_state)?;
        assert_eq!(prog_state.inst_ptr, 6);
        prog.update_state(&mut prog_state)?;
        assert_eq!(prog_state.inst_ptr, 7);
        prog.update_state(&mut prog_state)?;
        assert_eq!(prog_state.inst_ptr, 3);
        prog.update_state(&mut prog_state)?;
        assert_eq!(prog_state.inst_ptr, 4);
        prog.update_state(&mut prog_state)?;
        assert_eq!(prog_state.inst_ptr, 1);
        Ok(())
    }

    #[test]
    fn test_termination() -> Result<(), ProgramError> {
        let prog = example_program2();
        let mut prog_state = INITIAL_STATE;
        prog.update_state(&mut prog_state)?;
        assert_eq!(prog_state.inst_ptr, 1);
        prog.update_state(&mut prog_state)?;
        assert_eq!(prog_state.inst_ptr, 2);
        prog.update_state(&mut prog_state)?;
        assert_eq!(prog_state.inst_ptr, 6);
        prog.update_state(&mut prog_state)?;
        assert_eq!(prog_state.inst_ptr, 7);
        prog.update_state(&mut prog_state)?;
        assert_eq!(prog_state.inst_ptr, 8);
        prog.update_state(&mut prog_state)?;
        assert_eq!(prog_state.inst_ptr, 9);
        assert_eq!(
            prog.update_state(&mut prog_state),
            Err(ProgramError::ProgramTerminated)
        );
        Ok(())
    }
}
