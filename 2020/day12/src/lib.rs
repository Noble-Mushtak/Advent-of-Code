use snafu::Snafu;
use std::error::Error;
use std::fs;
use std::str::FromStr;

#[derive(PartialEq, Debug, Clone)]
enum TurnType {
    CC90,
    CC180,
    CC270,
}

impl From<TurnType> for isize {
    fn from(turn: TurnType) -> isize {
        match turn {
            TurnType::CC90 => 1,
            TurnType::CC180 => 2,
            TurnType::CC270 => 3,
        }
    }
}

#[derive(PartialEq, Debug)]
enum Direction {
    East,
    North,
    West,
    South,
}

impl From<isize> for Direction {
    fn from(val: isize) -> Self {
        match val.rem_euclid(4) {
            0 => Direction::East,
            1 => Direction::North,
            2 => Direction::West,
            3 => Direction::South,
            _ => unreachable!(),
        }
    }
}

impl From<Direction> for isize {
    fn from(dir: Direction) -> isize {
        match dir {
            Direction::East => 0,
            Direction::North => 1,
            Direction::West => 2,
            Direction::South => 3,
        }
    }
}

impl Direction {
    fn turn(self, turn_type: &TurnType) -> Self {
        (isize::from(self) + isize::from(turn_type.clone())).into()
    }
}

#[derive(PartialEq, Debug)]
enum Instruction {
    Move(Direction, usize),
    MoveForward(usize),
    Turn(TurnType),
}

#[derive(PartialEq, Debug, Snafu)]
enum ParseInstructionError {
    #[snafu(display("The first letter of \"{}\" is not a valid instruction", line))]
    BadInstruction { line: String },
    #[snafu(display(
        "\"{}\" does not contain an unsigned integer after the first letter",
        line
    ))]
    NonInteger { line: String },
    #[snafu(display("Instructions that turn {} degrees are not implemented", num))]
    UnimplementedTurn { num: usize },
}

impl FromStr for Instruction {
    type Err = ParseInstructionError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        use Instruction::*;
        use ParseInstructionError::*;

        let mut char_iter = input.chars();
        let first_char = char_iter.next();
        let num: usize = char_iter
            .collect::<String>()
            .parse()
            .map_err(|_| NonInteger {
                line: input.to_string(),
            })?;
        match first_char {
            Some('E') => Ok(Move(Direction::East, num)),
            Some('N') => Ok(Move(Direction::North, num)),
            Some('W') => Ok(Move(Direction::West, num)),
            Some('S') => Ok(Move(Direction::South, num)),
            Some('F') => Ok(MoveForward(num)),
            Some('L') => match num {
                90 => Ok(Turn(TurnType::CC90)),
                180 => Ok(Turn(TurnType::CC180)),
                270 => Ok(Turn(TurnType::CC270)),
                _ => Err(UnimplementedTurn { num }),
            },
            Some('R') => match num {
                90 => Ok(Turn(TurnType::CC270)),
                180 => Ok(Turn(TurnType::CC180)),
                270 => Ok(Turn(TurnType::CC90)),
                _ => Err(UnimplementedTurn { num }),
            },
            _ => Err(BadInstruction {
                line: input.to_string(),
            }),
        }
    }
}

#[derive(PartialEq, Debug)]
struct Instructions(Vec<Instruction>);

impl FromStr for Instructions {
    type Err = ParseInstructionError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        input
            .lines()
            .map(str::parse)
            .collect::<Result<_, _>>()
            .map(Instructions)
    }
}

#[derive(PartialEq, Debug)]
struct Location {
    x: isize,
    y: isize,
}

impl Location {
    fn move_by(self, dir: &Direction, magnitude: &usize) -> Self {
        let magnitude = *magnitude as isize;
        match dir {
            Direction::East => Location {
                x: self.x + magnitude,
                ..self
            },
            Direction::West => Location {
                x: self.x - magnitude,
                ..self
            },
            Direction::North => Location {
                y: self.y + magnitude,
                ..self
            },
            Direction::South => Location {
                y: self.y - magnitude,
                ..self
            },
        }
    }

    fn rotate(self, turn_type: &TurnType) -> Self {
        match turn_type {
            TurnType::CC90 => Location {
                x: -self.y,
                y: self.x,
            },
            TurnType::CC180 => Location {
                x: -self.x,
                y: -self.y,
            },
            TurnType::CC270 => Location {
                x: self.y,
                y: -self.x,
            },
        }
    }
}

#[derive(PartialEq, Debug)]
struct ShipState {
    loc: Location,
    dir: Direction,
    waypoint: Location,
}

const INIT_STATE: ShipState = ShipState {
    loc: Location { x: 0, y: 0 },
    dir: Direction::East,
    waypoint: Location { x: 10, y: 1 },
};

impl ShipState {
    fn follow_instruction_with_dir(self, instr: &Instruction) -> Self {
        use Instruction::*;

        match instr {
            Move(dir, mag) => ShipState {
                loc: self.loc.move_by(dir, mag),
                ..self
            },
            MoveForward(mag) => ShipState {
                loc: self.loc.move_by(&self.dir, mag),
                ..self
            },
            Turn(turn_type) => ShipState {
                dir: self.dir.turn(turn_type),
                ..self
            },
        }
    }

    fn follow_instruction_with_waypoint(self, instr: &Instruction) -> Self {
        use Instruction::*;

        match instr {
            Move(dir, mag) => ShipState {
                waypoint: self.waypoint.move_by(dir, mag),
                ..self
            },
            MoveForward(mag) => ShipState {
                loc: Location {
                    x: self.loc.x + (*mag as isize) * self.waypoint.x,
                    y: self.loc.y + (*mag as isize) * self.waypoint.y,
                },
                ..self
            },
            Turn(turn_type) => ShipState {
                waypoint: self.waypoint.rotate(turn_type),
                ..self
            },
        }
    }

    fn follow_instructions<F>(self, follow_instruction: F, instrs: &Instructions) -> Self
    where
        F: Fn(Self, &Instruction) -> Self,
    {
        instrs.0.iter().fold(self, follow_instruction)
    }

    fn follow_instructions_with_dir(self, instrs: &Instructions) -> Self {
        self.follow_instructions(ShipState::follow_instruction_with_dir, instrs)
    }

    fn follow_instructions_with_waypoint(self, instrs: &Instructions) -> Self {
        self.follow_instructions(ShipState::follow_instruction_with_waypoint, instrs)
    }
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let dirs: Instructions = fs::read_to_string("in.txt")?.parse()?;

    let destination = INIT_STATE.follow_instructions_with_dir(&dirs);
    println!(
        "Part 1: {}",
        destination.loc.x.abs() + destination.loc.y.abs()
    );
    let destination = INIT_STATE.follow_instructions_with_waypoint(&dirs);
    println!(
        "Part 2: {}",
        destination.loc.x.abs() + destination.loc.y.abs()
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_instr() {
        use Instruction::*;

        assert_eq!("R90".parse::<Instruction>().unwrap(), Turn(TurnType::CC270));
        assert_eq!("L90".parse::<Instruction>().unwrap(), Turn(TurnType::CC90));
        assert_eq!("F10".parse::<Instruction>().unwrap(), MoveForward(10));
        assert_eq!(
            "N10".parse::<Instruction>().unwrap(),
            Move(Direction::North, 10)
        );
        assert_eq!(
            "S10".parse::<Instruction>().unwrap(),
            Move(Direction::South, 10)
        );
        assert_eq!(
            "E10".parse::<Instruction>().unwrap(),
            Move(Direction::East, 10)
        );
        assert_eq!(
            "W10".parse::<Instruction>().unwrap(),
            Move(Direction::West, 10)
        );
    }

    fn example_instrs() -> Instructions {
        use Instruction::*;
        Instructions(vec![
            MoveForward(10),
            Move(Direction::North, 3),
            MoveForward(7),
            Turn(TurnType::CC270),
            MoveForward(11),
        ])
    }

    #[test]
    fn test_parse_instrs() {
        assert_eq!(
            "F10\nN3\nF7\nR90\nF11".parse::<Instructions>().unwrap(),
            example_instrs()
        );
    }

    #[test]
    fn test_follow() {
        assert_eq!(
            INIT_STATE.follow_instructions_with_dir(&example_instrs()),
            ShipState {
                loc: Location { x: 17, y: -8 },
                dir: Direction::South,
                ..INIT_STATE
            }
        );
        assert_eq!(
            INIT_STATE.follow_instructions_with_waypoint(&example_instrs()),
            ShipState {
                loc: Location { x: 214, y: -72 },
                waypoint: Location { x: 4, y: -10 },
                ..INIT_STATE
            }
        );
    }
}
