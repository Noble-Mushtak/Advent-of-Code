use std::error::Error;
use std::fs;

#[derive(PartialEq, Debug, Clone)]
enum Operation {
    Plus,
    Times,
}

impl Operation {
    fn apply(&self, num1: usize, num2: usize) -> usize {
        match self {
            Operation::Plus => num1 + num2,
            Operation::Times => num1 * num2,
        }
    }
}

#[derive(PartialEq, Debug)]
enum PreAtom {
    Number(usize),
    Parentheses(Box<PreExpression>),
}

#[derive(PartialEq, Debug)]
struct PreExpression {
    rightmost_atom: PreAtom,
    other_atoms: Vec<(PreAtom, Operation)>,
}

peg::parser! {
    grammar parser() for str {
        rule usize() -> usize
          = n:$(['0'..='9']+) {
            n.parse().unwrap()
        }

        rule operation() -> Operation
          = ch:$(['+' | '*']) {
            match ch {
                "+" => Operation::Plus,
                "*" => Operation::Times,
                _ => unreachable!(),
            }
        }

        rule preatom() -> PreAtom
          = num:usize() { PreAtom::Number(num) } /
            "(" exp:preexpression() ")" { PreAtom::Parentheses(Box::new(exp)) }

        pub(crate) rule preexpression() -> PreExpression
          = left:preatom() " " op:operation() " " right:preexpression() {
                let PreExpression { rightmost_atom, mut other_atoms } = right;
                other_atoms.push((left, op));
                PreExpression {
                    rightmost_atom,
                    other_atoms
                }
            } /
            atom:preatom() {
                PreExpression {
                    rightmost_atom: atom,
                    other_atoms: vec![],
                }
            }

        pub(crate) rule parse() -> Vec<PreExpression>
          = exps:(preexpression()**"\n") { exps }
    }
}

#[derive(PartialEq, Debug)]
enum Atom {
    Number(usize),
    Parentheses(Box<Expression>),
}

impl Atom {
    fn evaluate(&self) -> usize {
        match self {
            Atom::Number(num) => *num,
            Atom::Parentheses(exp) => exp.evaluate(),
        }
    }
}

#[derive(PartialEq, Debug)]
enum Expression {
    Atomic(Atom),
    Op(Operation, Box<Expression>, Box<Expression>),
}

impl Expression {
    fn add_left(self, op: Operation, left: Atom) -> Expression {
        match self {
            Expression::Atomic(_) => {
                Expression::Op(op, Box::new(Expression::Atomic(left)), Box::new(self))
            }
            Expression::Op(op2, left2, right) => {
                Expression::Op(op2, Box::new(left2.add_left(op, left)), right)
            }
        }
    }

    fn add_left_with_precedence(self, op: Operation, left: Atom) -> Expression {
        match self {
            Expression::Atomic(_) => {
                Expression::Op(op, Box::new(Expression::Atomic(left)), Box::new(self))
            }
            Expression::Op(op2, left2, right) => match (&op, &op2) {
                (&Operation::Times, &Operation::Plus) => Expression::Op(
                    op,
                    Box::new(Expression::Atomic(left)),
                    Box::new(Expression::Op(op2, left2, right)),
                ),
                _ => Expression::Op(
                    op2,
                    Box::new(left2.add_left_with_precedence(op, left)),
                    right,
                ),
            },
        }
    }

    fn evaluate(&self) -> usize {
        match self {
            Expression::Atomic(atom) => atom.evaluate(),
            Expression::Op(op, left, right) => op.apply(left.evaluate(), right.evaluate()),
        }
    }
}

impl PreAtom {
    fn to_atom<F>(&self, add_left: &F) -> Atom
    where
        F: Fn(Expression, Operation, Atom) -> Expression,
    {
        match self {
            PreAtom::Number(num) => Atom::Number(*num),
            PreAtom::Parentheses(preexp) => {
                Atom::Parentheses(Box::new(preexp.to_expression(add_left)))
            }
        }
    }
}

impl PreExpression {
    fn to_expression<F>(&self, add_left: &F) -> Expression
    where
        F: Fn(Expression, Operation, Atom) -> Expression,
    {
        self.other_atoms
            .iter()
            .map(|(preatom, op)| (preatom.to_atom(add_left), op.clone()))
            .fold(
                Expression::Atomic(self.rightmost_atom.to_atom(add_left)),
                |exp, (atom, op)| add_left(exp, op, atom),
            )
    }
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let preexps = parser::parse(&fs::read_to_string("in.txt")?[..])?;

    println!(
        "Part 1: {}",
        preexps
            .iter()
            .map(|preexp| preexp.to_expression(&Expression::add_left))
            .map(|exp| exp.evaluate())
            .sum::<usize>()
    );

    println!(
        "Part 2: {}",
        preexps
            .iter()
            .map(|preexp| preexp.to_expression(&Expression::add_left_with_precedence))
            .map(|exp| exp.evaluate())
            .sum::<usize>()
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        use Operation::*;
        use PreAtom::*;
        assert_eq!(
            parser::preexpression("2").unwrap(),
            PreExpression {
                rightmost_atom: Number(2),
                other_atoms: vec![],
            }
        );
        assert_eq!(
            parser::preexpression("2 + 3").unwrap(),
            PreExpression {
                rightmost_atom: Number(3),
                other_atoms: vec![(Number(2), Plus)],
            }
        );
        assert_eq!(
            parser::preexpression("2 + 3 * (4 + 5 * 2)").unwrap(),
            PreExpression {
                rightmost_atom: Parentheses(Box::new(PreExpression {
                    rightmost_atom: Number(2),
                    other_atoms: vec![(Number(5), Times), (Number(4), Plus)],
                })),
                other_atoms: vec![(Number(3), Times), (Number(2), Plus)],
            }
        );
    }

    #[test]
    fn test_to_exp1() {
        use Atom::*;
        use Expression::*;
        use Operation::*;
        assert_eq!(
            parser::preexpression("3")
                .unwrap()
                .to_expression(&Expression::add_left),
            Atomic(Number(3))
        );
        assert_eq!(
            parser::preexpression("2 + 3")
                .unwrap()
                .to_expression(&Expression::add_left),
            Op(
                Plus,
                Box::new(Atomic(Number(2))),
                Box::new(Atomic(Number(3)))
            )
        );
        assert_eq!(
            parser::preexpression("2 + 3 + 5")
                .unwrap()
                .to_expression(&Expression::add_left),
            Op(
                Plus,
                Box::new(Op(
                    Plus,
                    Box::new(Atomic(Number(2))),
                    Box::new(Atomic(Number(3)))
                )),
                Box::new(Atomic(Number(5)))
            )
        );
        assert_eq!(
            parser::preexpression("2 + 3 * (4 + 5 * 2)")
                .unwrap()
                .to_expression(&Expression::add_left),
            Op(
                Times,
                Box::new(Op(
                    Plus,
                    Box::new(Atomic(Number(2))),
                    Box::new(Atomic(Number(3)))
                )),
                Box::new(Atomic(Parentheses(Box::new(Op(
                    Times,
                    Box::new(Op(
                        Plus,
                        Box::new(Atomic(Number(4))),
                        Box::new(Atomic(Number(5)))
                    )),
                    Box::new(Atomic(Number(2)))
                )))))
            )
        );
    }

    #[test]
    fn test_eval() {
        assert_eq!(
            parser::preexpression("1 + 2 * 3 + 4 * 5 + 6")
                .unwrap()
                .to_expression(&Expression::add_left)
                .evaluate(),
            71
        );
        assert_eq!(
            parser::preexpression("1 + (2 * 3) + (4 * (5 + 6))")
                .unwrap()
                .to_expression(&Expression::add_left)
                .evaluate(),
            51
        );
        assert_eq!(
            parser::preexpression("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
                .unwrap()
                .to_expression(&Expression::add_left)
                .evaluate(),
            13632
        );
    }

    #[test]
    fn test_add_wp() {
        use Atom::*;
        use Expression::*;
        use Operation::*;

        let mut exp2 = Op(
            Plus,
            Box::new(Atomic(Number(3))),
            Box::new(Atomic(Number(4))),
        );
        exp2 = exp2.add_left_with_precedence(Times, Number(2));
        assert_eq!(
            exp2,
            Op(
                Times,
                Box::new(Atomic(Number(2))),
                Box::new(Op(
                    Plus,
                    Box::new(Atomic(Number(3))),
                    Box::new(Atomic(Number(4)))
                ))
            )
        );

        let mut exp = Atomic(Number(6));
        exp = exp.add_left_with_precedence(Plus, Number(5));
        assert_eq!(
            exp,
            Op(
                Plus,
                Box::new(Atomic(Number(5))),
                Box::new(Atomic(Number(6)))
            )
        );
        exp = exp.add_left_with_precedence(Times, Number(4));
        assert_eq!(
            exp,
            Op(
                Times,
                Box::new(Atomic(Number(4))),
                Box::new(Op(
                    Plus,
                    Box::new(Atomic(Number(5))),
                    Box::new(Atomic(Number(6)))
                ))
            )
        );
        exp = exp.add_left_with_precedence(Plus, Number(3));
        assert_eq!(
            exp,
            Op(
                Times,
                Box::new(Op(
                    Plus,
                    Box::new(Atomic(Number(3))),
                    Box::new(Atomic(Number(4)))
                )),
                Box::new(Op(
                    Plus,
                    Box::new(Atomic(Number(5))),
                    Box::new(Atomic(Number(6)))
                ))
            )
        );
        exp = exp.add_left_with_precedence(Times, Number(2));
        assert_eq!(
            exp,
            Op(
                Times,
                Box::new(Op(
                    Times,
                    Box::new(Atomic(Number(2))),
                    Box::new(Op(
                        Plus,
                        Box::new(Atomic(Number(3))),
                        Box::new(Atomic(Number(4)))
                    ))
                )),
                Box::new(Op(
                    Plus,
                    Box::new(Atomic(Number(5))),
                    Box::new(Atomic(Number(6)))
                ))
            )
        );
    }

    #[test]
    fn test_eval2() {
        assert_eq!(
            parser::preexpression("1 + 2 * 3 + 4 * 5 + 6")
                .unwrap()
                .to_expression(&Expression::add_left_with_precedence)
                .evaluate(),
            231
        );
        assert_eq!(
            parser::preexpression("1 + (2 * 3) + (4 * (5 + 6))")
                .unwrap()
                .to_expression(&Expression::add_left_with_precedence)
                .evaluate(),
            51
        );
        assert_eq!(
            parser::preexpression("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2")
                .unwrap()
                .to_expression(&Expression::add_left_with_precedence)
                .evaluate(),
            23340
        );
    }
}
