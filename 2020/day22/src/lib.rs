use std::cmp::Ordering;
use std::collections::{HashSet, VecDeque};
use std::error::Error;
use std::fs;

#[derive(PartialEq, Debug, Clone, Copy, Eq, Hash)]
struct Card(usize);

#[derive(PartialEq, Debug, Clone, Eq, Hash)]
struct Deck(VecDeque<Card>);

#[derive(PartialEq, Debug, Clone, Eq, Hash)]
struct SimpleGameState {
    player1: Deck,
    player2: Deck,
}

peg::parser! {
    grammar parser() for str {
        rule card() -> Card
          = n:$(['0'..='9']+) {
            Card(n.parse().unwrap())
        }

        pub(crate) rule parse() -> SimpleGameState
          = "Player 1:\n" deck1:(card()**"\n") "\n\nPlayer 2:\n" deck2:(card()**"\n") {
            SimpleGameState {
                player1: Deck(deck1.into_iter().collect()),
                player2: Deck(deck2.into_iter().collect()),
            }
        }
    }
}

impl Deck {
    fn handle_win(&mut self, winners_card: Card, losers_card: Card) {
        self.0.push_back(winners_card);
        self.0.push_back(losers_card);
    }

    fn score(&self) -> usize {
        self.0
            .iter()
            .rev()
            .enumerate()
            .fold(0, |cur_score, (i, card)| cur_score + (i + 1) * card.0)
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
enum GameEnded {
    Player1Wins,
    Player2Wins,
}

trait GameState {
    fn advance_state(&mut self) -> Result<(), GameEnded>;
    fn final_score(&self, outcome: GameEnded) -> usize;

    fn play_game_until_end(&mut self) -> GameEnded {
        loop {
            if let Err(outcome) = self.advance_state() {
                break outcome;
            }
        }
    }

    fn final_score_after_playing_game(&mut self) -> usize {
        let outcome = self.play_game_until_end();
        self.final_score(outcome)
    }
}

impl SimpleGameState {
    fn compare_and_handle_win(&mut self, player1_top: Card, player2_top: Card) {
        match player1_top.0.cmp(&player2_top.0) {
            Ordering::Equal => panic!("Players 1 and 2 have the same card: {}", player1_top.0),
            Ordering::Less => self.player2.handle_win(player2_top, player1_top),
            Ordering::Greater => self.player1.handle_win(player1_top, player2_top),
        };
    }

    fn get_top_cards(&mut self) -> Result<(Card, Card), GameEnded> {
        let player1_top = self.player1.0.pop_front().ok_or(GameEnded::Player2Wins)?;
        let player2_top = self.player2.0.pop_front().ok_or(GameEnded::Player1Wins)?;
        Ok((player1_top, player2_top))
    }
}

impl GameState for SimpleGameState {
    fn advance_state(&mut self) -> Result<(), GameEnded> {
        let (player1_top, player2_top) = self.get_top_cards()?;
        self.compare_and_handle_win(player1_top, player2_top);
        Ok(())
    }

    fn final_score(&self, outcome: GameEnded) -> usize {
        match outcome {
            GameEnded::Player1Wins => self.player1.score(),
            GameEnded::Player2Wins => self.player2.score(),
        }
    }
}

#[derive(PartialEq, Debug)]
struct RecursiveGameState {
    all_prev_states: HashSet<SimpleGameState>,
    cur_state: SimpleGameState,
    round_num: usize,
    game_num: usize,
}

impl RecursiveGameState {
    fn from_simple_state(simple_state: SimpleGameState, game_num: usize) -> Self {
        RecursiveGameState {
            all_prev_states: HashSet::new(),
            cur_state: simple_state,
            round_num: 0,
            game_num,
        }
    }
}

impl GameState for RecursiveGameState {
    fn advance_state(&mut self) -> Result<(), GameEnded> {
        if self.all_prev_states.contains(&self.cur_state) {
            return Err(GameEnded::Player1Wins);
        }
        self.all_prev_states.insert(self.cur_state.clone());

        if self.game_num == 0 {
            eprintln!("{} {}", self.game_num, self.round_num);
        }

        let (player1_top, player2_top) = self.cur_state.get_top_cards()?;
        if self.cur_state.player1.0.len() >= player1_top.0
            && self.cur_state.player2.0.len() >= player2_top.0
        {
            let mut sub_state = RecursiveGameState::from_simple_state(
                SimpleGameState {
                    player1: Deck(
                        self.cur_state
                            .player1
                            .0
                            .iter()
                            .take(player1_top.0)
                            .cloned()
                            .collect(),
                    ),
                    player2: Deck(
                        self.cur_state
                            .player2
                            .0
                            .iter()
                            .take(player2_top.0)
                            .cloned()
                            .collect(),
                    ),
                },
                self.game_num + 1,
            );

            let outcome = sub_state.play_game_until_end();
            match outcome {
                GameEnded::Player1Wins => {
                    self.cur_state.player1.handle_win(player1_top, player2_top)
                }
                GameEnded::Player2Wins => {
                    self.cur_state.player2.handle_win(player2_top, player1_top)
                }
            };
        } else {
            self.cur_state
                .compare_and_handle_win(player1_top, player2_top);
        }

        self.round_num += 1;
        Ok(())
    }

    fn final_score(&self, outcome: GameEnded) -> usize {
        self.cur_state.final_score(outcome)
    }
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let mut simple_state = parser::parse(&fs::read_to_string("in.txt")?[..])?;

    let mut recursive_state = RecursiveGameState::from_simple_state(simple_state.clone(), 0);

    println!("Part 1: {}", simple_state.final_score_after_playing_game());
    println!(
        "Part 2: {}",
        recursive_state.final_score_after_playing_game()
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn example_state() -> SimpleGameState {
        SimpleGameState {
            player1: Deck(
                vec![9, 2, 6, 3, 1]
                    .into_iter()
                    .map(|num| Card(num))
                    .collect(),
            ),
            player2: Deck(
                vec![5, 8, 4, 7, 10]
                    .into_iter()
                    .map(|num| Card(num))
                    .collect(),
            ),
        }
    }

    #[test]
    fn test_parse() {
        assert_eq!(
            parser::parse(
                "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10"
            ),
            Ok(example_state())
        );
    }

    #[test]
    fn test_simple_game() {
        let mut state = example_state();
        let outcome = state.play_game_until_end();
        assert_eq!(
            state,
            SimpleGameState {
                player1: Deck(VecDeque::new()),
                player2: Deck(
                    vec![3, 2, 10, 6, 8, 5, 9, 4, 7, 1]
                        .into_iter()
                        .map(|num| Card(num))
                        .collect()
                ),
            }
        );
        assert_eq!(outcome, GameEnded::Player2Wins);
        assert_eq!(state.final_score(outcome), 306);
    }

    #[test]
    fn test_recursive_game() {
        let mut state = RecursiveGameState::from_simple_state(example_state(), 0);
        let outcome = state.play_game_until_end();
        assert_eq!(
            state.cur_state,
            SimpleGameState {
                player1: Deck(VecDeque::new()),
                player2: Deck(
                    vec![7, 5, 6, 2, 4, 1, 10, 8, 9, 3]
                        .into_iter()
                        .map(|num| Card(num))
                        .collect()
                ),
            }
        );
        assert_eq!(outcome, GameEnded::Player2Wins);
        assert_eq!(state.final_score(outcome), 291);
    }
}
