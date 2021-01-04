use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::Debug;
use std::fs;
use std::rc::{Rc, Weak};

#[derive(PartialEq, Debug, Clone, Copy)]
enum Direction {
    Clockwise,
    CounterClockwise,
}

#[derive(PartialEq, Debug, Clone, Copy, Eq, Hash)]
struct Cup(usize);

#[derive(Debug)]
enum MaybeRc<T> {
    Rc(Rc<T>),
    Weak(Weak<T>),
}

impl<T> MaybeRc<T> {
    fn upgrade(&self) -> Option<Rc<T>> {
        match self {
            MaybeRc::Rc(rc) => Some(Rc::clone(&rc)),
            MaybeRc::Weak(weak) => weak.upgrade(),
        }
    }
}

#[derive(Debug)]
struct CircularNode<T> {
    val: T,
    clockwise: RefCell<MaybeRc<CircularNode<T>>>,
    counterclockwise: RefCell<Weak<CircularNode<T>>>,
}

const BAD_NODE_ERR: &str = "Could not upgrade weak pointer in circular list!";

#[derive(Debug)]
enum CircularList<T> {
    Empty,
    NonEmpty(Rc<CircularNode<T>>),
}

impl<T> CircularList<T> {
    fn primary_node(&self) -> Option<Rc<CircularNode<T>>> {
        match self {
            CircularList::Empty => None,
            CircularList::NonEmpty(node) => Some(Rc::clone(&node)),
        }
    }
}

fn node_get_neighbor<T>(node: &Rc<CircularNode<T>>, dir: Direction) -> Rc<CircularNode<T>> {
    let neighbor_rc;
    match dir {
        Direction::Clockwise => {
            let borrow = node.clockwise.borrow();
            neighbor_rc = borrow.upgrade().expect(BAD_NODE_ERR);
        }
        Direction::CounterClockwise => {
            let borrow = node.counterclockwise.borrow();
            neighbor_rc = borrow.upgrade().expect(BAD_NODE_ERR);
        }
    };
    neighbor_rc
}

#[derive(Debug)]
enum NodeOrValue<T> {
    Node(Rc<CircularNode<T>>),
    Value(T),
}

fn node_insert_clockwise<T>(node: &Rc<CircularNode<T>>, new_insert: NodeOrValue<T>) -> Rc<CircularNode<T>> {
    let clockwise_node = match &*node.clockwise.borrow() {
        MaybeRc::Rc(next_node) => MaybeRc::Rc(Rc::clone(&next_node)),
        MaybeRc::Weak(next_node) => MaybeRc::Weak(Weak::clone(&next_node)),
    };

    let new_node = match new_insert {
        NodeOrValue::Node(node2) => {
            *node2.clockwise.borrow_mut() = clockwise_node;
            *node2.counterclockwise.borrow_mut() = Rc::downgrade(&node);
            node2
        },
        NodeOrValue::Value(val) => Rc::new(CircularNode {
            val,
            clockwise: RefCell::new(clockwise_node),
            counterclockwise: RefCell::new(Rc::downgrade(&node)),
        }),
    };
    let c_rc = node_get_neighbor(&node, Direction::Clockwise);
    *c_rc.counterclockwise.borrow_mut() = Rc::downgrade(&new_node);
    *node.clockwise.borrow_mut() = MaybeRc::Rc(Rc::clone(&new_node));

    new_node
}

impl<T> CircularList<T> {
    fn insert_clockwise(&mut self, val: T) -> Rc<CircularNode<T>> {
        match self {
            CircularList::Empty => {
                let new_node = Rc::new(CircularNode {
                    val,
                    clockwise: RefCell::new(MaybeRc::Weak(Weak::new())),
                    counterclockwise: RefCell::new(Weak::new()),
                });
                *new_node.clockwise.borrow_mut() = MaybeRc::Weak(Rc::downgrade(&new_node));
                *new_node.counterclockwise.borrow_mut() = Rc::downgrade(&new_node);
                *self = CircularList::NonEmpty(Rc::clone(&new_node));
                new_node
            }
            CircularList::NonEmpty(node) => node_insert_clockwise(&node, NodeOrValue::Value(val)),
        }
    }

    fn insert_counterclockwise(&mut self, val: T) -> Rc<CircularNode<T>> {
        match self {
            CircularList::Empty => self.insert_clockwise(val),
            CircularList::NonEmpty(node) => {
                node_insert_clockwise(&node_get_neighbor(&node, Direction::CounterClockwise), NodeOrValue::Value(val))
            }
        }
    }

    fn new(vals: Vec<T>) -> Self {
        let mut vals_iter = vals.into_iter();
        let mut res = CircularList::Empty;
        if let Some(first_val) = vals_iter.next() {
            let mut last_node = res.insert_clockwise(first_val);
            for val in vals_iter {
                last_node = node_insert_clockwise(&last_node, NodeOrValue::Value(val));
            }
        }
        res
    }
}

fn node_map_into_vec<T, F, R>(node: &Rc<CircularNode<T>>, func: F) -> Vec<R>
where
    F: Fn(&Rc<CircularNode<T>>) -> R,
{
    let mut res = vec![];

    let mut node2 = Rc::clone(&node);
    loop {
        res.push(func(&node2));

        let next_node = node_get_neighbor(&node2, Direction::Clockwise);
        if Rc::ptr_eq(&node, &next_node) {
            break res;
        }
        node2 = next_node;
    }
}

impl<T> CircularList<T> {
    fn map_into_vec<F, R>(&self, func: F) -> Vec<R>
    where
        F: Fn(&Rc<CircularNode<T>>) -> R,
    {
        match self {
            CircularList::Empty => vec![],
            CircularList::NonEmpty(node) => node_map_into_vec(&node, func),
        }
    }
}

fn node_remove<T>(node: &Rc<CircularNode<T>>) -> Rc<CircularNode<T>> {
    let orig_node_copy = Rc::clone(&node);
    let cc_rc = node_get_neighbor(&node, Direction::CounterClockwise);
    let c_rc = node_get_neighbor(&node, Direction::Clockwise);

    let should_be_weak;
    if let MaybeRc::Weak(_) = &*cc_rc.clockwise.borrow() {
        should_be_weak = true;
    } else if let MaybeRc::Weak(_) = &*node.clockwise.borrow() {
        should_be_weak = true;
    } else {
        should_be_weak = false;
    }

    if should_be_weak {
        *cc_rc.clockwise.borrow_mut() = MaybeRc::Weak(Rc::downgrade(&c_rc));
    } else {
        *cc_rc.clockwise.borrow_mut() = MaybeRc::Rc(Rc::clone(&c_rc));
    }
    *c_rc.counterclockwise.borrow_mut() = Rc::downgrade(&cc_rc);

    orig_node_copy
}

impl<T> CircularList<T> {
    fn remove(&mut self) -> Option<Rc<CircularNode<T>>> {
        match self {
            CircularList::Empty => None,
            CircularList::NonEmpty(node) => {
                let orig_node_copy = node_remove(&node);
                let c_rc = node_get_neighbor(&node, Direction::Clockwise);

                if Rc::ptr_eq(&node, &c_rc) {
                    *self = CircularList::Empty;
                } else {
                    *node = c_rc;
                }

                Some(orig_node_copy)
            }
        }
    }

    fn has_multiple_nodes(&self) -> bool {
        match self {
            CircularList::Empty => false,
            CircularList::NonEmpty(node) => {
                let c_rc = node_get_neighbor(&node, Direction::Clockwise);
                !Rc::ptr_eq(&node, &c_rc)
            }
        }
    }
}

impl<T> Drop for CircularList<T> {
    fn drop(&mut self) {
        while self.has_multiple_nodes() {
            self.remove();
        }
    }
}

impl<T> CircularList<T> {
    fn rotate_clockwise(&mut self) {
        match self {
            CircularList::Empty => (),
            CircularList::NonEmpty(node) => {
                let cc_rc = node_get_neighbor(&node, Direction::CounterClockwise);
                let c_rc = node_get_neighbor(&node, Direction::Clockwise);

                *cc_rc.clockwise.borrow_mut() = MaybeRc::Rc(Rc::clone(&node));
                *node.clockwise.borrow_mut() = MaybeRc::Weak(Rc::downgrade(&c_rc));
                *node = c_rc;
            }
        }
    }
}

#[derive(Debug)]
struct CupsGameState {
    cups: CircularList<Cup>,
    min_cup_val: Option<usize>,
    max_cup_val: Option<usize>,
    locs: HashMap<Cup, Weak<CircularNode<Cup>>>,
}

const CUPS_PER_MOVE: usize = 3;

impl CupsGameState {
    fn new(vals: Vec<Cup>) -> Self {
        let mut vals_iter = vals.into_iter();
        let mut res = CupsGameState {
            cups: CircularList::Empty,
            min_cup_val: None,
            max_cup_val: None,
            locs: HashMap::new(),
        };

        if let Some(first_val) = vals_iter.next() {
            let mut min_val = first_val.0;
            let mut max_val = first_val.0;

            let mut last_node = res.cups.insert_clockwise(first_val);
            res.locs.insert(first_val, Rc::downgrade(&last_node));

            for val in vals_iter {
                min_val = std::cmp::min(min_val, val.0);
                max_val = std::cmp::max(max_val, val.0);

                last_node = node_insert_clockwise(&last_node, NodeOrValue::Value(val));
                res.locs.insert(val, Rc::downgrade(&last_node));
            }

            res.min_cup_val = Some(min_val);
            res.max_cup_val = Some(max_val);
        }
        res
    }

    fn perform_move(&mut self) {
        let node = self.cups.primary_node().unwrap();
        let min_val = self.min_cup_val.unwrap();
        let max_val = self.max_cup_val.unwrap();

        let mut removed_neighbors = vec![];
        let mut removed_values = vec![];
        for _ in 0..CUPS_PER_MOVE {
            let neighbor = node_get_neighbor(&node, Direction::Clockwise);
            node_remove(&neighbor);
            removed_values.push(neighbor.val);
            removed_neighbors.push(neighbor);
        }

        let mut destination = Cup(node.val.0 - 1);
        if destination.0 < min_val {
            destination.0 = max_val;
        }
        while removed_values.contains(&destination) {
            destination.0 -= 1;
            if destination.0 < min_val {
                destination.0 = max_val;
            }
        }

        let dest_loc = self
            .locs
            .get(&destination)
            .unwrap_or_else(|| panic!("Destination {} not found!", destination.0))
            .upgrade()
            .expect(BAD_NODE_ERR);
        for node in removed_neighbors.into_iter().rev() {
            node_insert_clockwise(&dest_loc, NodeOrValue::Node(node));
        }
    }

    fn perform_moves_and_find(&mut self, num_moves: usize, label: Cup) -> Rc<CircularNode<Cup>> {
        for _ in 0..num_moves {
            self.perform_move();
            self.cups.rotate_clockwise();
        }

        let mut cur_node = self.cups.primary_node().unwrap();
        while cur_node.val != label {
            cur_node = node_get_neighbor(&cur_node, Direction::Clockwise);
        }
        cur_node
    }
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let mut input = fs::read_to_string("in.txt")?
        .chars()
        .map(|ch| Cup(((ch as u8) - b'0') as usize))
        .collect::<Vec<_>>();
    let mut state = CupsGameState::new(input.clone());

    let one_node = state.perform_moves_and_find(100, Cup(1));
    let vals = node_map_into_vec(&one_node, |node| node.val);
    print!("Part 1: ");
    for val in vals.iter().skip(1) {
        print!("{}", val.0);
    }
    println!();

    for num in 10..=1_000_000 {
        input.push(Cup(num));
    }
    let mut state = CupsGameState::new(input.into_iter().collect());
    let one_node = state.perform_moves_and_find(10_000_000, Cup(1));
    let neighbor1 = node_get_neighbor(&one_node, Direction::Clockwise);
    let neighbor2 = node_get_neighbor(&neighbor1, Direction::Clockwise);
    println!("Part 2: {}", neighbor1.val.0 * neighbor2.val.0);

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_operations() {
        let mut lst = CircularList::Empty;
        for val in vec![3, 8, 9, 1, 2, 5, 4, 6, 7].into_iter() {
            lst.insert_clockwise(val);
        }

        assert_eq!(
            lst.map_into_vec(|node| node.val.clone()),
            vec![3, 7, 6, 4, 5, 2, 1, 9, 8]
        );
        assert_eq!(
            lst.map_into_vec(|node| Rc::strong_count(node)),
            vec![2, 2, 2, 2, 2, 2, 2, 2, 2]
        );
        assert_eq!(
            lst.map_into_vec(|node| Rc::weak_count(node)),
            vec![2, 1, 1, 1, 1, 1, 1, 1, 1]
        );
        lst.insert_counterclockwise(5);
        assert_eq!(
            lst.map_into_vec(|node| node.val.clone()),
            vec![3, 7, 6, 4, 5, 2, 1, 9, 8, 5]
        );
        assert_eq!(
            lst.map_into_vec(|node| Rc::strong_count(node)),
            vec![2, 2, 2, 2, 2, 2, 2, 2, 2, 2]
        );
        assert_eq!(
            lst.map_into_vec(|node| Rc::weak_count(node)),
            vec![2, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        );
    }

    fn example_list() -> CircularList<Cup> {
        CircularList::new(
            vec![3, 8, 9, 1, 2, 5, 4, 6, 7]
                .into_iter()
                .map(Cup)
                .collect(),
        )
    }

    #[test]
    fn test_example() {
        let lst = example_list();
        assert_eq!(
            lst.map_into_vec(|node| node.val.clone()),
            vec![
                Cup(3),
                Cup(8),
                Cup(9),
                Cup(1),
                Cup(2),
                Cup(5),
                Cup(4),
                Cup(6),
                Cup(7)
            ]
        );
        assert_eq!(
            lst.map_into_vec(|node| Rc::strong_count(node)),
            vec![2, 2, 2, 2, 2, 2, 2, 2, 2]
        );
        assert_eq!(
            lst.map_into_vec(|node| Rc::weak_count(node)),
            vec![2, 1, 1, 1, 1, 1, 1, 1, 1]
        );
    }

    #[test]
    fn test_remove_begin() {
        let mut cups = example_list();
        cups.remove();
        assert_eq!(
            cups.map_into_vec(|node| node.val.clone()),
            vec![
                Cup(8),
                Cup(9),
                Cup(1),
                Cup(2),
                Cup(5),
                Cup(4),
                Cup(6),
                Cup(7)
            ]
        );
        assert_eq!(
            cups.map_into_vec(|node| Rc::strong_count(node)),
            vec![2, 2, 2, 2, 2, 2, 2, 2]
        );
        assert_eq!(
            cups.map_into_vec(|node| Rc::weak_count(node)),
            vec![2, 1, 1, 1, 1, 1, 1, 1]
        );

        let mut cur_node = cups.primary_node().unwrap();
        for _ in 0..100 {
            assert!(cur_node.val.0 != 3);
            cur_node = node_get_neighbor(&cur_node, Direction::Clockwise);
        }
    }

    #[test]
    fn test_remove_middle() {
        let cups = example_list();

        {
            let cur_node = cups.primary_node().unwrap();
            let cur_node = node_get_neighbor(&cur_node, Direction::Clockwise);
            node_remove(&cur_node);
        }

        assert_eq!(
            cups.map_into_vec(|node| node.val.clone()),
            vec![
                Cup(3),
                Cup(9),
                Cup(1),
                Cup(2),
                Cup(5),
                Cup(4),
                Cup(6),
                Cup(7)
            ]
        );
        assert_eq!(
            cups.map_into_vec(|node| Rc::strong_count(node)),
            vec![2, 2, 2, 2, 2, 2, 2, 2]
        );
        assert_eq!(
            cups.map_into_vec(|node| Rc::weak_count(node)),
            vec![2, 1, 1, 1, 1, 1, 1, 1]
        );

        let mut cur_node = cups.primary_node().unwrap();
        for _ in 0..100 {
            assert!(cur_node.val.0 != 8);
            cur_node = node_get_neighbor(&cur_node, Direction::Clockwise);
        }
    }

    #[test]
    fn test_remove_end() {
        let cups = example_list();

        {
            let cur_node = cups.primary_node().unwrap();
            let cur_node = node_get_neighbor(&cur_node, Direction::CounterClockwise);
            node_remove(&cur_node);
        }

        assert_eq!(
            cups.map_into_vec(|node| node.val.clone()),
            vec![
                Cup(3),
                Cup(8),
                Cup(9),
                Cup(1),
                Cup(2),
                Cup(5),
                Cup(4),
                Cup(6)
            ]
        );
        assert_eq!(
            cups.map_into_vec(|node| Rc::strong_count(node)),
            vec![2, 2, 2, 2, 2, 2, 2, 2]
        );
        assert_eq!(
            cups.map_into_vec(|node| Rc::weak_count(node)),
            vec![2, 1, 1, 1, 1, 1, 1, 1]
        );

        let mut cur_node = cups.primary_node().unwrap();
        for _ in 0..100 {
            assert!(cur_node.val.0 != 7);
            cur_node = node_get_neighbor(&cur_node, Direction::Clockwise);
        }
    }

    fn example_state() -> CupsGameState {
        CupsGameState::new(
            vec![3, 8, 9, 1, 2, 5, 4, 6, 7]
                .into_iter()
                .map(Cup)
                .collect(),
        )
    }

    #[test]
    fn test_move_and_find() {
        let mut state = example_state();
        {
            let one_node = state.perform_moves_and_find(10, Cup(1));
            assert_eq!(
                node_map_into_vec(&one_node, |node| node.val.clone()),
                vec![
                    Cup(1),
                    Cup(9),
                    Cup(2),
                    Cup(6),
                    Cup(5),
                    Cup(8),
                    Cup(3),
                    Cup(7),
                    Cup(4)
                ]
            );
        }
        assert_eq!(
            state.cups.map_into_vec(|node| Rc::strong_count(node)),
            vec![2, 2, 2, 2, 2, 2, 2, 2, 2]
        );
        assert_eq!(
            state.cups.map_into_vec(|node| Rc::weak_count(node)),
            vec![3, 2, 2, 2, 2, 2, 2, 2, 2]
        );

        {
            let one_node = state.perform_moves_and_find(90, Cup(1));
            assert_eq!(
                node_map_into_vec(&one_node, |node| node.val.clone()),
                vec![
                    Cup(1),
                    Cup(6),
                    Cup(7),
                    Cup(3),
                    Cup(8),
                    Cup(4),
                    Cup(5),
                    Cup(2),
                    Cup(9)
                ]
            );
        }
        assert_eq!(
            state.cups.map_into_vec(|node| Rc::strong_count(node)),
            vec![2, 2, 2, 2, 2, 2, 2, 2, 2]
        );
        assert_eq!(
            state.cups.map_into_vec(|node| Rc::weak_count(node)),
            vec![3, 2, 2, 2, 2, 2, 2, 2, 2]
        );
    }
}
