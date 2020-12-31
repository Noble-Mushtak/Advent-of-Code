use itertools::Itertools;
use multimap::MultiMap;
use std::collections::{HashMap, HashSet, VecDeque};
use std::error::Error;
use std::fs;
use std::hash::Hash;

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
struct Ingredient(String);

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
struct Allergen(String);

#[derive(PartialEq, Debug)]
struct Food {
    ingredients: HashSet<Ingredient>,
    definite_allergens: HashSet<Allergen>,
}

peg::parser! {
    grammar parser() for str {
        rule word() -> String
          = word:$(['a'..='z']+) { word.to_string() }

        rule food() -> Food
          = ingredients:(word()**" ") " (contains " allergens:(word()**", ") ")" {
            Food {
                ingredients: ingredients.into_iter().map(Ingredient).collect(),
                definite_allergens: allergens.into_iter().map(Allergen).collect()
            }
        }

        pub(crate) rule parse() -> Vec<Food>
          = foods:(food()**"\n") { foods }
    }
}

#[derive(PartialEq, Debug)]
struct AllergenInfo {
    all_allergens: Vec<Allergen>,
    all_ingredients: Vec<Ingredient>,
    allergen_map: MultiMap<Ingredient, Allergen>,
}

fn hashset_union<T: Eq + Hash + Clone>(set1: HashSet<T>, set2: HashSet<T>) -> HashSet<T> {
    set1.union(&set2).cloned().collect()
}

fn hashset_intersection<T: Eq + Hash + Clone>(set1: HashSet<T>, set2: HashSet<T>) -> HashSet<T> {
    set1.intersection(&set2).cloned().collect()
}

fn calc_allergen_info(foods: &[Food]) -> AllergenInfo {
    let all_allergens = foods
        .iter()
        .map(|food| food.definite_allergens.clone())
        .fold(HashSet::new(), hashset_union);
    let all_ingredients = foods
        .iter()
        .map(|food| food.ingredients.clone())
        .fold(HashSet::new(), hashset_union);
    let mut allergen_map = MultiMap::new();

    for allergen in all_allergens.iter() {
        for ingredient in foods
            .iter()
            .filter(|food| food.definite_allergens.contains(allergen))
            .map(|food| food.ingredients.clone())
            .fold(all_ingredients.clone(), hashset_intersection)
        {
            allergen_map.insert(ingredient.clone(), allergen.clone());
        }
    }

    AllergenInfo {
        all_allergens: all_allergens.into_iter().collect(),
        all_ingredients: all_ingredients.into_iter().collect(),
        allergen_map,
    }
}

fn calc_true_allergen_map(info: AllergenInfo) -> HashMap<Ingredient, Allergen> {
    let mut capacities = HashMap::new();
    let mut edges = HashMap::new();
    let mut add_edge = |v1, v2| edges.entry(v1).or_insert(vec![]).push(v2);
    let mut add_edge_with_capacity = |v1, v2| {
        add_edge(v1, v2);
        add_edge(v2, v1);
        capacities.insert((v1, v2), 1);
    };

    let source = info.all_ingredients.len() + info.all_allergens.len();
    let sink = info.all_ingredients.len() + info.all_allergens.len() + 1;
    for (i, ingred) in info.all_ingredients.iter().enumerate() {
        add_edge_with_capacity(source, i);
        for allergen in info.allergen_map.get_vec(ingred).unwrap_or(&vec![]) {
            add_edge_with_capacity(
                i,
                info.all_ingredients.len()
                    + info
                        .all_allergens
                        .iter()
                        .position(|allergen2| allergen2 == allergen)
                        .unwrap(),
            );
        }
    }
    for i in 0..info.all_allergens.len() {
        add_edge_with_capacity(info.all_ingredients.len() + i, sink);
    }

    //From Day 16
    let mut flow = HashMap::new();
    let find_path_with_flow = |flow: &HashMap<_, _>| {
        let mut visited = HashSet::new();
        let mut last_vert = HashMap::new();
        let mut queue = VecDeque::new();

        visited.insert(source);
        queue.push_back(source);
        while let Some(cur_vert) = queue.pop_front() {
            if cur_vert == sink {
                let mut path = vec![sink];
                let mut cur_vert = &sink;
                while let Some(prev_vert) = last_vert.get(cur_vert) {
                    path.insert(0, *prev_vert);
                    cur_vert = prev_vert;
                }
                return Some(path);
            }
            for &next_vert in edges.get(&cur_vert).unwrap_or(&vec![]) {
                if !visited.contains(&next_vert)
                    && flow.get(&(cur_vert, next_vert)).unwrap_or(&0)
                        < capacities.get(&(cur_vert, next_vert)).unwrap_or(&0)
                {
                    last_vert.insert(next_vert, cur_vert);
                    visited.insert(next_vert);
                    queue.push_back(next_vert);
                }
            }
        }
        None
    };

    while let Some(path) = find_path_with_flow(&flow) {
        for (&v1, &v2) in path.iter().tuple_windows() {
            match flow.get_mut(&(v1, v2)) {
                Some(val) => *val += 1,
                None => {
                    flow.insert((v1, v2), 1);
                }
            }
            match flow.get_mut(&(v2, v1)) {
                Some(val) => *val -= 1,
                None => {
                    flow.insert((v2, v1), -1);
                }
            }
        }
    }

    let mut mapping = HashMap::new();
    for ((i, j), fl) in flow {
        if fl > 0 && i != source && j != sink {
            mapping.insert(
                info.all_ingredients[i].clone(),
                info.all_allergens[j - info.all_ingredients.len()].clone(),
            );
        }
    }
    mapping
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let foods = parser::parse(&fs::read_to_string("in.txt")?[..])?;

    let info = calc_allergen_info(&foods[..]);
    let good_ingredients = info
        .all_ingredients
        .iter()
        .filter(|ingred| !info.allergen_map.contains_key(ingred))
        .collect::<HashSet<_>>();
    println!(
        "Part 1: {}",
        foods
            .iter()
            .map(|food| food.ingredients.clone())
            .flatten()
            .filter(|ingred| good_ingredients.contains(ingred))
            .count()
    );

    let mut true_list = calc_true_allergen_map(info).into_iter().collect::<Vec<_>>();
    true_list.sort_by(|(_, allergen1), (_, allergen2)| allergen1.0.cmp(&allergen2.0));
    println!(
        "Part 2: {}",
        true_list
            .iter()
            .map(|(ingred, _)| ingred.0.clone())
            .join(",")
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        assert!(parser::parse(
            "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)"
        )
        .is_ok());
    }

    fn example_data() -> Vec<Food> {
        parser::parse(
            "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)",
        )
        .unwrap()
    }

    #[test]
    fn test_allergen_info_and_mapping() {
        let foods = example_data();
        let info = calc_allergen_info(&foods);
        assert_eq!(
            info.all_ingredients
                .iter()
                .cloned()
                .collect::<HashSet<_>>()
                .difference(&info.allergen_map.keys().cloned().collect::<HashSet<_>>())
                .cloned()
                .collect::<HashSet<_>>(),
            vec!["kfcds", "nhms", "sbzzf", "trh"]
                .into_iter()
                .map(|slice| Ingredient(slice.to_string()))
                .collect::<HashSet<_>>()
        );

        let map = calc_true_allergen_map(info);
        assert_eq!(
            map,
            vec![
                (
                    Ingredient(String::from("mxmxvkd")),
                    Allergen(String::from("dairy"))
                ),
                (
                    Ingredient(String::from("sqjhc")),
                    Allergen(String::from("fish"))
                ),
                (
                    Ingredient(String::from("fvjkl")),
                    Allergen(String::from("soy"))
                )
            ]
            .into_iter()
            .collect()
        );
    }
}
