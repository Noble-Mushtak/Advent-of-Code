use itertools::Itertools;
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
    all_allergens: HashSet<Allergen>,
    allergen_map: HashMap<Ingredient, HashSet<Allergen>>,
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
    let mut allergen_map = all_ingredients
        .clone()
        .into_iter()
        .map(|ingredient| (ingredient, HashSet::new()))
        .collect::<HashMap<_, _>>();

    for allergen in all_allergens.iter() {
        for ingredient in foods
            .iter()
            .filter(|food| food.definite_allergens.contains(allergen))
            .map(|food| food.ingredients.clone())
            .fold(all_ingredients.clone(), hashset_intersection)
        {
            allergen_map
                .get_mut(&ingredient)
                .unwrap()
                .insert(allergen.clone());
        }
    }

    AllergenInfo {
        all_allergens,
        allergen_map,
    }
}

fn calc_true_allergen_map(info: AllergenInfo) -> HashMap<Ingredient, Allergen> {
    let allergens_vec = info.all_allergens.into_iter().collect::<Vec<_>>();
    let ingreds_vec = info
        .allergen_map
        .iter()
        .map(|(ingred, _)| ingred)
        .cloned()
        .collect::<Vec<_>>();

    let mut capacities = HashMap::new();
    let mut edges = HashMap::new();
    let mut add_edge = |v1, v2| edges.entry(v1).or_insert(vec![]).push(v2);
    let mut add_edge_with_capacity = |v1, v2| {
        add_edge(v1, v2);
        add_edge(v2, v1);
        capacities.insert((v1, v2), 1);
    };

    let source = ingreds_vec.len() + allergens_vec.len();
    let sink = ingreds_vec.len() + allergens_vec.len() + 1;
    for (i, ingred) in ingreds_vec.iter().enumerate() {
        add_edge_with_capacity(source, i);
        for allergen in info.allergen_map[ingred].iter() {
            add_edge_with_capacity(
                i,
                ingreds_vec.len()
                    + allergens_vec
                        .iter()
                        .position(|allergen2| allergen2 == allergen)
                        .unwrap(),
            );
        }
    }
    for i in 0..allergens_vec.len() {
        add_edge_with_capacity(ingreds_vec.len() + i, sink);
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
                ingreds_vec[i].clone(),
                allergens_vec[j - ingreds_vec.len()].clone(),
            );
        }
    }
    mapping
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let foods = parser::parse(&fs::read_to_string("in.txt")?[..])?;

    let mut info = calc_allergen_info(&foods[..]);
    let good_ingredients = info
        .allergen_map
        .iter()
        .filter(|(_, poss_allergens)| poss_allergens.is_empty())
        .map(|(ingred, _)| ingred)
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

    info.allergen_map = info
        .allergen_map
        .into_iter()
        .filter(|(_, poss_allergens)| !poss_allergens.is_empty())
        .collect();
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
            info.allergen_map
                .iter()
                .filter(|(ingredient, poss_allergens)| poss_allergens.is_empty())
                .map(|(ingred, _)| ingred.clone())
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
