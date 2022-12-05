use itertools::Itertools;
use std::fs;
use std::collections::HashSet;

pub fn main() {
    let input = fs::read_to_string("../inputs/3").expect("no file?");
    let data = input.trim().split("\n");

    // part one
    fn halve(s: &str) -> (String, String) {
        (s[0..s.len() / 2].to_string(), s[s.len() / 2..].to_string())
    }

    fn intersection((fst, snd): (String, String)) -> Vec<char> {
        let mut a: HashSet<char> = HashSet::from_iter(fst.chars());
        let b: HashSet<char> = HashSet::from_iter(snd.chars());
        a.retain(|&k| b.contains(&k));
        a.into_iter().collect()
    }

    fn priority(c: &char) -> i32 {
        ('a'..='z').chain('A'..='Z').position(|l| l == *c).unwrap() as i32 + 1
    }

    println!("{:?}", data.clone()
                         .map(halve)
                         .map(intersection)
                         .concat()
                         .iter()
                         .map(priority)
                         .sum::<i32>());

    // part two
    println!("{:?}", data.collect::<Vec<_>>()
                         .chunks(3)
                         // intersect three:
                         .map(|xs| intersection(
                                     (intersection((xs[0].to_string(),
                                                    xs[1].to_string()))
                                        .into_iter()
                                        .collect(),
                                     xs[2].to_string())))
                         .concat()
                         .iter()
                         .map(priority)
                         .sum::<i32>());

}
