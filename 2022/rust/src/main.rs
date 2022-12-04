use std::{
    fs::File,
    io:: {BufReader, BufRead},
};

fn parse() -> Vec<i32> {
    let br = BufReader::new(File::open("../inputs/1").unwrap());

    let mut v = Vec::new();
    let mut tmp = Vec::new();

    for line in br.lines() {
        let line = line.unwrap();
        if let Ok(result) = line.parse::<i32>() {
           tmp.push(result);
        } else {
            v.push(tmp.iter().sum());
            tmp = Vec::new();
        }
    }
    v
}

fn main() {
    let mut data = parse();

    // PartOne:
    println!("{:?}", data.iter().fold(std::i32::MIN, |a, b| a.max(*b)));
    // PartTwo:
    data.sort();
    data.reverse();
    println!("{:?}", data.iter().take(3).sum::<i32>());
}
