#[path = "util.rs"]
mod util;

pub fn part_one(input: &Vec<i32>) -> usize {
    return input.iter()
                .zip(input[1..].iter())
                .filter(|(a, b)| b > a)
                .count();
}

pub fn part_two(input: &Vec<i32>) -> usize {
    let sliding_window = input.iter()
                              .zip(input[1..].iter())
                              .zip(input[2..].iter())
                              .map(|((a, b), c)| (a + b + c))
                              .collect();
    return part_one(&sliding_window)
}

pub fn do_main() {
    let lines = util::read_lines("../inputs/1");
    let parsed: Vec<i32> = lines.iter()
                                .map(|x| x.parse::<i32>().unwrap())
                                .collect();

    println!("{:?}", part_one(&parsed));
    println!("{:?}", part_two(&parsed));
}
