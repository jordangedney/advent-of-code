use std::fs;

fn parse() -> Vec<((i32, i32), (i32, i32))> {
    // Giving up 😢
    // let input = fs::read_to_string("../inputs/4").expect("no file?");
    // let data = input.trim().split("\n")
    //            .map(|x| x.split(","))
    //            .map(|x| x.map(|y| y.split("-")))
    //            .map(|x| x.map(|y| y.map(|z| z.parse::<i32>())));

    let input = fs::read_to_string("../inputs/4").expect("no file?");
    let mut data = Vec::new();
    for line in input.trim().split("\n") {
        let mut tmp = Vec::new();
        for assigned in line.split(","){
            let range: Vec<&str> = assigned.split("-").collect();
            tmp.push((range[0].parse().unwrap(),
                      range[1].parse().unwrap()))
        }
        data.push((tmp[0], tmp[1]));
    }
    data
}

pub fn main() {
    let data = parse();

    // part one
    fn contains (((x, y), (a, b)): ((i32, i32), (i32, i32))) -> bool {
        (x <= a && y >=b) || (a <= x && b >= y)
    }

    println!("{:?}", data.iter().filter(|x| contains(**x)).collect::<Vec<_>>().len());

    // part two
    fn any_elem (((x, y), (a, b)): ((i32, i32), (i32, i32))) -> bool {
        (x..=y).any(|i| (a..=b).contains(&i))
    }

    println!("{:?}", data.iter().filter(|x| any_elem(**x)).collect::<Vec<_>>().len())

}
