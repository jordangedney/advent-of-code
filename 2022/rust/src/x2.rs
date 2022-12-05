use std::{
    fs,
};

#[derive(Debug, Clone, Copy, PartialEq)]
enum Move
  { Rock = 1
  , Paper
  , Scissors
  // Even hackier the second time
  , Lose
  , Draw
  , Win
  }

fn parse(translate : fn(token: &str) -> Move) -> Vec<Vec<Move>>{
    let input = fs::read_to_string("../inputs/2").expect("no file?");
    let lines = input.trim()
                     .split("\n")
                     .map(|x| x.split(" "))
                     .map(|xs| xs.map(|y| translate(y)))
                     .map(|x| x.collect())
                     .collect();
    lines
}

pub fn main() {
    // part one:
    fn translate_p1 (token: &str) -> Move {
        match token
        { "A" => Move::Rock
        , "B" => Move::Paper
        , "C" => Move::Scissors
        , "X" => Move::Rock
        , "Y" => Move::Paper
        , "Z" => Move::Scissors
        , &_ => panic!("you done fucked")
        }
    }

    fn won (round: Vec<Move>) -> bool {
        match round[..]
            { [Move::Scissors, Move::Rock]     => true
            , [Move::Rock,     Move::Paper]    => true
            , [Move::Paper,    Move::Scissors] => true
            , _                                => false
            }
    }

    fn score_round (round: Vec<Move>) -> i32 {
        if round [0] == round[1] { 3 }
        else if won(round) { 6 }
        else { 0 }
    }

    let data = parse(translate_p1)
        .iter()
        .map(|r| r[1] as i32 + score_round(r.to_vec()))
        .sum::<i32>();

    println!("{:?}", data);

    // part two:
    fn translate_p2 (token: &str) -> Move {
        match token
        { "A" => Move::Rock
        , "B" => Move::Paper
        , "C" => Move::Scissors
        , "X" => Move::Lose
        , "Y" => Move::Draw
        , "Z" => Move::Win
        , &_ => panic!("you done fucked")
        }
    }

    fn get_move (round: Vec<Move>) -> Move {
        match round[1]
        { Move::Lose =>
          match round[0]
          { Move::Rock     => Move::Scissors
          , Move::Paper    => Move::Rock
          , Move::Scissors => Move::Paper
          , _ => panic!("you done fucked")
          }
        , Move::Draw =>
          match round[0]
          { Move::Rock     => Move::Rock
          , Move::Paper    => Move::Paper
          , Move::Scissors => Move::Scissors
          , _ => panic!("you done fucked")
          }
        , Move::Win =>
          match round[0]
          { Move::Rock     => Move::Paper
          , Move::Paper    => Move::Scissors
          , Move::Scissors => Move::Rock
          , _ => panic!("you done fucked")
          }
        , _ => panic!("you done fucked")
        }
    }

    let data = parse(translate_p2)
        .iter()
        .map(|r| vec!(r[0], get_move(r.to_vec())))
        .map(|r| r[1] as i32 + score_round(r.to_vec()))
        .sum::<i32>();

    println!("{:?}", data)
}
