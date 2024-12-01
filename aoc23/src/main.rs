use std::{env::args, fs::read};

mod days;
use crate::days::*;

type Solution = fn(&String) -> i64;

const SOLUTIONS: &'static [(Solution, Solution)] = &[
    (day01::solution1, day01::solution2),
    (day02::solution1, day02::solution2),
];

fn main() -> Result<(), std::io::Error> {
    let day: u8 = args()
        .nth(1)
        .map(|s| s.parse().unwrap())
        .filter(|&n| 0 < n && n <= 25)
        .expect("pass a number between 1 and 25");

    let input = read_input_file(day)?;

    println!("## solution for day {}", day);

    let (solve1, solve2) = SOLUTIONS[day as usize - 1];
    println!("solution 1: {}", solve1(&input));
    println!("solution 2: {}", solve2(&input));

    Ok(())
}

fn read_input_file(day: u8) -> Result<String, std::io::Error> {
    let file_name = format!("input/day{}.txt", day);
    return Ok(String::from_utf8(read(file_name)?).unwrap());
}
