#[derive(Default)]
struct Draw {
    r: i64,
    g: i64,
    b: i64,
}

impl Draw {
    fn r(r: i64) -> Self {
        Draw { r, g: 0, b: 0 }
    }
    fn g(g: i64) -> Self {
        Draw { r: 0, g, b: 0 }
    }
    fn b(b: i64) -> Self {
        Draw { r: 0, g: 0, b }
    }

    fn add(self, other: Self) -> Self {
        Draw {
            r: self.r + other.r,
            g: self.g + other.g,
            b: self.b + other.b,
        }
    }

    fn max(self, other: &Self) -> Self {
        Draw {
            r: self.r.max(other.r),
            g: self.g.max(other.g),
            b: self.b.max(other.b),
        }
    }
}

fn parse_game(game: &str) -> (i64, Vec<Draw>) {
    let (game, revealed) = game.split_once(":").unwrap();
    let id = game.split_once(" ").unwrap().1.parse().unwrap();

    let draws = revealed
        .split_terminator(";")
        .map(parse_draws)
        .collect::<Vec<_>>();

    (id, draws)
}

fn parse_draws(balls: &str) -> Draw {
    balls
        .split_terminator(",")
        .map(|balls| {
            let (n, color) = balls.split_once(" ").unwrap().1.split_once(" ").unwrap();
            let amount = n.parse().unwrap();
            match color.chars().nth(0).unwrap() {
                'r' => Draw::r(amount),
                'g' => Draw::g(amount),
                'b' => Draw::b(amount),
                _ => panic!("not rgb"),
            }
        })
        .fold(Draw::default(), Draw::add)
}

pub fn solution1(input: &String) -> i64 {
    input
        .lines()
        .map(parse_game)
        .filter(|(_, draws)| {
            draws
                .iter()
                .all(|draw| draw.r <= 12 && draw.g <= 13 && draw.b <= 14)
        })
        .fold(0, |acc, (id, _)| acc + id)
}

pub fn solution2(input: &String) -> i64 {
    input
        .lines()
        .map(|l| parse_game(l).1)
        .map(|draws| draws.iter().fold(Draw::default(), Draw::max))
        .map(|draw| draw.r * draw.g * draw.b)
        .sum()
}
