pub fn solution1(input: &String) -> i64 {
    input
        .lines()
        .map(|l| {
            let digits = l.chars().filter(|c| c.is_numeric()).collect::<Vec<char>>();
            format!("{}{}", digits[0], digits.last().unwrap())
                .parse::<i64>()
                .unwrap()
        })
        .sum()
}

pub fn solution2(input: &String) -> i64 {
    fn first_and_last(mut s: &str) -> (i64, i64) {
        const PATTERNS: [&str; 9] = [
            "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
        ];

        let mut first = None;
        let mut last = None;

        while s.len() > 0 {
            let begin = s.chars().nth(0).unwrap();
            let digit = if begin.is_digit(10) {
                begin as i64 - '0' as i64
            } else {
                let mut result = None;
                for (value, pattern) in PATTERNS.iter().enumerate() {
                    if s.starts_with(pattern) {
                        result = Some(value as i64 + 1);
                        break;
                    };
                }

                result.unwrap_or(-1)
            };

            if digit == -1 {
            } else if first.is_none() {
                first = Some(digit);
            } else {
                last = Some(digit);
            }

            (_, s) = s.split_at(1);
        }

        if let (Some(only_digit), None) = (first, last) {
            (only_digit, only_digit)
        } else {
            (first.unwrap(), last.unwrap())
        }
    }

    input
        .lines()
        .map(|l| {
            let (first, last) = first_and_last(l);
            first * 10 + last
        })
        .sum()
}
