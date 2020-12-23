use snafu::Snafu;
use std::error::Error;
use std::fs;
use std::str::FromStr;

pub fn run() -> Result<(), Box<dyn Error>> {
    fs::read_to_string("in.txt")?.parse()?;
}

#[cfg(test)]
mod tests {
    use super::*;
}
