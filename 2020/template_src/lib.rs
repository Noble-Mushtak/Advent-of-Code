use std::fs;
use std::error::Error;
use std::str::FromStr;
use snafu::Snafu;

pub fn run() -> Result<(), Box<dyn Error>> {
    fs::read_to_string("in.txt")?.parse()?;
}

#[cfg(test)]
mod tests {
    use super::*;
}
