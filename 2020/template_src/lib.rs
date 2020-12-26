use std::error::Error;
use std::fs;

pub fn run() -> Result<(), Box<dyn Error>> {
    //parser::parse(&fs::read_to_string("in.txt")?[..])?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
}
