fn main() {
    std::process::exit(match day06::run() {
        Ok(_) => 0,
        Err(err) => {
            eprintln!("Error: {}", err);
            1
        }
    });
}