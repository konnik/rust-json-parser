mod parser;

use parser::parse_json;

fn main() {
    match parse_json("null") {
        Some(value) => println!("SUCCESS: Parsed as {value:?}"),
        None => print!("FAILURE: Could not parse input."),
    }
}
