mod parser;

use parser::parse_json;
use std::env;

fn main() {
    let mut args = env::args();
    args.next();
    let json = args.next().expect("No json argument.");

    match parse_json(&json) {
        Some(value) => println!("SUCCESS: Parsed as {value:?}"),
        None => println!("FAILURE: Could not parse input."),
    }
}
