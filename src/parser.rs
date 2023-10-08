#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum JsonValue {
    Null,
    Bool(bool),
}

/// Parse a string into a JsonValue.
pub fn parse_json<'a>(input: &'a str) -> Option<JsonValue> {
    match element().parse(input) {
        Some(("", value)) => Some(value),
        _ => None,
    }
}

fn element<'a>() -> impl Parser<'a, JsonValue> {
    ws().followed_by(value())
        .and_then(|v| ws().and_then(move |_| BoxParser::new(succeed(v))))
}

fn value<'a>() -> impl Parser<'a, JsonValue> {
    return js_null().or(js_true()).or(js_false());
}

fn js_null<'a>() -> impl Parser<'a, JsonValue> {
    literal("null").map(|_| JsonValue::Null)
}

fn js_true<'a>() -> BoxParser<'a, JsonValue> {
    char('t')
        .followed_by(char('r'))
        .followed_by(char('u'))
        .followed_by(char('e'))
        .map(|_| JsonValue::Bool(true))
}

fn js_false<'a>() -> impl Parser<'a, JsonValue> {
    literal("false").map(|_| JsonValue::Bool(false))
}

type ParseResult<'a, T> = Option<(&'a str, T)>;

/// Parser trait
trait Parser<'a, A> {
    /// Run a parser on the given input string.
    fn parse(&self, input: &'a str) -> ParseResult<'a, A>;

    /// Transforms the value of a parser to another value using the
    /// provided function.
    /// Yes, you're right, this makes our parser a functor!
    fn map<F, B>(self, f: F) -> BoxParser<'a, B>
    where
        Self: Sized + 'a,
        A: 'a,
        B: 'a,
        F: Fn(A) -> B + 'a,
    {
        BoxParser::new(move |input| {
            self.parse(input)
                .map(|(next_input, result)| (next_input, f(result)))
        })
    }

    /// Combines two parsers into one which tries  
    /// the first parser and if it fails tries the second parser.
    fn or(self, other: impl Parser<'a, A> + 'a) -> BoxParser<'a, A>
    where
        Self: Sized + 'a,
        A: 'a,
    {
        return BoxParser::new(move |input: &'a str| {
            self.parse(input).or_else(|| other.parse(input))
        });
    }

    /// Make our parser a monad!
    fn and_then<F, B>(self, f: F) -> BoxParser<'a, B>
    where
        Self: Sized + 'a,
        B: 'a,
        F: Fn(A) -> BoxParser<'a, B> + 'a,
    {
        BoxParser::new(move |input: &'a str| {
            self.parse(input)
                .and_then(|(rest, a_value)| f(a_value).parse(rest))
        })
    }

    /// The >> operator in Haskell
    fn followed_by<B>(self, next_parser: impl Parser<'a, B> + 'a) -> BoxParser<'a, B>
    where
        Self: Sized + 'a,
        B: 'a,
    {
        BoxParser::new(move |input: &'a str| {
            self.parse(input)
                .and_then(|(rest, _)| next_parser.parse(rest))
        })
    }
}

struct BoxParser<'a, T> {
    parser: Box<dyn Parser<'a, T> + 'a>,
}

impl<'a, T> BoxParser<'a, T> {
    fn new<P>(parser: P) -> Self
    where
        P: Parser<'a, T> + 'a,
    {
        BoxParser {
            parser: Box::new(parser),
        }
    }
}

impl<'a, T> Parser<'a, T> for BoxParser<'a, T> {
    fn parse(&self, input: &'a str) -> ParseResult<'a, T> {
        self.parser.parse(input)
    }
}

impl<'a, F, T> Parser<'a, T> for F
where
    F: Fn(&'a str) -> ParseResult<T>,
{
    fn parse(&self, input: &'a str) -> ParseResult<'a, T> {
        self(input)
    }
}

fn succeed<'a, T>(value: T) -> impl Parser<'a, T>
where
    T: Clone,
{
    move |input| Some((input, value.clone()))
}

fn char<'a>(expected_char: char) -> BoxParser<'a, char> {
    matches(move |ch| ch == expected_char)
}

fn lazy<'a, A, F>(parser: F) -> BoxParser<'a, A>
where
    F: Fn() -> BoxParser<'a, A> + 'a,
{
    BoxParser::new(move |input| parser().parse(input))
}

fn ws<'a>() -> BoxParser<'a, ()> {
    let space = char(' ').map(|_| ()).followed_by(lazy(|| ws()));
    let newline = char('\n').map(|_| ()).followed_by(lazy(|| ws()));
    let cr = char('\r').map(|_| ()).followed_by(lazy(|| ws()));
    let tab = char('\t').map(|_| ()).followed_by(lazy(|| ws()));

    BoxParser::new(space.or(newline).or(cr).or(tab).or(succeed(())))
}

fn matches<'a, F>(predicate: F) -> BoxParser<'a, char>
where
    F: Fn(char) -> bool + 'a,
{
    BoxParser::new(move |input: &'a str| match input.chars().next() {
        Some(first_char) if predicate(first_char) => Some((&input[1..], first_char)),
        _ => None,
    })
}

fn literal<'a>(literal: &str) -> impl Parser<'a, &str> {
    move |input: &'a str| {
        if input.starts_with(literal) {
            let remaining_input = &input[literal.len()..];

            Some((remaining_input, literal))
        } else {
            None
        }
    }
}

// const TRUE: BoxParser<'_, JsonValue> = literal("true").map(|_| JsonValue::Bool(true));
// const FALSE: BoxParser<'_, JsonValue> = literal("false").map(|_| JsonValue::Bool(false));
// const NULL: BoxParser<'_, JsonValue> = literal("null").map(|_| JsonValue::Null);

#[cfg(test)]
mod tests {
    use super::{parse_json, JsonValue};

    #[test]
    fn test_parse_json_null() {
        assert_eq!(parse_json("null"), Some(JsonValue::Null));
        assert_eq!(parse_json("  null  "), Some(JsonValue::Null));
    }

    #[test]
    fn test_parse_json_booleans() {
        assert_eq!(parse_json("true"), Some(JsonValue::Bool(true)));
        assert_eq!(parse_json("false"), Some(JsonValue::Bool(false)));
        assert_eq!(parse_json(" \ntrue \t"), Some(JsonValue::Bool(true)));
        assert_eq!(parse_json(" false  \t\n"), Some(JsonValue::Bool(false)));
    }
}
