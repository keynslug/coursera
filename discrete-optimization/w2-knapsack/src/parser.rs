extern crate pom;

use std::io;
use std::str::{FromStr, from_utf8};

use super::solver::{Item, Problem};
use pom::parser::{Parser, one_of, sym, end, list};

fn problem<'a>() -> Parser<'a, u8, Problem> {
    let capacity = tuple();
    let items = items();
    let parser = capacity + items - end();
    parser.map(|((count, capacity), v)| {
        assert_eq!(v.len(), count);
        Problem { capacity, items: v }
    })
}

fn items<'a>() -> Parser<'a, u8, Vec<Item>> {
    let items = list(tuple(), newline());
    items.map(|v|
        v.into_iter()
            .enumerate()
            .map(|(i, (value, weight))| {
                Item { ident: i, value, weight }
            })
            .collect()
    )
}

fn tuple<'a>() -> Parser<'a, u8, (usize, usize)> {
    space() * number() - space() + number() - space() - newline()
}

fn number<'a>() -> Parser<'a, u8, usize> {
    let integer = one_of(b"123456789") - one_of(b"0123456789").repeat(0..) | sym(b'0');
    integer.collect()
        .convert(from_utf8)
        .convert(usize::from_str)
}

fn space<'a>() -> Parser<'a, u8, ()> {
    one_of(b" \t").repeat(0..).discard()
}

fn newline<'a>() -> Parser<'a, u8, ()> {
    one_of(b"\r\n").repeat(0..).discard()
}

pub fn parse(reader: &mut impl io::Read) -> Result<Problem, pom::Error> {
    let mut input = Vec::new();
    let _size = reader.read_to_end(&mut input).unwrap();
    let parser = problem();
    parser.parse(&input[..])
}
