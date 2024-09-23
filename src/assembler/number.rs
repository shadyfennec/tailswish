//! Nom parsers to recognize and parse binary, decimal and hexadecimal numbers
//! in multiple forms in an assembly file.
//!
//! The convention to parse number in supported assembly file involves indicating
//! if numbers are immediate, and the base they use if it is not base-10.
//!
//! - `#` indicates a number is a literal.
//! - `%` indicates a number is in base-2 (binary).
//! - `$` indicates a number is in base-16 (hexadecimal).
//!
//! For example, a literal base-16 number is written with `#$`.

use nom::{
    bytes::complete::tag,
    character::complete::{hex_digit1, one_of},
    combinator::{map, map_res, opt, recognize},
    multi::many1,
    sequence::{pair, preceded},
    IResult,
};

/// Recognizes an optional + or - sign.
pub fn sign(input: &str) -> IResult<&str, Option<char>> {
    opt(one_of("+-"))(input)
}

/// Recognizes one or more binary digits (0 or 1).
pub fn bin_digit1(input: &str) -> IResult<&str, &str> {
    recognize(many1(one_of("01")))(input)
}

/// Recognizes one or more decimal digits (0 through 9).
pub fn dec_digit1(input: &str) -> IResult<&str, &str> {
    recognize(many1(one_of("0123456789")))(input)
}

/// Parses a literal 16-bit hexadecimal unsigned integer.
pub fn hex_literal_16(input: &str) -> IResult<&str, u16> {
    map_res(preceded(tag("#$"), hex_digit1), |s: &str| {
        u16::from_str_radix(s, 16)
    })(input)
}

/// Parses a literal 8-bit hexadecimal unsigned integer.
pub fn hex_literal_8(input: &str) -> IResult<&str, u8> {
    map_res(preceded(tag("#$"), hex_digit1), |s: &str| {
        u8::from_str_radix(s, 16)
    })(input)
}

/// Parses a literal 8-bit hexadecimal signed integer.
pub fn hex_literal_8_signed(input: &str) -> IResult<&str, i8> {
    map_res(
        preceded(
            tag("#$"),
            map(pair(sign, hex_digit1), |(s, d)| match s {
                None => d.to_string(),
                Some(c) => format!("{c}{d}"),
            }),
        ),
        |s: String| i8::from_str_radix(&s, 16),
    )(input)
}

/// Parses a literal 8-bit binary number.
pub fn bin_literal_8(input: &str) -> IResult<&str, u8> {
    map_res(preceded(tag("#%"), bin_digit1), |s| {
        u8::from_str_radix(s, 2)
    })(input)
}

/// Parses a literal 8-bit signed binary number.
pub fn bin_literal_8_signed(input: &str) -> IResult<&str, i8> {
    map_res(
        preceded(
            tag("#%"),
            map(pair(sign, bin_digit1), |(s, d)| match s {
                None => d.to_string(),
                Some(c) => format!("{c}{d}"),
            }),
        ),
        |s: String| i8::from_str_radix(&s, 2),
    )(input)
}

/// Parses a 16-bit hexadecimal address.
pub fn hex_addr(input: &str) -> IResult<&str, u16> {
    map_res(preceded(tag("$"), hex_digit1), |s: &str| {
        u16::from_str_radix(s, 16)
    })(input)
}

/// Parses a literal 8-bit unsigned decimal number.
pub fn dec_literal_8(input: &str) -> IResult<&str, u8> {
    map_res(preceded(tag("#"), dec_digit1), |s: &str| s.parse::<u8>())(input)
}

/// Parses a literal 8-bit signed decimal number.
pub fn dec_literal_8_signed(input: &str) -> IResult<&str, i8> {
    map_res(
        preceded(
            tag("#"),
            map(pair(sign, dec_digit1), |(s, d)| match s {
                None => d.to_string(),
                Some(c) => format!("{c}{d}"),
            }),
        ),
        |s: String| i8::from_str_radix(&s, 16),
    )(input)
}
