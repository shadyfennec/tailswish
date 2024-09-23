//! Instruction parsers.

use nom::{
    branch::alt,
    bytes::complete::tag,
    combinator::{map, opt},
    sequence::{pair, preceded},
    IResult,
};

use super::{
    ident,
    number::{
        bin_literal_8, bin_literal_8_signed, dec_literal_8, dec_literal_8_signed, hex_addr,
        hex_literal_16, hex_literal_8, hex_literal_8_signed,
    },
    ws, AddrOperand, ByteOperand, CondFlag, Instruction, OffsetOperand,
};

// A little macro to make single keyword instructions with no operands easier
// to parse.
macro_rules! tag_operations {
    ($($n:ident, $v:ident, $s:literal);*) => {
        $(
            #[doc = "Parses the `"]
            #[doc = $s]
            #[doc = "` instruction."]
            fn $n(input:&str) -> IResult<&str, Instruction> {
                map(ws(tag($s)), |_| Instruction::$v)(input)
            }
        )*
    };
}

tag_operations! {
    nop, Nop, "nop";
    add, Add, "add";
    sub, Sub, "sub";
    mul, Mul, "mul";
    cmp, Cmp, "cmp";
    inc, Inc, "inc";
    swp, Swp, "swp";
    rot, Rot, "rot";
    pop, Pop, "pop";
    dup, Dup, "dup";
    trap, Trap, "trap"
}

/// Parses an address used as an operand of an instruction. It can be either
/// a [literal 16-bit value](hex_literal_16), or a label [identifier](ident).
fn addr_operand(input: &str) -> IResult<&str, AddrOperand> {
    alt((
        map(hex_literal_16, AddrOperand::Immediate),
        map(ident, |s| AddrOperand::Label(s.to_string())),
    ))(input)
}

/// Parses a signed offset as an operand of an instruction.
fn offset_operand(input: &str) -> IResult<&str, OffsetOperand> {
    alt((
        map(bin_literal_8_signed, OffsetOperand::Immediate),
        map(hex_literal_8_signed, OffsetOperand::Immediate),
        map(dec_literal_8_signed, OffsetOperand::Immediate),
    ))(input)
}

/// Parses an unsigned byte as an operand of an instruction. It can be a literal
/// value (in [decimal](dec_literal_8), [binary](bin_literal_8) or [hexadecimal](hex_literal_8)),
/// a byte located at a certain address in memory, indicated by a literal address
/// or a label.
fn byte_operand(input: &str) -> IResult<&str, ByteOperand> {
    alt((
        map(bin_literal_8, ByteOperand::Immediate),
        map(hex_literal_8, ByteOperand::Immediate),
        map(dec_literal_8, ByteOperand::Immediate),
        map(hex_addr, ByteOperand::AtAddr),
        map(ident, |s| ByteOperand::AtLabel(s.to_string())),
    ))(input)
}

/// Parses a `push` instruction.
fn push(input: &str) -> IResult<&str, Instruction> {
    map(preceded(ws(tag("push")), byte_operand), |op| {
        Instruction::Push(op)
    })(input)
}

/// Parses the condition (or lack thereof) for a branching instruction ([`call`],
/// [`jmp`], [`br`] or [`ret`]).
fn condition(input: &str) -> IResult<&str, (bool, CondFlag)> {
    pair(
        map(opt(tag("n")), |o| o.is_none()),
        alt((
            map(tag("z"), |_| CondFlag::Z),
            map(tag("v"), |_| CondFlag::V),
            map(tag("c"), |_| CondFlag::C),
        )),
    )(input)
}

/// Parses the family of [`jmp`] instructions, with their respective conditions.
fn jmp(input: &str) -> IResult<&str, Instruction> {
    map(
        ws(pair(
            ws(alt((
                map(tag("jmp"), |_| None),
                map(preceded(tag("j"), condition), Some),
            ))),
            ws(addr_operand),
        )),
        |(c, a)| Instruction::Jmp(c, a),
    )(input)
}

/// Parses the family of [`br`] instructions, with their respective conditions.
fn br(input: &str) -> IResult<&str, Instruction> {
    map(
        ws(pair(
            ws(alt((
                map(tag("br"), |_| None),
                map(preceded(tag("b"), condition), Some),
            ))),
            ws(offset_operand),
        )),
        |(c, o)| Instruction::Br(c, o),
    )(input)
}

/// Parses the family of [`call`] instructions, with their respective conditions.
fn call(input: &str) -> IResult<&str, Instruction> {
    map(
        ws(pair(
            ws(alt((
                map(tag("call"), |_| None),
                map(preceded(tag("c"), condition), Some),
            ))),
            ws(addr_operand),
        )),
        |(c, a)| Instruction::Call(c, a),
    )(input)
}

/// Parses the family of [`ret`] instructions, with their respective conditions.
fn ret(input: &str) -> IResult<&str, Instruction> {
    map(
        ws(ws(alt((
            map(tag("ret"), |_| None),
            map(preceded(tag("r"), condition), Some),
        )))),
        Instruction::Ret,
    )(input)
}

/// Parses instructions in an assembly file.
pub fn instruction(input: &str) -> IResult<&str, Instruction> {
    alt((
        nop, push, swp, rot, add, sub, mul, cmp, inc, jmp, br, call, ret, pop, dup, trap,
    ))(input)
}
