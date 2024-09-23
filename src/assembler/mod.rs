//! The assembler, capable of parsing and assembling textual assembly code into
//! binary data for the stack machine to execute.

use std::{
    collections::HashMap,
    io::{BufReader, Read},
};

use itertools::Itertools;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, multispace0, newline, none_of},
    combinator::{all_consuming, eof, map, recognize},
    error::ParseError,
    multi::{many0, many0_count, many1},
    sequence::{delimited, pair, preceded, terminated},
    IResult, Parser,
};
use thiserror::Error;

use crate::opcode::{ConditionFlag, Operation as Opcode};

mod number;

mod instruction;
use instruction::instruction;

/// An error occuring during assembly file parsing and assembly.
#[derive(Debug, Error)]
pub enum Error {
    /// I/O error (see [`std::io::Error`]).
    #[error(transparent)]
    IO(#[from] std::io::Error),
    /// An error occured during parsing.
    #[error("parse error: {}", .0)]
    Parse(String),
    /// The resulting assembly program is longer than 65536 bytes.
    #[error("passed program is longer than maximum 65536 bytes length")]
    TooLong,
    /// Sections of the programs intersect.
    #[error("sections at ${:04x} and ${:04x} intersect", .0, .1)]
    IntersectingSections(u16, u16),
    /// Duplicate symbols are detected.
    #[error("duplicate symbol '{}'", .0)]
    DuplicateSymbol(String),
    /// A symbol referenced in the program is missing.
    #[error("missing symbol '{}'", .0)]
    MissingSymbol(String),
    /// A section was parsed but no entry point into enough free space was found.
    #[error("couldn't find available space to insert section")]
    NoSpaceAvailable,
}

/// An address serving as an instruction operand.
#[derive(Debug)]
enum AddrOperand {
    Label(String),
    Immediate(u16),
}

/// A byte serving as an instruction operand.
#[derive(Debug)]
enum ByteOperand {
    Immediate(u8),
    AtAddr(u16),
    AtLabel(String),
}

impl ByteOperand {
    /// Returns the length (in bytes) of the byte operand.
    pub fn len(&self) -> u16 {
        match self {
            ByteOperand::Immediate(_) => 1,
            ByteOperand::AtAddr(_) => 2,
            ByteOperand::AtLabel(_) => 2,
        }
    }
}

/// A signed offset as an instruction operand.
#[derive(Debug)]
enum OffsetOperand {
    Immediate(i8),
}

/// A condition flag used in certain instructions that are executed if the corresponding
/// CPU flag has a certain value.
#[derive(Debug, Clone, Copy)]
enum CondFlag {
    C,
    V,
    Z,
}

/// The list of instructions to parse.
#[derive(Debug)]
enum Instruction {
    Nop,
    Jmp(Option<(bool, CondFlag)>, AddrOperand),
    Br(Option<(bool, CondFlag)>, OffsetOperand),
    Push(ByteOperand),
    Pop,
    Add,
    Sub,
    Mul,
    Cmp,
    Inc,
    Swp,
    Rot,
    Dup,
    Call(Option<(bool, CondFlag)>, AddrOperand),
    Ret(Option<(bool, CondFlag)>),
    Trap,
}

impl Instruction {
    /// Returns the length (in bytes) of the instruction and its operands, if any.
    pub fn len(&self) -> u16 {
        match self {
            Instruction::Nop
            | Instruction::Add
            | Instruction::Sub
            | Instruction::Mul
            | Instruction::Cmp
            | Instruction::Ret(_)
            | Instruction::Inc
            | Instruction::Swp
            | Instruction::Rot
            | Instruction::Pop
            | Instruction::Dup
            | Instruction::Trap => 1,
            Instruction::Push(op) => 1 + op.len(),
            Instruction::Jmp(_, _) | Instruction::Br(_, _) | Instruction::Call(_, _) => 3,
        }
    }
}

/// A label in a program, naming a certain address.
#[derive(Debug)]
struct Label {
    name: String,
}

/// A statement in an assembly program.
#[derive(Debug)]
enum Statement {
    Label(Label),
    Operation(Instruction),
    Comment,
}

impl Statement {
    /// Returns the length (in bytes) of a statement.
    pub fn len(&self) -> u16 {
        match self {
            Statement::Label(_) => 0,
            Statement::Operation(o) => o.len(),
            Statement::Comment => 0,
        }
    }
}

/// A section of an assembly program, composed of [`Statements`](Statement).
#[derive(Debug)]
struct Section {
    statements: Vec<Statement>,
}

impl Section {
    /// Returns the length (in bytes) of the section.
    pub fn len(&self) -> Option<u16> {
        self.statements
            .iter()
            .map(Statement::len)
            .try_fold(0u16, u16::checked_add)
    }
}

/// An assembler, capable of stitching together assembly program sections into
/// a single block of executable code.
#[derive(Debug, Default)]
pub struct Assembler {
    sections: HashMap<u16, Section>,
}

impl Assembler {
    /// Parses a section of code and integrates it into the currently-built program.
    /// An optional entry point can be specified if needed.
    pub fn parse<R>(&mut self, input: R, entry: Option<u16>) -> Result<(), Error>
    where
        R: Read,
    {
        let mut reader = BufReader::new(input);
        let mut input = String::new();
        reader.read_to_string(&mut input)?;

        // Parse the section.
        let section = Section {
            statements: section(&input)
                .map(|(_, p)| p)
                .map_err(|e| Error::Parse(e.to_string()))?,
        };

        let len = section.len().ok_or(Error::TooLong)?;

        if let Some(entry) = entry {
            // An entry point is specified: insert it there and error if it
            // intersects with another section.
            if let Some(start) = self.sections.iter().find_map(|(&start, s)| {
                let l = s.len().unwrap();

                if entry < start + l && start < entry + len {
                    Some(start)
                } else {
                    None
                }
            }) {
                Err(Error::IntersectingSections(entry, start))
            } else {
                self.sections.insert(entry, section);

                Ok(())
            }
        } else {
            // Find the first starting point in memory where the program section
            // will fit.

            #[allow(clippy::single_range_in_vec_init)]
            let available_spaces = self
                .sections
                .iter()
                .map(|(entry, s)| *entry..*entry + s.len().unwrap())
                .fold(vec![0u16..65535], |mut v, s| {
                    let idx = v.iter().position(|x| x.contains(&s.start)).unwrap();

                    let x = v.remove(idx);

                    v.push(x.start..s.start);
                    v.push(s.end..x.end);
                    v
                });

            if let Some(entry) = available_spaces
                .into_iter()
                .find(|s| s.len() < len as usize)
            {
                self.sections.insert(entry.start, section);
                Ok(())
            } else {
                Err(Error::NoSpaceAvailable)
            }
        }
    }

    /// Assembles the stored section, resolving labels and returning a list of
    /// binary programs with their start address.
    pub fn assemble(&self) -> Result<Vec<(u16, Vec<u8>)>, Error> {
        let labels: HashMap<String, u16> = self
            .sections
            .iter()
            .map(|(&start, section)| {
                section
                    .statements
                    .iter()
                    .try_fold((0, HashMap::new()), |(mut cursor, mut labels), s| {
                        match s {
                            Statement::Label(l) => {
                                if labels.contains_key(&l.name) {
                                    return Err(Error::DuplicateSymbol(l.name.clone()));
                                } else {
                                    labels.insert(l.name.clone(), cursor + start);
                                }
                            }
                            Statement::Operation(o) => cursor += o.len(),
                            Statement::Comment => {}
                        }

                        Ok((cursor, labels))
                    })
                    .map(|(_, labels)| labels)
            })
            .flatten_ok()
            .collect::<Result<_, _>>()?;

        let mut sections = Vec::new();

        for (&start, section) in &self.sections {
            let mut v = Vec::new();

            let get_label = |l: &str, labels: &HashMap<String, u16>| {
                labels
                    .get(l)
                    .copied()
                    .ok_or(Error::MissingSymbol(l.to_string()))
            };

            let condition = |c: &Option<(bool, CondFlag)>| {
                c.map(|(b, c)| crate::opcode::Condition {
                    flag: match c {
                        CondFlag::C => ConditionFlag::Carry,
                        CondFlag::V => ConditionFlag::Overflow,
                        CondFlag::Z => ConditionFlag::Zero,
                    },
                    expected: b,
                })
            };

            for statement in &section.statements {
                match statement {
                    Statement::Comment => {}
                    Statement::Label(_) => {}
                    Statement::Operation(o) => v.extend(
                        match o {
                            Instruction::Nop => Opcode::Nop,
                            // Operation::Jmp(op) => Opcode::JumpAbsolute(match op {
                            //     AddrOperand::Label(l) => get_label(l, &labels)?,
                            //     AddrOperand::Immediate(v) => *v,
                            // }),
                            Instruction::Push(op) => match op {
                                ByteOperand::Immediate(v) => Opcode::PushLiteral(*v),
                                ByteOperand::AtAddr(v) => Opcode::PushMemory(*v),
                                ByteOperand::AtLabel(l) => {
                                    Opcode::PushMemory(get_label(l, &labels)?)
                                }
                            },
                            Instruction::Pop => Opcode::Pop,
                            Instruction::Add => Opcode::Add,
                            Instruction::Sub => Opcode::Subtract,
                            Instruction::Mul => Opcode::Multiply,
                            Instruction::Cmp => Opcode::Compare,
                            Instruction::Inc => Opcode::Increment,
                            Instruction::Swp => Opcode::Swap,
                            Instruction::Rot => Opcode::Rotate,
                            Instruction::Dup => Opcode::Duplicate,
                            Instruction::Jmp(c, op) => Opcode::JumpAbsolute(
                                condition(c),
                                match op {
                                    AddrOperand::Label(l) => get_label(l, &labels)?,
                                    AddrOperand::Immediate(v) => *v,
                                },
                            ),
                            Instruction::Br(c, op) => Opcode::JumpRelative(
                                condition(c),
                                match op {
                                    OffsetOperand::Immediate(v) => *v,
                                },
                            ),
                            Instruction::Call(c, op) => Opcode::Call(
                                condition(c),
                                match op {
                                    AddrOperand::Label(l) => get_label(l, &labels)?,
                                    AddrOperand::Immediate(v) => *v,
                                },
                            ),
                            Instruction::Ret(c) => Opcode::Return(condition(c)),
                            Instruction::Trap => Opcode::DebugTrap,
                        }
                        .to_byte_vec(),
                    ),
                }
            }

            sections.push((start, v));
        }

        Ok(sections)
    }
}

/// Parses something with whitespace on either side.
pub fn ws<'a, O, E: ParseError<&'a str>, F>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Parser<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

/// Parses an identifier, composed of alphanumeric characters and `_`. The first
/// character cannot be numeric.
pub fn ident(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input)
}

/// Parses a label, composed of an [identifier](ident) followed by a semicolon
/// (`:`).
fn label(input: &str) -> IResult<&str, Label> {
    map(terminated(ident, tag(":")), |s| Label {
        name: s.to_string(),
    })(input)
}

/// Parses a comment in the assembly file that won't be assembled into anything.
/// Effectively gets discarded at the end.
pub fn comment(input: &str) -> IResult<&str, &str> {
    recognize(terminated(
        preceded(tag(";"), many0(none_of("\n"))),
        alt((recognize(newline), eof)),
    ))(input)
}

/// Parses a [`Statement`], which can either be a [label], a [comment] or an [instruction](instruction()).
fn statement(input: &str) -> IResult<&str, Statement> {
    ws(alt((
        map(label, Statement::Label),
        map(comment, |_| Statement::Comment),
        map(instruction, Statement::Operation),
    )))(input)
}

/// Parses a section of assembly, composed of [statements](statement).
fn section(input: &str) -> IResult<&str, Vec<Statement>> {
    all_consuming(many1(statement))(input)
}
