//! Opcode encoding and decoding.

use std::fmt;

/// A flag used by some instructions to determine if it should be executed or not,
/// based on CPU register flags?
#[derive(Debug, Clone, Copy)]
pub enum ConditionFlag {
    /// Check the `zero` flag, set if the last arithmetic instruction resulted in `0`.
    Zero,
    /// Check the `overflow` flag, set if the last operation resulted in an integer
    /// overflow or underflow.
    Overflow,
    /// Unused for the moment.
    Carry,
}

/// A condition to meet to execute certain instructions. Composed of a [`ConditionFlag`]
/// and its expected value, either true or false.
#[derive(Debug, Clone, Copy)]
pub struct Condition {
    /// The flag to check.
    pub flag: ConditionFlag,
    /// The value it should have to continue.
    pub expected: bool,
}

/// An instruction for the stack machine.
///
/// In all the annotations for the work stack in documentation, the stack is
/// read from left to right, with the rightmost element being the top of the
/// stack. Furthermore, CPU flags are indicated in brackets at the right of the
/// stack when they are set.
#[derive(Debug, Clone, Copy)]
pub enum Operation {
    /// `nop`: Does nothing.
    Nop,
    /// `push #<operand>`: Pushes a literal byte onto the work stack.
    ///
    /// ```asm
    /// push #3     ; 1 2 -> 1 2 3
    /// ```
    PushLiteral(u8),
    /// `push <operand>`: Pushes a byte at the specified address onto the work stack.
    ///
    /// ```asm
    /// push $4000  ; 1 2 -> 1 2 [$4000] (the byte at addres $4000)
    /// ```
    PushMemory(u16),
    /// `swp`: Swaps the two topmost elements of the work stack.
    ///
    /// ```asm
    /// swp         ; 1 2 3 -> 1 3 2
    /// ```
    Swap,
    /// `rot`: Rotates right the three topmost elements of the work stack.
    /// ```asm
    /// rot         ; 1 2 3 -> 3 1 2
    /// ```
    Rotate,
    /// `pop`: Pops an element from the work stack.
    /// ```asm
    /// pop         ; 1 2 3 -> 1 2
    /// ```
    Pop,
    /// `add`: Adds the topmost element of the work stack to the element before
    /// it, and removes the former from the work stack.
    ///
    /// ```asm
    /// push #31    ; 31
    /// push #86    ; 31 86
    /// add         ; 127
    /// ```
    ///
    /// Affects the following CPU flags:
    /// - `zero` if the result of the operation is equal to 0
    /// - `overflow` if the result of the operation overflowed over the 8-bit limit.
    Add,
    /// `sub`: Subtracts the top element of the work stack from the elemnt before
    /// it, and removes the former from the work stack.
    ///
    /// ```asm
    /// push #100   ; 100
    /// push #75    ; 100 75
    /// sub         ; 25
    /// ```
    ///
    /// Affects the following CPU flags:
    /// - `zero` if the result of the operation is equal to 0
    /// - `overflow` if the result of the operation underflowed under 0.
    Subtract,
    /// `mul`: Multiplies the topmost element of the work stack with the element
    /// before it, and removes the former from the work stack.
    ///
    /// ```asm
    /// push #7     ; 7
    /// push #8     ; 7 8
    /// mul         ; 56
    /// ```
    ///
    /// Affects the following CPU flags:
    /// - `zero` if the result of the operation is equal to 0
    /// - `overflow` if the result of the operation overflowed over the 8-bit limit.
    Multiply,
    /// `cmp`: Performs the same operation as `sub`, but doesn't actually modify
    /// the elements in the work stack and keeps the topmost element in it. This
    /// operation still affects the CPU flags.
    ///
    /// ```asm
    /// push #3     ; 3
    /// push #1     ; 3 1
    /// push #2     ; 3 1 2
    /// add         ; 3 3
    /// cmp         ; 3 3    [Z]
    /// ```
    ///
    /// Affects the following CPU flags:
    /// - `zero` if the result of the operation is equal to 0
    /// - `overflow` if the result of the operation underflowed under 0.
    Compare,
    /// `inc`: Increments the top element of the work stack by one.
    ///
    /// ```asm
    /// push #255   ; 255
    /// inc         ; 0     [V]
    /// ```
    /// Affects the following CPU flags:
    /// - `zero` if the result of the operation is equal to 0
    /// - `overflow` if the result of the operation overflowed over the 8-bit limit.
    Increment,
    /// `dup`: Duplicates the topmost element of the work stack.
    ///
    /// ```asm
    /// push #3     ; 3
    /// push #6     ; 3 6
    /// add         ; 9
    /// dup         ; 9 9
    /// ```
    Duplicate,
    /// `jmp` and variants: Jump execution to a specified address in memory. This
    /// instruction has 7 variants depending on necessary conditions:
    ///
    /// | Instruction | Flag concerned | Expected value |
    /// |-------------|----------------|----------------|
    /// | `jmp`       | *None*         | *Always jump*  |
    /// | `jz`        | Zero           | True           |
    /// | `jv`        | Overflow       | True           |
    /// | `jc`        | Carry          | True           |
    /// | `jnz`       | Zero           | False          |
    /// | `jnv`       | Overflow       | False          |
    /// | `jnc`       | Carry          | False          |
    JumpAbsolute(Option<Condition>, u16),
    /// `br` and variants: Jump execution to an offset based on the current address.
    /// This offset is a signed 8-bit value. This instruction has 7 variants
    /// depending on necessary conditions:
    ///
    /// | Instruction | Flag concerned | Expected value   |
    /// |-------------|----------------|------------------|
    /// | `br`        | *None*         | *Always branch*  |
    /// | `bz`        | Zero           | True             |
    /// | `bv`        | Overflow       | True             |
    /// | `bc`        | Carry          | True             |
    /// | `bnz`       | Zero           | False            |
    /// | `bnv`       | Overflow       | False            |
    /// | `bnc`       | Carry          | False            |
    JumpRelative(Option<Condition>, i8),
    /// `call` and variants: Calls a subroutine at the specified address. This
    /// also jumps execution to the address, but it also pushes the address of
    /// the instruction after the `call` onto the return stack, used in conjunction
    /// with [`ret`](Operation::Return). This instruction has 7 variants depending
    /// on necessary conditions:
    /// | Instruction | Flag concerned | Expected value   |
    /// |-------------|----------------|------------------|
    /// | `call`      | *None*         | *Always call*    |
    /// | `cz`        | Zero           | True             |
    /// | `cv`        | Overflow       | True             |
    /// | `cc`        | Carry          | True             |
    /// | `cnz`       | Zero           | False            |
    /// | `cnv`       | Overflow       | False            |
    /// | `cnc`       | Carry          | False            |
    Call(Option<Condition>, u16),
    /// `ret` and variants: Returns from a subroutine, popping a 16-bit address
    /// from the return stack and continuing execution there. This instruction
    /// has 7 variants depending on necessary conditions:
    ///
    /// | Instruction | Flag concerned | Expected value   |
    /// |-------------|----------------|------------------|
    /// | `ret`       | *None*         | *Always return*  |
    /// | `rz`        | Zero           | True             |
    /// | `rv`        | Overflow       | True             |
    /// | `rc`        | Carry          | True             |
    /// | `rnz`       | Zero           | False            |
    /// | `rnv`       | Overflow       | False            |
    /// | `rnc`       | Carry          | False            |
    Return(Option<Condition>),
    /// Essentially a [`nop`](Operation::Nop), but also signals to the CPU to
    /// notify an eventual inspector that this instruction was used. Mainly
    /// used by a debugger to invariably break execution even if no breakpoint
    /// is set.
    DebugTrap,
}

impl Operation {
    /// Returns the length (in bytes) of the instruction with its eventual operands.
    pub fn len(&self) -> u16 {
        match self {
            Operation::Nop
            | Operation::Add
            | Operation::Subtract
            | Operation::Multiply
            | Operation::Compare
            | Operation::Return(_)
            | Operation::Increment
            | Operation::Swap
            | Operation::Rotate
            | Operation::Pop
            | Operation::Duplicate
            | Operation::DebugTrap => 1,
            Operation::PushLiteral(_) | Operation::JumpRelative(_, _) => 2,
            Operation::PushMemory(_) | Operation::JumpAbsolute(_, _) | Operation::Call(_, _) => 3,
        }
    }

    /// Returns a vector composed of the bytes necessary to encode the operation.
    pub fn to_byte_vec(self) -> Vec<u8> {
        let bytes: [u8; 3] = (self).into();

        bytes[..self.len() as usize].to_owned()
    }
}

impl From<[u8; 3]> for Operation {
    fn from([opcode, op1, op2]: [u8; 3]) -> Self {
        let op16 = u16::from_be_bytes([op1, op2]);
        let low_nybble = opcode & 0b1111;

        // Creates a Condition from the low nybble of an opcode.
        let condition = |x| match x {
            0 => None,
            1 => Some(Condition {
                flag: ConditionFlag::Zero,
                expected: true,
            }),
            2 => Some(Condition {
                flag: ConditionFlag::Overflow,
                expected: true,
            }),
            3 => Some(Condition {
                flag: ConditionFlag::Carry,
                expected: true,
            }),
            4 => Some(Condition {
                flag: ConditionFlag::Zero,
                expected: false,
            }),
            5 => Some(Condition {
                flag: ConditionFlag::Overflow,
                expected: false,
            }),
            6 => Some(Condition {
                flag: ConditionFlag::Carry,
                expected: false,
            }),
            _ => unreachable!(),
        };

        match opcode {
            0x00 => Self::Nop,
            0x01 => Self::PushLiteral(op1),
            0x02 => Self::PushMemory(op16),
            0x03 => Self::Swap,
            0x04 => Self::Rotate,
            0x05 => Self::Pop,
            0x06 => Self::Duplicate,
            0x83 => Self::Add,
            0x84 => Self::Subtract,
            0x85 => Self::Multiply,
            0x86 => Self::Compare,
            0x87 => Self::Increment,
            0xA0..=0xA6 => Self::JumpAbsolute(condition(low_nybble), op16),
            0xB0..=0xB6 => Self::JumpRelative(condition(low_nybble), op1 as _),
            0xC0..=0xC6 => Self::Call(condition(low_nybble), op16),
            0xD0..=0xD6 => Self::Return(condition(low_nybble)),
            0xFF => Self::DebugTrap,
            _ => panic!("unknown opcode {opcode:02x}"),
        }
    }
}

impl From<Operation> for [u8; 3] {
    fn from(value: Operation) -> Self {
        let from_op16 = |a: u8, b: u16| {
            let [op1, op2] = b.to_be_bytes();
            [a, op1, op2]
        };

        let condition_nybble = |x: Option<Condition>| match x {
            None => 0x0,
            Some(Condition {
                flag: ConditionFlag::Zero,
                expected: true,
            }) => 0x01,
            Some(Condition {
                flag: ConditionFlag::Overflow,
                expected: true,
            }) => 0x02,
            Some(Condition {
                flag: ConditionFlag::Carry,
                expected: true,
            }) => 0x03,
            Some(Condition {
                flag: ConditionFlag::Zero,
                expected: false,
            }) => 0x04,
            Some(Condition {
                flag: ConditionFlag::Overflow,
                expected: false,
            }) => 0x05,
            Some(Condition {
                flag: ConditionFlag::Carry,
                expected: false,
            }) => 0x06,
        };

        match value {
            Operation::Nop => [0x00, 0x00, 0x00],
            Operation::PushLiteral(v) => [0x01, v, 0x00],
            Operation::PushMemory(v) => from_op16(0x02, v),
            Operation::Swap => [0x03, 0x00, 0x00],
            Operation::Rotate => [0x04, 0x00, 0x00],
            Operation::Pop => [0x05, 0x00, 0x00],
            Operation::Duplicate => [0x06, 0x00, 0x00],
            Operation::Add => [0x83, 0x00, 0x00],
            Operation::Subtract => [0x84, 0x00, 0x00],
            Operation::Multiply => [0x85, 0x00, 0x00],
            Operation::Compare => [0x86, 0x00, 0x00],
            Operation::Increment => [0x87, 0x00, 0x00],
            Operation::JumpAbsolute(c, a) => from_op16(0xA0 + condition_nybble(c), a),
            Operation::JumpRelative(c, o) => [0xB0 + condition_nybble(c), o as _, 0x00],
            Operation::Call(c, a) => from_op16(0xC0 + condition_nybble(c), a),
            Operation::Return(c) => [0xD0 + condition_nybble(c), 0x00, 0x00],
            Operation::DebugTrap => [0xFF, 0x00, 0x00],
        }
    }
}

impl fmt::Display for Operation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let condition = |c: &Option<Condition>, a: &str, b: &str, f: &mut fmt::Formatter<'_>| {
            write!(f, "{}", a)?;
            match c {
                None => write!(f, "{}", b),
                Some(Condition {
                    flag: ConditionFlag::Zero,
                    expected: true,
                }) => write!(f, "z"),
                Some(Condition {
                    flag: ConditionFlag::Overflow,
                    expected: true,
                }) => write!(f, "v"),
                Some(Condition {
                    flag: ConditionFlag::Carry,
                    expected: true,
                }) => write!(f, "c"),
                Some(Condition {
                    flag: ConditionFlag::Zero,
                    expected: false,
                }) => write!(f, "nz"),
                Some(Condition {
                    flag: ConditionFlag::Overflow,
                    expected: false,
                }) => write!(f, "nv"),
                Some(Condition {
                    flag: ConditionFlag::Carry,
                    expected: false,
                }) => write!(f, "nc"),
            }
        };

        match self {
            Operation::Nop => write!(f, "nop"),
            Operation::PushLiteral(v) => write!(f, "push #${v:02x}"),
            Operation::PushMemory(addr) => write!(f, "push ${addr:04x}"),
            Operation::Swap => write!(f, "swp"),
            Operation::Rotate => write!(f, "rot"),
            Operation::Duplicate => write!(f, "dup"),
            Operation::Pop => write!(f, "pop"),
            Operation::Add => write!(f, "add"),
            Operation::Subtract => write!(f, "sub"),
            Operation::Multiply => write!(f, "mul"),
            Operation::Compare => write!(f, "cmp"),
            Operation::Increment => write!(f, "inc"),
            Operation::JumpAbsolute(c, a) => {
                condition(c, "j", "mp", f)?;
                write!(f, " #${a:04x}")
            }
            Operation::JumpRelative(c, o) => {
                condition(c, "b", "ra", f)?;
                write!(f, " #{o:+02}")
            }
            Operation::Call(c, a) => {
                condition(c, "c", "all", f)?;
                write!(f, " #${a:04x}")
            }
            Operation::Return(c) => condition(c, "r", "et", f),
            Operation::DebugTrap => write!(f, "trap"),
        }
    }
}
