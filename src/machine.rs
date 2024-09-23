//! The stack machine.

use std::io::Read;

use crate::{
    assembler::Assembler,
    opcode::{Condition, ConditionFlag, Operation},
};

/// A decision to take during CPU execution, determined by the [Inspector].
#[derive(Debug, Clone, Copy)]
pub enum InspectorResult {
    /// Continue onto the next instruction.
    Continue,
    /// End execution and exit the emulator.
    Exit,
    /// Re-prompt the inspector.
    Loop,
}

/// An inspector function, used to execute user-defined code at each CPU step.
/// The inspector is provided mutable access to the machine's registers and memory,
/// debug trap status, the next 3 bytes to be parsed as opcode and operands, and
/// the decoded instruction. It returns an [`InspectorResult`].
pub type Inspector<'a> =
    Box<dyn FnMut(&mut Machine<'a>, bool, [u8; 3], &Operation) -> InspectorResult>;

/// The registers of the stack machine.
#[derive(Debug, Default)]
pub struct RegisterSet {
    /// `pc`: The program counter.
    pub pc: u16,
    /// `wsp`: The work stack pointer.
    pub wsp: u8,
    /// `rsp`: The returns stack pointer.
    pub rsp: u8,
    /// Unused for the moment.
    pub carry: bool,
    /// Indicates if the last arithmetic operation resulted in a zero.
    pub zero: bool,
    /// Indicates if the last arithmetic operation resulted in an overflow or underflow.
    pub overflow: bool,
}

/// The stack machine, capable of executing programs using a complex stack
/// operation set.
///
/// It possesses two different stacks of the same size (256 bytes): a *work* stack,
/// used to store general purpose data by the user; and a *return* stack, used
/// to keep track of subroutine return pointers. The machine also has access to
/// a 65536-large memory space, that holds the program and eventual memory-mapped
/// external devices.
pub struct Machine<'a> {
    /// The associated memory of the program. There is no concept of ROM here,
    /// so all program data is mutable, enabling self-modifying programs.
    pub ram: &'a mut [u8],
    /// The registers of the stack machine. No general-purpose registers, only
    /// stack pointers, program counters and CPU flags.
    pub registers: RegisterSet,
    /// The work stack, user-manipulated. Indexed by the `wsp` register.
    pub work_stack: [u8; 256],
    /// The return stack, used by `call` and `ret` instructions. Indexed by the
    /// `rsp` register.
    pub return_stack: [u8; 256],
    inspect: Option<Inspector<'a>>,
    assembler: Assembler,
}

impl<'a> Machine<'a> {
    /// Creates a new machine with a specified RAM region.
    pub fn new(ram: &'a mut [u8]) -> Self {
        assert!(
            ram.len() <= u16::MAX as usize + 1,
            "machine ram cannot be larger than {}",
            u16::MAX
        );

        Self {
            ram,
            registers: Default::default(),
            work_stack: [0; 256],
            return_stack: [0; 256],
            inspect: None,
            assembler: Default::default(),
        }
    }

    /// Sets the inspector with a user-defined inspector function.
    pub fn set_inspect(&mut self, inspect: Inspector<'a>) {
        self.inspect = Some(inspect);
    }

    /// Insert a section of program (as a human-readable assembly text file)
    /// to be parsed and assembled.
    pub fn insert_program<R>(&mut self, input: R, entry: Option<u16>)
    where
        R: Read,
    {
        self.assembler.parse(input, entry).unwrap();
    }

    /// Run the program starting at the specified entrypoint.
    pub fn run(mut self, entry: u16) {
        self.registers.pc = entry;

        // Assemble the program into RAM.
        for (start, section) in self.assembler.assemble().unwrap() {
            if start as usize + section.len() < self.ram.len() {
                self.ram[start as usize..start as usize + section.len()].copy_from_slice(&section);
            } else {
                panic!(
                    "section ${start:04x} of length {} is out of bounds for RAM",
                    section.len()
                )
            }
        }

        // Function to assert if a condition for a branching or return instruction
        // is satisfied based on the register flags.
        let check_condition = |c: Option<Condition>, r: &RegisterSet| match c {
            None => true,
            Some(Condition {
                flag: ConditionFlag::Zero,
                expected,
            }) => r.zero == expected,
            Some(Condition {
                flag: ConditionFlag::Overflow,
                expected,
            }) => r.overflow == expected,
            Some(Condition {
                flag: ConditionFlag::Carry,
                expected,
            }) => r.carry == expected,
        };

        'outer: loop {
            // Fetch the bytes for the next instruction?
            let (opcode, op1, op2) = (
                self.ram[self.registers.pc as usize],
                self.ram[self.registers.pc.wrapping_add(1) as usize],
                self.ram[self.registers.pc.wrapping_add(2) as usize],
            );

            // Decode the instruction frop its opcode.
            let operation = Operation::from([opcode, op1, op2]);

            // The eventual address to jump to if a jump is performed.
            let mut jump = None;
            // Set to true if a debug trap instruction is executed.
            let mut trap = false;

            // Execute the instruction.
            match operation {
                Operation::Nop => {}
                Operation::PushLiteral(v) => {
                    self.push_work(v);
                }
                Operation::PushMemory(addr) => {
                    let v = self.ram[addr as usize];
                    self.push_work(v);
                    self.registers.zero = v == 0;
                    self.registers.carry = false;
                    self.registers.overflow = false;
                }
                Operation::Swap => {
                    let a = self.work_stack[self.registers.wsp as usize];
                    let b = self.work_stack[self.registers.wsp.wrapping_add(1) as usize];
                    self.work_stack[self.registers.wsp as usize] = b;
                    self.work_stack[self.registers.wsp.wrapping_add(1) as usize] = a;
                }
                Operation::Rotate => {
                    let a = self.work_stack[self.registers.wsp as usize];
                    let b = self.work_stack[self.registers.wsp.wrapping_add(1) as usize];
                    let c = self.work_stack[self.registers.wsp.wrapping_add(2) as usize];
                    self.work_stack[self.registers.wsp as usize] = b;
                    self.work_stack[self.registers.wsp.wrapping_add(1) as usize] = c;
                    self.work_stack[self.registers.wsp.wrapping_add(2) as usize] = a;
                }
                Operation::Duplicate => {
                    let v = self.pop_work();
                    self.push_work(v);
                    self.push_work(v);
                }
                Operation::Pop => {
                    let v = self.pop_work();
                    self.registers.zero = v == 0;
                    self.registers.carry = false;
                    self.registers.overflow = false;
                }
                Operation::Add => {
                    let v = self.pop_work();
                    let (v, o) = self.work_stack[self.registers.wsp as usize].overflowing_add(v);
                    self.registers.overflow = o;
                    self.registers.zero = v == 0;
                    self.work_stack[self.registers.wsp as usize] = v;
                }
                Operation::Subtract => {
                    let v = self.pop_work();
                    let (v, o) = self.work_stack[self.registers.wsp as usize].overflowing_sub(v);
                    self.registers.overflow = o;
                    self.registers.zero = v == 0;
                    self.work_stack[self.registers.wsp as usize] = v;
                }
                Operation::Multiply => {
                    let v = self.pop_work();
                    let (v, o) = self.work_stack[self.registers.wsp as usize].overflowing_mul(v);
                    self.registers.overflow = o;
                    self.registers.zero = v == 0;
                    self.work_stack[self.registers.wsp as usize] = v;
                }
                Operation::Compare => {
                    let v = self.pop_work();
                    let (nv, o) = self.work_stack[self.registers.wsp as usize].overflowing_sub(v);
                    self.registers.overflow = o;
                    self.registers.zero = nv == 0;
                    self.push_work(v);
                }
                Operation::Increment => {
                    let (v, o) = self.work_stack[self.registers.wsp as usize].overflowing_add(1);
                    self.registers.overflow = o;
                    self.registers.zero = v == 0;
                    self.work_stack[self.registers.wsp as usize] = v;
                }
                Operation::JumpAbsolute(c, a) => {
                    if check_condition(c, &self.registers) {
                        jump = Some(a);
                    }
                }
                Operation::JumpRelative(c, o) => {
                    if check_condition(c, &self.registers) {
                        let (new_pc, o) = self.registers.pc.overflowing_add_signed(o as i16);
                        self.registers.overflow = o;
                        self.registers.zero = new_pc == 0;
                        jump = Some(new_pc);
                    }
                }
                Operation::Call(c, a) => {
                    if check_condition(c, &self.registers) {
                        let [h, l] = self
                            .registers
                            .pc
                            .wrapping_add(operation.len())
                            .to_be_bytes();
                        self.push_ret(h);
                        self.push_ret(l);
                        jump = Some(a);
                    }
                }
                Operation::Return(c) => {
                    if check_condition(c, &self.registers) {
                        let l = self.pop_ret();
                        let h = self.pop_ret();
                        jump = Some(u16::from_be_bytes([h, l]));
                    }
                }
                Operation::DebugTrap => {
                    trap = true;
                }
            }

            // Inspector loop
            if let Some(mut inspect) = self.inspect.take() {
                'inner: loop {
                    match (inspect)(&mut self, trap, [opcode, op1, op2], &operation) {
                        InspectorResult::Continue => break 'inner,
                        InspectorResult::Exit => break 'outer,
                        InspectorResult::Loop => {}
                    }
                }

                self.inspect = Some(inspect);
            }

            self.registers.pc =
                jump.unwrap_or_else(|| self.registers.pc.wrapping_add(operation.len()));
        }
    }

    /// Pushes a byte onto the work stack and decrement `wsp`.
    fn push_work(&mut self, v: u8) {
        self.registers.wsp = self.registers.wsp.wrapping_sub(1);
        self.work_stack[self.registers.wsp as usize] = v;
    }

    /// Pops a byte from the work stack and increment `wsp`.
    fn pop_work(&mut self) -> u8 {
        let v = self.work_stack[self.registers.wsp as usize];
        self.registers.wsp = self.registers.wsp.wrapping_add(1);
        v
    }

    /// Pushes a byte onto the return stack and decrement `rsp`.
    fn push_ret(&mut self, v: u8) {
        self.registers.rsp = self.registers.rsp.wrapping_sub(1);
        self.return_stack[self.registers.rsp as usize] = v;
    }

    /// Pops a byte from the return stack and increment `rsp`.
    fn pop_ret(&mut self) -> u8 {
        let v = self.return_stack[self.registers.rsp as usize];
        self.registers.rsp = self.registers.rsp.wrapping_add(1);
        v
    }
}
