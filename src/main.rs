use std::io::{BufRead, Write};

mod assembler;
mod machine;
use machine::{InspectorResult, Machine};
mod opcode;

const PROGRAM: &str = include_str!("../examples/division.s");

/// Generates a debugger function run at every step of the emulator's CPU.
fn debugger<'a>() -> machine::Inspector<'a> {
    // Whether to display the state of the work stack.
    let mut display_work = false;
    // Whether to skip user interaction and display (such as when continuing
    // execution waiting for a trap or checkpoint).
    let mut skip = false;
    // The list of breakpoints to watch for.
    let mut breakpoints = Vec::new();

    Box::new(move |machine, trap, [opcode, op1, op2], operation| {
        let breakpoint = breakpoints.iter().position(|x| x == &machine.registers.pc);

        if !skip || trap || breakpoint.is_some() {
            skip = false;

            // Let the user know a breakpoint is reached.
            if let Some(breakpoint) = breakpoint {
                println!(
                    "Breakpoint {} reached: ${:04x}",
                    breakpoint + 1,
                    breakpoints[breakpoint]
                );
            }

            // Status line
            print!("pc: 0x{:04x}, ", machine.registers.pc);
            print!("wsp: 0x{:02x}, ", machine.registers.wsp);
            print!("rsp: 0x{:02x}, ", machine.registers.rsp);
            print!(
                "flags: [{}{}{}], ",
                if machine.registers.carry { 'C' } else { '-' },
                if machine.registers.overflow { 'V' } else { '-' },
                if machine.registers.zero { 'Z' } else { '-' },
            );
            print!("opcode: {opcode:02x} {op1:02x} {op2:02x} ({operation}) ");

            // Function to display the work stack.
            let work = |machine: &Machine| {
                println!("work stack (wsp = ${:02x}):", machine.registers.wsp);
                for i in 0..256 {
                    if i % 16 == 0 {
                        print!("${i:02x} |");
                    }
                    print!(" {:02x}", machine.work_stack[i]);
                    if i % 16 == 15 {
                        println!();
                    }
                }
            };

            // Display the work stack if it is set to display at every step
            if display_work {
                work(machine);
            }

            // Get user input
            std::io::stdout().flush().unwrap();
            let mut buf = String::new();
            std::io::stdin().lock().read_line(&mut buf).unwrap();

            let mut tokens = buf.split_whitespace();

            let n = tokens.next().map(|s| s.to_lowercase());

            if let Some(n) = n {
                match n.as_str() {
                    "" => InspectorResult::Continue,
                    "q" | "quit" | "exit" => InspectorResult::Exit,
                    "w" | "ws" | "work" => {
                        work(machine);
                        InspectorResult::Loop
                    }
                    "d" | "display" | "disp" => {
                        let n = tokens.next().map(|s| s.to_lowercase());
                        if let Some(n) = n {
                            match n.as_str() {
                                "w" | "ws" | "work" => {
                                    display_work = true;
                                    InspectorResult::Loop
                                }
                                _ => InspectorResult::Loop,
                            }
                        } else {
                            InspectorResult::Loop
                        }
                    }
                    "r" | "remove" | "rem" => {
                        let n = tokens.next().map(|s| s.to_lowercase());
                        if let Some(n) = n {
                            match n.as_str() {
                                "w" | "ws" | "work" => {
                                    display_work = false;
                                    InspectorResult::Loop
                                }
                                _ => InspectorResult::Loop,
                            }
                        } else {
                            InspectorResult::Loop
                        }
                    }
                    "c" | "cont" | "continue" => {
                        skip = true;
                        InspectorResult::Continue
                    }
                    "b" | "break" => {
                        let n = tokens.next().map(|s| s.to_lowercase());
                        if let Some(n) = n {
                            let x = n.strip_prefix('$').unwrap_or(&n);
                            let x = x.strip_prefix("0x").unwrap_or(x);

                            match u16::from_str_radix(x, 16) {
                                Ok(v) => {
                                    breakpoints.push(v);
                                }
                                Err(e) => {
                                    eprintln!("Couldn't parse breakpoint addr: {e}");
                                }
                            }
                            InspectorResult::Loop
                        } else {
                            InspectorResult::Loop
                        }
                    }
                    "delete" => {
                        let n = tokens.next().map(|s| s.to_lowercase());
                        if let Some(n) = n {
                            match n.parse::<usize>() {
                                Ok(v) => {
                                    if breakpoints.len() > v || v == 0 {
                                        eprintln!("No breakpoint {v} (indices start at 1)")
                                    } else {
                                        breakpoints.remove(v);
                                    }
                                }
                                Err(e) => {
                                    eprintln!("Couldn't parse breakpoint index: {e}")
                                }
                            }
                            InspectorResult::Loop
                        } else {
                            InspectorResult::Loop
                        }
                    }
                    _ => InspectorResult::Continue,
                }
            } else {
                InspectorResult::Continue
            }
        } else {
            InspectorResult::Continue
        }
    })
}

fn main() {
    let mut ram = vec![0; 65536].into_boxed_slice();
    let mut machine = Machine::new(&mut ram);

    machine.insert_program(PROGRAM.as_bytes(), Some(0x0000));

    machine.set_inspect(debugger());

    machine.run(0x0000);
}
