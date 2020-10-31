use std::{
    env::args,
    fs::File,
    io::{stdin, stdout, Read, Write},
};

const TAPE_LENGTH: usize = 1 << 20;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Cmd {
    Incr(u8),
    Right(usize),
    Open(usize),
    Close(usize),
    In,
    Out,
    Clear,
}

pub fn parse(s: &str) -> Result<Vec<Cmd>, ()> {
    let mut cmd = Vec::new();
    let mut stack = Vec::new();
    for c in s.chars() {
        match c {
            '+' => match cmd.last_mut() {
                Some(Cmd::Incr(i)) => *i = i.wrapping_add(1),
                _ => cmd.push(Cmd::Incr(1)),
            },
            '-' => match cmd.last_mut() {
                Some(Cmd::Incr(i)) => *i = i.wrapping_add(u8::MAX),
                _ => cmd.push(Cmd::Incr(u8::MAX)),
            },
            '>' => match cmd.last_mut() {
                Some(Cmd::Right(i)) => *i += 1,
                _ => cmd.push(Cmd::Right(1)),
            },
            '<' => match cmd.last_mut() {
                Some(Cmd::Right(i)) => *i = (*i + TAPE_LENGTH - 1) % TAPE_LENGTH,
                _ => cmd.push(Cmd::Right(TAPE_LENGTH - 1)),
            },
            '[' => {
                stack.push(cmd.len());
                cmd.push(Cmd::Open(usize::MAX));
            }
            ']' => {
                if let Some(i) = stack.pop() {
                    if cmd.len() == i + 2 && cmd.last() == Some(&Cmd::Incr(u8::MAX)) {
                        cmd.pop(); // Incr
                        cmd.pop(); // Open
                        cmd.push(Cmd::Clear);
                    } else {
                        cmd.push(Cmd::Close(i + 1));
                        cmd[i] = Cmd::Open(cmd.len());
                    }
                } else {
                    return Err(());
                }
            }
            ',' => cmd.push(Cmd::In),
            '.' => cmd.push(Cmd::Out),
            _ => (),
        }
    }
    Ok(cmd)
}

pub fn execute<R: Read, W: Write>(cmd: &[Cmd], cin: &mut R, cout: &mut W) {
    let mut tape = vec![0u8; TAPE_LENGTH];
    let mut pp = 0;
    let mut tp = 0;
    while pp < cmd.len() {
        match cmd[pp] {
            Cmd::Incr(i) => {
                tape[tp] = tape[tp].wrapping_add(i);
                pp += 1;
            }
            Cmd::Right(i) => {
                tp = (tp + i) % TAPE_LENGTH;
                pp += 1;
            }
            Cmd::Open(i) => {
                if tape[tp] == 0 {
                    pp = i;
                } else {
                    pp += 1;
                }
            }
            Cmd::Close(i) => {
                if tape[tp] == 0 {
                    pp += 1;
                } else {
                    pp = i;
                }
            }
            Cmd::Clear => {
                tape[tp] = 0;
                pp += 1;
            }
            Cmd::In => {
                let mut buf = [0];
                tape[tp] = match cin.read_exact(&mut buf) {
                    Ok(()) => buf[0],
                    Err(_) => u8::MAX,
                };
                pp += 1;
            }
            Cmd::Out => {
                cout.write_all(&[tape[tp]]).unwrap();
                cout.flush().unwrap();
                pp += 1;
            }
        }
    }
}

fn main() {
    let mut args = args().skip(1);
    let mut program = String::new();
    File::open(&args.next().expect("Usage: ./naive-interpreter <bf>"))
        .unwrap()
        .read_to_string(&mut program)
        .unwrap();
    let cmd = parse(&program).unwrap();
    let mut cout = stdout();
    let mut cin = stdin();
    execute(&cmd, &mut cin, &mut cout);
}
