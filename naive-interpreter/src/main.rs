use std::error::Error;
use std::{
    env::args,
    fs::File,
    io::{stdin, stdout, Read, Write},
};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Cmd {
    Incr,
    Decr,
    Right,
    Left,
    Open(usize),
    Close(usize),
    In,
    Out,
}

pub fn parse(s: &str) -> Result<Vec<Cmd>, ()> {
    let mut cmd = Vec::new();
    let mut stack = Vec::new();
    for c in s.chars() {
        match c {
            '+' => cmd.push(Cmd::Incr),
            '-' => cmd.push(Cmd::Decr),
            '>' => cmd.push(Cmd::Right),
            '<' => cmd.push(Cmd::Left),
            '[' => {
                stack.push(cmd.len());
                cmd.push(Cmd::Open(usize::MAX));
            }
            ']' => {
                if let Some(i) = stack.pop() {
                    cmd.push(Cmd::Close(i));
                    cmd[i] = Cmd::Open(cmd.len());
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
    const N: usize = 1 << 20;
    let mut tape = vec![0u8; N];
    let mut pp = 0;
    let mut tp = 0;
    while pp < cmd.len() {
        match cmd[pp] {
            Cmd::Incr => {
                tape[tp] = tape[tp].wrapping_add(1);
                pp += 1;
            }
            Cmd::Decr => {
                tape[tp] = tape[tp].wrapping_sub(1);
                pp += 1;
            }
            Cmd::Right => {
                tp += 1;
                pp += 1;
            }
            Cmd::Left => {
                tp = (tp + N - 1) % N;
                pp += 1
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
