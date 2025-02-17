use clap::Parser;
use rc::{asm, codegen, parse, tacky, token::tokenize};
use std::{
    io::Write,
    path::PathBuf,
    process::{exit, Command, Stdio},
};

#[derive(Parser, Debug)]
struct Args {
    file: PathBuf,
    #[arg(long, action = clap::ArgAction::SetTrue)]
    lex: Option<bool>,
    #[arg(long, action = clap::ArgAction::SetTrue)]
    parse: Option<bool>,
    #[arg(long, action = clap::ArgAction::SetTrue)]
    tacky: Option<bool>,
    #[arg(long, action = clap::ArgAction::SetTrue)]
    codegen: Option<bool>,
}

fn main() {
    let args = Args::parse();

    let source_path = args.file.canonicalize().expect("invalid file path");
    let source = source_path.to_str().unwrap();

    let preprocess = Command::new("gcc")
        .args(&["-E", "-P", source, "-o", "-"])
        .output()
        .expect("failed to start command");

    let mut ts = tokenize(preprocess.stdout).unwrap_or_else(|_| exit(1));

    if matches!(args.lex, Some(true)) {
        return;
    }

    let p = parse::parse(&mut ts).unwrap_or_else(|_| exit(1));

    if matches!(args.parse, Some(true)) {
        return;
    }

    let p = tacky::convert(p).unwrap_or_else(|_| exit(1));

    if matches!(args.tacky, Some(true)) {
        return;
    }

    let p = asm::convert(p).unwrap_or_else(|_| exit(1));

    if matches!(args.codegen, Some(true)) {
        return;
    }

    let code = codegen::generate(p).unwrap_or_else(|_| exit(1));

    let exe_dir = source_path.parent().unwrap();
    let exe_file = source_path.file_stem().unwrap();
    let exe_path = exe_dir.join(exe_file);
    let exe_path = exe_path.to_str().unwrap();

    let mut assemble = Command::new("gcc")
        .args(&["-x", "assembler", "-", "-o", exe_path])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("failed to spawn command");

    // 一時ファイル作るのが面倒なので標準入力でなんとかする
    assemble
        .stdin
        .as_mut()
        .unwrap()
        .write_all(code.as_bytes())
        .expect("failed pass assembly to assemble");

    let output = assemble.wait_with_output().expect("failed to assemble");

    println!("{}", String::from_utf8_lossy(&output.stdout));
}
