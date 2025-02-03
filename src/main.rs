use clap::Parser;
use rc::{parse, token::tokenize};
use std::{
    path::PathBuf,
    process::{exit, Command},
};

#[derive(Parser, Debug)]
struct Args {
    file: PathBuf,
    #[arg(long, action = clap::ArgAction::SetTrue)]
    lex: Option<bool>,
    #[arg(long, action = clap::ArgAction::SetTrue)]
    parse: Option<bool>,
    #[arg(long, action = clap::ArgAction::SetTrue)]
    codegen: Option<bool>,
}

fn main() {
    let args = Args::parse();

    let abs_path = args.file.canonicalize().expect("invalid file path");
    let abs_path = abs_path.to_str().unwrap();

    let preprocess = Command::new("gcc")
        .args(&["-E", "-P", abs_path, "-o", "-"])
        .output()
        .expect("failed to start command");

    let mut _ts = tokenize(preprocess.stdout).unwrap_or_else(|_| exit(1));

    if let Some(_) = args.lex {
        return;
    }

    let mut _p = parse::parse(&mut _ts).unwrap();

    if let Some(_) = args.parse {
        return;
    }

    if let Some(_) = args.codegen {
        return;
    }

    // let mut assemble = Command::new("gcc")
    //     .args(&["-x", "c", "-", "-o", "out"])
    //     .stdin(Stdio::piped())
    //     .stdout(Stdio::piped())
    //     .spawn()
    //     .expect("failed to spawn command");

    // // 一時ファイル作るのが面倒なので標準入力でなんとかする
    // assemble
    //     .stdin
    //     .as_mut()
    //     .unwrap()
    //     .write_all(&preprocess.stdout)
    //     .expect("failed pass assembly to assemble");

    // let output = assemble.wait_with_output().expect("failed to assemble");

    // println!("{}", String::from_utf8_lossy(&output.stdout));
}
