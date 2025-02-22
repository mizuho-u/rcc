use rc::{asm, codegen::generate, parse, tacky, token};

#[test]
fn immediate() {
    let mut result = token::tokenize(" int main(void) { return 100; } ".into()).unwrap();
    let result = parse::parse(&mut result).unwrap();
    let result = tacky::convert(result).unwrap();
    let result = asm::convert(result).unwrap();
    let result = generate(result).unwrap();

    let expect = "\t.global main\nmain:\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp\n\tsubq\t$0, %rsp\n\tmovl\t$100, %eax\n\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret\n\n\t.section\t.note.GNU-stack,\"\",@progbits\n";

    assert_eq!(result, expect, "{result}")
}

#[test]
fn binop() {
    let mut result = token::tokenize(" int main(void) { return 2 * (3 + 4); } ".into()).unwrap();
    let result = parse::parse(&mut result).unwrap();
    let result = tacky::convert(result).unwrap();
    let result = asm::convert(result).unwrap();
    let result = generate(result).unwrap();

    let expect = "\t.global main\nmain:\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp\n\tsubq\t$8, %rsp\n\tmovl\t$3, -4(%rbp)\n\taddl\t$4,-4(%rbp)\n\tmovl\t$2, -8(%rbp)\n\tmovl\t-8(%rbp), %r11d\n\timull\t-4(%rbp),%r11d\n\tmovl\t%r11d, -8(%rbp)\n\tmovl\t-8(%rbp), %eax\n\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret\n\n\t.section\t.note.GNU-stack,\"\",@progbits\n";

    assert_eq!(result, expect, "{result}")
}

// #[test]
// fn lop() {
//     let mut result = token::tokenize(" int main(void) { return 1 >= 2; } ".into()).unwrap();
//     let result = parse::parse(&mut result).unwrap();
//     let result = tacky::convert(result).unwrap();
//     let result = asm::convert(result).unwrap();
//     let result = generate(result).unwrap();

//     let expect = "\t.global main\nmain:\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp\n\tsubq\t$8, %rsp\n\tmovl\t$3, -4(%rbp)\n\taddl\t$4,-4(%rbp)\n\tmovl\t$2, -8(%rbp)\n\tmovl\t-8(%rbp), %r11d\n\timull\t-4(%rbp),%r11d\n\tmovl\t%r11d, -8(%rbp)\n\tmovl\t-8(%rbp), %eax\n\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret\n\n\t.section\t.note.GNU-stack,\"\",@progbits\n";

//     assert_eq!(result, expect, "{result}")
// }
