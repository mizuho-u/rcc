use rc::{asm, codegen::generate, parse, tacky, token};

#[test]
fn immediate() {
    let result = tokenize_to_generate(" int main(void) { return 100; } ");

    let expect = "\t.global main\nmain:\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp\n\tsubq\t$0, %rsp\n\tmovl\t$100, %eax\n\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret\n\n\t.section\t.note.GNU-stack,\"\",@progbits\n";

    assert_eq!(result, expect, "{result}")
}

#[test]
fn binop() {
    let result = tokenize_to_generate(" int main(void) { return 2 * (3 + 4); } ");

    let expect = "\t.global main\nmain:\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp\n\tsubq\t$8, %rsp\n\tmovl\t$3, -4(%rbp)\n\taddl\t$4,-4(%rbp)\n\tmovl\t$2, -8(%rbp)\n\tmovl\t-8(%rbp), %r11d\n\timull\t-4(%rbp),%r11d\n\tmovl\t%r11d, -8(%rbp)\n\tmovl\t-8(%rbp), %eax\n\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret\n\n\t.section\t.note.GNU-stack,\"\",@progbits\n";

    assert_eq!(result, expect, "{result}")
}

#[test]
fn a() {
    let result = tokenize_to_generate(
        " int main(void) {
    int a = 1;
    switch(a) {
        case 0: return 0;
        case 2: return 0;
        case 3: return 0;
    }
    return 1;
} ",
    );

    let expect = "\t.global main\nmain:\n\tpushq\t%rbp\n\tmovq\t%rsp, %rbp\n\tsubq\t$8, %rsp\n\tmovl\t$3, -4(%rbp)\n\taddl\t$4,-4(%rbp)\n\tmovl\t$2, -8(%rbp)\n\tmovl\t-8(%rbp), %r11d\n\timull\t-4(%rbp),%r11d\n\tmovl\t%r11d, -8(%rbp)\n\tmovl\t-8(%rbp), %eax\n\tmovq\t%rbp, %rsp\n\tpopq\t%rbp\n\tret\n\n\t.section\t.note.GNU-stack,\"\",@progbits\n";

    assert_eq!(result, expect, "{result}")
}

fn tokenize_to_generate(p: &str) -> String {
    let mut result = token::tokenize(p.into()).unwrap();

    let mut result = parse::parse(&mut result).unwrap();
    parse::validate(&mut result).unwrap();

    let result = tacky::convert(result).unwrap();

    let result = asm::convert(result).unwrap();

    generate(result).unwrap()
}
