use proptest::collection::*;
use proptest::prelude::*;
use treewalk::ast::*;
use treewalk::pretty_printer::*;

fn arb_literal() -> BoxedStrategy<Literal> {
    //TODO: add missing literals
    prop_oneof![Just(Literal::NilLiteral),].boxed()
}

fn arb_expression() -> BoxedStrategy<Expr> {
    //TODO: add missing expressions
    prop_oneof![arb_literal().prop_map(Expr::Literal),].boxed()
}

fn arb_statement() -> BoxedStrategy<Statement> {
    //TODO: add missing statements
    prop_oneof![
        arb_expression().prop_map(Statement::Print),
        arb_expression().prop_map(Statement::Expression),
        arb_expression().prop_map(|e|Statement::Return(Some(e))),
        Just(Statement::Return(None)),
        ].boxed()
}

pub fn arb_program() -> VecStrategy<BoxedStrategy<Statement>> {
    prop::collection::vec(arb_statement(), 0..10)
}

pub fn arb_program_text() -> BoxedStrategy<String> {
    //TODO: provide some sample identifiers
    let mut identifier_map = IdentifierMap::new();
    identifier_map.from_name("identifier");
    arb_program()
        .prop_map(move |statements| {
            let mut text = String::new();
            for s in statements {
                s.pretty_print_into(&identifier_map, &mut text);
                text.push_str("\n");
            }
            text
        })
        .boxed()
}

#[macro_export]
macro_rules! rulox_implementation_tests {
    ($implementation:ident) => {
        use arb::program::*;
        use user_interface::*;

        proptest! {
        #[test]
        fn doesnt_crash(ref input in "\\PC*") {
            let mut ruloxvm = $implementation::new();
            //TODO: this might not terminate if the input contains an infinite loop
            ruloxvm.run(input)
        }
        }

        proptest! {
        #[test]
        fn handles_a_valid_program(ref input in arb_program_text()) {
            let mut ruloxvm = $implementation::new();
            //TODO: this might not terminate if the input contains an infinite loop
            let result = ruloxvm.run(input);
            assert!(result != RunResult::InvalidProgram);
        }
        }
    };
}
