use proptest::collection::*;
use proptest::prelude::*;
use treewalk::ast::*;
use treewalk::pretty_printer::*;

fn arb_identifier(identifier_map: &mut IdentifierMap) -> BoxedStrategy<Identifier> {
    prop_oneof![
        Just(identifier_map.from_name("this")),
        Just(identifier_map.from_name("init")),
        Just(identifier_map.from_name("super")),
        Just(identifier_map.from_name("a")),
        Just(identifier_map.from_name("b")),
        Just(identifier_map.from_name("c")),
    ]
    .boxed()
}

fn arb_literal() -> BoxedStrategy<Literal> {
    //TODO: add missing literals
    prop_oneof![Just(Literal::NilLiteral),].boxed()
}

fn arb_expression() -> BoxedStrategy<Expr> {
    //TODO: add missing expressions
    prop_oneof![arb_literal().prop_map(Expr::Literal),].boxed()
}

fn arb_statement(identifier_map: &mut IdentifierMap) -> BoxedStrategy<Statement> {
    //TODO: add missing statements
    prop_oneof![
        arb_expression().prop_map(Statement::Print),
        arb_expression().prop_map(Statement::Expression),
        arb_expression().prop_map(|e|Statement::Return(Some(e))),
        Just(Statement::Return(None)),
        arb_identifier(identifier_map).prop_map(Statement::VariableDefinition),
        ].boxed()
}

pub fn arb_program(identifier_map: &mut IdentifierMap) -> VecStrategy<BoxedStrategy<Statement>> {
    prop::collection::vec(arb_statement(identifier_map), 0..10)
}

pub fn arb_program_text() -> BoxedStrategy<String> {
    //TODO: provide some sample identifiers
    let mut identifier_map = IdentifierMap::new();
    arb_program(&mut identifier_map)
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
        fn doesnt_crash_with_random_input(ref input in "\\PC*") {
            let mut ruloxvm = $implementation::new();
            //TODO: this might not terminate if the input contains an infinite loop
            ruloxvm.run(input)
        }
        }

        proptest! {
        #[test]
        fn doesnt_crash_with_arbitrary_program(ref input in arb_program_text()) {
            let mut ruloxvm = $implementation::new();
            //TODO: this might not terminate if the input contains an infinite loop
            let _result = ruloxvm.run(input);
            // arb_program_text isn't good enough to generate valid
            // programs from the ast structure.
            // Lexical scope resolution rules and some special cases
            // make it quite tricky to do it right.
            // This test should at least help us expose interesting
            // behaviours more easily.
        }
        }
    };
}
