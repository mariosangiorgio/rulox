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
    ].boxed()
}

fn arb_literal() -> BoxedStrategy<Literal> {
    //TODO: add missing literals
    prop_oneof![
        Just(Literal::NilLiteral),
        Just(Literal::BoolLiteral(false)),
        Just(Literal::BoolLiteral(true)),
        ].boxed()
}

fn arb_expression() -> BoxedStrategy<Expr> {
    //TODO: add missing expressions
    prop_oneof![arb_literal().prop_map(Expr::Literal),].boxed()
}

fn arb_simple_statement(identifier_map: &mut IdentifierMap) -> BoxedStrategy<Statement> {
    prop_oneof![
        arb_expression().prop_map(Statement::Print),
        arb_expression().prop_map(Statement::Expression),
        arb_expression().prop_map(|e| Statement::Return(Some(e))),
        Just(Statement::Return(None)),
        arb_identifier(identifier_map).prop_map(Statement::VariableDefinition),
        (arb_identifier(identifier_map), arb_expression())
            .prop_map(|(id, expr)| Statement::VariableDefinitionWithInitalizer(id, expr)),
    ].boxed()
}

fn arb_complex_statement(
    identifier_map: &mut IdentifierMap,
    depth: u8,
) -> BoxedStrategy<Statement> {
    prop_oneof![
        arb_statements(identifier_map, depth).prop_map(|statements| Statement::Block(Box::new(
            Block {
                statements: statements
            }
        ))),
        (arb_expression(), arb_statement(identifier_map, depth)).prop_map(|(expr, statement)| {
            Statement::IfThen(Box::new(IfThen {
                condition: expr,
                then_branch: statement,
            }))
        }),
        (
            arb_expression(),
            arb_statement(identifier_map, depth),
            arb_statement(identifier_map, depth)
        ).prop_map(
            |(expr, then_statement, else_statement)| Statement::IfThenElse(Box::new(IfThenElse {
                condition: expr,
                then_branch: then_statement,
                else_branch: else_statement
            }))
        ),
        (arb_expression(), arb_statement(identifier_map, depth)).prop_map(|(expr, statement)| {
            Statement::While(Box::new(While {
                condition: expr,
                body: statement,
            }))
        }),
        //TODO: add FunctionDefinition
        //TODO: add Class
    ].boxed()
}

fn arb_statement(identifier_map: &mut IdentifierMap, depth: u8) -> BoxedStrategy<Statement> {
    if depth < 5 {
        prop_oneof![
            arb_simple_statement(identifier_map),
            arb_complex_statement(identifier_map, depth + 1),
        ].boxed()
    } else {
        arb_simple_statement(identifier_map)
    }
}

pub fn arb_statements(
    identifier_map: &mut IdentifierMap,
    depth: u8,
) -> VecStrategy<BoxedStrategy<Statement>> {
    prop::collection::vec(arb_statement(identifier_map, depth), 0..10)
}

pub fn arb_program_text() -> BoxedStrategy<String> {
    //TODO: provide some sample identifiers
    let mut identifier_map = IdentifierMap::new();
    arb_statements(&mut identifier_map, 0)
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
