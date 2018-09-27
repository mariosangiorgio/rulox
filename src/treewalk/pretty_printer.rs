use treewalk::ast::*;

pub trait PrettyPrint {
    fn pretty_print_into(&self, identifier_map: &IdentifierMap, pretty_printed: &mut String) -> ();
    fn pretty_print(&self, identifier_map: &IdentifierMap) -> String {
        let mut pretty_printed = String::new();
        self.pretty_print_into(identifier_map, &mut pretty_printed);
        pretty_printed
    }
}

impl PrettyPrint for Expr {
    fn pretty_print_into(&self, identifier_map: &IdentifierMap, pretty_printed: &mut String) -> () {
        match *self {
            Expr::This(_, _) => pretty_printed.push_str("this"),
            Expr::Super(_, _, ref identifier) => {
                pretty_printed.push_str("super.");
                identifier.pretty_print_into(identifier_map, pretty_printed)
            }
            Expr::Literal(ref l) => l.pretty_print_into(identifier_map, pretty_printed),
            Expr::Unary(ref u) => u.pretty_print_into(identifier_map, pretty_printed),
            Expr::Binary(ref b) => b.pretty_print_into(identifier_map, pretty_printed),
            Expr::Logic(ref b) => b.pretty_print_into(identifier_map, pretty_printed),
            Expr::Grouping(ref g) => g.pretty_print_into(identifier_map, pretty_printed),
            Expr::Identifier(ref _h, ref i) => i.pretty_print_into(identifier_map, pretty_printed),
            Expr::Assignment(ref a) => a.pretty_print_into(identifier_map, pretty_printed),
            Expr::Call(ref c) => c.pretty_print_into(identifier_map, pretty_printed),
            Expr::Get(ref g) => g.pretty_print_into(identifier_map, pretty_printed),
            Expr::Set(ref s) => s.pretty_print_into(identifier_map, pretty_printed),
        }
    }
}

impl PrettyPrint for UnaryOperator {
    fn pretty_print_into(
        &self,
        _identifier_map: &IdentifierMap,
        pretty_printed: &mut String,
    ) -> () {
        match *self {
            UnaryOperator::Bang => pretty_printed.push_str("!"),
            UnaryOperator::Minus => pretty_printed.push_str("-"),
        }
    }
}

impl PrettyPrint for BinaryOperator {
    fn pretty_print_into(
        &self,
        _identifier_map: &IdentifierMap,
        pretty_printed: &mut String,
    ) -> () {
        match *self {
            BinaryOperator::Minus => pretty_printed.push_str("-"),
            BinaryOperator::Plus => pretty_printed.push_str("+"),
            BinaryOperator::Slash => pretty_printed.push_str("/"),
            BinaryOperator::Star => pretty_printed.push_str("*"),
            BinaryOperator::Equal => pretty_printed.push_str("=="),
            BinaryOperator::NotEqual => pretty_printed.push_str("!="),
            BinaryOperator::Less => pretty_printed.push_str("<"),
            BinaryOperator::LessEqual => pretty_printed.push_str("<="),
            BinaryOperator::Greater => pretty_printed.push_str(">"),
            BinaryOperator::GreaterEqual => pretty_printed.push_str(">="),
        }
    }
}

impl PrettyPrint for LogicOperator {
    fn pretty_print_into(
        &self,
        _identifier_map: &IdentifierMap,
        pretty_printed: &mut String,
    ) -> () {
        match *self {
            LogicOperator::Or => pretty_printed.push_str("or"),
            LogicOperator::And => pretty_printed.push_str("and"),
        }
    }
}

impl PrettyPrint for Literal {
    fn pretty_print_into(
        &self,
        _identifier_map: &IdentifierMap,
        pretty_printed: &mut String,
    ) -> () {
        match *self {
            Literal::NilLiteral => pretty_printed.push_str("null"),
            Literal::BoolLiteral(ref b) => pretty_printed.push_str(&b.to_string()),
            Literal::StringLiteral(ref s) => pretty_printed.push_str(s),
            Literal::NumberLiteral(n) => pretty_printed.push_str(&n.to_string()),
        }
    }
}

impl PrettyPrint for Identifier {
    fn pretty_print_into(&self, identifier_map: &IdentifierMap, pretty_printed: &mut String) -> () {
        pretty_printed.push_str(identifier_map.lookup(self).unwrap())
    }
}

impl PrettyPrint for Target {
    fn pretty_print_into(&self, identifier_map: &IdentifierMap, pretty_printed: &mut String) -> () {
        match *self {
            Target::Identifier(ref i) => i.pretty_print_into(identifier_map, pretty_printed),
        }
    }
}

impl PrettyPrint for Assignment {
    fn pretty_print_into(&self, identifier_map: &IdentifierMap, pretty_printed: &mut String) -> () {
        self.lvalue
            .pretty_print_into(identifier_map, pretty_printed);
        pretty_printed.push_str(" = ");
        self.rvalue
            .pretty_print_into(identifier_map, pretty_printed);
    }
}

impl PrettyPrint for Grouping {
    fn pretty_print_into(&self, identifier_map: &IdentifierMap, pretty_printed: &mut String) -> () {
        pretty_printed.push_str("(group ");
        self.expr.pretty_print_into(identifier_map, pretty_printed);
        pretty_printed.push_str(")");
    }
}

impl PrettyPrint for UnaryExpr {
    fn pretty_print_into(&self, identifier_map: &IdentifierMap, pretty_printed: &mut String) -> () {
        pretty_printed.push_str("(");
        self.operator
            .pretty_print_into(identifier_map, pretty_printed);
        pretty_printed.push_str(" ");
        self.right.pretty_print_into(identifier_map, pretty_printed);
        pretty_printed.push_str(")");
    }
}

impl PrettyPrint for LogicExpr {
    fn pretty_print_into(&self, identifier_map: &IdentifierMap, pretty_printed: &mut String) -> () {
        pretty_printed.push_str("(");
        self.operator
            .pretty_print_into(identifier_map, pretty_printed);
        pretty_printed.push_str(" ");
        self.left.pretty_print_into(identifier_map, pretty_printed);
        pretty_printed.push_str(" ");
        self.right.pretty_print_into(identifier_map, pretty_printed);
        pretty_printed.push_str(")");
    }
}

impl PrettyPrint for BinaryExpr {
    fn pretty_print_into(&self, identifier_map: &IdentifierMap, pretty_printed: &mut String) -> () {
        pretty_printed.push_str("(");
        self.operator
            .pretty_print_into(identifier_map, pretty_printed);
        pretty_printed.push_str(" ");
        self.left.pretty_print_into(identifier_map, pretty_printed);
        pretty_printed.push_str(" ");
        self.right.pretty_print_into(identifier_map, pretty_printed);
        pretty_printed.push_str(")");
    }
}

impl PrettyPrint for Statement {
    fn pretty_print_into(&self, identifier_map: &IdentifierMap, pretty_printed: &mut String) -> () {
        match *self {
            Statement::Print(ref e) => {
                pretty_printed.push_str("print ");
                e.pretty_print_into(identifier_map, pretty_printed);
                pretty_printed.push_str(";");
            }
            Statement::Return(ref e) => {
                pretty_printed.push_str("return");
                if let Some(ref e) = *e {
                    pretty_printed.push_str(" ");
                    e.pretty_print_into(identifier_map, pretty_printed);
                }
                pretty_printed.push_str(";");
            }
            Statement::Expression(ref e) => {
                e.pretty_print_into(identifier_map, pretty_printed);
                pretty_printed.push_str(";");
            }
            Statement::VariableDefinition(ref identifier) => {
                pretty_printed.push_str("var ");
                identifier.pretty_print_into(identifier_map, pretty_printed);
                pretty_printed.push_str(";");
            }
            Statement::VariableDefinitionWithInitalizer(ref identifier, ref initializer) => {
                pretty_printed.push_str("var ");
                identifier.pretty_print_into(identifier_map, pretty_printed);
                pretty_printed.push_str(" = ");
                initializer.pretty_print_into(identifier_map, pretty_printed);
                pretty_printed.push_str(";");
            }
            Statement::Block(ref b) => {
                pretty_printed.push_str("{ ");
                for statement in &b.statements {
                    statement.pretty_print_into(identifier_map, pretty_printed);
                    pretty_printed.push_str(" ");
                }
                pretty_printed.push_str("}");
            }
            Statement::IfThen(ref c) => {
                pretty_printed.push_str("if ( ");
                c.condition
                    .pretty_print_into(identifier_map, pretty_printed);
                pretty_printed.push_str(" ) ");
                c.then_branch
                    .pretty_print_into(identifier_map, pretty_printed);
            }
            Statement::IfThenElse(ref c) => {
                pretty_printed.push_str("if ( ");
                c.condition
                    .pretty_print_into(identifier_map, pretty_printed);
                pretty_printed.push_str(" ) ");
                c.then_branch
                    .pretty_print_into(identifier_map, pretty_printed);
                pretty_printed.push_str(" else ");
                c.else_branch
                    .pretty_print_into(identifier_map, pretty_printed);
            }
            Statement::While(ref l) => {
                pretty_printed.push_str("while ( ");
                l.condition
                    .pretty_print_into(identifier_map, pretty_printed);
                pretty_printed.push_str(" ) ");
                l.body.pretty_print_into(identifier_map, pretty_printed);
            }
            Statement::FunctionDefinition(ref f) => {
                if let FunctionKind::Function = f.kind {
                    pretty_printed.push_str("fun ");
                }
                f.name.pretty_print_into(identifier_map, pretty_printed);
                pretty_printed.push_str(" (");
                for argument in &f.arguments {
                    argument.pretty_print_into(identifier_map, pretty_printed);
                    pretty_printed.push_str(" ");
                }
                pretty_printed.push_str(") ");
                f.body.pretty_print_into(identifier_map, pretty_printed);
            }
            Statement::Class(ref c) => {
                pretty_printed.push_str("class ");
                c.name.pretty_print_into(identifier_map, pretty_printed);
                pretty_printed.push_str(" {");
                for method in &c.methods {
                    pretty_printed.push_str(" ");
                    Statement::FunctionDefinition(method.clone())
                        .pretty_print_into(identifier_map, pretty_printed);
                }
                pretty_printed.push_str(" }");
            }
        };
    }
}

impl PrettyPrint for Call {
    fn pretty_print_into(&self, identifier_map: &IdentifierMap, pretty_printed: &mut String) -> () {
        self.callee
            .pretty_print_into(identifier_map, pretty_printed);
        pretty_printed.push_str("( ");
        for arg in &self.arguments {
            arg.pretty_print_into(identifier_map, pretty_printed);
            pretty_printed.push_str(" ");
        }
        pretty_printed.push_str(")");
    }
}

impl PrettyPrint for Get {
    fn pretty_print_into(&self, identifier_map: &IdentifierMap, pretty_printed: &mut String) -> () {
        self.instance
            .pretty_print_into(identifier_map, pretty_printed);
        pretty_printed.push_str(".");
        self.property
            .pretty_print_into(identifier_map, pretty_printed);
    }
}

impl PrettyPrint for Set {
    fn pretty_print_into(&self, identifier_map: &IdentifierMap, pretty_printed: &mut String) -> () {
        self.instance
            .pretty_print_into(identifier_map, pretty_printed);
        pretty_printed.push_str(".");
        self.property
            .pretty_print_into(identifier_map, pretty_printed);
        pretty_printed.push_str(" = ");
        self.value.pretty_print_into(identifier_map, pretty_printed);
    }
}

#[cfg(test)]
mod tests {
    use treewalk::ast::*;
    use treewalk::pretty_printer::PrettyPrint;

    #[test]
    fn literal() {
        let identifier_map = IdentifierMap::new();
        let string = String::from("abc");
        let expr = Expr::Literal(Literal::StringLiteral(string.clone()));
        assert_eq!(string, expr.pretty_print(&identifier_map));
    }

    #[test]
    fn complex_expression() {
        let identifier_map = IdentifierMap::new();
        let subexpr1 = UnaryExpr {
            operator: UnaryOperator::Minus,
            right: Expr::Literal(Literal::NumberLiteral(123f64)),
        };
        let subexpr2 = Grouping {
            expr: Expr::Literal(Literal::NumberLiteral(45.67f64)),
        };
        let binary_expr = BinaryExpr {
            left: Expr::Unary(Box::new(subexpr1)),
            operator: BinaryOperator::Star,
            right: Expr::Grouping(Box::new(subexpr2)),
        };
        let expr = Expr::Binary(Box::new(binary_expr));
        assert_eq!(
            "(* (- 123) (group 45.67))",
            &expr.pretty_print(&identifier_map)
        );
    }

    #[test]
    fn block() {
        let mut identifier_map = IdentifierMap::new();
        let mut handle_factory = VariableUseHandleFactory::new();
        let identifier = identifier_map.from_name(&"x");
        let statements = vec![
            Statement::VariableDefinitionWithInitalizer(
                identifier.clone(),
                Expr::Literal(Literal::BoolLiteral(true)),
            ),
            Statement::Print(Expr::Identifier(handle_factory.next(), identifier.clone())),
        ];
        let block = Statement::Block(Box::new(Block {
            statements: statements,
        }));
        assert_eq!(
            "{ var x = true; print x; }",
            &block.pretty_print(&identifier_map)
        );
    }
}
