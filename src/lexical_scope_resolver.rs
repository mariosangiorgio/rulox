use ast::*;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Depth {
    Global,
    Local(usize),
}

pub struct LexicalScopes {
    depths: HashMap<ExpressionHandle, Depth>,
}

impl LexicalScopes {
    fn new() -> LexicalScopes {
        LexicalScopes { depths: HashMap::new() }
    }

    pub fn get_depth(&self, handle: &ExpressionHandle) -> &Depth {
        self.depths.get(handle).unwrap()
    }
}

pub enum LexicalScopesResolutionError {
    ReadLocalInItsOwnInitializer,
}

trait LexicalScopesResolver {
    fn resolve(&self,
               &mut ProgramLexicalScopesResolver)
               -> Result<(), LexicalScopesResolutionError>;
}

pub struct ProgramLexicalScopesResolver {
    scopes: Vec<HashMap<String, bool>>,
    lexical_scopes: LexicalScopes,
}

impl ProgramLexicalScopesResolver {
    pub fn new() -> ProgramLexicalScopesResolver {
        ProgramLexicalScopesResolver {
            scopes: vec![],
            lexical_scopes: LexicalScopes::new(),
        }
    }

    fn begin_scope(&mut self) -> () {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) -> () {
        self.scopes.pop();
    }

    fn declare(&mut self, identifier: String) -> () {
        let scopes = self.scopes.len();
        if scopes == 0 {
            return;
        };
        self.scopes[scopes - 1].insert(identifier, false);
    }

    fn define(&mut self, identifier: String) -> () {
        let scopes = self.scopes.len();
        if scopes == 0 {
            return;
        };
        self.scopes[scopes - 1].insert(identifier, true);
    }

    fn is_defined(&self, identifier: &String) -> bool {
        let scopes = self.scopes.len();
        scopes != 0 && self.scopes[scopes - 1][identifier]
    }

    fn resolve_local(&mut self, handle: ExpressionHandle, identifier: &String) -> () {
        let max_depth = self.scopes.len();
        for depth in 0..max_depth {
            if self.scopes[max_depth - depth].contains_key(identifier) {
                self.lexical_scopes
                    .depths
                    .insert(handle, Depth::Local(depth));
                return;
            }
        }
        self.lexical_scopes.depths.insert(handle, Depth::Global);
    }

    fn resolve(&mut self, statement: &Statement) -> Result<(), LexicalScopesResolutionError> {
        statement.resolve(self)
    }
}

impl LexicalScopesResolver for Statement {
    fn resolve(&self,
               resolver: &mut ProgramLexicalScopesResolver)
               -> Result<(), LexicalScopesResolutionError> {
        match *self {
            Statement::Block(ref b) => {
                resolver.begin_scope();
                for statement in &b.statements {
                    try!(statement.resolve(resolver));
                }
                resolver.end_scope();
                Ok(())
            }
            Statement::VariableDefinition(ref identifier) => {
                //TODO: avoid all these copies
                resolver.declare(identifier.name.to_owned());
                resolver.define(identifier.name.to_owned());
                Ok(())
            }
            Statement::VariableDefinitionWithInitalizer(ref identifier, ref initializer) => {
                //TODO: avoid all these copies
                resolver.declare(identifier.name.to_owned());
                try!(initializer.resolve(resolver));
                resolver.define(identifier.name.to_owned());
                Ok(())
            }
            Statement::FunctionDefinition(ref f) => f.resolve(resolver),
            Statement::Expression(ref e) => e.resolve(resolver),
            Statement::IfThen(ref s) => {
                s.condition
                    .resolve(resolver)
                    .and_then(|_| s.then_branch.resolve(resolver))
            }
            Statement::IfThenElse(ref s) => {
                s.condition
                    .resolve(resolver)
                    .and_then(|_| s.then_branch.resolve(resolver))
                    .and_then(|_| s.else_branch.resolve(resolver))
            }
            Statement::While(ref s) => {
                s.condition
                    .resolve(resolver)
                    .and_then(|_| s.body.resolve(resolver))
            }
            Statement::Print(ref e) => e.resolve(resolver),
            Statement::Return(None) => Ok(()),
            Statement::Return(Some(ref e)) => e.resolve(resolver),
        }
    }
}

impl LexicalScopesResolver for Expr {
    fn resolve(&self,
               resolver: &mut ProgramLexicalScopesResolver)
               -> Result<(), LexicalScopesResolutionError> {
        match *self {
            Expr::Identifier(ref handle, ref identifier) => {
                if !resolver.is_defined(&identifier.name) {
                    Err(LexicalScopesResolutionError::ReadLocalInItsOwnInitializer)
                } else {
                    resolver.resolve_local(*handle, &identifier.name);
                    Ok(())
                }
            }
            Expr::Assignment(ref assigment) => assigment.resolve(resolver),
            Expr::Literal(_) => Ok(()),
            Expr::Unary(ref e) => e.right.resolve(resolver),
            Expr::Binary(ref e) => {
                e.left
                    .resolve(resolver)
                    .and_then(|_| e.right.resolve(resolver))
            }
            Expr::Logic(ref e) => {
                e.left
                    .resolve(resolver)
                    .and_then(|_| e.right.resolve(resolver))
            }
            Expr::Grouping(ref e) => e.expr.resolve(resolver),
            Expr::Call(ref e) => {
                try!(e.callee.resolve(resolver));
                for argument in e.arguments.iter() {
                    try!(argument.resolve(resolver));
                }
                Ok(())
            }
        }
    }
}

impl LexicalScopesResolver for Assignment {
    fn resolve(&self,
               resolver: &mut ProgramLexicalScopesResolver)
               -> Result<(), LexicalScopesResolutionError> {
        try!{self.rvalue.resolve(resolver)};
        let Target::Identifier(ref identifier) = self.lvalue;
        resolver.resolve_local(self.handle, &identifier.name);
        Ok(())
    }
}

impl LexicalScopesResolver for FunctionDefinition {
    fn resolve(&self,
               resolver: &mut ProgramLexicalScopesResolver)
               -> Result<(), LexicalScopesResolutionError> {
        //TODO: avoid all these copies
        resolver.declare(self.name.name.to_owned());
        resolver.define(self.name.name.to_owned());
        resolver.begin_scope();
        for argument in self.arguments.iter() {
            resolver.declare(argument.name.to_owned());
            resolver.define(argument.name.to_owned());
        }
        let _ = try!(self.body.resolve(resolver));
        resolver.end_scope();
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use scanner::*;
    use parser::*;
    use lexical_scope_resolver::*;

    #[test]
    fn global_variable() {
        let (tokens, _) = scan(&"var a = 0;{fun f() {print a;}}");
        let statements = Parser::new().parse(&tokens).unwrap();
        let mut lexical_scope_resolver = ProgramLexicalScopesResolver::new();
        for statement in statements.iter() {
            let _ = lexical_scope_resolver.resolve(&statement);
        }
        let lexical_scopes = lexical_scope_resolver.lexical_scopes;
        let mut expression_handle_factory = ExpressionHandleFactory::new();
        let _ = expression_handle_factory.next(); // Declaration
        let handle = expression_handle_factory.next(); // Use
        assert_eq!(&Depth::Global, lexical_scopes.get_depth(&handle));
    }

    #[test]
    fn captured_variable() {
        let (tokens, _) = scan(&"{var a = 0;fun f() {print a;}}");
        let statements = Parser::new().parse(&tokens).unwrap();
        let mut lexical_scope_resolver = ProgramLexicalScopesResolver::new();
        for statement in statements.iter() {
            let _ = lexical_scope_resolver.resolve(&statement);
        }
        let lexical_scopes = lexical_scope_resolver.lexical_scopes;
        let mut expression_handle_factory = ExpressionHandleFactory::new();
        let _ = expression_handle_factory.next(); // Declaration
        let handle = expression_handle_factory.next(); // Use
        assert_eq!(&Depth::Local(1), lexical_scopes.get_depth(&handle));
    }

    #[test]
    fn lexical_capture() {
        let (tokens, _) = scan(&"var a = 0;{fun f() {print a;} var a = 1;}");
        let statements = Parser::new().parse(&tokens).unwrap();
        let mut lexical_scope_resolver = ProgramLexicalScopesResolver::new();
        for statement in statements.iter() {
            let _ = lexical_scope_resolver.resolve(&statement);
        }
        let lexical_scopes = lexical_scope_resolver.lexical_scopes;
        let mut expression_handle_factory = ExpressionHandleFactory::new();
        let _ = expression_handle_factory.next(); // Declaration
        let handle = expression_handle_factory.next(); // Use
        assert_eq!(&Depth::Global, lexical_scopes.get_depth(&handle));
    }



    #[test]
    fn error_on_shadowing() {
        let (tokens, _) = scan(&"var a = 0;{var a = a;}");
        let statements = Parser::new().parse(&tokens).unwrap();
        let mut lexical_scope_resolver = ProgramLexicalScopesResolver::new();
        assert!(lexical_scope_resolver.resolve(&statements[0]).is_ok());
        assert!(lexical_scope_resolver.resolve(&statements[1]).is_err());
    }
}
