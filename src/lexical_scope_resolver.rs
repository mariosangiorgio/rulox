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
                resolver.define(identifier.name.to_owned());
                Ok(())
            }
            Statement::VariableDefinitionWithInitalizer(ref identifier, ref initializer) => {
                try!(initializer.resolve(resolver));
                resolver.define(identifier.name.to_owned());
                Ok(())
            }
            _ => unimplemented!(),
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
            _ => unimplemented!(),
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
            lexical_scope_resolver.resolve(&statement);
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
            lexical_scope_resolver.resolve(&statement);
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
            lexical_scope_resolver.resolve(&statement);
        }
        let lexical_scopes = lexical_scope_resolver.lexical_scopes;
        let mut expression_handle_factory = ExpressionHandleFactory::new();
        let _ = expression_handle_factory.next(); // Declaration
        let handle = expression_handle_factory.next(); // Use
        assert_eq!(&Depth::Global, lexical_scopes.get_depth(&handle));
    }
}
