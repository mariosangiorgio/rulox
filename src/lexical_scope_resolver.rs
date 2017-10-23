use ast::*;
use std::collections::HashMap;

pub type Depth = usize;

pub struct LexicalScopes {
    depths: HashMap<VariableUseHandle, Depth>,
}

impl LexicalScopes {
    fn new() -> LexicalScopes {
        LexicalScopes { depths: HashMap::new() }
    }

    pub fn get_depth(&self, handle: &VariableUseHandle) -> Option<&Depth> {
        self.depths.get(handle)
    }
}

#[derive(Debug)]
pub enum LexicalScopesResolutionError {
    ReadLocalInItsOwnInitializer,
}

trait LexicalScopesResolver {
    fn resolve(&self,
               &mut ProgramLexicalScopesResolver)
               -> Result<(), LexicalScopesResolutionError>;
}

#[derive(PartialEq)]
enum VariableDefinition {
    Undefined,
    Declared,
    Defined,
}

pub struct ProgramLexicalScopesResolver {
    // Note that this doesn't track globals at all
    scopes: Vec<HashMap<String, VariableDefinition>>,
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
        self.scopes[scopes - 1].insert(identifier, VariableDefinition::Declared);
    }

    fn define(&mut self, identifier: String) -> () {
        let scopes = self.scopes.len();
        if scopes == 0 {
            return;
        };
        self.scopes[scopes - 1].insert(identifier, VariableDefinition::Defined);
    }

    fn resolve_local(&mut self, handle: VariableUseHandle, identifier: &String) -> () {
        let max_depth = self.scopes.len();
        for depth in 0..max_depth {
            if self.scopes[max_depth - depth - 1].contains_key(identifier) {
                self.lexical_scopes.depths.insert(handle, depth);
                return;
            }
        }
        // If we failed to find it in the locals, it must be a global.
        // It might not be there right now, but it might appear later on.
        // We will know it only at runtime.
        self.lexical_scopes.depths.insert(handle, max_depth);
    }

    pub fn resolve(&mut self, statement: &Statement) -> Result<(), LexicalScopesResolutionError> {
        statement.resolve(self)
    }

    pub fn get_depth(&self, handle: &VariableUseHandle) -> Option<&Depth> {
        self.lexical_scopes.get_depth(handle)
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
                let scopes = resolver.scopes.len();
                if scopes != 0 &&
                   resolver.scopes[scopes - 1]
                       .get(&identifier.name)
                       .unwrap_or(&VariableDefinition::Undefined) ==
                   &VariableDefinition::Declared {
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
            assert!(lexical_scope_resolver.resolve(&statement).is_ok());
        }
        let lexical_scopes = lexical_scope_resolver.lexical_scopes;
        let mut handle_factory = VariableUseHandleFactory::new();
        let handle = handle_factory.next(); // Use of a in the function
        assert_eq!(Some(&3), lexical_scopes.get_depth(&handle));
    }

    #[test]
    fn captured_variable() {
        let (tokens, _) = scan(&"{var a = 0;fun f() {print a;}}");
        let statements = Parser::new().parse(&tokens).unwrap();
        let mut lexical_scope_resolver = ProgramLexicalScopesResolver::new();
        for statement in statements.iter() {
            assert!(lexical_scope_resolver.resolve(&statement).is_ok());
        }
        let lexical_scopes = lexical_scope_resolver.lexical_scopes;
        let mut handle_factory = VariableUseHandleFactory::new();
        let handle = handle_factory.next(); // Use of a in the function
        assert_eq!(Some(&2), lexical_scopes.get_depth(&handle));
    }

    #[test]
    fn lexical_capture() {
        let (tokens, _) = scan(&"var a = 0;{fun f() {print a;} var a = 1;}");
        let statements = Parser::new().parse(&tokens).unwrap();
        let mut lexical_scope_resolver = ProgramLexicalScopesResolver::new();
        for statement in statements.iter() {
            assert!(lexical_scope_resolver.resolve(&statement).is_ok());
        }
        let lexical_scopes = lexical_scope_resolver.lexical_scopes;
        let mut handle_factory = VariableUseHandleFactory::new();
        let handle = handle_factory.next(); // Use of a in the function
        assert_eq!(Some(&3), lexical_scopes.get_depth(&handle));
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
