use fnv::FnvHashMap;
use std::collections::hash_map::Entry;
use treewalk::ast::*;

pub type Depth = usize;

pub struct LexicalScopes {
    depths: FnvHashMap<VariableUseHandle, Depth>,
}

impl LexicalScopes {
    fn new() -> LexicalScopes {
        LexicalScopes {
            depths: FnvHashMap::default(),
        }
    }

    pub fn get_depth(&self, handle: VariableUseHandle) -> Option<&Depth> {
        self.depths.get(&handle)
    }
}

#[derive(Debug)]
pub enum LexicalScopesResolutionError {
    ReadLocalInItsOwnInitializer,
    VariableAlreadyExistsInScope,
    ReturnFromTopLevelCode,
    ReturnFromInitializer,
    UseOfThisOutsideAClass,
    UseOfSuperOutsideAClass,
    UseOfSuperOutsideASubClass,
}

trait LexicallyScoped {
    fn resolve(&self, &mut LexicalScopesResolver) -> Result<(), LexicalScopesResolutionError>;
}

#[derive(PartialEq)]
enum VariableDefinition {
    Undefined,
    Declared,
    Defined,
}

#[derive(PartialEq, Clone, Copy)]
enum ClassType {
    None,
    Class,
    Subclass,
}

pub struct LexicalScopesResolver {
    // Note that this doesn't track globals at all
    scopes: Vec<FnvHashMap<Identifier, VariableDefinition>>,
    current_function: Option<FunctionKind>,
    current_class: ClassType,
    lexical_scopes: LexicalScopes,
}

impl LexicalScopesResolver {
    pub fn new() -> LexicalScopesResolver {
        LexicalScopesResolver {
            scopes: vec![],
            current_function: None,
            current_class: ClassType::None,
            lexical_scopes: LexicalScopes::new(),
        }
    }

    fn begin_scope(&mut self) -> () {
        self.scopes.push(FnvHashMap::default());
    }

    fn end_scope(&mut self) -> () {
        self.scopes.pop();
    }

    fn declare(&mut self, identifier: Identifier) -> Result<(), LexicalScopesResolutionError> {
        let scopes = self.scopes.len();
        if scopes == 0 {
            return Ok(());
        };
        match self.scopes[scopes - 1].entry(identifier) {
            Entry::Occupied(_) => Err(LexicalScopesResolutionError::VariableAlreadyExistsInScope),
            Entry::Vacant(v) => {
                v.insert(VariableDefinition::Declared);
                Ok(())
            }
        }
    }

    fn define(&mut self, identifier: Identifier) -> () {
        let scopes = self.scopes.len();
        if scopes == 0 {
            return;
        };
        self.scopes[scopes - 1].insert(identifier, VariableDefinition::Defined);
    }

    fn resolve_local(&mut self, handle: VariableUseHandle, identifier: Identifier) -> () {
        let max_depth = self.scopes.len();
        for depth in 0..max_depth {
            if self.scopes[max_depth - depth - 1].contains_key(&identifier) {
                self.lexical_scopes.depths.insert(handle, depth);
                return;
            }
        }
        // If we failed to find it in the locals, it must be a global.
        // It might not be there right now, but it might appear later on.
        // We will know it only at runtime.
        self.lexical_scopes.depths.insert(handle, max_depth);
    }

    #[allow(dead_code)] // Used in tests
    pub fn resolve(&mut self, statement: &Statement) -> Result<(), LexicalScopesResolutionError> {
        statement.resolve(self)
    }

    pub fn resolve_all(
        &mut self,
        statements: &[Statement],
    ) -> Result<(), Vec<LexicalScopesResolutionError>> {
        let mut resolution_errors = vec![];
        for statement in statements {
            match statement.resolve(self) {
                Ok(_) => (),
                Err(e) => resolution_errors.push(e),
            }
        }
        if resolution_errors.is_empty() {
            Ok(())
        } else {
            Err(resolution_errors)
        }
    }

    pub fn get_depth(&self, handle: VariableUseHandle) -> Option<&Depth> {
        self.lexical_scopes.get_depth(handle)
    }
}

impl LexicallyScoped for Statement {
    fn resolve(
        &self,
        resolver: &mut LexicalScopesResolver,
    ) -> Result<(), LexicalScopesResolutionError> {
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
                try!(resolver.declare(*identifier));
                resolver.define(*identifier);
                Ok(())
            }
            Statement::VariableDefinitionWithInitalizer(ref identifier, ref initializer) => {
                try!(resolver.declare(*identifier));
                try!(initializer.resolve(resolver));
                resolver.define(*identifier);
                Ok(())
            }
            Statement::FunctionDefinition(ref f) => f.resolve(resolver),
            Statement::Class(ref c) => {
                try!(resolver.declare(c.name));
                let enclosing_class = resolver.current_class;
                resolver.current_class = ClassType::Class;
                resolver.define(c.name);
                if let Some(ref superclass) = c.superclass {
                    resolver.current_class = ClassType::Subclass;
                    try!(superclass.resolve(resolver));
                    resolver.begin_scope();
                    resolver.define(Identifier::super_identifier());
                }
                resolver.begin_scope();
                resolver.define(Identifier::this());
                for method in &c.methods {
                    try!(method.resolve(resolver));
                }
                resolver.end_scope();
                if c.superclass.is_some() {
                    resolver.end_scope();
                }
                resolver.current_class = enclosing_class;
                Ok(())
            }
            Statement::Expression(ref e) => e.resolve(resolver),
            Statement::IfThen(ref s) => s
                .condition
                .resolve(resolver)
                .and_then(|_| s.then_branch.resolve(resolver)),
            Statement::IfThenElse(ref s) => s
                .condition
                .resolve(resolver)
                .and_then(|_| s.then_branch.resolve(resolver))
                .and_then(|_| s.else_branch.resolve(resolver)),
            Statement::While(ref s) => s
                .condition
                .resolve(resolver)
                .and_then(|_| s.body.resolve(resolver)),
            Statement::Print(ref e) => e.resolve(resolver),
            Statement::Return(ref r) => match resolver.current_function {
                Some(FunctionKind::Initializer) => {
                    Err(LexicalScopesResolutionError::ReturnFromInitializer)
                }
                Some(_) => match *r {
                    None => Ok(()),
                    Some(ref e) => e.resolve(resolver),
                },
                None => Err(LexicalScopesResolutionError::ReturnFromTopLevelCode),
            },
        }
    }
}

impl LexicallyScoped for Expr {
    fn resolve(
        &self,
        resolver: &mut LexicalScopesResolver,
    ) -> Result<(), LexicalScopesResolutionError> {
        match *self {
            Expr::This(ref handle, ref identifier) => {
                if let ClassType::None = resolver.current_class {
                    return Err(LexicalScopesResolutionError::UseOfThisOutsideAClass);
                }
                resolver.resolve_local(handle.clone(), *identifier);
                Ok(())
            }
            Expr::Super(ref handle, ref super_identifier, _member_identifier) => {
                match resolver.current_class {
                    ClassType::None => Err(LexicalScopesResolutionError::UseOfSuperOutsideAClass),
                    ClassType::Class => {
                        Err(LexicalScopesResolutionError::UseOfSuperOutsideASubClass)
                    }
                    _ => {
                        resolver.resolve_local(handle.clone(), *super_identifier);
                        Ok(())
                    }
                }
            }
            Expr::Identifier(ref handle, ref identifier) => {
                let scopes = resolver.scopes.len();
                if scopes != 0
                    && resolver.scopes[scopes - 1]
                        .get(&identifier)
                        .unwrap_or(&VariableDefinition::Undefined)
                        == &VariableDefinition::Declared
                {
                    Err(LexicalScopesResolutionError::ReadLocalInItsOwnInitializer)
                } else {
                    resolver.resolve_local(*handle, *identifier);
                    Ok(())
                }
            }
            Expr::Assignment(ref assigment) => assigment.resolve(resolver),
            Expr::Literal(_) => Ok(()),
            Expr::Unary(ref e) => e.right.resolve(resolver),
            Expr::Binary(ref e) => e
                .left
                .resolve(resolver)
                .and_then(|_| e.right.resolve(resolver)),
            Expr::Logic(ref e) => e
                .left
                .resolve(resolver)
                .and_then(|_| e.right.resolve(resolver)),
            Expr::Grouping(ref e) => e.expr.resolve(resolver),
            Expr::Call(ref e) => {
                try!(e.callee.resolve(resolver));
                for argument in &e.arguments {
                    try!(argument.resolve(resolver));
                }
                Ok(())
            }
            Expr::Get(ref g) => {
                try!(g.instance.resolve(resolver));
                Ok(())
            }
            Expr::Set(ref s) => {
                try!(s.value.resolve(resolver));
                try!(s.instance.resolve(resolver));
                Ok(())
            }
        }
    }
}

impl LexicallyScoped for Assignment {
    fn resolve(
        &self,
        resolver: &mut LexicalScopesResolver,
    ) -> Result<(), LexicalScopesResolutionError> {
        try!{self.rvalue.resolve(resolver)};
        let Target::Identifier(ref identifier) = self.lvalue;
        resolver.resolve_local(self.handle, *identifier);
        Ok(())
    }
}

impl LexicallyScoped for FunctionDefinition {
    fn resolve(
        &self,
        resolver: &mut LexicalScopesResolver,
    ) -> Result<(), LexicalScopesResolutionError> {
        try!(resolver.declare(self.name));
        let enclosing_function = resolver.current_function;
        resolver.current_function = Some(self.kind);
        resolver.define(self.name);
        resolver.begin_scope();
        for argument in &self.arguments {
            try!(resolver.declare(*argument));
            resolver.define(*argument);
        }
        try!(self.body.resolve(resolver));
        resolver.end_scope();
        resolver.current_function = enclosing_function;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use frontend::scanner::*;
    use treewalk::lexical_scope_resolver::*;
    use treewalk::parser::*;

    #[test]
    fn global_variable() {
        let (tokens, _) = scan(&"var a = 0;{fun f() {print a;}}");
        let statements = Parser::default().parse(&tokens).unwrap();
        let mut lexical_scope_resolver = LexicalScopesResolver::new();
        for statement in statements.iter() {
            assert!(lexical_scope_resolver.resolve(&statement).is_ok());
        }
        let lexical_scopes = lexical_scope_resolver.lexical_scopes;
        let mut handle_factory = VariableUseHandleFactory::new();
        let handle = handle_factory.next(); // Use of a in the function
        assert_eq!(Some(&3), lexical_scopes.get_depth(handle));
    }

    #[test]
    fn captured_variable() {
        let (tokens, _) = scan(&"{var a = 0;fun f() {print a;}}");
        let statements = Parser::default().parse(&tokens).unwrap();
        let mut lexical_scope_resolver = LexicalScopesResolver::new();
        for statement in statements.iter() {
            assert!(lexical_scope_resolver.resolve(&statement).is_ok());
        }
        let lexical_scopes = lexical_scope_resolver.lexical_scopes;
        let mut handle_factory = VariableUseHandleFactory::new();
        let handle = handle_factory.next(); // Use of a in the function
        assert_eq!(Some(&2), lexical_scopes.get_depth(handle));
    }

    #[test]
    fn lexical_capture() {
        let (tokens, _) = scan(&"var a = 0;{fun f() {print a;} var a = 1;}");
        let statements = Parser::default().parse(&tokens).unwrap();
        let mut lexical_scope_resolver = LexicalScopesResolver::new();
        for statement in statements.iter() {
            assert!(lexical_scope_resolver.resolve(&statement).is_ok());
        }
        let lexical_scopes = lexical_scope_resolver.lexical_scopes;
        let mut handle_factory = VariableUseHandleFactory::new();
        let handle = handle_factory.next(); // Use of a in the function
        assert_eq!(Some(&3), lexical_scopes.get_depth(handle));
    }

    #[test]
    fn error_on_shadowing() {
        let (tokens, _) = scan(&"var a = 0;{var a = a;}");
        let statements = Parser::default().parse(&tokens).unwrap();
        let mut lexical_scope_resolver = LexicalScopesResolver::new();
        assert!(lexical_scope_resolver.resolve(&statements[0]).is_ok());
        assert!(lexical_scope_resolver.resolve(&statements[1]).is_err());
    }

    #[test]
    fn error_on_local_redeclaration() {
        let (tokens, _) = scan(&"fun bad() {var a = 1;var a = 2;}");
        let statements = Parser::default().parse(&tokens).unwrap();
        let mut lexical_scope_resolver = LexicalScopesResolver::new();
        assert!(lexical_scope_resolver.resolve(&statements[0]).is_err());
    }

    #[test]
    fn global_redeclaration_is_allowed() {
        let (tokens, _) = scan(&"var a = 1;var a = 2;");
        let statements = Parser::default().parse(&tokens).unwrap();
        let mut lexical_scope_resolver = LexicalScopesResolver::new();
        assert!(lexical_scope_resolver.resolve(&statements[0]).is_ok());
        assert!(lexical_scope_resolver.resolve(&statements[1]).is_ok());
    }

    #[test]
    fn error_on_return_outside_a_function() {
        let (tokens, _) = scan(&"return;");
        let statements = Parser::default().parse(&tokens).unwrap();
        let mut lexical_scope_resolver = LexicalScopesResolver::new();
        assert!(lexical_scope_resolver.resolve(&statements[0]).is_err());
    }
}
