theme: Fira, 5

#[fit] rulox
##[fit] Crafting Interpreters in Rust
### @mariosangiorgio
<!--
What I learnt implementing http://www.craftinginterpreters.com in Rust

This is a deckset http://www.decksetapp.com
-->
---

#[fit] Tokens

---

## Represent them with an algebraic data type
```rust
pub enum Token {
  LeftParen,
  RightParen,
  /* ... */
  Identifier(String),
  StringLiteral(String),
  NumberLiteral(f64),
}
```

---

## Pattern matching helps with scanning
```rust
let token = match self.advance() {
    '(' => Token::LeftParen,
    ')' => Token::RightParen,
    /*...*/
    c if is_digit(c) => self.number(),
    c if is_alpha(c) => self.identifier(), 
    /*...*/
}                       
```
---

#[fit]Abstract Syntaxt Tree

---

# An `enum` of `struct`

```rust
pub struct BinaryExpr {
    pub left: Expr,
    pub operator: Operator,
    pub right: Expr,
}

pub enum Expr {
    Literal(Literal),
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Grouping(Grouping),
}
```

---
# Not quite

##[fit] recursive type `ast::Expr` has infinite size

---

# The fundamental theorem of software engineering

>All problems in computer science can be solved by another level of indirection
-- David Wheeler

---

# Boxes have a fixed size (they're pointers)

```rust
pub enum Expr {
    Literal(Literal),
    Unary(Box<UnaryExpr>),
    Binary(Box<BinaryExpr>),
    Grouping(Box<Grouping>),
}
```

---

# `trait` for zero-cost user friendliness
```rust
trait PrettyPrint {
    fn pretty_print_into(&self, pretty_printed: &mut String) -> ();
    fn pretty_print(&self) -> String {
        let mut pretty_printed = String::new();
        &self.pretty_print_into(&mut pretty_printed);
        pretty_printed
    }
}
```

---

# Visitors? No, just pattern matching
```rust
impl PrettyPrint for Expr {
    fn pretty_print_into(&self, pretty_printed: &mut String) -> () {
        match self {
            &Expr::Literal(ref l) => l.pretty_print_into(pretty_printed),
            &Expr::Unary(ref u) => (*u).pretty_print_into(pretty_printed),
            &Expr::Binary(ref b) => (*b).pretty_print_into(pretty_printed),
            &Expr::Grouping(ref g) => (*g).pretty_print_into(pretty_printed),
        }
    }
}
```