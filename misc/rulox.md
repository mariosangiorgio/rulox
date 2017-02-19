#[fit] rulox
##[fit] Crafting Interpreters in Rust
### @mariosangiorgio
<!--
What I learnt implementing http://www.craftinginterpreters.com in Rust

This is a deckset http://www.decksetapp.com
-->
---

## Tokens
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
Algebraic data types for the win

---

## Pattern matching
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

#[fit]Look ma, no visitors!

---

#[fit] recursive type `ast::Expr` has infinite size