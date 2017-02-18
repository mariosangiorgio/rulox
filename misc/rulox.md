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