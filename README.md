# rulox
A lox interpreter written in Rust
[![Build Status](https://travis-ci.org/mariosangiorgio/rulox.svg?branch=master)](https://travis-ci.org/mariosangiorgio/rulox)

http://www.craftinginterpreters.com

## Interactive mode
Just execute `rulox` and type instructions in the REPL

## Scripted mode
Pass a `lox` source file as the first parameter to `rulox`. E.g. `rulox hello_world.lox`.

## Development
Rulox only uses the standard Rust toolchain so you can get up and running just invoking `cargo`.
If you want to produce an executable just use `cargo build release`.
If you're developing you might be interested in `cargo test`, which runs all the unit tests, `cargo fmt --`, to make sure the code follows the standard formatting, and `rustup run nightly cargo clippy` for some extra check by the linter.