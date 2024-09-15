#![feature(control_flow_enum)]
#![feature(if_let_guard)]
#![feature(let_chains)]
#![feature(try_blocks)]

#[cfg(not(target_arch = "wasm32"))]
pub use std::dbg;
#[cfg(target_arch = "wasm32")]
pub use wasm_rs_dbg::dbg;

mod analyses;
mod ast;
mod cli;
mod rulesets;
mod solvers;
mod wasm;

pub use analyses::*;
pub use ast::*;
pub use cli::*;
pub use rulesets::*;
pub use solvers::*;
pub use wasm::*;
