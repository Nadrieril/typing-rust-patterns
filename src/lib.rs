#![feature(control_flow_enum)]
#![feature(if_let_guard)]
#![feature(let_chains)]
#![feature(try_blocks)]

#[cfg(not(target_arch = "wasm32"))]
pub use std::dbg;
#[cfg(target_arch = "wasm32")]
pub use wasm_rs_dbg::dbg;

mod analysis;
mod ast;
mod cli;
mod compare;
mod compute_rules;
mod explore;
mod option_bundles;
mod parser;
mod printer;
mod solver;
mod typing_rules;
#[cfg(target_arch = "wasm32")]
mod wasm;

pub use analysis::*;
pub use ast::*;
pub use cli::*;
pub use compare::*;
pub use compute_rules::*;
pub use explore::*;
pub use solver::*;
pub use typing_rules::*;
#[cfg(target_arch = "wasm32")]
pub use wasm::*;
