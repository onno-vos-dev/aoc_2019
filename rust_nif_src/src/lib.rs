#[macro_use] extern crate rustler;
#[macro_use] extern crate lazy_static;

use rustler::{Env, Term, NifResult, Encoder};

mod atoms {
    rustler_atoms! {
        atom ok;
        //atom error;
        //atom __true__ = "true";
        //atom __false__ = "false";
    }
}

rustler_export_nifs! {
    "aoc",
    [("fuel", 1, fuel)],
    None
}

fn fuel<'a>(env: Env<'a>, args: &[Term<'a>]) -> NifResult<Term<'a>> {
    let x: i64 = try!(args[0].decode());
    let y: i64 = x / 3 - 2;
    Ok((atoms::ok(), y).encode(env))
}
