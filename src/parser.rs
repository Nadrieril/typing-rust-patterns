//! Parser for our types, using `nom`.
//!
//! Note: for consistent whitespace handling, each parser consumes trailing whitespace after it.
use nom::{
    bytes::complete::{tag, take_while},
    character::complete::{multispace0, multispace1},
    error::ParseError,
    multi::separated_list0,
    Parser,
};
use nom_supreme::{error::ErrorTree, ParserExt};

use crate::*;

/// Extra methods on parsers.
trait ParserExtExt<I, O, E>: Parser<I, O, E> + Sized
where
    I: Clone,
    E: ParseError<I>,
{
    fn followed_by<F, O2>(self, suffix: F) -> impl Parser<I, O, E>
    where
        F: Parser<I, O2, E>,
    {
        self.terminated(suffix)
    }
}
impl<I, O, E, P> ParserExtExt<I, O, E> for P
where
    I: Clone,
    E: ParseError<I>,
    P: Parser<I, O, E>,
{
}

type ParseCtx<'a> = &'a Arenas<'a>;

pub fn complete_parse_typing_request<'a, 'i>(
    ctx: &'a Arenas<'a>,
    i: &'i str,
) -> Result<TypingRequest<'a>, ErrorTree<&'i str>> {
    nom_supreme::final_parser::final_parser(parse_typing_request::<ErrorTree<&str>>(ctx))(i)
}

fn parse_typing_request<'a, 'i, E>(ctx: ParseCtx<'a>) -> impl Parser<&'i str, TypingRequest<'a>, E>
where
    E: ParseError<&'i str>,
{
    parse_pattern_alloc(ctx)
        .followed_by(tag(":"))
        .followed_by(multispace0)
        .and(parse_type_alloc(ctx))
        .map(|(pat, ty)| TypingRequest { pat, ty })
}

fn parse_pattern_alloc<'a, 'i, E>(ctx: ParseCtx<'a>) -> impl Parser<&'i str, &'a Pattern<'a>, E>
where
    E: ParseError<&'i str>,
{
    move |i| {
        parse_pattern(ctx)
            .map(|pat| ctx.pat_arena.alloc(pat) as &_)
            .parse(i)
    }
}

fn parse_pattern<'a, 'i, E>(ctx: ParseCtx<'a>) -> impl Parser<&'i str, Pattern<'a>, E>
where
    E: ParseError<&'i str>,
{
    move |i| {
        parse_tuple_pattern(ctx)
            .or(parse_ref_pattern(ctx))
            .or(parse_binding(ctx))
            .parse(i)
    }
}

fn parse_tuple_pattern<'a, 'i, E>(ctx: ParseCtx<'a>) -> impl Parser<&'i str, Pattern<'a>, E>
where
    E: ParseError<&'i str>,
{
    tag("[")
        .followed_by(multispace0)
        .precedes(
            separated_list0(tag(",").followed_by(multispace1), parse_pattern(ctx))
                .followed_by(tag("]"))
                .cut(),
        )
        .followed_by(multispace0)
        .map(|pats| {
            let pats = ctx.pat_arena.alloc_extend(pats);
            Pattern::Tuple(pats)
        })
}

fn parse_ref_pattern<'a, 'i, E>(ctx: ParseCtx<'a>) -> impl Parser<&'i str, Pattern<'a>, E>
where
    E: ParseError<&'i str>,
{
    tag("&")
        .followed_by(multispace0)
        .precedes(parse_mutability())
        .and(parse_pattern_alloc(ctx))
        .map(|(mutable, pat)| Pattern::Ref(mutable, pat))
}

fn parse_binding<'a, 'i, E>(ctx: ParseCtx<'a>) -> impl Parser<&'i str, Pattern<'a>, E>
where
    E: ParseError<&'i str>,
{
    let ident = take_while(|c: char| c.is_alphabetic() || c == '_');
    parse_mutability()
        .and(parse_binding_mode())
        .and(ident)
        .followed_by(multispace0)
        .map(|((mtbl, mode), name)| {
            let name = ctx.str_arena.alloc_str(name);
            Pattern::Binding(mtbl, mode, name)
        })
}

fn parse_binding_mode<'a, 'i, E>() -> impl Parser<&'i str, BindingMode, E>
where
    E: ParseError<&'i str>,
{
    tag("ref ")
        .followed_by(multispace0)
        .precedes(parse_mutability())
        .opt()
        .map(|by_ref| {
            if let Some(mtbl) = by_ref {
                BindingMode::ByRef(mtbl)
            } else {
                BindingMode::ByMove
            }
        })
}

fn parse_type_alloc<'a, 'i, E>(ctx: ParseCtx<'a>) -> impl Parser<&'i str, &'a Type<'a>, E>
where
    E: ParseError<&'i str>,
{
    move |i| {
        parse_type(ctx)
            .map(|pat| ctx.type_arena.alloc(pat) as &_)
            .parse(i)
    }
}

fn parse_type<'a, 'i, E>(ctx: ParseCtx<'a>) -> impl Parser<&'i str, Type<'a>, E>
where
    E: ParseError<&'i str>,
{
    move |i| {
        parse_tuple_type(ctx)
            .or(parse_ref_type(ctx))
            .or(parse_type_var(ctx))
            .parse(i)
    }
}

fn parse_tuple_type<'a, 'i, E>(ctx: ParseCtx<'a>) -> impl Parser<&'i str, Type<'a>, E>
where
    E: ParseError<&'i str>,
{
    tag("[")
        .followed_by(multispace0)
        .precedes(
            separated_list0(tag(",").followed_by(multispace1), parse_type(ctx))
                .followed_by(tag("]"))
                .cut(),
        )
        .followed_by(multispace0)
        .map(|pats| {
            let pats = ctx.type_arena.alloc_extend(pats);
            Type::Tuple(pats)
        })
}

fn parse_ref_type<'a, 'i, E>(ctx: ParseCtx<'a>) -> impl Parser<&'i str, Type<'a>, E>
where
    E: ParseError<&'i str>,
{
    tag("&")
        .followed_by(multispace0)
        .precedes(parse_mutability())
        .and(parse_type_alloc(ctx))
        .map(|(mutable, pat)| Type::Ref(mutable, pat))
}

fn parse_type_var<'a, 'i, E>(ctx: ParseCtx<'a>) -> impl Parser<&'i str, Type<'a>, E>
where
    E: ParseError<&'i str>,
{
    let ident = take_while(|c: char| c.is_alphabetic() || c == '_');
    ident.followed_by(multispace0).map(|name| {
        let name = ctx.str_arena.alloc_str(name);
        Type::Var(name)
    })
}

fn parse_mutability<'a, 'i, E>() -> impl Parser<&'i str, Mutable, E>
where
    E: ParseError<&'i str>,
{
    tag("mut ").followed_by(multispace0).opt().map(|x| {
        if x.is_some() {
            Mutable::Yes
        } else {
            Mutable::No
        }
    })
}

#[test]
fn test_roundtrip() {
    let arenas = Arenas::default();
    let idempotent_test_strings = [
        "x: T",
        "ref x: T",
        "ref mut x: T",
        "mut x: T",
        "mut ref x: T",
        "mut ref mut x: T",
        "&x: T",
        "&mut x: T",
        "&[x]: T",
        "&[x, y]: T",
        "x: &T",
        "x: &mut T",
        "x: [T, U, V]",
        "&[ref x, &mut mut ref mut y]: &[&mut [T], &U, V]",
    ];
    let other_test_strings = [(
        "&  [ ref  x  ,  mut  y ]  :  & [ & mut  T , [   U ] ]",
        "&[ref x, mut y]: &[&mut T, [U]]",
    )];

    let test_strings = idempotent_test_strings
        .into_iter()
        .map(|s| (s, s))
        .chain(other_test_strings);
    for (input, expected) in test_strings {
        let pat = complete_parse_typing_request(&arenas, input)
            .map_err(|e| e.to_string())
            .unwrap();
        assert_eq!(pat.to_string(), expected);
    }
}
