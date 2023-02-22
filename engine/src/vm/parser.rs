use crate::vm::ast;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric1, char, digit1, newline, space0, space1};
use nom::combinator::{eof, fail, map, map_res, recognize, success, value};
use nom::multi::{many0_count, many_till};
use nom::sequence::{delimited, pair, preceded};
use nom::IResult;

fn integer(input: &str) -> IResult<&str, usize> {
    map_res(digit1, |str: &str| str.parse::<usize>())(input)
}

// Identifiers can contain _ and numbers, but not start with them
// Inspired by:
//   https://docs.rs/nom/latest/nom/recipes/index.html#rust-style-identifiers
fn ident(input: &str) -> IResult<&str, &str> {
    recognize(pair(alpha1, many0_count(alt((alphanumeric1, tag("_"))))))(input)
}

fn endl(input: &str) -> IResult<&str, ()> {
    preceded(spaces, alt((value((), newline), value((), eof))))(input)
}

fn spaces(input: &str) -> IResult<&str, ()> {
    value((), space0)(input)
}

fn sep(input: &str) -> IResult<&str, ()> {
    value((), delimited(spaces, char(','), spaces))(input)
}

fn id(input: &str) -> IResult<&str, ast::Id> {
    alt((
        map(ident, |id| ast::Id::Name(id.to_string())),
        map(delimited(char('['), integer, char(']')), |id| {
            ast::Id::Id(id)
        }),
    ))(input)
}

fn term_descr(input: &str) -> IResult<&str, ast::TermDescr> {
    alt((
        map(id, |id| ast::TermDescr::Ref(id)),
        value(ast::TermDescr::Hole, char('_')),
    ))(input)
}

pub fn script(input: &str) -> IResult<&str, Vec<ast::Action>> {
    map(many_till(action, eof), |(acts, _)| acts)(input)
}

fn action(input: &str) -> IResult<&str, ast::Action> {
    let (input, _) = spaces(input)?;
    let (input, cmd) = ident(input)?;
    let (input, act) = match cmd {
        "insert" => act_insert(input),
        "insert_at" => act_insert_at(input),
        "split" => act_split(input),
        "solve" => act_solve(input),
        "refine" => act_refine(input),
        "hide" => act_hide(true, input),
        "reveal" => act_hide(false, input),
        "succeed" => success(ast::Action::Succeed)(input),
        "fail" => success(ast::Action::Fail)(input),
        _ => fail(input),
    }?;
    let (input, _) = endl(input)?;
    success(act)(input)
}

fn act_insert(input: &str) -> IResult<&str, ast::Action> {
    let (input, _) = space1(input)?;
    let (input, sub) = ident(input)?;
    let (input, _) = sep(input)?;
    let (input, desc) = term_descr(input)?;
    match sub {
        "node" => success(ast::Action::InsertNode(desc))(input),
        "morphism" => success(ast::Action::InsertMorphism(desc))(input),
        _ => fail(input),
    }
}

fn act_insert_at(input: &str) -> IResult<&str, ast::Action> {
    let (input, _) = space1(input)?;
    let (input, at) = integer(input)?;
    let (input, _) = sep(input)?;
    let (input, mph) = term_descr(input)?;
    success(ast::Action::InsertMorphismAt(at, mph))(input)
}

fn act_split(input: &str) -> IResult<&str, ast::Action> {
    let (input, _) = space1(input)?;
    map(term_descr, |desc| ast::Action::Split(desc))(input)
}

fn act_solve(input: &str) -> IResult<&str, ast::Action> {
    let (input, _) = space1(input)?;
    map(term_descr, |desc| ast::Action::Solve(desc))(input)
}

fn act_refine(input: &str) -> IResult<&str, ast::Action> {
    let (input, _) = space1(input)?;
    let (input, d1) = term_descr(input)?;
    let (input, _) = sep(input)?;
    let (input, d2) = term_descr(input)?;
    success(ast::Action::Refine(d1, d2))(input)
}

fn act_hide(hide: bool, input: &str) -> IResult<&str, ast::Action> {
    let (input, _) = space1(input)?;
    let (input, cat) = ident(input)?;
    let (input, _) = space1(input)?;
    let (input, d) = term_descr(input)?;
    match (hide, cat) {
        (true, "node") => success(ast::Action::HideNode(d))(input),
        (false, "node") => success(ast::Action::RevealNode(d))(input),
        (true, "morphism") => success(ast::Action::HideMorphism(d))(input),
        (false, "morphism") => success(ast::Action::RevealMorphism(d))(input),
        (true, "face") => success(ast::Action::HideFace(d))(input),
        (false, "face") => success(ast::Action::RevealFace(d))(input),
        _ => fail(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::vm::ast;
    use crate::vm::parser;
    use nom::combinator::not;

    #[test]
    fn termdescr() {
        fn test(input: &str, expected: ast::TermDescr) {
            assert_eq!(parser::term_descr(input), Ok(("", expected)))
        }
        fn test_fail(input: &str) {
            assert_eq!(not(parser::term_descr)(input), Ok((input, ())))
        }

        test("[55]", ast::TermDescr::Ref(ast::Id::Id(55)));
        test("[197]", ast::TermDescr::Ref(ast::Id::Id(197)));
        test(
            "hello",
            ast::TermDescr::Ref(ast::Id::Name("hello".to_string())),
        );
        test("_", ast::TermDescr::Hole);
        test_fail("[-18]");
        test_fail("1hoy");
    }

    #[test]
    fn action() {
        use ast::Action::*;
        use ast::Id;
        use ast::TermDescr::*;

        // Successes
        fn test(input: &str, expected: ast::Action) {
            assert_eq!(parser::action(input), Ok(("", expected)))
        }

        test("insert node, [10]", InsertNode(Ref(Id::Id(10))));
        test("insert morphism, _", InsertMorphism(Hole));
        test(
            "insert_at 5, toto",
            InsertMorphismAt(5, Ref(Id::Name("toto".to_string()))),
        );
        test("split [2]", Split(Ref(Id::Id(2))));
        test("solve fce1", Solve(Ref(Id::Name("fce1".to_string()))));
        test("refine [1], [2]", Refine(Ref(Id::Id(1)), Ref(Id::Id(2))));
        test("hide node [1]", HideNode(Ref(Id::Id(1))));
        test("reveal morphism [3]", RevealMorphism(Ref(Id::Id(3))));
        test("succeed", Succeed);
        test("fail", Fail);

        test(
            "  insert_at\t5        ,   [0]    ",
            InsertMorphismAt(5, Ref(Id::Id(0))),
        );
    }
}
