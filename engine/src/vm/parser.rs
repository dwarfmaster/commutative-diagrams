use crate::vm::ast;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alpha1, alphanumeric1, char, digit1, newline, space0, space1};
use nom::combinator::{eof, fail, map, map_res, recognize, success, value};
use nom::error::ParseError;
use nom::multi::{many0_count, many1_count, many_till};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::{IResult, Offset};

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
    let sep = pair(newline, spaces);
    let case_eof = value((), pair(many0_count(sep), eof));
    let sep = pair(newline, spaces);
    let case_newline = value((), many1_count(sep));
    value((), pair(spaces, alt((case_eof, case_newline))))(input)
}

fn spaces(input: &str) -> IResult<&str, ()> {
    value((), space0)(input)
}

fn sep(input: &str) -> IResult<&str, ()> {
    value((), delimited(spaces, char(','), spaces))(input)
}

pub struct Parser<'a> {
    complete: &'a str,
    offset: usize,
}

impl<'a> Parser<'a> {
    pub fn new(offset: usize, input: &'a str) -> Self {
        Self {
            complete: input,
            offset,
        }
    }

    pub fn parse(&self) -> IResult<&str, ast::AST> {
        self.script(self.complete)
    }

    fn with_annot<O, E: ParseError<&'a str>, F>(
        &self,
        mut parser: F,
    ) -> impl FnMut(&'a str) -> IResult<&'a str, ast::Annot<O>, E> + '_
    where
        F: nom::Parser<&'a str, O, E> + 'a,
    {
        move |input: &'a str| {
            let i = input.clone();
            match parser.parse(i) {
                Ok((i, r)) => {
                    let start = self.offset + self.complete.offset(&input);
                    let end = self.offset + self.complete.offset(&i);
                    Ok((
                        i,
                        ast::Annot {
                            value: r,
                            range: std::ops::Range { start, end },
                        },
                    ))
                }
                Err(e) => Err(e),
            }
        }
    }

    fn id(&'a self, input: &'a str) -> IResult<&'a str, ast::Id> {
        alt((
            map(ident, |id| ast::Id::Name(id.to_string())),
            map(delimited(char('['), integer, char(']')), |id| {
                ast::Id::Id(id)
            }),
        ))(input)
    }

    fn term_descr(&'a self, input: &'a str) -> IResult<&'a str, ast::TermDescr> {
        let parse_id = self.with_annot(|i| self.id(i));
        alt((
            map(parse_id, |id| ast::TermDescr::Ref(id)),
            value(ast::TermDescr::Hole, char('_')),
        ))(input)
    }

    fn script(&'a self, input: &'a str) -> IResult<&'a str, Vec<ast::Annot<ast::Action>>> {
        preceded(
            alt((endl, spaces)),
            map(
                many_till(terminated(self.with_annot(|i| self.action(i)), endl), eof),
                |(acts, _)| acts,
            ),
        )(input)
    }

    fn action(&'a self, input: &'a str) -> IResult<&'a str, ast::Action> {
        let (input, _) = spaces(input)?;
        let (input, cmd) = ident(input)?;
        let (input, act) = match cmd {
            "insert" => self.act_insert(input),
            "insert_at" => self.act_insert_at(input),
            "split" => self.act_split(input),
            "solve" => self.act_solve(input),
            "pull" => self.act_pull(input),
            "push" => self.act_push(input),
            "shrink" => self.act_shrink(input),
            "refine" => self.act_refine(input),
            "hide" => self.act_hide(true, input),
            "reveal" => self.act_hide(false, input),
            "succeed" => success(ast::Action::Succeed)(input),
            "fail" => success(ast::Action::Fail)(input),
            _ => fail(input),
        }?;
        success(act)(input)
    }

    fn loc_term_descr(&'a self, input: &'a str) -> IResult<&'a str, ast::Annot<ast::TermDescr>> {
        self.with_annot(|i| self.term_descr(i))(input)
    }

    fn act_insert(&'a self, input: &'a str) -> IResult<&'a str, ast::Action> {
        let (input, _) = space1(input)?;
        let (input, sub) = ident(input)?;
        let (input, _) = sep(input)?;
        let (input, desc) = self.loc_term_descr(input)?;
        match sub {
            "node" => success(ast::Action::InsertNode(desc))(input),
            "morphism" => success(ast::Action::InsertMorphism(desc))(input),
            _ => fail(input),
        }
    }

    fn act_insert_at(&'a self, input: &'a str) -> IResult<&'a str, ast::Action> {
        let (input, _) = space1(input)?;
        let (input, at) = self.with_annot(integer)(input)?;
        let (input, _) = sep(input)?;
        let (input, mph) = self.loc_term_descr(input)?;
        success(ast::Action::InsertMorphismAt(at, mph))(input)
    }

    fn act_split(&'a self, input: &'a str) -> IResult<&'a str, ast::Action> {
        let (input, _) = space1(input)?;
        map(|i| self.loc_term_descr(i), |desc| ast::Action::Split(desc))(input)
    }

    fn act_solve(&'a self, input: &'a str) -> IResult<&'a str, ast::Action> {
        let (input, _) = space1(input)?;
        alt((
            map(
                tuple((self.with_annot(integer), sep, |i| self.loc_term_descr(i))),
                |(size, _, d)| ast::Action::Solve(Some(size), d),
            ),
            map(
                |i| self.loc_term_descr(i),
                |desc| ast::Action::Solve(None, desc),
            ),
        ))(input)
    }

    fn act_refine(&'a self, input: &'a str) -> IResult<&'a str, ast::Action> {
        let (input, _) = space1(input)?;
        let (input, d1) = self.loc_term_descr(input)?;
        let (input, _) = sep(input)?;
        let (input, d2) = self.loc_term_descr(input)?;
        success(ast::Action::Refine(d1, d2))(input)
    }

    fn act_hide(&'a self, hide: bool, input: &'a str) -> IResult<&'a str, ast::Action> {
        let (input, _) = space1(input)?;
        let (input, cat) = ident(input)?;
        let (input, _) = space1(input)?;
        let (input, d) = self.loc_term_descr(input)?;
        match (hide, cat) {
            (true, "node") => success(ast::Action::HideNode(d))(input),
            (false, "node") => success(ast::Action::RevealNode(d))(input),
            (true, "morphism") => success(ast::Action::HideMorphism(d))(input),
            (false, "morphism") => success(ast::Action::RevealMorphism(d))(input),
            (true, "face") => success(ast::Action::HideFace(d))(input),
            (false, "face") => success(ast::Action::RevealFace(d))(input),
            _ => fail(input),
        }
    }

    fn act_pull(&'a self, input: &'a str) -> IResult<&'a str, ast::Action> {
        let (input, _) = space1(input)?;
        let (input, fce) = self.loc_term_descr(input)?;
        let (input, _) = sep(input)?;
        let (input, span) = alt((
            value(None, char('*')),
            map(integer, |i| Some(i))))(input)?;
        success(ast::Action::PullFace(fce, span))(input)
    }

    fn act_push(&'a self, input: &'a str) -> IResult<&'a str, ast::Action> {
        let (input, _) = space1(input)?;
        let (input, fce) = self.loc_term_descr(input)?;
        let (input, _) = sep(input)?;
        let (input, span) = alt((
            value(None, char('*')),
            map(integer, |i| Some(i))))(input)?;
        success(ast::Action::PushFace(fce, span))(input)
    }

    fn act_shrink(&'a self, input: &'a str) -> IResult<&'a str, ast::Action> {
        let (input, _) = space1(input)?;
        let (input, fce) = self.loc_term_descr(input)?;
        success(ast::Action::ShrinkFace(fce))(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::vm::ast;
    use crate::vm::parser::Parser;
    use nom::combinator::not;

    #[test]
    fn termdescr() {
        fn test(input: &str, expected: ast::TermDescr) {
            let p = Parser::new(0, input);
            assert_eq!(p.term_descr(input), Ok(("", expected)))
        }
        fn test_fail<'a>(input: &'a str) {
            let p = Parser::<'a>::new(0, input);
            assert_eq!(not(|i| { p.term_descr(i) })(input), Ok((input, ())));
        }

        test(
            "[55]",
            ast::TermDescr::Ref(ast::Annot {
                value: ast::Id::Id(55),
                range: 0..4,
            }),
        );
        test(
            "[197]",
            ast::TermDescr::Ref(ast::Annot {
                value: ast::Id::Id(197),
                range: 0..5,
            }),
        );
        test(
            "hello",
            ast::TermDescr::Ref(ast::Annot {
                value: ast::Id::Name("hello".to_string()),
                range: 0..5,
            }),
        );
        test("_", ast::TermDescr::Hole);
        test_fail("[-18]");
        test_fail("1hoy");
    }

    #[test]
    fn action() {
        use ast::Action::*;
        use ast::Annot;
        use ast::Id;
        use ast::TermDescr::*;

        // Successes
        fn test(input: &str, expected: ast::Action) {
            let p = Parser::new(0, input);
            assert_eq!(p.action(input), Ok(("", expected)))
        }

        test(
            "insert node, [10]",
            InsertNode(Annot {
                value: Ref(Annot {
                    value: Id::Id(10),
                    range: 13..17,
                }),
                range: 13..17,
            }),
        );
        test(
            "insert morphism, _",
            InsertMorphism(Annot {
                value: Hole,
                range: 17..18,
            }),
        );
        test(
            "insert_at 5, toto",
            InsertMorphismAt(
                Annot {
                    value: 5,
                    range: 10..11,
                },
                Annot {
                    value: Ref(Annot {
                        value: Id::Name("toto".to_string()),
                        range: 13..17,
                    }),
                    range: 13..17,
                },
            ),
        );
        test(
            "split [2]",
            Split(Annot {
                value: Ref(Annot {
                    value: Id::Id(2),
                    range: 6..9,
                }),
                range: 6..9,
            }),
        );
        test(
            "solve fce1",
            Solve(
                None,
                Annot {
                    value: Ref(Annot {
                        value: Id::Name("fce1".to_string()),
                        range: 6..10,
                    }),
                    range: 6..10,
                },
            ),
        );
        test(
            "solve 10, fce1",
            Solve(
                Some(Annot {
                    value: 10,
                    range: 6..8,
                }),
                Annot {
                    value: Ref(Annot {
                        value: Id::Name("fce1".to_string()),
                        range: 10..14,
                    }),
                    range: 10..14,
                },
            ),
        );
        test(
            "refine [1], [2]",
            Refine(
                Annot {
                    value: Ref(Annot {
                        value: Id::Id(1),
                        range: 7..10,
                    }),
                    range: 7..10,
                },
                Annot {
                    value: Ref(Annot {
                        value: Id::Id(2),
                        range: 12..15,
                    }),
                    range: 12..15,
                },
            ),
        );
        test(
            "hide node [1]",
            HideNode(Annot {
                value: Ref(Annot {
                    value: Id::Id(1),
                    range: 10..13,
                }),
                range: 10..13,
            }),
        );
        test(
            "reveal morphism [3]",
            RevealMorphism(Annot {
                value: Ref(Annot {
                    value: Id::Id(3),
                    range: 16..19,
                }),
                range: 16..19,
            }),
        );
        test("succeed", Succeed);
        test("fail", Fail);

        test(
            "  insert_at\t5        ,   [0]",
            InsertMorphismAt(
                Annot {
                    value: 5,
                    range: 12..13,
                },
                Annot {
                    value: Ref(Annot {
                        value: Id::Id(0),
                        range: 25..28,
                    }),
                    range: 25..28,
                },
            ),
        );
    }
}
