use crate::vm::ast;
use nom::branch::alt;
use nom::character::complete::{
    alpha1, alphanumeric1, char, digit1, newline, one_of, space0, space1,
};
use nom::combinator::{eof, fail, map, map_res, recognize, success, value};
use nom::error::ParseError;
use nom::multi::{many0, many0_count, many1_count, many_till};
use nom::sequence::{delimited, pair, preceded, terminated, tuple};
use nom::{IResult, Offset};

fn integer(input: &str) -> IResult<&str, usize> {
    map_res(digit1, |str: &str| str.parse::<usize>())(input)
}

// Identifiers can contain _ and numbers, but not start with them
// Inspired by:
//   https://docs.rs/nom/latest/nom/recipes/index.html#rust-style-identifiers
fn ident(input: &str) -> IResult<&str, &str> {
    let extra = recognize(one_of("_.!/;"));
    recognize(pair(alpha1, many0_count(alt((alphanumeric1, extra)))))(input)
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

    fn name(&'a self, input: &'a str) -> IResult<&'a str, ast::Annot<String>> {
        self.with_annot(map(ident, |id| id.to_string()))(input)
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
            "apply" => self.act_lemma(input),
            "hide" => self.act_hide(true, input),
            "reveal" => self.act_hide(false, input),
            "succeed" => success(ast::Action::Succeed)(input),
            "fail" => success(ast::Action::Fail)(input),
            _ => fail(input),
        }?;
        success(act)(input)
    }

    fn act_insert(&'a self, input: &'a str) -> IResult<&'a str, ast::Action> {
        let (input, _) = space1(input)?;
        let (input, sub) = ident(input)?;
        let (input, _) = sep(input)?;
        let (input, desc) = self.name(input)?;
        match sub {
            "node" => success(ast::Action::InsertNode(desc))(input),
            "morphism" => success(ast::Action::InsertMorphism(desc))(input),
            _ => fail(input),
        }
    }

    fn act_insert_at(&'a self, input: &'a str) -> IResult<&'a str, ast::Action> {
        let (input, _) = space1(input)?;
        let (input, at) = self.name(input)?;
        let (input, _) = sep(input)?;
        let (input, mph) = self.name(input)?;
        success(ast::Action::InsertMorphismAt(at, mph))(input)
    }

    fn act_split(&'a self, input: &'a str) -> IResult<&'a str, ast::Action> {
        let (input, _) = space1(input)?;
        map(|i| self.name(i), |desc| ast::Action::Split(desc))(input)
    }

    fn act_solve(&'a self, input: &'a str) -> IResult<&'a str, ast::Action> {
        let (input, _) = space1(input)?;
        alt((
            map(
                tuple((self.with_annot(integer), sep, |i| self.name(i))),
                |(size, _, d)| ast::Action::Solve(Some(size), d),
            ),
            map(|i| self.name(i), |desc| ast::Action::Solve(None, desc)),
        ))(input)
    }

    fn act_hide(&'a self, hide: bool, input: &'a str) -> IResult<&'a str, ast::Action> {
        let (input, _) = space1(input)?;
        let (input, cat) = ident(input)?;
        let (input, _) = space1(input)?;
        let (input, d) = self.name(input)?;
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
        let (input, fce) = self.name(input)?;
        let (input, _) = sep(input)?;
        let (input, span) = alt((value(None, char('*')), map(integer, |i| Some(i))))(input)?;
        success(ast::Action::PullFace(fce, span))(input)
    }

    fn act_push(&'a self, input: &'a str) -> IResult<&'a str, ast::Action> {
        let (input, _) = space1(input)?;
        let (input, fce) = self.name(input)?;
        let (input, _) = sep(input)?;
        let (input, span) = alt((value(None, char('*')), map(integer, |i| Some(i))))(input)?;
        success(ast::Action::PushFace(fce, span))(input)
    }

    fn act_shrink(&'a self, input: &'a str) -> IResult<&'a str, ast::Action> {
        let (input, _) = space1(input)?;
        let (input, fce) = self.name(input)?;
        success(ast::Action::ShrinkFace(fce))(input)
    }

    fn act_lemma(&'a self, input: &'a str) -> IResult<&'a str, ast::Action> {
        let (input, _) = space1(input)?;
        let (input, lemma) = self.with_annot(ident)(input)?;
        let parse_match =
            |input: &'a str| -> IResult<&'a str, (ast::Annot<String>, ast::Annot<String>)> {
                let (input, _) = space1(input)?;
                let (input, id1) = self.name(input)?;
                let (input, _) = char(':')(input)?;
                let (input, id2) = self.name(input)?;
                success((id1, id2))(input)
            };
        let (input, matching) = many0(parse_match)(input)?;
        success(ast::Action::Lemma(lemma.map(|s| s.to_string()), matching))(input)
    }
}

#[cfg(test)]
mod tests {
    use crate::vm::ast;
    use crate::vm::parser::Parser;

    #[test]
    fn action() {
        use ast::Action::*;
        use ast::Annot;

        // Successes
        fn test(input: &str, expected: ast::Action) {
            let p = Parser::new(0, input);
            assert_eq!(p.action(input), Ok(("", expected)))
        }

        test(
            "insert node, toto",
            InsertNode(Annot {
                value: "toto".to_string(),
                range: 13..17,
            }),
        );
        test(
            "insert morphism, x",
            InsertMorphism(Annot {
                value: "x".to_string(),
                range: 17..18,
            }),
        );
        test(
            "insert_at x, toto",
            InsertMorphismAt(
                Annot {
                    value: "x".to_string(),
                    range: 10..11,
                },
                Annot {
                    value: "toto".to_string(),
                    range: 13..17,
                },
            ),
        );
        test(
            "split tot",
            Split(Annot {
                value: "tot".to_string(),
                range: 6..9,
            }),
        );
        test(
            "solve fce1",
            Solve(
                None,
                Annot {
                    value: "fce1".to_string(),
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
                    value: "fce1".to_string(),
                    range: 10..14,
                },
            ),
        );
        test(
            "hide node xxx",
            HideNode(Annot {
                value: "xxx".to_string(),
                range: 10..13,
            }),
        );
        test(
            "reveal morphism yyy",
            RevealMorphism(Annot {
                value: "yyy".to_string(),
                range: 16..19,
            }),
        );
        test("succeed", Succeed);
        test("fail", Fail);

        test(
            "  insert_at\tx        ,   xxx",
            InsertMorphismAt(
                Annot {
                    value: "x".to_string(),
                    range: 12..13,
                },
                Annot {
                    value: "xxx".to_string(),
                    range: 25..28,
                },
            ),
        );

        test(
            "apply Loader.funct_ctx p0:Goal0 Lem0:p",
            Lemma(
                Annot {
                    value: "Loader.funct_ctx".to_string(),
                    range: 6..22,
                },
                vec![
                    (
                        Annot {
                            value: "p0".to_string(),
                            range: 23..25,
                        },
                        Annot {
                            value: "Goal0".to_string(),
                            range: 26..31,
                        },
                    ),
                    (
                        Annot {
                            value: "Lem0".to_string(),
                            range: 32..36,
                        },
                        Annot {
                            value: "p".to_string(),
                            range: 37..38,
                        },
                    ),
                ],
            ),
        );
    }
}
