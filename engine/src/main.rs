pub mod anyterm;
pub mod data;
pub mod dsl;
pub mod graph;
pub mod parser;
pub mod pretty;
pub mod rpc;
pub mod substitution;
pub mod unification;

use data::ProofObject;
use data::{ActualCategory, ActualFunctor, ActualMorphism, ActualObject};
use data::{CategoryData, FunctorData, MorphismData, ObjectData};
use dsl::{cat, funct, mph, obj};
use graph::Graph;
use substitution::Substitutable;

use std::fs::File;
use std::vec::Vec;

use clap::{Parser, Subcommand};
use rmp_serde::encode;

fn messagepack_to_file(path: &str, gr: &Graph) {
    let mut file = File::create(path).unwrap();
    encode::write(&mut file, gr).unwrap();
}

fn test_main(packfile: &str) {
    let mut ctx = data::Context::new();

    let cat = cat!(ctx, :0);
    let src_cat = cat!(ctx, :1);
    let a = obj!(ctx, (:2) in cat);
    let b = obj!(ctx, (:3) in cat);
    let c = obj!(ctx, (:4) in src_cat);
    let f = funct!(ctx, (:5) : src_cat => cat);
    let c = obj!(ctx, f _0 c);
    let m1 = mph!(ctx, (?0) : a -> b);
    let m2 = mph!(ctx, (?1) : b -> c);
    let gr: graph::Graph = graph::Graph {
        nodes: vec![a, b, c.clone()],
        edges: vec![vec![(1, m1.clone())], vec![(2, m2.clone())], Vec::new()],
        faces: Vec::new(),
    };

    let cat_unk = cat!(ctx, ?2);
    let x = obj!(ctx, (?3) in cat_unk);
    let y = obj!(ctx, (?4) in cat_unk);
    let f = mph!(ctx, (:6) : x -> y);
    let gr2: graph::Graph = graph::Graph {
        nodes: vec![x, y],
        edges: vec![vec![(1, f)], Vec::new()],
        faces: Vec::new(),
    };

    let mces = graph::mces::MCES::new(&mut ctx, &gr, &gr2);
    for (sol, sigma) in mces {
        let gr_subst = gr.clone().subst(&ctx, &sigma);
        let gr2_subst = gr2.clone().subst(&ctx, &sigma);
        graph::span_viz(&mut ctx, &gr_subst, &gr2_subst, &sol)
    }

    messagepack_to_file(packfile, &gr);
}

fn embed() {}

#[derive(Parser, Debug)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Test {
        #[arg(short, long)]
        packfile: Option<String>,
    },
    Embed,
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Test { packfile } => {
            let packfile = packfile.unwrap_or("gr.mp".to_string());
            test_main(&packfile)
        }
        Commands::Embed => embed(),
    }
}
