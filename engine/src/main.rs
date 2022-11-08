
pub mod data;

fn main() {
    let po : data::ProofObject = data::ProofObject{
      id: 1,
      printed: String::from("Coucou"),
    };
    println!("{:#?}", po);
}
