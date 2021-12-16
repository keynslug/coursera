mod solver;
mod parser;

fn main() {
    use solver::Item;
    let mut stdin = std::io::stdin();
    let mut problem = parser::parse(&mut stdin).unwrap();
    problem.presort(|lhs: &Item, rhs: &Item| {
        lhs.weight.cmp(&rhs.weight)
    });
    println!("{:#?}", problem);
    println!("{:#?}", problem.solve());
}
