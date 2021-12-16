extern crate num_rational;
extern crate num_traits;

use num_rational::Ratio;

pub type Capacity = usize;
pub type Weight = Capacity;
pub type Value = usize;

#[derive(Debug, PartialEq, Eq)]
pub struct ItemID(usize);

impl std::fmt::Display for ItemID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{}", self.0))
    }
}

#[derive(Debug, Default)]
pub struct Problem {
    pub capacity: Capacity,
    pub items: Vec<Item>
}

#[derive(PartialEq, Eq)]
pub struct Item {
    pub ident: ItemID,
    pub weight: Weight,
    pub value: Value
}

impl std::fmt::Debug for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use num_traits::cast::ToPrimitive;
        f.write_fmt(format_args!("Item {}: value = {}, weight = {}, density = {:.6}",
            self.ident,
            self.value,
            self.weight,
            self.density().to_f64().unwrap()
        ))
    }
}

impl Item {

    pub fn density(&self) -> Ratio<Value> {
        Ratio::new(self.value, self.weight)
    }

}

#[derive(Debug, Default)]
pub struct Solution {
    pub optimum: Value,
    pub choices: Vec<ItemID>
}

trait PartialSolution {
    fn capacity(&self) -> Capacity;
    fn value(&self) -> Value;
    fn choice(&self, item: ItemID) -> Option<bool>;
    fn feasible(&self) -> bool;
}

impl PartialSolution for Problem {

    fn capacity(&self) -> Capacity {
        self.capacity
    }

    fn value(&self) -> Value {
        0
    }

    fn choice(&self, _item: ItemID) -> Option<bool> {
        None
    }

    fn feasible(&self) -> bool {
        true
    }

}

trait Estimator<'a> {
    fn estimate<T: PartialSolution>(&'a self, solution: &T) -> Value;
}

#[derive(Debug)]
struct LinearRelaxation<'a> {
    density_order: Vec<&'a Item>
}

impl<'a> LinearRelaxation<'a> {

    pub fn new(problem: &'a Problem) -> Self {
        let mut density_order: Vec<&Item> = problem.items.iter().collect();
        density_order.sort_by(|a, b| {
            a.density().cmp(&b.density()).reverse()
        });
        Self {
            density_order
        }
    }

}

impl<'a> Estimator<'a> for LinearRelaxation<'a> {

    fn estimate<T: PartialSolution>(&'a self, solution: &T) -> Value {
        let mut estimate = solution.value();
        let mut capacity_left = solution.capacity();
        for item in self.density_order.iter() {
            if let Some(_) = solution.choice(item.ident) {
                continue;
            }
            if capacity_left > item.weight {
                estimate += item.value;
                capacity_left -= item.weight;
            }
            else {
                estimate += (item.density() * capacity_left).ceil().to_integer();
                break;
            }
        }
        estimate
    }

}

#[derive(Debug, Clone)]
struct SolutionState<'a> {
    problem: &'a Problem,
    choices: Vec<Option<bool>>,
    value: Value,
    capacity_left: isize
}

impl<'a> SolutionState<'a> {

    pub fn init(problem: &'a Problem) -> Self {
        Self {
            problem,
            choices: vec![None; problem.items.len()],
            value: 0,
            capacity_left: problem.capacity as isize
        }
    }

    pub fn include(self, item: ItemID) -> Self {
        debug_assert!(self.choices[item].is_none());
        let mut next = self;
        next.choices[item].replace(true);
        let item = &next.problem.items[item];
        next.value += item.value;
        next.capacity_left -= item.weight as isize;
        next
    }

    pub fn exclude(self, item: ItemID) -> Self {
        debug_assert!(self.choices[item].is_none());
        let mut next = self;
        next.choices[item].replace(false);
        next
    }

    pub fn discard(self, item: ItemID) -> Self {
        let mut next = self;
        if let Some(true) = next.choices[item].take() {
            let item = &next.problem.items[item];
            next.value -= item.value;
            next.capacity_left += item.weight as isize;
        }
        next
    }

}

impl<'a> From<SolutionState<'a>> for Solution {
    fn from(state: SolutionState) -> Self {
        Self {
            optimum: state.value,
            choices: state.choices.iter()
                .enumerate()
                .filter(|(_, choice)| choice.unwrap_or(false))
                .map(|(idx, _)| state.problem.items[idx].ident)
                .collect()
        }
    }
}

impl<'a> PartialSolution for SolutionState<'a> {

    fn capacity(&self) -> Capacity {
        std::convert::TryFrom::try_from(self.capacity_left).unwrap_or(0)
    }

    fn value(&self) -> Value {
        self.value
    }

    fn choice(&self, item: ItemID) -> Option<bool> {
        self.choices[item]
    }

    fn feasible(&self) -> bool {
        self.capacity_left >= 0
    }
}

impl Problem {

    pub fn presort<F>(&mut self, f: F) where
        F: Fn(&Item, &Item) -> std::cmp::Ordering
    {
        self.items.sort_by(f);
    }

    pub fn solve(&self) -> Solution {
        let mut state = SolutionState::init(&self);
        let mut best = state.clone();
        let estimator = LinearRelaxation::new(&self);

        let size = self.items.len();
        let mut level = 0;
        let mut visited = 0usize;
        let mut infeasible = 0usize;
        let mut pruned = 0usize;
        loop {
            let choice = state.choice(level);
            match choice {
                None => {
                    // println!("choose {:?} = {:?}", level, true);
                    state = state.include(level);
                    if state.feasible() == false {
                        // println!("infeasible = {}", level);
                        // infeasible += 2usize.pow((size - level - 1) as u32);
                        infeasible += 1;
                        continue;
                    }
                },
                Some(true) => {
                    // println!("choose {:?} = {:?}", level, false);
                    state = state.discard(level).exclude(level);
                },
                Some(false) => {
                    // println!("unchoose {:?}", level);
                    state = state.discard(level);
                    if level == 0 {
                        break;
                    }
                    else {
                        level -= 1;
                        continue;
                    }
                }
            }
            if level == size - 1 {
                visited += 1;
                if state.value() > best.value() {
                    // println!("picked best = {:?}", state.value());
                    best = state.clone();
                }
            }
            else {
                let estimate = estimator.estimate(&state);
                // println!("estimate = {:?} / best = {:?}", estimate, best.value());
                if estimate > best.value() {
                    level += 1;
                } else {
                    // println!("pruned = {}", level);
                    // pruned += 2usize.pow((size - level - 1) as u32);
                    pruned += 1;
                }
            }
        }

        println!("visited = {}", visited);
        println!("infeasible = {}", infeasible);
        println!("pruned = {}", pruned);

        Solution::from(best)
    }

}
