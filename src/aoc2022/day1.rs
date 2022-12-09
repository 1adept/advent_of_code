pub fn tasks() {
    let mut elfes = Vec::new();
    elfes.push(Elf::default());
    for calory in super::load_input(1).lines() {
        if calory.is_empty() {
            elfes.push(Elf::default());
            continue;
        }

        let last_elf = elfes.last_mut().unwrap();
        let parsed = calory.parse::<u64>().unwrap();
        last_elf.calories += parsed;
    }

    elfes.sort_by_key(|e| e.calories);
    elfes.reverse();

    println!(
        "Most calories carried by elfe {:?}",
        &elfes.first().unwrap()
    );

    let top_3_calories = elfes.iter().take(3).map(|e| e.calories).sum::<u64>();

    println!("Calories carried by the top 3 elfes: {top_3_calories}");
}

#[derive(Debug, Default)]
struct Elf {
    pub calories: u64,
}
