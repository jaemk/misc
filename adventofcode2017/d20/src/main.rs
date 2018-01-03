use std::collections::HashMap;


static INPUT: &'static str = include_str!("../input.txt");


type Error = Box<std::error::Error>;
type Result<T> = std::result::Result<T, Error>;


#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
struct Property {
    x: isize,
    y: isize,
    z: isize,
}
impl Property {
    fn origin_distance(&self) -> isize {
        self.x.abs() + self.y.abs() + self.z.abs()
    }
}


#[derive(Debug, Clone)]
struct Particle {
    id: usize,
    steps: usize,
    avg_dist: f64,
    dist_total: f64,
    position: Property,
    velocity: Property,
    accel: Property,
}
impl std::str::FromStr for Particle {
    type Err = Error;
    fn from_str(s: &str) -> Result<Self> {
        let props = s.trim().split(", ").map(|prop| {
            let coords = prop.chars()
                .skip(3)
                .take_while(|c| *c != '>')
                .collect::<String>();
            let coords = coords.split(",")
                .map(|n| Ok(n.parse::<isize>()?) )
                .collect::<Result<Vec<isize>>>()?;
            Ok(Property { x: coords[0], y: coords[1], z: coords[2] })
        }).collect::<Result<Vec<Property>>>()?;

        let dist = props[0].origin_distance() as f64;
        Ok(Self {
            id: 0,
            steps: 0,
            avg_dist: dist,
            dist_total: dist,
            position: props[0],
            velocity: props[1],
            accel: props[2],
        })
    }
}


/// Particle is wrapped in an Option for part2
fn parse_particles(input: &str) -> Result<Vec<Option<Particle>>> {
    input.trim().lines()
        .enumerate()
        .map(|(i, line)| {
            let mut particle = line.parse::<Particle>()?;
            particle.id = i;
            Ok(Some(particle))
        }).collect()
}


/// Particle is wrapped in an Option for part2
fn particle_step(particles: &mut [Option<Particle>]) {
    for particle in particles.iter_mut() {
        if let &mut Some(ref mut particle) = particle {
            particle.velocity.x += particle.accel.x;
            particle.velocity.y += particle.accel.y;
            particle.velocity.z += particle.accel.z;

            particle.position.x += particle.velocity.x;
            particle.position.y += particle.velocity.y;
            particle.position.z += particle.velocity.z;

            particle.steps += 1;
            particle.dist_total += particle.position.origin_distance() as f64;
            particle.avg_dist = particle.dist_total / (particle.steps as f64);
        }
    }
}


fn part1(input: &str) -> Result<usize> {
    let mut particles = parse_particles(input)?;
    for _ in 0..1_000 {
        particle_step(&mut particles);
    }

    // pull particles out of their `Option`s and sort by dist
    let mut particles = particles.into_iter().filter_map(|p| p).collect::<Vec<_>>();
    particles.sort_by(|a, b| a.avg_dist.partial_cmp(&b.avg_dist).expect("Failed comparing floats"));
    Ok(particles[0].id)
}


fn part1_quick(input: &str) -> Result<usize> {
    let particles = parse_particles(input)?;
    let mut particles = particles.into_iter().filter_map(|p| p).collect::<Vec<_>>();
    particles.sort_by(|a, b| a.accel.origin_distance().cmp(&b.accel.origin_distance()));
    Ok(particles[0].id)
}


fn prune_collisions(particles: &mut [Option<Particle>]) {
    let mut positions = HashMap::with_capacity(particles.len());
    for particle in particles.iter() {
        if let &Some(ref particle) = particle {
            let e = positions.entry(particle.position).or_insert(vec![]);
            e.push(particle.id);
        }
    }

    let mut to_remove = vec![];
    for (_position, ids) in &positions {
        if ids.len() > 1 {
            to_remove.extend_from_slice(&ids);
        }
    }

    for index in to_remove.into_iter() {
        particles[index] = None;
    }
}


fn part2(input: &str) -> Result<usize> {
    let mut particles = parse_particles(input)?;
    for _ in 0..1_000 {
        prune_collisions(&mut particles);
        particle_step(&mut particles);
    }
    Ok(particles.into_iter().filter_map(|p| p).count())
}


fn run() -> Result<()> {
    println!("d20-p1-quick: {:?}", part1_quick(INPUT)?);
    println!("d20-p1-simul: {:?}", part1(INPUT)?);
    println!("d20-p2: {:?}", part2(INPUT)?);

    Ok(())
}


pub fn main() {
    if let Err(e) = run() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    static TEST_INPUT: &'static str = r##"
p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>
"##;

    #[test]
    fn p1() {
        assert_eq!(part1(TEST_INPUT).unwrap(), 0);
    }

    static TEST_INPUT_2: &'static str = r##"
p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>
"##;

    #[test]
    fn p2() {
        assert_eq!(part2(TEST_INPUT_2).unwrap(), 1);
    }

}
