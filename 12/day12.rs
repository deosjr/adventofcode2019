
struct Vector {
    x: i32,
    y: i32,
    z: i32
}

impl Vector {
    fn new(x:i32, y:i32, z:i32) -> Vector {
        Vector{x:x, y:y, z:z}
    }

    fn abs_sum(&self) -> i32 {
        self.x.abs() + self.y.abs() + self.z.abs()
    }

    fn plus(&mut self, other: &Vector) {
        self.x += other.x;
        self.y += other.y;
        self.z += other.z;
    }
}

impl std::fmt::Debug for Vector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<x={}, y={}, z={}>", self.x, self.y, self.z)
    }
}

struct Moon {
    position: Vector,
    velocity: Vector
}

impl Moon {
    fn new(x:i32, y:i32, z:i32) -> Moon {
        Moon{ 
            position: Vector::new(x, y, z),
            velocity: Vector::new(0, 0, 0)
        }
    }

    fn update_velocity(&mut self, other: &mut Moon) {
        self.velocity.x += f(self.position.x, other.position.x);
        self.velocity.y += f(self.position.y, other.position.y);
        self.velocity.z += f(self.position.z, other.position.z);
    }

    fn update_position(&mut self) {
        self.position.plus(&self.velocity);
    }

    fn energy(&self) -> i32 {
        self.position.abs_sum() * self.velocity.abs_sum()
    }
}

impl std::fmt::Debug for Moon {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "pos={:?}, vel={:?}", self.position, self.velocity)
    }
}

fn f(a: i32, b: i32) -> i32 {
    if a < b {
        1
    } else if a > b {
        -1
    } else {
        0
    }
}

fn gcd(x: i64, y: i64) -> i64 {
    if x == 0 {
        y.abs()
    } else {
        gcd(y % x, x)
    }
}

fn lcm(x: i64, y: i64) -> i64 {
    (x * y) / gcd(x, y)
}

fn main() {
    let moon1 = Moon::new(1, 3, -11);
    let moon2 = Moon::new(17, -10, -8);
    let moon3 = Moon::new(-1, -15, 2);
    let moon4 = Moon::new(12, -4, -4);

    let mut moons = vec![moon1, moon2, moon3, moon4];

    let combinations = vec![(0,1), (0,2), (0,3), (1,2), (1,3), (2,3)];

    let mut originalx = [0; 8];
    let mut originaly = [0; 8];
    let mut originalz = [0; 8];
    for (i, m) in moons.iter().enumerate() {
        originalx[2*i] = m.position.x;
        originalx[2*i + 1] = m.velocity.x;
        originaly[2*i] = m.position.y;
        originaly[2*i + 1] = m.velocity.y;
        originalz[2*i] = m.position.z;
        originalz[2*i + 1] = m.velocity.z;
    }

    let mut xloop = 0;
    let mut yloop = 0;
    let mut zloop = 0;

    let mut dimx = [0; 8];
    let mut dimy = [0; 8];
    let mut dimz = [0; 8];

    for i in 1..1000000 {
        for (a,b) in &combinations {
            let (left, right) = moons.split_at_mut(*b);
            let m1 = left.get_mut(*a).unwrap();
            let m2 = right.get_mut(0).unwrap();
            m1.update_velocity(m2);
            m2.update_velocity(m1);
        }
        for (j, m) in moons.iter_mut().enumerate() {
            m.update_position();
            dimx[2*j] = m.position.x;
            dimx[2*j + 1] = m.velocity.x;
            dimy[2*j] = m.position.y;
            dimy[2*j + 1] = m.velocity.y;
            dimz[2*j] = m.position.z;
            dimz[2*j + 1] = m.velocity.z;
        }
        if i == 1000 {
            let energy = moons.iter().map(|x| x.energy()).sum::<i32>();
            println!("Part 1: {}", energy);
        }
        if xloop == 0 && originalx == dimx {
            xloop = i;
        }
        if yloop == 0 && originaly == dimy {
            yloop = i;
        }
        if zloop == 0 && originalz == dimz {
            zloop = i;
        }
        if xloop != 0 && yloop != 0 && zloop != 0 {
            let ans = [xloop, yloop, zloop].iter().fold(1, |acc, &x| lcm(acc, x));
            println!("Part 2: {}", ans);
            break
        }
    }
}
