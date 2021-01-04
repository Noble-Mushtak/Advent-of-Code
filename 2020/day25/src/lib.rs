use std::error::Error;

const BASE: usize = 7;
const MOD: usize = 20201227;

fn modulo_exp(base: usize, mut exp: usize, modulo: usize) -> usize {
    let mut ans = 1;
    let mut base_to_power_of_2 = base;
    while exp != 0 {
        if (exp & 1) != 0 {
            ans = (ans * base_to_power_of_2) % modulo;
        }
        base_to_power_of_2 = (base_to_power_of_2 * base_to_power_of_2) % modulo;
        exp >>= 1;
    }
    ans
}

fn discrete_log(base: usize, res: usize, modulo: usize) -> usize {
    let mut ans = 0;
    let mut cur_res = 1;
    while cur_res != res {
        cur_res = (cur_res * base) % modulo;
        ans += 1;
    }
    ans
}

fn calc_enc_key(pub_key1: usize, pub_key2: usize) -> usize {
    let priv_key1 = discrete_log(BASE, pub_key1, MOD);
    modulo_exp(pub_key2, priv_key1, MOD)
}

pub fn run() -> Result<(), Box<dyn Error>> {
    let c_pub_key = 11349501;
    let d_pub_key = 5107328;

    println!("Part 1: {}", calc_enc_key(c_pub_key, d_pub_key));

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_enc_key() {
        assert_eq!(calc_enc_key(5764801, 17807724), 14897079);
    }
}
