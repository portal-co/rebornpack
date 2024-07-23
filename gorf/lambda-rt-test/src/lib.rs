extern crate alloc;
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn id() {
        let id = lambda_rt::lamc!("\\x.x");
    }
}
