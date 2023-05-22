/// Calculates the efficiency of a compression algorithm.
#[must_use]
#[allow(clippy::cast_precision_loss)]
pub fn compression_factor(start_cost: usize, end_cost: usize) -> f64 {
    start_cost as f64 / end_cost as f64
}
