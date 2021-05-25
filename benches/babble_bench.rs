use babble::anti_unify::*;
use babble::smiley_lang::*;
use criterion::*;

// Lifted from egg
// macro_rules! bench_fn {
//     (
//         $(#[$meta:meta])*
//         $name:ident, $rules:expr,
//         $start:literal
//         =>
//         $($goal:literal),+ $(,)?
//         $(@check $check_fn:expr)?
//     ) => {
//         bench_fn! {
//             $(#[$meta])*
//             $name, $rules,
//             runner = $crate::Runner::<_, _, ()>::default(),
//             $start => $( $goal ),+
//             $(@check $check_fn)?
//         }
//     };
//
//     (
//         $(#[$meta:meta])*
//         $name:ident, $rules:expr,
//         runner = $runner:expr,
//         $start:literal
//         =>
//         $($goal:literal),+ $(,)?
//         $(@check $check_fn:expr)?
//     ) => {
//         $(#[$meta])*
//         fn $name() {
//             let _ = env_logger::builder().is_test(true).try_init();
//             let name = stringify!($name);
//             let start: $crate::RecExpr<_> = $start.parse().unwrap();
//             let rules = $rules;
//
//             let runner: $crate::Runner<_, _, ()> = $crate::test::run(name, || {
//                 let mut runner = $runner.with_expr(&start);
//                 if let Some(lim) = $crate::test::env_var("EGG_NODE_LIMIT") {
//                     runner = runner.with_node_limit(lim)
//                 }
//                 if let Some(lim) = $crate::test::env_var("EGG_ITER_LIMIT") {
//                     runner = runner.with_iter_limit(lim)
//                 }
//                 if let Some(lim) = $crate::test::env_var("EGG_TIME_LIMIT") {
//                     runner = runner.with_time_limit(std::time::Duration::from_secs(lim))
//                 }
//                 runner.run(&rules)
//             }).report(|r| &r.iterations);
//             runner.print_report();
//
//             let goals = &[$(
//                 $goal.parse().unwrap()
//             ),+];
//
//             // NOTE this is a bit of hack, we rely on the fact that the
//             // initial root is the last expr added by the runner. We can't
//             // use egraph.find_expr(start) because it may have been pruned
//             // away
//             let id = runner.egraph.find(*runner.roots.last().unwrap());
//             runner.egraph.check_goals(id, goals);
//
//             $( ($check_fn)(runner) )?
//         }
//     };
// }
//
// bench_fn! {
//     anti_unif_1, rules(),
//     "(let s1 (rotate 50 (move 2 4 (scale 3 circle))) (let s2 (rotate 50 (move 2 4 (scale 3 line))) (+ s1 s2)))"
//     =>
//     "(let f (fn c (rotate 50 (move 2 4 (scale 3 c)))) (let s1 (app f circle) (let s2 (app f line) (+ s1 s2))))"
// }

fn criterion_benchmark(c: &mut Criterion) {
    let mut g = EGraph::new(());
    let expr = r"
(let s1 (+ (move 4 4 (scale 2 line)) (+ (move 3 2 line) (+ (move 4 3 (scale 9 circle)) (move 5 2 line))))
  (let s2 (+ (move 4 4 (scale 2 circle)) (+ (move 3 2 circle) (+ (move 4 3 (scale 9 circle)) (move 5 2 circle))))
    (+ s1 s2)))".parse().unwrap();
//    let expr = r"
//(let s1 (+ (move 3 2 line) (+ (move 4 3 (scale 9 circle)) (move 5 2 line)))
//  (let s2 (+ (move 3 2 circle) (+ (move 4 3 (scale 9 circle)) (move 5 2 circle)))
//    (+ s1 s2)))".parse().unwrap();
    let root = g.add_expr(&expr);
    // benchmarking anti-unif!
    c.bench_function("test_anti_unif_1", |b| {
        b.iter(|| anti_unify(g.clone(), root))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
