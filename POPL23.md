# `babble` POPL 2023 Artifact

This is the artifact for paper #94, 
 "`babble`: Learning Better Abstractions with E-Graphs and Anti-Unification".

# Claims

This artifact validates the following quantitative claims.

1. `babble` achieves better compression ratios than DreamCoder on the DreamCoder benchmarks,
  and it does so much faster than DreamCoder. (Figure 11)
2. `babble` acheives better compression when given domain-specific equivalences. (Figure 11)
3. `babble` can compress the larger CogSci benchmark suite as claimed in Table 2.

# Installation

## VM installation 

TODO

Run `git pull` to ensure you have the latest version.

## Manual installation 

Clone the project using the `--recursive` flag to get the submodules.

```bash
git clone --recursive -b popl23 https://github.com/dcao/babble.git
cd babble
```

If you are working with an existing clone, make sure to pull the submodules. 
A fresh clone using the `--recursive` flag won't need this.

```bash
git checkout popl23
git submodule init
git pull --recurse-submodules 
```

### Requirements

These are all preinstalled in the VM, but you can easily install them on your own machine.

- Rust version 1.63
- Python > 3.8
    - matplotlib
    - numpy
- GNU make

# Running the evaluation

## Sanity check

Check that everything is okay by building `babble`,
 this is done automatically by the `Makefile`,
 but it's a quick way to check that things are working.
This takes about 2 minutes.

```bash
cargo build --release
```

From the `babble` directory, then run:
```bash
make plots-quick
```

This runs a subset of the evaluation (just the `physics` benchmark).
It runs in parallel, taking about 1.5 minutes on a 6-core laptop.
Like `make plots`, it will print the names of the plots created under `harness/plots`.
Those plots should look like those from Figure 11.
(Note that `make plots-quick` will omit some data, so the plot will re-scale).


## Figure 11: Babble vs Dreamcoder plots

Run `make plots`. This takes about 10 minutes on a 6-core laptop.
The plots are stored in `harness/plots` as both pdfs and pngs.

### Validate Claim 1

Inspect the following plots the comprise Figure 11.
These links will only be clickable after running `make plots`.

- Fig 11a: [`harness/plots/list.png`](harness/plots/list.png)
- Fig 11b: [`harness/plots/physics.png`](harness/plots/physics.png)
- Fig 11c: [`harness/plots/text.png`](harness/plots/text.png)
- Fig 11d: [`harness/plots/logo.png`](harness/plots/logo.png)
- Fig 11e: [`harness/plots/towers.png`](harness/plots/towers.png)

To validate claim 1, 
 observe that `babble` (blue X) 
 tends to be down (faster) 
 and to the right (higher/better compression ratio)
 than DreamCoder (purple O).

The DreamCoder numbers are pulled from a log, 
 and should be identical to those in the paper.
The `babble` compression ratios should not vary,
 but the time may depending on your machine.
Regardless, it should be considerably faster than DreamCoder.

### Validate Claim 2

Inspect the following plots the comprise Figure 11.
These links will only be clickable after running `make plots`.

- Fig 11a: [`harness/plots/list.png`](harness/plots/list.png)
- Fig 11b: [`harness/plots/physics.png`](harness/plots/physics.png)

We only supplied domain-specific rewrites for the list and physics domains.

"BabbleSyn" (green triangle) denotes 
 `babble` without any domain-specific rewrites (it just does "syntactic" learning).
Observe that BabbleSyn does not compress as well as `babble`, it's further left.

"EqSat" (red triangle) denotes
 just running equality saturation with the domain-specific rewrites, 
 but not doing library learning.
This baseline should run very quickly (low on the graph),
 but achieve relatively little compression,
 since it is not learning any abstractions.

## Table 2: Babble on CogSci dataset

Run `make cogsci-table`.
This takes about 3-4 minutes on a 6-core laptop.
This prints a latex formatted table that looks something like the following:

```latex
Nuts \& Bolts   &      19009 &       2059 &       9.23 &      27.89 &       1744 &      10.90 &      62.72
Vehicles        &      35427 &       6477 &       5.47 &     113.45 &       5505 &       6.44 &     114.75
Gadgets         &      35713 &       6798 &       5.25 &     108.22 &       5037 &       7.09 &     119.30
Furniture       &      42936 &      10539 &       4.07 &     167.18 &       9417 &       4.56 &     146.73
Nuts \& Bolts (clean) &      18259 &       2215 &       8.24 &      30.38 &       1744 &      10.47 &      66.06
```

It also writes the scatterplot from Figure 12 at 
[`harness/plots/cogsci-scatter.png`](harness/plots/cogsci-scatter.png)

### Validate Claim 3

Inspect the table printed by `make cogsci-table`. 
You can freely re-run this command to regenerate the table,
 the results will be cached after the first run.

The columns are the same as those in the paper. 
Confirm that this data is the same as that shown in the paper,
 modulo small variation in the time columns depending on your machine.

# Guide to project structure

The top level of the `babble` project folder contains several folders and files. Of the top-level files, one is of particular interest: the `Makefile` is used as the task runner for generating the various evaluation figures. Full usage of the `Makefile` to reproduce the evaluation results in the paper are detailed above. The `Makefile` also contains several commands to build the project, which are detailed in the file itself.

The project has four top-level folders total. Two of these are somewhat auxiliary: the `babble-macros` folder is a Rust crate which defines a macro `rewrite_rules`, used during development for adding rewrite rules ad-hoc while running equality saturation, while the `examples` folder contains a list of `babble` examples used for testing and demo purposes (these examples aren't used for the actual evaluation in the `babble` paper).

Outside of this, the main two sub-folders of interest are `src` and `harness`. `src`. `src` contains all the source code for `babble` itself, while `harness` contains the data needed to generate the evaluation, the generated files created in the process of generating the evaluation, and the scripts which generate the evaluation. We detail the structure of each of these parts of the `babble` project below.

## `src`

TODO

## `harness`

TODO