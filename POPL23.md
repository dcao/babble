# Babble POPL 2023 Artifact

This is the artifact for paper #94, 
 "babble: Learning Better Abstractions with E-Graphs and Anti-Unification".

# Claims

TODO

# Installation

## VM installation 

TODO

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

## Sanity check

This is done automatically by the `Makefile`,
 but just check that everything is okay by building `babble`.
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

# Running the evaluation

## Figure 11: Babble vs Dreamcoder plots

Run `make plots`. This takes about 10 minutes on a 6-core laptop.
The plots are stored in `harness/plots` as both pdfs and pngs.

TODO explain plots

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

It also writes the scatterplot from Figure 12 at `harness/plots/cogsci-scatter.{png,pdf}`

# Guide to project structure

TODO