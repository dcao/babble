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

Run `make plots`. This takes about 10 minutes on a 6-core laptop.

## What is generated

TODO

# Guide to project structure

TODO