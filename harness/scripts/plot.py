#!/usr/bin/env python3

from itertools import cycle
from matplotlib import pyplot as plt
import csv
import sys
import numpy as np

import matplotlib
matplotlib.rcParams['pdf.fonttype'] = 42
matplotlib.rcParams['ps.fonttype'] = 42
matplotlib.rcParams['savefig.bbox'] = 'tight'

from multiprocessing.dummy import Pool

import os.path
import subprocess
import shlex
from pathlib import Path
from glob import glob

def read_dict(filename):
    data = []
    with open(filename) as f:
        for row in csv.DictReader(f):
            for k in row:
                try:
                    row[k] = float(row[k])
                except Exception:
                    pass
            data.append(row)
    return data

def read_list(filename):
    data = []
    with open(filename) as f:
        for row in csv.reader(f):
            for i, v in enumerate(row):
                try:
                    row[i] = float(v)
                except Exception:
                    pass
            data.append(row)
    return data

def plot_scatter(data, **kwargs):
    xs = [r['initial cost'] / r['final cost'] for r in data]
    ys = [r['total time'] for r in data]
    plt.plot(xs, ys, lw=0, **kwargs)

SRC_TIME = max(os.path.getmtime(f) for f in glob("src/**/*.rs", recursive=True))
SRC_TIME = 0 # FIXME
DC_DATA = read_dict("harness/data_gen/dc_res.csv")

def get_dc_data_wrapped(kwargs):
    return get_dc_data(**kwargs)

def get_dc_data(
    domain, *, 
    beam_size=400, 
    lps=1,
    rounds=20,
    max_arity=2,
    use_all=True,
    lib_iter_limit=1,
    mode="babble",
):
    if mode == "dc":
        filtered = [r for r in DC_DATA if r["name"].startswith(domain)]
        assert filtered
        return filtered

    args = " ".join([
        f"--domain={domain} --beam-size={beam_size} --lps={lps}",
        f"--mode={mode} --rounds={rounds} --max-arity={max_arity}",
        f"--use-all={int(use_all)} --lib-iter-limit={lib_iter_limit}",
    ])
    fname = args.replace("--", "").replace(" ", "__").replace("-", "") + ".csv"
    outdir = Path("harness/data_gen/cache")
    outdir.mkdir(parents=True, exist_ok=True)
    fname = outdir / fname
    cmd = f"cargo run --release --bin=benchmark -- {args} --output={fname}"

    run_and_cache(cmd, fname)

    data = read_dict(fname)
    return data

def run_and_cache(cmd, fname, *other_inputs):
    try:
        mtime = os.path.getmtime(fname)
    except FileNotFoundError:
        mtime = 0
        print(f"{fname} not present")
    
    input_mtimes = [SRC_TIME] + [os.path.getmtime(f) for f in other_inputs]
    if mtime < max(input_mtimes):
        subprocess.run(shlex.split(cmd))
    else:
        print(f"Cache hit on {fname}: {mtime} < {SRC_TIME}")

# foobar_list = { 
#     # "Babble1first": dict(domain="physics", lps=1, rounds=20, beam_size=1, use_all=False),
#     # "Babble10": dict(domain="physics", lps=1, rounds=20, beam_size=10),
#     "Babble400-l1-r20": dict(domain="physics", lps=1, rounds=20, beam_size=400),
#     "Babble400-l1-r20-ma3": dict(domain="physics", lps=1, rounds=20, beam_size=400, max_arity=3),
#     # "Babble400-l1-r40": dict(domain="physics", lps=1, rounds=40, beam_size=400),
#     # "Babble400-l2-r1": dict(domain="physics", lps=2, rounds=1, beam_size=400, lib_iter_limit=2),
#     # "Babble400-l2-r5": dict(domain="physics", lps=2, rounds=5, beam_size=400, lib_iter_limit=2),
#     # "Babble400-l2-r10": dict(domain="physics", lps=2, rounds=10, beam_size=400, lib_iter_limit=2),
#     # "Babble400-l4-r5": dict(domain="physics", lps=4, rounds=5, beam_size=400, lib_iter_limit=2),
#     # "Babble400first": dict(domain="physics", lps=1, rounds=20, beam_size=400, use_all=False),
#     # "Babble2": dict(domain="physics", lps=2, rounds=1, beam_size=1000),
#     # "Babble5": dict(domain="physics", lps=5, rounds=1),
#     # "Babble20": dict(domain="physics", lps=20, rounds=1),
#     "AU only": dict(domain="physics", lps=1, rounds=20, mode="au"),
#     "EqSat only": dict(domain="physics", lps=1, rounds=20, mode="eqsat"),
#     "DreamCoder": dict(domain="physics", mode="dc"),
# }

def plot(domain, request, name=None):
    name = name or domain
    plt.clf()
    plot_params, data_params = zip(*request)
    for d in data_params:
        d["domain"] = domain
    
    with Pool() as p:
        values = p.map(get_dc_data_wrapped, data_params)

    # markers = cycle("xo^v")
    for (params, data) in zip(plot_params, values):
        params.setdefault('marker', 'o')
        plot_scatter(data, fillstyle='none', alpha=0.6, **params)

    plt.xlabel('Compression Ratio')
    plt.ylabel('Time (seconds)')
    plt.yscale('log')
    plt.legend()
    plt.savefig(f"harness/plots/{name}.png")
    plt.savefig(f"harness/plots/{name}.pdf")

if __name__ == "__main__":

    basic = [
        (dict(marker="x", color="blue", label="Babble"), dict()),
        (dict(marker="o", color="darkviolet", label="DreamCoder"), dict(mode="dc")),
    ]

    ablate = [
        (dict(marker="^", color="green", label="BabbleSyn"), dict(mode="au")),
        (dict(marker="v", color="red", label="EqSat"), dict(mode="eqsat")),
    ]

    rounds = [
        (dict(marker="^", color="green", label="Babble r=2"), dict(rounds=2)),
        (dict(marker="v", color="red", label="Babble r=10"), dict(rounds=10)),
        (dict(marker="x", color="blue", label="Babble r=20"), dict(rounds=20)),
        (dict(marker="o", color="darkviolet", label="DreamCoder"), dict(mode="dc")),
    ]

    plot("list", basic + ablate)
    plot("physics", basic + ablate)
    plot("text", basic)
    plot("towers", basic)
    plot("logo", basic)

    plot("list", rounds, name = "list-rounds")
    plot("physics", rounds, name = "physics-rounds")
    plot("text", rounds, name = "text-rounds")
    plot("towers", rounds, name = "towers-rounds")
    plot("logo", rounds, name = "logo-rounds")
