import subprocess
from dataclasses import dataclass
import sys
import csv
import os
import matplotlib.pyplot as plt
from matplotlib.ticker import FormatStrFormatter
from matplotlib.pyplot import cm
import numpy as np

def param_sweep(path_to_drawing_bab):
    # beams = "10 50 100 200 500 1000"
    # lps = "1 3 5 10"
    # rounds = "2 5 10"
    beams = "5 10"
    lps = "1 2"
    rounds = "1 2 3"
    max_arity = "3"
    subprocess.run(["time", "cargo", "run", "--release", "--bin=drawings", "--", path_to_drawing_bab] +
                   ["--beams"] + beams.split() + ["--lps"] + lps.split() + ["--rounds"] + rounds.split() + ["--max-arity", max_arity])

def parse_results_csv(path):
    FIELDS = \
        [ '__0'
        , '__1'
        , 'beam_size_1'
        , 'beam_size_2'
        , 'lps'
        , '__2'
        , '__3'
        , 'init_size'
        , 'final_size'
        , 'compression'
        , 'time' ]
    with open(path) as f:
        rows = list(csv.DictReader(f, fieldnames=FIELDS))

    # add derived "round" field
    cfg_round = {}
    for r in rows:
        cfg = (r['beam_size_1'], r['beam_size_2'], r['lps'])
        if cfg not in cfg_round:
            cfg_round[cfg] = 0
        cfg_round[cfg] += 1 
        r['round'] = cfg_round[cfg]

    return rows

def mkplot(rows, xField, yField, plot_dir):
    assert xField in ['round', 'beam_size_1', 'lps']
    assert yField in ['compression', 'time', 'memory']

    groupBy = ['round', 'beam_size_1', 'lps']
    groupBy.remove(xField)

    group = {}
    for r in rows:
        g = tuple([r[f] for f in groupBy])
        if g not in group:
            group[g] = []
        group[g].append(r)

    plt.figure()
    color = iter(cm.rainbow(np.linspace(0, 1, len(group))))
    for g in group:
        xs = [float(r[xField]) for r in group[g]]
        # ys = [f'{float(r[yField]):7.4f}' for r in group[g]]
        ys = [float(r[yField]) for r in group[g]]
        plt.plot(xs, ys, marker="o", c=next(color), label=str(g))
    fnm = os.path.join(plot_dir, '{}-{}.pdf'.format(xField, yField))
    plt.legend(loc="upper right")
    plt.title('{} vs {} over all {}'.format(yField, xField, ','.join(groupBy)))
    plt.savefig(fnm)
    plt.show()

def mkplots(rows):
    plot_dir = os.path.join('plot_results')
    if not os.path.exists(plot_dir):
       os.makedirs(plot_dir)

    for x in ['round', 'beam_size_1', 'lps']:
        for y in ['compression', 'time']: # TODO memory
            mkplot(rows, x, y, plot_dir)

def analyze_data(p):
    mkplots(parse_results_csv(p))

usage = """USAGE: python param_sweep.py path_to_drawing_benchmark.bab
"""

if __name__ == "__main__":
    if(len(sys.argv) != 2):
        print(usage)
    else:
        fnm = sys.argv[1]
        if (fnm.split(".")[1] != "bab"):
            print("Must provide .bab file")
        else:
            param_sweep(sys.argv[1])
            analyze_data("target/res_drawing.csv")
