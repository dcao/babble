#!/usr/bin/env python3

from itertools import cycle
from matplotlib import pyplot as plt
import csv
import sys

import numpy as np


def plot_bars(data, keys, width=0.8):
    # n = float(len(data))
    xs = np.arange(len(data))
    print(xs)
    for i, k in enumerate(keys):
        x = xs - width / 2.0 + i / len(keys) * width
        y = [row['initial'] / row[k] for row in data]
        plt.bar(x, y, width=width / len(keys), align="edge")
    plt.xticks(xs, xs)


LINESTYLES = cycle(["-", "--", "-.", ":"])


def plot_lines(data, keys, sort=False):
    for (key, label), line in zip(keys.items(), LINESTYLES):
        ys = [r['initial'] / r[key] for r in data]
        if sort:
            ys.sort(reverse=True)
        xs = list(range(1, len(data) + 1))
        plt.plot(xs, ys, label=label, linestyle=line)
        plt.xlabel('number of benchmarks')
        plt.ylabel('with compression ratio at least')
        xticks = list(range(0, len(data), 5))
        if abs(xticks[-1] - len(data)) <= 2:
            xticks.pop()
        xticks.append(len(data))
        xticks[0] = 1
        plt.xticks(xticks, xticks)
        # ax = plt.gca()
        # ax.xaxis.get_major_locator().set_params(integer=True)


if __name__ == "__main__":

    try:
        _, filename, output = sys.argv
    except ValueError:
        print("Usage: plot.py {input} {output}")
        sys.exit(1)

    data = []
    with open(filename) as f:
        for row in csv.DictReader(f):
            for k in row:
                try:
                    row[k] = int(row[k])
                except Exception:
                    pass
            data.append(row)

    data.sort(key=lambda r: r['initial'] / r['dc'])

    plot_lines(data, {
        'all dsrs': "Babble",
        'all none': "AU only",
        'all eqsat': "EqSat only",
        'dc': "DreamCoder",
    }, sort=True)

    # plt.ylim((0.9, None))
    plt.legend()
    plt.savefig(output, bbox_inches="tight")

    print(list(data[0].keys()))
