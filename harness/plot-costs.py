#!/usr/bin/env python3

from matplotlib.figure import Figure
import csv


def main():
    costs = {
        "first none": [],
        "first dsrs": [],
        "all none": [],
        "all dsrs": [],
    }

    with open('data_gen/costs.csv', newline='') as file:
        reader = csv.DictReader(file)
        for row in reader:
            if True:  #float(row['first none']) > float(row['first dsrs']):
                for field, value in row.items():
                    if field != "benchmark":
                        costs[field].append(float(value))

    xs = range(len(costs['first none']))

    figure = Figure()
    plot = figure.add_subplot()
    plot.set_title('Dreamcoder Experiments')
    plot.set_xlabel('benchmark')
    plot.set_ylabel('space saving percentage')

    width = 0.15
    margin = 0.01
    num_bars = len(costs.keys())
    for i, (key, cost) in enumerate(costs.items()):
        offset = ((i - (num_bars / 2.0))) * width
        plot.bar(
            [x + offset for x in xs],
            cost,
            width=(width - margin),
            label=key
        )

    plot.set_ylim(bottom=0, top=25)
    plot.legend()

    for i, label in enumerate(plot.get_xticklabels()):
        label.set_visible(False)

    figure.savefig('costs.svg')


main()
