#!/usr/bin/env python3

from matplotlib.figure import Figure
import csv


def main():
    compression = {
        "all dsrs/all none": [],
        "first dsrs/first none": [],
        "all dsrs/first dsrs": [],
        "all none/first none": [],
        "all dsrs/dc": [],
        "all none/dc": [],
    }

    with open('harness/data_gen/compression.csv', newline='') as file:
        reader = csv.DictReader(file)
        for row in reader:
            for field, value in row.items():
                compression[field].append(float(value))

    figure = Figure()
    plot = figure.add_subplot()
    plot.set_title('Babble (DSRs) vs DreamCoder')
    plot.set_xlabel('percent improvement')

    axis_width = plot.spines['left'].get_linewidth()
    plot.axvline(0, linewidth=axis_width, color='black', dashes=(8, 12))

    # plot.set_yticks(
    #     range(1, len(compression.keys()) + 1),
    #     labels=compression.keys()
    # )

    plot.hist(compression['all dsrs/dc'], bins=20)
    #plot.violinplot(compression.values(), vert=False, showmedians=True)
    figure.tight_layout()

    figure.savefig('harness/plots/compression.svg')


main()
