#!/usr/bin/env python3

from matplotlib.figure import Figure
import csv

def main():
    x = []
    y = []
    with open('data/dsr-impact.csv', newline='') as file:
        reader = csv.DictReader(file)
        for row in reader:
            x.append(row['benchmark'])
            y.append(float(row['percent improvement']))

    figure = Figure()
    plot = figure.add_subplot()
    plot.set_title('Impact of Domain-Specific Rewrites')
    plot.set_xlabel('benchmark')
    plot.set_ylabel('percent improvement')

    plot.bar(x, y)

    # Draw the y=0 zero axis
    axis_width = plot.spines['bottom'].get_linewidth()
    plot.axhline(linewidth=axis_width, color='black')

    # Only label every 5th benchmark
    for i, label in enumerate(plot.get_xticklabels()):
        if i % 5 != 0:
            label.set_visible(False)

    figure.savefig('dsr-impact.svg')


main()
