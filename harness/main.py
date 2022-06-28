#!/usr/bin/python3
from rich.console import Console
from rich.panel import Panel
import inspect

c = Console()


def print(s=""):
    c.print(s)


def confirm(s):
    c.input(f"{s} [dim]⏎ to continue[/] ")


def main():
    print(
        Panel.fit(
            (
                "this script is responsible for running all of the scripts and\n"
                "benches required for the babble paper. for right now, this\n"
                "script just prompts you to run all of the commands yourself,\n"
                "but this will be progressively automated."
            ),
            title="[green bold]babble/harness/main.py[/]",
        )
    )
    print()

    print("[magenta bold]generating data[/]")
    print("> parsing dreamcoder data")
    confirm("[bold]; python3 harness/scripts/parse_dc.py harness/data/dreamcoder-benchmarks")
    print("[dim]> written to harness/data_gen/dc_res.csv")
    print()

    print("[magenta bold]generating graphs[/]")
    print("> generating param sweep cactus plot")
    confirm("[bold]; python3 harness/scripts/param_sweep.py data/cogsci/nuts-bolts.bab")
    print("[dim]> written graph to harness/plots/cactus-time-compression.pdf")


if __name__ == "__main__":
    main()
