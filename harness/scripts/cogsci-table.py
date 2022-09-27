#!/usr/bin/env python3

from pathlib import Path
from multiprocessing.dummy import Pool
from plot import run_and_cache, read_list

from matplotlib import pyplot as plt

def get_cogsci_row(
    *,
    name,
    input,
    beam_size=400, 
    lps=1,
    rounds=20,
    max_arity=2,
    dsrs=None,
    **kwargs,
):
    args = f"{input} --beams={beam_size} --lps={lps} --rounds={rounds} --max-arity={max_arity}"
    dsr_inputs = []
    if dsrs:
        args += f" --dsr={dsrs}"
        dsr_inputs.append(dsrs)
    fname = args.replace("--", "").replace(" ", "__") \
                .replace("-", "").replace("/", "_").replace(".", "_") + ".csv"

    outdir = Path("harness/data_gen/cache")
    outdir.mkdir(parents=True, exist_ok=True)
    fname = outdir / fname
    cmd = f"cargo run --release --bin=drawings -- {args} --output={fname}"

    run_and_cache(cmd, fname, *dsr_inputs)
    data = read_list(fname)
    initial_cost, final_cost, compression, num_libs, time = data[-1][-5:]

    # sanity check that compression =~= initial_cost / final_cost
    assert abs(compression - initial_cost / final_cost) < 0.1

    return dict(
        initial_cost=int(initial_cost), 
        final_cost=int(final_cost), 
        compression=compression, 
        num_libs=int(num_libs),
        time=time,
    )

def make_table(row_configs):
    # automatically add the "no dsrs version"
    all_row_configs = []
    for rc in row_configs:
        all_row_configs.append(rc)
        if rc.get('dsrs'):
            rc = rc.copy()
            rc.pop('dsrs')
            rc['name'] += " (no eqs)"
            all_row_configs.append(rc)
    row_configs = all_row_configs

    with Pool() as p:
        results = p.map(lambda d: get_cogsci_row(**d), row_configs)

    # template = "{:30} & {:10} & {:10} & {:15} & {:15} \\\\"
    # print(template.format(
    #     "\\bf Benchmark", 
    #     "\\bf Input Size", 
    #     "\\bf Output Size",
    #     "\\bf Compression", 
    #     "\\bf Time (s)", 
    # ))
    # for conf, res in zip(row_configs, results):
    #     row = "{:30} & {:10} & {:10} & {:10.2f} & {:10.2f} \\\\"
    #     print(row.format(
    #         conf["name"], 
    #         res["initial_cost"], 
    #         res["final_cost"],
    #         res["compression"], 
    #         res["time"], 
    #     ))

    for i in range(0, len(results), 2):

        with_dsrs = results[i]
        without_dsrs = results[i + 1]

        print("{:15} & {:10} & {:10} & {:10.2f} & {:10.2f} & {:10} & {:10.2f} & {:10.2f}".format(
            row_configs[i]['name'],
            without_dsrs['initial_cost'],
            without_dsrs['final_cost'],
            without_dsrs['compression'],
            without_dsrs['time'],
            with_dsrs['final_cost'],
            with_dsrs['compression'],
            with_dsrs['time'],
        ))

        xs = [with_dsrs['compression'], without_dsrs['compression']]
        ys = [with_dsrs['time'], without_dsrs['time']]
        name = row_configs[i]['name'].replace('\\', '')
        marker = row_configs[i]['marker']
        color = row_configs[i]['color']
        plt.plot(xs, ys, label=name, marker=marker, markerfacecolor='white', color=color)
        plt.plot(xs[0], ys[0], marker=marker, color=color)
        # x, dx = xs[0], xs[1] - xs[0]
        # y, dy = ys[0], ys[1] - ys[0]
        # # plt.arrow(x, y, dx, dy, label=name)
        # plt.annotate("", 
        # xy=(xs[0], ys[0]),
        # xytext=(xs[1], ys[1]),
        # arrowprops=dict(arrowstyle="->")
        # )

    plt.xlim((0, None))
    plt.legend()
    plt.xlabel('Compression Ratio')
    plt.ylabel('Time (seconds)')
    plt.savefig(f"harness/plots/cogsci-scatter.png")
    plt.savefig(f"harness/plots/cogsci-scatter.pdf")
    print(f"Wrote harness/plots/cogsci-scatter.png")
    print(f"Wrote harness/plots/cogsci-scatter.pdf")
        
if __name__ == "__main__":
    make_table([
        dict(
            name="Nuts \& Bolts",
            input="harness/data/cogsci/nuts-bolts.bab",
            dsrs="harness/data/benchmark-dsrs/drawings.nuts-bolts.rewrites",
            marker='o',
            color='red',
        ),
        dict(
            name="Vehicles",
            input="harness/data/cogsci/wheels.bab",
            dsrs="harness/data/benchmark-dsrs/drawings.wheels.rewrites",
            marker='s',
            color='darkviolet',
        ),
        dict(
            name="Gadgets",
            input="harness/data/cogsci/dials.bab",
            dsrs="harness/data/benchmark-dsrs/drawings.dials.rewrites",
            marker='^',
            color='blue',
        ),
        dict(
            name="Furniture",
            input="harness/data/cogsci/furniture.bab",
            dsrs="harness/data/benchmark-dsrs/drawings.furniture.rewrites",
            marker='v',
            color='green',
        ),
        dict(
            name="Nuts \& Bolts (clean)",
            input="harness/data/cogsci/nuts-bolts-cleaned.bab",
            dsrs="harness/data/benchmark-dsrs/drawings.nuts-bolts.rewrites",
            marker='o',
            color='red',
        ),
    ])
