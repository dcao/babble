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

    template = "{:30} & {:15} & {:15} & {:15} & {:10} & {:10} \\\\"
    print(template.format("\\bf Benchmark", "\\bf Time (s)", "\\bf \# Libs", "\\bf Compression", "in", "out"))
    for conf, res in zip(row_configs, results):
        row = "{:30} & {:15.2f} & {:15} & {:15.2f} & {:10} & {:10} \\\\"
        print(row.format(
              conf["name"], res["time"], res["num_libs"], res["compression"], res["initial_cost"], res["final_cost"]))

    made_legend = False
    def bar3(a, b, c, **kwargs):
        nonlocal made_legend
        assert a >= b >= c
        if not made_legend:
            kwargs['label'] = 'Improvement without eqs'
        plt.bar(height = a - b, bottom = b, width = 1,
                color="orange", edgecolor="black", **kwargs)
        if not made_legend:
            kwargs['label'] = 'Additional improvement with eqs'
        plt.bar(height = b - c, bottom = c, width = 1,
                color="none", edgecolor="black", hatch="///", **kwargs)
        made_legend = True

    ticks = []
    labels = []
    for i in range(0, len(results), 4):
        ticks.append(i + 1)
        labels.append(row_configs[i]['name'].replace('\\', ''))

        initial = results[i]['initial_cost']
        final_no_dsr = results[i + 1]['final_cost']
        final_dsr = results[i]['final_cost']

        bump = 0.3
        bar3(initial, final_no_dsr, final_dsr, x = i + bump)

        # cleaned
        initial = results[i + 2]['initial_cost']
        final_dsr = results[i + 2]['final_cost']
        final_no_dsr = results[i + 3]['final_cost']

        bar3(initial, final_no_dsr, final_dsr, x = i + 2 - bump)

    plt.xticks(ticks, labels)
        
if __name__ == "__main__":
    make_table([
        dict(
            name="Nuts \& Bolts",
            input="harness/data/cogsci/nuts-bolts.bab",
            dsrs="harness/data/benchmark-dsrs/drawings.nuts-bolts.rewrites"
        ),
        dict(
            name="Nuts \& Bolts clean",
            input="harness/data/cogsci/nuts-bolts-cleaned.bab",
            dsrs="harness/data/benchmark-dsrs/drawings.nuts-bolts.rewrites"
        ),
        dict(
            name="Vehicles",
            input="harness/data/cogsci/wheels.bab",
            dsrs="harness/data/benchmark-dsrs/drawings.wheels.rewrites"
        ),
        dict(
            name="Vehicles clean",
            input="harness/data/cogsci/wheels-cleaned.bab",
            dsrs="harness/data/benchmark-dsrs/drawings.wheels.rewrites"
        ),
        dict(
            name="Gadgets",
            input="harness/data/cogsci/dials.bab",
            dsrs="harness/data/benchmark-dsrs/drawings.dials.rewrites"
        ),
        dict(
            name="Gadgets clean",
            input="harness/data/cogsci/dials-cleaned.bab",
            dsrs="harness/data/benchmark-dsrs/drawings.dials.rewrites"
        ),
        dict(
            name="Furniture",
            input="harness/data/cogsci/furniture.bab",
            dsrs="harness/data/benchmark-dsrs/drawings.furniture.rewrites"
        ),
        dict(
            name="Furniture clean",
            input="harness/data/cogsci/furniture-cleaned.bab",
            dsrs="harness/data/benchmark-dsrs/drawings.furniture.rewrites"
        ),
    ])

    plt.legend()
    plt.yscale('log')
    plt.ylabel('AST size')
    yticks = [1000, 3000, 10000, 30000, 100000]
    plt.yticks(yticks, yticks)
    plt.savefig('out.png', dpi=300)