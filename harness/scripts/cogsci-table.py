#!/usr/bin/env python3

from pathlib import Path
from multiprocessing.dummy import Pool
from plot import run_and_cache, read_list

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
        initial_cost=initial_cost, 
        final_cost=final_cost, 
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

    template = "{:30} & {:8} & {:8} & {:11} \\\\"
    print(template.format("Benchmark", "Time", "\# Libs", "Compression"))
    for conf, res in zip(row_configs, results):
        row = "{:30} & {:8.2f} & {:8} & {:11.2f} \\\\"
        print(row.format(
              conf["name"], res["time"], res["num_libs"], res["compression"]))

if __name__ == "__main__":
    make_table([
        dict(
            name="Nuts and Bolts",
            input="harness/data/cogsci/nuts-bolts.bab",
            dsrs="harness/data/benchmark-dsrs/drawings.nuts.rewrites"
        ),
        dict(
            name="Wheels",
            input="harness/data/cogsci/wheels.bab",
            dsrs="harness/data/benchmark-dsrs/drawings.wheels.rewrites"
        ),
        dict(
            name="Dials",
            input="harness/data/cogsci/dials.bab",
            dsrs="harness/data/benchmark-dsrs/drawings.dials.rewrites"
        ),
        dict(
            name="Furniture",
            input="harness/data/cogsci/furniture.bab",
            dsrs="harness/data/benchmark-dsrs/drawings.furniture.rewrites"
        ),
    ])