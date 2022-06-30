import csv
import random
import subprocess
import sys
import time
import numpy as np
from matplotlib import pyplot as plt

# This script designs the generalization
# experiment from the Stitch paper (in Section 5.2.3).
# It takes as input a bab file and split it into two parts.
# First part (test data) is 50 randomly sampled programs from the 250.
# Second part (training data) has n programs where
# n = 20%, 40%, 60%, 80%, 100% of the remaining 200 programs in the file.
# We run babble using the generalization feature of the experiment infrastructure
# and record the compression of the test data.
# Each treatment is repeated 50 times, i.e., the entire description
# above is repeated 50 times for each value of n.


def get_compression(outfile):
    with open(outfile, 'r') as file:
        for l in file.readlines():
            if "compression ratio" in l:
                return float(l.split()[-1].split(")")[0])


def plot(res_name):
    xs = []
    yss = []
    with open(res_name, 'r') as res:
        rdr = csv.reader(res)
        for r in rdr:
            xs.append(float(r[0]))
            yss.append(r[1:])

    ys_mean = [sum([float(y) for y in ys], 0) / len(ys) for ys in yss]
    ys_std = [np.std([float(y) for y in ys]) for ys in yss]

    print(ys_mean)
    print(ys_std)

    fig, ax = plt.subplots(1)
    ax.plot(xs, ys_mean, color='blue')
    ax.fill_between(xs, [y_mean - y_std for y_mean, y_std in zip(ys_mean, ys_std)],
                    [y_mean + y_std for y_mean, y_std in zip(ys_mean, ys_std)], alpha=0.2)
    fnm = res_name.split(".")[0]
    plt.savefig(fnm + ".pdf")


def generalize(bab_file):
    #train_size = [40, 80, 120, 160, 200]
    train_size = [40, 80, 120, 160, 200]
    num_replication = 5
    test_size = 50

    all_progs = []

    # all all programs to a list
    with open(bab_file, 'r') as bab:
        for l in bab.readlines():
            all_progs.append(l)

    # # remove the test (held out) data
    # test_data = random.sample(all_progs, test_size)
    # assert(len(test_data) == test_size)
    # with open("test.bab", 'w') as te:
    #     for td in test_data:
    #         te.write(td)

    # # get the rest of the  200 training data
    # all_train = list(set(all_progs) - set(test_data))
    # assert(len(all_train) == 200)

    curr_time = time.strftime("%Y%m%d-%H%M%S")
    res_name = "generalization_result" + curr_time + ".csv"
    with open(res_name, 'w') as result:
        for t in train_size:
            compressions = []
            for nr in range(num_replication):

                # remove the test (held out) data
                test_data = random.sample(all_progs, test_size)
                with open("test.bab", 'w') as te:
                    for td in test_data:
                        te.write(td)

                # get the rest of the  200 training data
                all_train = list(set(all_progs) - set(test_data))
                assert(len(all_train) == 200)

                train_data = random.sample(all_train, t)
                with open("train.bab", 'w') as tr:
                    for td in train_data:
                        tr.write(td)
                try:
                    output = open("stdout.txt", 'w')
                    subprocess.Popen(["timeout", "-v", "30m", "cargo", "run", "--release", "--bin=drawings", "--",
                                      "train.bab", "test.bab", "--beams", "2000", "--lps", "1", "--rounds", "10", "--max-arity", "3"],
                                     stderr=subprocess.PIPE, stdout=output).communicate()
                    output.close()
                    compression = get_compression("stdout.txt")
                    compressions.append(compression)
                except:
                    print(
                        "Experiment failed for training size {0} and replication {1}".format(t, nr))
                    compressions.append(-1.0)
            cols = [t] + compressions
            row = ','.join([str(c) for c in cols])
            print(row)
            result.write(row + '\n')
    plot(res_name)


USAGE = """Please provide a .bab file.
"""

if __name__ == "__main__":
    if(len(sys.argv) != 2):
        print(USAGE)
        exit()
    else:
        fnm = sys.argv[1]
        if (fnm.split(".")[1] != "bab"):
            print(USAGE)
            exit()
        else:
            generalize(sys.argv[1])
            #plot("generalization_result20220629-210728.csv")
