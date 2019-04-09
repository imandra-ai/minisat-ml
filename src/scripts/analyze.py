#!/usr/bin/env python3

import sys, csv, argparse, os

def read_csv(f):
    with open(f) as fd:
        content = fd.readlines()
    return list(csv.DictReader(content)) 

def analyze_file(f, potential_errors=False, plot=None):
    """Analyze result file {f} (which should be a .csv file).
    
    Print per-solver analysis, and errors which happen quickly (true errors, not timeouts).
    If `plot` is provided, call `mkplot.py` to produce a nice plot of the results.
    """
    print(f"## analyze `{f}`")
    table = read_csv(f)
    print(f"read {len(table)} records")
    if not table: return
    provers = [x for x in table[0].keys() if ".time" not in x and x != "problem"]
    print(f"provers: {provers}")
    sat = {}
    unsat = {}
    unknown = {}
    error = {}
    total_time = {}
    if potential_errors:
        quick_errors = []
    for row in table:
        for prover in provers:
            res = row[prover]
            time = float(row[prover + '.time'])
            if res == 'unsat':
                unsat[prover] = 1 + unsat.get(prover, 0)
                total_time[prover] = time + total_time.get(prover,0)
            elif res == 'sat':
                sat[prover] = 1 + sat.get(prover, 0)
                total_time[prover] = time + total_time.get(prover,0)
            elif res == 'unknown':
                unknown[prover] = 1 + unknown.get(prover, 0)
            elif res == 'error':
                error[prover] = 1 + error.get(prover, 0)
                time = float(row[prover + '.time'])
                if potential_errors and time < 5:
                    quick_errors.append((prover, row['problem'], time))
            else:
                print(f"unknown result for {prover} on {row}: {res}")
    for prover in provers:
        n = sat.get(prover,0)+unsat.get(prover,0)
        print(f"{prover:{12}}: sat {sat.get(prover,0):6}" \
            f" | unsat {unsat.get(prover,0):6}" \
            f" | solved {n:6}" \
            f" | unknown {unknown.get(prover,0):6}" \
            f" | error {error.get(prover,0):6}" \
            f" | total-time {total_time.get(prover,0):{12.5}}s" \
            f" | avg-time {0 if n==0 else total_time.get(prover,0)/n:{6.3}}s")

    if potential_errors:
        for (prover,filename,time) in quick_errors:
            print(f"potential error: {prover} on `{filename}` after {time}")

    if plot:
        print(f"plotting into {plot}…")
        import tempfile, json, subprocess
        with tempfile.TemporaryDirectory(prefix='analyze') as tmpdir:
            json_files = []
            # produce json files
            for prover in provers:
                filename = os.path.join(tmpdir, prover + '.json')
                json_files.append(filename)
                stats = {}
                for row in table:
                    res = row[prover]
                    ok = (res == 'unsat' or res=='sat')
                    time = float(row[prover + '.time'])
                    stats[row['problem']] = {'status': ok, 'rtime': time}
                j = { 'preamble': {'program': prover}, 'stats': stats, }
                with open(filename, 'w') as out:
                    #print(json.dumps(j, indent=2))
                    out.write(json.dumps(j, indent=2))
            subprocess.call(["mkplot.py", "--save-to="+plot, "-b", "png"] + json_files)


def main(files, potential_errors=False, plot=False) -> ():
    for f in files:
        analyze_file(f, potential_errors=potential_errors, plot=plot)

if __name__ == "__main__":
    p = argparse.ArgumentParser('analyze result files')
    p.add_argument('files', nargs='+', help='files to analyze')
    p.add_argument('--errors', dest='potential_errors', \
            action='store_true', help='detect potential errors')
    p.add_argument('--plot', dest='plot', help='produce a plot')
    args = p.parse_args()
    main(files=args.files, potential_errors=args.potential_errors, plot=args.plot)
