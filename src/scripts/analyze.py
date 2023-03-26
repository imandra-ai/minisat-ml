#!/usr/bin/env python3

import sys, csv, argparse, os, typing

def read_csv(f: str) -> typing.List[dict]:
    with open(f) as fd:
        content = fd.readlines()
    return list(csv.DictReader(content)) 

def analyze_file(f:str, potential_errors=False, plot=None, mfleury=False):
    """Analyze result file {f} (which should be a .csv file).
    
    Print per-solver analysis, and errors which happen quickly (true errors, not timeouts).
    If `plot` is provided, call `mkplot.py` to produce a nice plot of the results.
    If `mfleury` is true, use the alternative format provided by Mathias Fleury
    """
    print(f"## analyze `{f}`")
    table = read_csv(f)
    print(f"read {len(table)} records")
    if not table: return
    if mfleury:
        time_suffix = '_overall_time'
        res_suffix = '_result'
        provers = [x.split('_result')[0] for x in table[0].keys() if "_result" in x]
    else:
        time_suffix = '.time'
        res_suffix = ''
        provers = [x for x in table[0].keys() if ".time" not in x and x != "problem" and x != "status"]
    print(f"provers: {provers}")
    sat: typing.Dict = {}
    unsat: typing.Dict = {}
    unknown: typing.Dict = {}
    error: typing.Dict = {}
    total_time: typing.Dict = {}
    if potential_errors:
        quick_errors = []
    for row in table:
        for prover in provers:
            res = row[prover + res_suffix]
            time = row[prover + time_suffix]
            time = float(time) if time != 'NULL' else 0
            if res == 'unsat':
                unsat[prover] = 1 + unsat.get(prover, 0)
                total_time[prover] = time + total_time.get(prover,0)
            elif res == 'sat':
                sat[prover] = 1 + sat.get(prover, 0)
                total_time[prover] = time + total_time.get(prover,0)
            elif res == 'unknown' or res == 'timeout' or res == 'NULL':
                unknown[prover] = 1 + unknown.get(prover, 0)
            elif res == 'error':
                error[prover] = 1 + error.get(prover, 0)
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
            f" | total-time {total_time.get(prover,0):14.3f}s" \
            f" | avg-time {0 if n==0 else total_time.get(prover,0)/n:8.3f}s")

    if potential_errors:
        for (prover,filename,time) in quick_errors:
            print(f"potential error: {prover} on `{filename}` after {time}")

    if plot:
        print(f"plotting into {plot}…")
        import tempfile, json, subprocess
        with tempfile.TemporaryDirectory(prefix='analyze') as tmpdir:
            json_files: typing.List[str] = []
            # produce json files
            for prover in provers:
                filename = os.path.join(tmpdir, prover + '.json')
                json_files.append(filename)
                stats = {}
                for row in table:
                    res = row[prover + res_suffix]
                    ok = (res == 'unsat' or res=='sat')
                    time = row[prover + time_suffix]
                    time = float(time) if time != 'NULL' else 0
                    stats[row['problem']] = {'status': ok, 'rtime': time}
                j = { 'preamble': {'program': prover}, 'stats': stats, }
                with open(filename, 'w') as out:
                    #print(json.dumps(j, indent=2))
                    out.write(json.dumps(j, indent=2))
            subprocess.call(["mkplot.py", "--save-to="+plot, "-b", "png"] + json_files)


def main(files, **kwargs) -> None:
    for f in files:
        analyze_file(f, **kwargs)

if __name__ == "__main__":
    p = argparse.ArgumentParser('analyze result files')
    p.add_argument('files', nargs='+', help='files to analyze')
    p.add_argument('--errors', dest='potential_errors', \
            action='store_true', help='detect potential errors')
    p.add_argument('--plot', dest='plot', help='produce a plot')
    p.add_argument('--mfleury', dest='mfleury', action='store_true',
            help="use mathias fleury's input format")
    args = p.parse_args()
    main(files=args.files, potential_errors=args.potential_errors,
            plot=args.plot, mfleury=args.mfleury)
