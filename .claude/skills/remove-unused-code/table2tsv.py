# Converts find-unused's rendered output table(s) into "name<TAB>location" lines
import sys
for line in open(sys.argv[1], encoding="utf-8"):
    if line.startswith("│"):
        cells = [c.strip() for c in line.strip().strip("│").split("│")]
        if len(cells) == 2 and cells[0] != "Instance":
            print(f"{cells[0]}\t{cells[1]}")
