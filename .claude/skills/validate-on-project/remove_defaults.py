#!/usr/bin/env python3
"""Removes the default values find-unused reports as unused, e.g. `com.example.Foo.bar(baz = ...)`.

Usage: python3 remove_defaults.py findings.tsv [--apply]

`findings.tsv` is `table2tsv.py` output from a run without `--transitive` (in transitive mode, a default that only unused
code relies on is reported too, and removing it breaks that code). Rows that aren't default values are ignored. Each
location is the parameter's name; the default is everything after the first `=` at bracket depth 0, up to the next `,`
or closing bracket, not counting a trailing comment. Without `--apply` it only prints what it would remove; review that
before applying. Every edit is computed before any file is written, and a default it can't parse is reported and left
alone.
"""
import collections
import os
import re
import sys

CHAR_LITERAL = re.compile(r"'(?:\\u[0-9a-fA-F]{4}|\\.|[^\\'\n])'")


class ParseError(Exception):
    pass


def skip_splice(s, i):
    """The index after the `}` that closes the `${` of an interpolated string, whose `{` is at `i`"""
    depth, j = 0, i
    while j < len(s):
        k = skip_literal_or_comment(s, j)
        if k != j:
            j = k
            continue
        if s[j] == "{":
            depth += 1
        elif s[j] == "}":
            depth -= 1
            if depth == 0:
                return j + 1
        j += 1
    raise ParseError("unterminated string")


def skip_literal_or_comment(s, i):
    """The index after the string, char literal or comment starting at `i`, or `i` if there's none there"""
    # `s"..."`, `f"..."` and the like can contain `${...}`, which can contain strings of its own
    interpolated = i > 0 and (s[i - 1].isalnum() or s[i - 1] == "_")
    if s.startswith('"""', i):
        j = i + 3
        while not s.startswith('"""', j):
            if j >= len(s):
                raise ParseError("unterminated string")
            j = skip_splice(s, j + 1) if interpolated and s.startswith("${", j) else j + 1
        # A triple-quoted string can end with more quotes, which belong to it
        while s.startswith('""""', j):
            j += 1
        return j + 3
    if s[i] == '"':
        j = i + 1
        while j < len(s) and s[j] != '"':
            if s[j] == "\n":
                raise ParseError("unterminated string")
            if interpolated and s.startswith("${", j):
                j = skip_splice(s, j + 1)
            else:
                j += 2 if s[j] == "\\" else 1
        if j >= len(s):
            raise ParseError("unterminated string")
        return j + 1
    if s[i] == "'":
        m = CHAR_LITERAL.match(s, i)
        return m.end() if m else i
    if s.startswith("//", i):
        end = s.find("\n", i)
        return len(s) if end < 0 else end
    if s.startswith("/*", i):
        depth, j = 1, i + 2
        while depth > 0:
            if j >= len(s):
                raise ParseError("unterminated comment")
            if s.startswith("/*", j):
                depth, j = depth + 1, j + 2
            elif s.startswith("*/", j):
                depth, j = depth - 1, j + 2
            else:
                j += 1
        return j
    return i


def find_default_equals(s, i):
    """The `=` that starts the default value of the parameter whose type starts at `i`"""
    depth = 0
    while i < len(s):
        j = skip_literal_or_comment(s, i)
        if j != i:
            i = j
            continue
        ch = s[i]
        if ch in "([{":
            depth += 1
        elif ch in ")]}":
            if depth == 0:
                raise ParseError("no default value found")
            depth -= 1
        elif ch == "," and depth == 0:
            raise ParseError("no default value found")
        elif ch == "=" and depth == 0 and s[i - 1] not in "=!<>:" and (i + 1 >= len(s) or s[i + 1] not in "=>:"):
            return i
        i += 1
    raise ParseError("no default value found")


def default_end(s, i):
    """The end of the last code (not whitespace or a comment) of the default value starting at `i`"""
    depth, last_code = 0, i
    while i < len(s):
        j = skip_literal_or_comment(s, i)
        if j != i:
            if not (s.startswith("//", i) or s.startswith("/*", i)):
                last_code = j
            i = j
            continue
        ch = s[i]
        if ch in "([{":
            depth += 1
        elif ch in ")]}":
            if depth == 0:
                return last_code
            depth -= 1
        elif ch == "," and depth == 0:
            return last_code
        if not ch.isspace():
            last_code = i + 1
        i += 1
    raise ParseError("no end of the default value found")


def removal(s, start, param):
    """The span to delete for the default of `param`, whose name starts at `start`"""
    name = f"`{param}`" if s.startswith("`", start) else param
    if not s.startswith(name, start):
        raise ParseError(f"expected `{param}` at the reported position, found {s[start:start + 30]!r}")
    eq = find_default_equals(s, start + len(name))
    b = eq
    while s[b - 1] in " \t":
        b -= 1
    return b, default_end(s, eq + 1)


def main():
    by_file = collections.defaultdict(list)
    for line in open(sys.argv[1], encoding="utf-8"):
        name, _, loc = line.rstrip("\n").partition("\t")
        if not name.endswith(" = ...)") or loc.count(":") < 2:
            continue
        path, l, c = loc.rsplit(":", 2)
        by_file[path].append((int(l), int(c), name))

    edits, failures = {}, 0
    for path, items in by_file.items():
        s = open(path, encoding="utf-8", newline="").read()
        offsets = [0]
        for ln in s.split("\n"):
            offsets.append(offsets[-1] + len(ln) + 1)
        spans = []
        for l, c, name in sorted(items):
            param = name.rsplit("(", 1)[1].split(" = ")[0]
            try:
                b, e = removal(s, offsets[l - 1] + c - 1, param)
            except ParseError as err:
                print(f"{os.path.relpath(path)}:{l}: {name}: SKIPPED, {err}")
                failures += 1
                continue
            print(f"{os.path.relpath(path)}:{l}: {name}: removing {s[b:e]!r}")
            spans.append((b, e))
        # Last first, so earlier offsets stay valid
        for b, e in sorted(spans, reverse=True):
            s = s[:b] + s[e:]
        edits[path] = s

    if "--apply" in sys.argv:
        for path, s in edits.items():
            open(path, "w", encoding="utf-8", newline="").write(s)
    if failures:
        print(f"{failures} default(s) skipped")


if __name__ == "__main__":
    main()
