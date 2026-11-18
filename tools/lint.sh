#!/bin/sh
set -u
cd "$(git rev-parse --show-toplevel)"

base=${LINT_BASE:-}
if [ -z "$base" ]; then
    git fetch --no-tags -q origin main:refs/remotes/origin/main 2>/dev/null
    base=$(git merge-base refs/remotes/origin/main HEAD 2>/dev/null)
fi
if [ -z "$base" ] || echo "$base" | grep -q '^0*$'; then
    echo "lint: cannot find the base of the change" >&2
    exit 1
fi

status=0
fail() {
    echo "lint: $1" >&2
    status=1
}

added=$(mktemp)
trap 'rm -f "$added"' EXIT
git diff -U0 --no-color "$base" HEAD -- '*.pk' '*.sv' '*.fs' '*.lisp' '*.sh' '*.js' '*.yaml' '*.yml' 'Makefile' |
    awk '
        /^\+\+\+ b\// { file = substr($0, 7); next }
        /^\+\+\+ / { file = ""; next }
        /^@@/ { split($3, a, ","); line = substr(a[1], 2) + 0; next }
        /^\+/ && file != "" { print file "\t" line "\t" substr($0, 2); line++ }
    ' >"$added"

suppress='(shellcheck[ ]disable|eslint-[d]isable|no[q]a|no[l]int|NO[L]INT|lint:[i]gnore|prettier-[i]gnore|@ts-[i]gnore|@ts-expect-[e]rror|pylint: *[d]isable|clang-format[ ]off|NO[S]ONAR)'
if grep -E "$(printf '\t')[0-9]+$(printf '\t')" "$added" | grep -E "$suppress" >&2; then
    fail "a note that silences a lint was added. Fix the code the lint points at."
fi

comment='[[:space:]]*(#|//|;|/\*)'
if awk -F '\t' '$3 ~ /^#!/ { next } { print }' "$added" |
    grep -E "^[^$(printf '\t')]*$(printf '\t')[0-9]+$(printf '\t')$comment" |
    grep -v -i -E 'copyright|license|SPDX' >&2; then
    fail "a comment line was added. Name the code so that it needs no comment, or encode the rule in a check."
fi

if grep -E "$(printf '\t')[0-9]+$(printf '\t').*[[:space:]]$" "$added" >&2; then
    fail "an added line ends with a space or a tab."
fi

if grep -E "$(printf '\t')[0-9]+$(printf '\t').*$(printf '\t')" "$added" | grep -v '^Makefile' >&2; then
    fail "an added line has a tab. Use spaces."
fi

for f in $(git ls-files '*.sh'); do
    sh -n "$f" || fail "$f does not parse"
done
if command -v shellcheck >/dev/null; then
    git ls-files '*.sh' | xargs shellcheck --severity=error || fail "shellcheck found an error"
else
    echo "lint: shellcheck is not installed, skipped" >&2
fi
if command -v node >/dev/null; then
    for f in $(git ls-files '*.js'); do
        node --check "$f" || fail "$f does not parse"
    done
else
    echo "lint: node is not installed, skipped" >&2
fi

[ "$status" = 0 ] && echo "lint: ok"
exit "$status"
