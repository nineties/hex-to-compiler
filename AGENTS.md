# Agent rules

## Rules and what enforces them

Fix a repeated mistake at the highest level that works: the structure of the code first, then types, then a lint, then a regression test. A rule written only here is the weakest, so add one here only for a judgment call. When a correction repeats a rule that nothing enforces, enforce it in the same change.

| Rule | Enforced by |
|---|---|
| Do not change `bootstrap/planck0.sv` or its documentation page without koichi's permission | review of the pull request |
| Do not add a comment line. Name the code so it needs none. Keep only copyright and license lines | `tools/lint.sh` (CI job `lint`) |
| Do not add a note that silences a lint (`shellcheck disable`, `noqa`, and so on). Fix the code | `tools/lint.sh` |
| No trailing whitespace and no tab in added lines (a Makefile may have tabs) | `tools/lint.sh` |
| Shell scripts have no shellcheck error, and scripts parse | `tools/lint.sh` |
| Run the whole tests in GitHub Actions only | `runtests` in `.github/workflows/runtests.yaml` |

`tools/lint.sh` checks the lines a change adds, not the lines already in the repository, so it passes today and stops new mistakes. Run `sh tools/lint.sh` before pushing.
