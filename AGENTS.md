# Agent rules

## Rules and what enforces them

Fix a repeated mistake at the highest level that works: the structure of the code first, then types, then a lint, then a regression test. A rule written only here is the weakest, so add one here only for a judgment call. When a correction repeats a rule that nothing enforces, enforce it in the same change.

| Rule | Enforced by |
|---|---|
| Do not add a comment line to a .pk file. Name the code so it needs none. Keep only copyright and license lines | review of the pull request |
| Run the whole tests in GitHub Actions only | `runtests` in `.github/workflows/runtests.yaml` |
