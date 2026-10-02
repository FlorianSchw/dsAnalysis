# Project: dsAnalysis

## What this package is

An R package that creates DataSHIELD **analysis project** environments
(`initProject()`): a project folder with a login script, a DSLite test
setup with mock data, a `main.R`, `dependencies.R` and a `config.yml`
with a `production` and a `testing` profile. Helper functions:
`add_dsPackage()`, `initMockData()`, `update_MockData()`,
`find_script()`. Templates live in `inst/templates/` (`datashield/`,
`dslite/`, `utils/`).

It is also an **analyst-side helper package** in DataSHIELD terms — not
a client, not a server package — and the starting point for the
reusable workflow `datashield-analysis-suggest.yml` in
`FlorianSchw/package-workflows` (local clone:
`../package-workflows`), which drafts analysis scripts from a project's
`config/analysis-plan.yml`.

## How to work here

- Don't commit or push unless asked; leave changes in the working tree
  for review.
- The only branch is `main`.
- When something depends on `package-workflows`, read the referenced file
  there instead of guessing; its `CLAUDE.md` and `dev-notes/` hold the
  design.

## To do

Agreed in the package-workflows sessions (2026-09-30 to 2026-10-02).
Cross items out when done instead of deleting them.

### A. Bot suggestions for this package (roxygen and tests)

1. ~~**Caller files** `.github/workflows/roxygen-suggest.yml` and
   `.github/workflows/test-suggest.yml`~~ (done 2026-10-02), starting from
   `../package-workflows/examples/roxygen-suggest.yml` and
   `../package-workflows/examples/test-suggest.yml`. Adjust:
   - `pull_request: branches: [main]` (there is no `dev`), with
     `paths: ['R/**']`;
   - `with: datashield: true`, `datashield-type: utility`,
     `sweep-base: main` (a scheduled sweep otherwise looks for `dev`);
   - permissions `id-token: write`, `contents: write`,
     `pull-requests: write`, `issues: write`;
   - the `keepalive` job from the examples (scheduled workflow);
   - secrets `ANTHROPIC_ORG_ID`, `ANTHROPIC_SVAC_ID` and
     `ANTHROPIC_FDRL_ROXYGEN` / `ANTHROPIC_FDRL_TESTS`, plus optional
     `APP_CLIENT_ID` / `APP_PRIVATE_KEY`.
   Optional inputs (comma-separated strings): `accept-reasons`,
   `code-issue-confidence` (`high`, `high,medium`, … or `none`),
   `dslite-setup` (tests: `auto`, `create`, `never`). The inputs are
   documented in `../package-workflows/.github/workflows/*.yml`.
2. ~~**DESCRIPTION:**~~ (done 2026-10-02) `initMockData()` and `update_MockData()` use
   DataSHIELD connections, so their tests run against DSLite. Add
   `DSLite` and `dsBase` to `Suggests`, and `datashield/dsBase` to
   `Remotes` (not on CRAN).
3. ~~**Outside the repo (the user):** in the Anthropic Console a service
   account for dsAnalysis and one federation rule per workflow (claim
   `job_workflow_ref` naming the workflow file); the repository secrets;
   the GitHub App or "Allow GitHub Actions to create and approve pull
   requests"~~ (done). See `../package-workflows/docs/claude-setup.qmd` and
   `docs/getting-started.qmd`.

Test setup: nothing to change. `tests/testthat/setup.R` reads
`config-testing.yml` and doesn't connect, so the test workflow
(`dslite-setup: auto`) creates its own `setup-dslite.R` next to it.

### B. The analysis template (`initProject()`)

Checklist from `../package-workflows/dev-notes/analysis-suggest.md`,
section "What dsAnalysis needs":

4. ~~Remove the placeholders~~ (done 2026-10-02) `inst/templates/datashield/02_QualityCheck.R`,
   `03_DescriptiveStatistics.R` and `99_DSLiteLearning.R` (and their
   copying in `initProject()`).
5. ~~Add a plan template~~ (done 2026-10-02) that `initProject()` puts at
   `config/analysis-plan.yml`, from
   `../package-workflows/examples/analysis-plan.yml`, keeping its header
   that the content is sent to the Claude API.
6. ~~Add the caller `.github/workflows/datashield-analysis-suggest.yml`~~ (done 2026-10-02) to
   the project template, from
   `../package-workflows/examples/datashield-analysis-suggest.yml`.
7. ~~Optional: empty marked blocks the bot fills~~ (done 2026-10-02) —
   - in `main.R`:
     `#### bot-suggest: scripts (updated by datashield-analysis-suggest)`
     … `#### bot-suggest: scripts end`;
   - in `dependencies.R`:
     `#### bot-suggest: packages (updated by datashield-analysis-suggest)`
     … `#### bot-suggest: packages end`.
   Without them the bot appends its own. The marker texts are the
   `markers:` settings in `../package-workflows/config/analysis-suggest.yml`.
8. ~~The project's `.gitignore` must not exclude `utils/mock_data/`~~ (holds, tested 2026-10-02) — the
   bot commits its mock data there.

**Interface — keep stable** (the bot reads and edits these; paths in
the `paths:` settings of `config/analysis-suggest.yml`):
- the step markers `#### Step 1: …` to `#### Step 7: …` in
  `inst/templates/dslite/01_DSLite_Setup.R` (used by `add_dsPackage()` /
  `update_MockData()`; the bot looks for `#### Step 2: Import of mock
  data files`);
- exactly one `symbol = "..."` in that login call, and the
  `<name> <<- DSI::datashield.login(` form — the bot sets the symbol to
  the plan's and may append an alias line
  `<name> <<- conns  # bot-suggest: ...`;
- the `production` / `testing` profiles in the project `config.yml`.

### C. Functions

9. ~~**Bug in `add_dsPackage()`**~~ (fixed 2026-10-02: early return) (`R/add_dsPackage.R`): when every given
   package is already present, `new_dsPackage_length` (line ~73) is 0,
   and the `1:new_dsPackage_length` loops (lines ~91, ~112, ~144) run
   for 1 and 0 — they write `library(Client)` and break block 4. It
   should be a no-op (`seq_len()`, or return early).
10. ~~`update_MockData()` suits CI~~ (checked 2026-10-02, unchanged): pass `table_names` so it doesn't parse
    `01_DS_Login.R`; each `.rda` holds an object named after its server
    (`study1.rda` → `study1`). Keep it that way.

### D. README

11. ~~A data protection note~~ (done 2026-10-02): the analysis plan's content (variable names,
    categories, study names, steps) goes to the Claude API; data,
    credentials and `R/01_DS_Login.R` never do.
12. ~~A setup guide for analysis projects~~ (done 2026-10-02): the three secrets
    (`ANTHROPIC_ORG_ID`, `ANTHROPIC_SVAC_ID`, `ANTHROPIC_FDRL_ANALYSIS`)
    and "Allow GitHub Actions to create and approve pull requests".
13. ~~How to try the scripts~~ (done 2026-10-02): `R_CONFIG_ACTIVE = 'testing'` in
    `.Renviron`, restart R, run `R/main.R`.

### E. Later

14. `DESCRIPTION`: CRAN releases instead of
    `Remotes: datashield/dsBaseClient`, once available.
15. `.github/workflows/metadata_extraction.yaml` calls
    `FlorianSchw/datashield-workflows@master`, which is to be removed —
    replace or drop it then.
16. Optional: `R-CMD-check.yaml` (r-lib's, daily) could become a short
    caller of `../package-workflows/.github/workflows/r-cmd-check.yml`.
