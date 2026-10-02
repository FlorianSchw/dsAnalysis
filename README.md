# dsAnalysis

dsAnalysis creates DataSHIELD **analysis project** environments. A new
project comes with a login script for the live servers, a DSLite test
setup with mock data, `renv`, and a `config.yml` to switch between the
two. A GitHub workflow can draft starter analysis scripts from an
analysis plan you write.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("FlorianSchw/dsAnalysis")
```

## Creating a project

``` r
dsAnalysis::initProject(path = "~/projects", name = "my-analysis")
```

This creates:

| Path | What it is |
|------|------------|
| `R/main.R` | Runs the analysis: logs in, then sources the analysis scripts |
| `R/01_DS_Login.R` | Login to the live DataSHIELD servers (production) |
| `R/99_DSLiteLearning.R` | Looking at the server-side data in testing mode |
| `R/99_package_citations.R` | Writes the citations of the packages used to `citations/` (with `grateful`) |
| `utils/setup/01_DSLite_Setup.R` | Login to a local DSLite instance with mock data (testing) |
| `utils/mock_data/` | Mock data for DSLite |
| `config.yml` | The `production` and `testing` profiles |
| `config/analysis-plan.yml` | Your analysis plan, read by the analysis scripts workflow |
| `.github/workflows/datashield-analysis-suggest.yml` | The analysis scripts workflow |
| `dependencies.R` | Packages tracked by `renv` |
| `.Renviron` | Credentials and the active profile; not committed |

Other helpers: `add_dsPackage()` adds a DataSHIELD server package to the
DSLite setup, `update_MockData()` points it at other mock data.

## Production and testing

`R_CONFIG_ACTIVE` in the project's `.Renviron` decides where `R/main.R`
logs in:

- `R_CONFIG_ACTIVE = 'production'` (the default): the live servers, via
  `R/01_DS_Login.R`;
- `R_CONFIG_ACTIVE = 'testing'`: the local DSLite instance with mock
  data, via `utils/setup/01_DSLite_Setup.R`.

Restart R after changing it.

## Analysis scripts from your plan

Describe your analysis in `config/analysis-plan.yml`: the studies, the
variables and the steps. Each push that changes the plan runs the
`datashield-analysis-suggest` workflow from
[package-workflows](https://github.com/FlorianSchw/package-workflows).
It drafts a script per step, tests all of them on mock data built from
your variables, and proposes them as a pull request. The scripts are a
starting point: read and adapt them. The analysis stays your
responsibility.

Details:
[DataSHIELD analysis scripts](https://github.com/FlorianSchw/package-workflows/blob/main/docs/datashield-analysis-suggest.qmd).

### Data protection

The **content of the analysis plan is sent to the Claude API**: variable
names and labels, categories, study names and the steps. Don't put
anything confidential in the plan.

Your **data, credentials and `R/01_DS_Login.R` are never sent**. The
scripts are tested on generated mock data only.

### Setup

Once per project repository on GitHub:

1. **Claude access.** In the Anthropic Console, a service account for
   the repository and a federation rule for the workflow (claim
   `job_workflow_ref`, naming
   `FlorianSchw/package-workflows/.github/workflows/datashield-analysis-suggest.yml@refs/heads/main`).
   See
   [Claude setup](https://github.com/FlorianSchw/package-workflows/blob/main/docs/claude-setup.qmd).
2. **Repository secrets** (Settings → Secrets and variables → Actions):

   | Secret | Value |
   |--------|-------|
   | `ANTHROPIC_ORG_ID` | Your Anthropic organization ID |
   | `ANTHROPIC_SVAC_ID` | The service account of this repository |
   | `ANTHROPIC_FDRL_ANALYSIS` | The federation rule of the workflow |

3. **Pull requests.** Turn on "Allow GitHub Actions to create and
   approve pull requests" (Settings → Actions → General). Alternatively,
   set up a GitHub App with the secrets `APP_CLIENT_ID` and
   `APP_PRIVATE_KEY`; then the bot's pull requests also trigger your
   other checks.

### Trying the scripts

The bot's pull request adds its mock data to `utils/mock_data/` and
points the DSLite setup at it. To run the scripts the way the workflow
tested them:

1. Check out the pull request's branch.
2. Set `R_CONFIG_ACTIVE = 'testing'` in `.Renviron`.
3. Restart R.
4. Run `R/main.R`.

Set it back to `'production'` to run the analysis on the live servers.
