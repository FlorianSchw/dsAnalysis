# dsAnalysis

dsAnalysis creates DataSHIELD **analysis project** environments. A new
project comes with a login script for the live servers, a DSLite test
setup with mock data, `renv`, and a `config.yml` to switch between the
two. A GitHub workflow can draft starter analysis scripts from an
analysis plan you write.

## Getting started

1. **Install dsAnalysis:**

   ``` r
   # install.packages("remotes")
   remotes::install_github("FlorianSchw/dsAnalysis")
   ```

2. **Create a project:**

   ``` r
   dsAnalysis::initProject(path = "~/projects", name = "my-analysis")
   ```

3. **Fill in `.Renviron`** in the new project with your servers' URLs,
   users and passwords, and restart R. See
   [`.Renviron` and credentials](#renviron-and-credentials) — read this
   before you put the project on GitHub.

4. **Try it locally** in testing mode, without any server: set
   `R_CONFIG_ACTIVE = 'testing'` in `.Renviron`, restart R and run
   `R/main.R`. See [Production and testing](#production-and-testing).

5. **Put the project on GitHub** (optional, but needed for the analysis
   scripts workflow): create a repository and push the project. Check
   first that `.Renviron` is not among the files to commit.

6. **Set up the analysis scripts workflow** (optional): see
   [Setup](#setup).

If you'd rather not set this up yourself, or get stuck,
[open an issue](https://github.com/FlorianSchw/dsAnalysis/issues).

## What a project contains

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
| `.Renviron` | Credentials and the active profile; **never committed** |
| `README.md` | The project's own README, with these rules for everyone working on it |

Other helpers: `add_dsPackage()` adds a DataSHIELD server package to the
DSLite setup, `update_MockData()` points it at other mock data.

## `.Renviron` and credentials

`.Renviron` in the project folder holds everything that must not be
shared:

- the servers' URLs, users and passwords (`OBIBA1_URL`, `OBIBA1_USER`,
  `OBIBA1_PWD`, …), which `R/01_DS_Login.R` reads with `Sys.getenv()`;
- `R_CONFIG_ACTIVE`, which decides whether you work on the live servers
  or in testing mode.

R reads it when it starts: **restart R after every change**.

`initProject()` fills it with the public OBiBa demo servers, so a new
project works out of the box. Replace them with your own servers.

**Keep credentials out of GitHub:**

- `initProject()` adds `.Renviron` to `.gitignore`, so git never commits
  it. Don't remove that line and don't force-add the file.
- Never write a URL with a password, a user name with a password, or a
  token into `R/01_DS_Login.R` or any other script. Scripts are
  committed, and the login reads everything from `.Renviron` for exactly
  that reason.
- Treat anything you push to GitHub as public, even in a private
  repository: its history keeps every version. If a password was ever
  pushed, **change the password** — deleting the file or the commit is
  not enough.
- Real data never goes into the project. The only data in it is
  generated mock data in `utils/mock_data/`, and `results/` is ignored by
  git except for its placeholders.

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

The workflow **never needs your server credentials**: it runs in testing
mode, on DSLite with mock data. Don't put DataSHIELD users or passwords
into GitHub secrets — the only secrets it needs are the Anthropic ones
below.

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

No Anthropic account, or not sure about any of this?
[Open an issue](https://github.com/FlorianSchw/dsAnalysis/issues) and
we'll set it up with you.

### Trying the scripts

The bot's pull request adds its mock data to `utils/mock_data/` and
points the DSLite setup at it. To run the scripts the way the workflow
tested them:

1. Check out the pull request's branch.
2. Set `R_CONFIG_ACTIVE = 'testing'` in `.Renviron`.
3. Restart R.
4. Run `R/main.R`.

Set it back to `'production'` to run the analysis on the live servers.

## Help

Questions, problems, or want help with the setup?
[Open an issue](https://github.com/FlorianSchw/dsAnalysis/issues).
