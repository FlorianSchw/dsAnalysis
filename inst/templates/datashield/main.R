#### file for running all R Scripts

#### Step 1: Connecting to servers
#### Logging in to DataSHIELD servers through production (01_DS_Login.R)
#### or testing setup with DSLite (01_DSLite_Setup.R)

cfg_login_folder <- config::get(value = "login_folder")
cfg_login_file <- config::get(value = "login_file")

source(here::here(cfg_login_folder, cfg_login_file))

#### Step 2: Analysis scripts
#### the datashield-analysis-suggest workflow lists the scripts from config/analysis-plan.yml here
#### bot-suggest: scripts (updated by datashield-analysis-suggest)
#### bot-suggest: scripts end
