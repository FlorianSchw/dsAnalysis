#'
#' @title Function to get mock data from connected data sources
#' @description The function will create a new DataSHIELD analysis project environment
#' @details This function adaptssome things.
#' @param location specifies the location where the project shall be installed
#' @param initiate_git XXXX
#' @return the path of the script_name
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @import renv
#' @import usethis
#' @export
#'


initMockdata <- function(){






}






renv::install("sofiasiamp/dsSupportClient")
library(dsSupportClient)


aaa <- dsSupportClient::ds.summaryVars("D")
bbb <- ds.wrapper("D", ds_function = ds.class)
ccc <- ds.tableBatch("D")
ddd <- ds.wrapper("D", ds_function = ds.numNA)




eee <- ds.levels("D$DIS_AMI")


#### general


#### "D" hardcoded for now

vars_missing <- dsSupportClient::ds.wrapper("D", ds_function = ds.numNA)
vars_class <- dsSupportClient::ds.wrapper("D", ds_function = ds.class)

vars_cont <- vars_class |>
  dplyr::filter(if_all(everything(), ~ .x == "numeric")) |>
  tibble::rownames_to_column() |>
  select(1) |>
  pull()

vars_cat <- vars_class |>
  dplyr::filter(if_all(everything(), ~ .x == "factor")) |>
  tibble::rownames_to_column() |>
  select(1) |>
  pull()

#### cont vars

vars_cont_stats <- dsSupportClient::ds.summaryVars("D")

mock_data_cont <- list()

for (i in 1:length(vars_cont_stats)){

  datasource_length <- vars_cont_stats[[i]][2] |>
    pull()
  datasource_length <- as.numeric(datasource_length)[1]

  vars_cont_negative <- vars_cont_stats[[i]] |>
    select(3)|>
    filter(if_any(1, ~ .x < 0)) |>
    tibble::rownames_to_column() |>
    select(1) |>
    pull()

  datasource_cont_stats <- vars_cont_stats[[i]] |>
    select(c(6,11)) |>
    mutate(across(everything(), ~ as.numeric(.))) |>
    tibble::rownames_to_column() |>
    rename(Variable = 1,
           Mean = 2,
           Std = 3) |>
    mutate(Entries = purrr::map2(Mean, Std, ~ rnorm(n = datasource_length,
                                                    mean = .x,
                                                    sd = .y))) |>
    select(Variable, Entries) |>
    pivot_wider(names_from = "Variable",
                values_from = "Entries") |>
    unnest(cols = all_of(vars_cont))|>
    mutate(across(!all_of(vars_cont_negative), ~ ifelse(.x < 0, 0, .x)))


  mock_data_cont[[i]] <- datasource_cont_stats


}

#### "D" hardcoded for now
for (k in 1:length(vars_cat)){

  var_cat_level <- ds.levels(paste0("D$",vars_cat[1]))
  names_list <- names(var_cat_level)
  list2 <- cbind(var_cat_level, names_list)

  for (j in 1:length(var_cat_level)){

    number_categories <- length(var_cat_level[[1]]$Levels)

    #### hard-coded divison
    list3 <- as_tibble(list2) |>
      unnest_wider(col = 1) |>
      unnest_longer(col = 1) |>
      mutate(Entries = purrr::map(.x = Levels, .f = ~ rep(x = .x,
                                                          times = ceiling(datasource_length/number_categories))))


  }


}


#### cat vars

# adjusted ds.levels according to dplyr::filter ds.class all_factor
## rep function based of length + categories for equal distribution


## final
## adjust according to NA-level

#### save according to name given to be in-line with analyst linguistics
#### and done
