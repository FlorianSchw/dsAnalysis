

update_MockData <- function(folder_name = NULL, table_names = NULL){

}

#### some more clauses should be tested
if(is.null(folder_name)){
  stop("You need to provide a valid folder with mock data.", call.=FALSE)
}

mockData_files <- list.files(path = here::here(paste0("utils/mock_data/", folder_name)))
mockData_files_name <- stringr::str_remove(string = mockData_files,
                                           pattern = ".rda")

if(!is.null(table_names)){
  if(!(length(table_names) == length(mockData_files))){

    stop("The length of the provided table names and mock data files are not the same.", call.=FALSE)

  }
}

if(is.null(table_names)){

  message("No table names were given. Hence, they will be retrieved from the 01_DS_Login.R file!")


  ds_login_codelines <- readLines(con = here::here("R", "01_DS_Login.R"))
  ds_login_codelines <- str_replace_all(string = ds_login_codelines, pattern = " ", replacement = "")

  builderAppend_line <- which(stringr::str_detect(ds_login_codelines, "builder[$]append"))
  logindata_line <- which(stringr::str_detect(ds_login_codelines, "logindata<-"))

  #### builder in eine list packen zum loopen später
  builder1 <- ds_login_codelines[builderAppend_line[1]:(builderAppend_line[2]-1)]
  builder2 <- ds_login_codelines[builderAppend_line[2]:(builderAppend_line[3]-1)]
  builder3 <- ds_login_codelines[builderAppend_line[3]:logindata_line]

  table_new <- c()
  server_new <- c()

  for (i in 1:length(mockData_files)){

    tmp_check <- stringr::str_detect(builder1[1], mockData_files_name[i])

    if(tmp_check){

      test <- as_tibble(builder1) |>
        filter(str_detect(string = value,
                          pattern = "table=")) |>
        mutate(desired_string = str_replace_all(string = str_split_i(string = value,
                                                                     pattern = "\\.",
                                                                     i = 2),
                                                pattern = "[[:punct:]]", ""))

    }



  }



}












dslite_setup_codelines <- readLines(con = here::here("utils/setup", "01_DSLite_Setup.R"))

step1_text <- "#### Step 1: Loading necessary libraries"
step2_text <- "#### Step 2: Import of mock data files"
step3_text <- "#### Step 3: Defining the server-side data in a new dslite server"
step4_text <- "#### Step 4: Defining the server-side settings"
step5_text <- "#### Step 5: Building the logindata object"
step6_text <- "#### Step 6: Login to the different DSLite Servers"
step7_text <- "#### Step 7: Cleaning the environment"

step1_line <- which(dslite_setup_codelines == step1_text)
step2_line <- which(dslite_setup_codelines == step2_text)
step3_line <- which(dslite_setup_codelines == step3_text)
step4_line <- which(dslite_setup_codelines == step4_text)
step5_line <- which(dslite_setup_codelines == step5_text)
step6_line <- which(dslite_setup_codelines == step6_text)
step7_line <- which(dslite_setup_codelines == step7_text)

block0 <- dslite_setup_codelines[1:(step1_line-1)]
block1 <- dslite_setup_codelines[step1_line:(step2_line-1)]
block2 <- dslite_setup_codelines[step2_line:(step3_line-1)]
block3 <- dslite_setup_codelines[step3_line:(step4_line-1)]
block4 <- dslite_setup_codelines[step4_line:(step5_line-1)]
block5 <- dslite_setup_codelines[step5_line:(step6_line-1)]
block6 <- dslite_setup_codelines[step6_line:(step7_line-1)]
block7 <- dslite_setup_codelines[step7_line:length(dslite_setup_codelines)]












