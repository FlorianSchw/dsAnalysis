#'
#' @title Function to add dsPackages to the 01_DSLite_Setup.R file
#' @description XXX
#' @details XXX.
#' @return adjusted R Script
#' @author Florian Schwarz for the German Institute of Human Nutrition
#' @param dsPackage name or names of the DataSHIELD server-side packages to add to DSLite instance
#' @import stringr
#' @export
#'

add_dsPackage <- function(dsPackage = NULL){

  if(is.null(dsPackage)){
    stop("No package name has been given.",call.=FALSE)
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


  #### general


  dupl_int <- c()

  for (p in 1:length(dsPackage)){

    package_duplicate <- any(stringr::str_detect(string = block1,
                                                 pattern = dsPackage[p]))



    if(package_duplicate){

      dupl_int[p] <- FALSE
      message(paste0("The DataSHIELD package ", dsPackage[p], " is already included in the DSLite Setup."))

    } else {

      dupl_int[p] <- TRUE

    }

  }

  dsPackage_unique <- dsPackage[dupl_int]
  new_dsPackage_length <- length(dsPackage_unique)

  #### block 1
  number_elements <- length(block1)
  block1_new <- block1

  if(!(block1_new[number_elements] == "")){
    empty_line_adj <- 0
  } else {
    empty_line_adj <- 1
  }

  if(new_dsPackage_length == 1){

    block1_new[number_elements-empty_line_adj+1] <- paste0("library(",dsPackage_unique,"Client)")

  } else {

    for (i in 1:new_dsPackage_length){
      block1_new[number_elements-empty_line_adj+i] <- paste0("library(",dsPackage_unique[i],"Client)")
    }

  }

  new_length <- length(block1_new)
  block1_new[new_length+1] <- ""

  #### block 4
  dsPackage_configuration_end <- which(block4 == "dslite.server$profile()")
  number_dsPackage_current <- dsPackage_configuration_end - 2L

  block4_new <- c()
  block4_new[1] <- step4_text


  if(number_dsPackage_current == 1L){

    block4_new[2] <- paste0("dslite.server$config(DSLite::defaultDSConfiguration(include=c(\"dsBase\",")

    for (k in 1:new_dsPackage_length){

      if(!(k == new_dsPackage_length)){

        block4_new[2+k] <- paste0("                                                              \"",dsPackage_unique[k],"\",")

      } else if(k == new_dsPackage_length){

        block4_new[2+k] <- paste0("                                                              \"",dsPackage_unique[k],"\")))")

      }
    }

    block4_new[3+k] <- block4[dsPackage_configuration_end]
    block4_new[4+k] <- block4[dsPackage_configuration_end+1]
    block4_new[5+k] <- block4[dsPackage_configuration_end+2]


  } else if(number_dsPackage_current > 1L){

    for(k in 1:number_dsPackage_current){

      if(!(k == number_dsPackage_current)){

        block4_new[1+k] <- block4[1+k]

      } else if(k == number_dsPackage_current){

        block4_new[1+k] <- stringr::str_replace(string = block4[1+k],
                                                pattern = "\\)\\)\\)",
                                                replacement = ",")

        for (j in 1:new_dsPackage_length){

          if(!(j == new_dsPackage_length)){

            block4_new[1+k+j] <- paste0("                                                              \"",dsPackage_unique[j],"\",")

          } else if(j == new_dsPackage_length){

            block4_new[1+k+j] <- paste0("                                                              \"",dsPackage_unique[j],"\")))")
          }
        }
      }
    }

    block4_new[2+k+j] <- block4[dsPackage_configuration_end]
    block4_new[3+k+j] <- block4[dsPackage_configuration_end+1]
    block4_new[4+k+j] <- block4[dsPackage_configuration_end+2]


  }

  dslite_setup_codelines_new <- c(block0,
                                  block1_new,
                                  block2,
                                  block3,
                                  block4_new,
                                  block5,
                                  block6,
                                  block7)


  writeLines(text = dslite_setup_codelines_new,con = here::here("utils/setup", "01_DSLite_Setup.R"))

}


