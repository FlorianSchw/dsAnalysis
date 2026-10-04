#### line number of a step marker in 01_DSLite_Setup.R; stops if the file doesn't have it
dslite_step_line <- function(codelines, step){

  markers <- c("#### Step 1: Loading necessary libraries",
               "#### Step 2: Import of mock data files",
               "#### Step 3: Defining the server-side data in a new dslite server",
               "#### Step 4: Defining the server-side settings",
               "#### Step 5: Building the logindata object",
               "#### Step 6: Login to the different DSLite Servers",
               "#### Step 7: Cleaning the environment")

  step_line <- which(codelines == markers[step])

  if(!(length(step_line) == 1)){
    stop(paste0("Could not find the line '", markers[step],
                "' in 01_DSLite_Setup.R. Please don't edit the step markers."),
         call. = FALSE)
  }

  step_line
}
