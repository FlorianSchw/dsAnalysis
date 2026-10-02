#### Final step: creating citations for the used R packages

#### The grateful package makes it very easy to collect all information of packages used
#### either in a session, in all files or in an even more granular approach
library(grateful)

#### omit = NULL has been added to the function call to also include the grateful package itself
#### in the citations as it is omitted by default
cite_packages(out.dir = here::here("citations"),
              pkgs = "All",
              omit = NULL)

#### Note that while this seems to be a real time-saver, we do not have much experience with it yet
#### So a lot of the specifications that the function(s) allow are not fully tried yet
