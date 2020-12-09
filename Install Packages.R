# install.packages("renv")
library(renv)
renv::install("gower")
renv::install("ipred")
renv::install("timeDate")
renv::install("dplyr")
renv::install("lubridate")
renv::install("withr")

# optional packages
renv::install("ddalpha")
renv::install("fastICA")
renv::install("dimRed")
renv::install("kernlab")

# packages for vignettes
renv::install("rsample")
renv::install("modeldata")
