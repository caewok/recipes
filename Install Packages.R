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
renv::install("rmarkdown")
renv::install("knitr")

# for using the optional dtplyr
renv::install("dtplyr")

# for testing
renv::install("testthat")
renv::install("RcppRoll")
renv::install("RSpectra")
renv::install("igraph")


renv::snapshot()
renv::install("RANN")

# renv::install("NMF")
# renv::install("Biobase")
