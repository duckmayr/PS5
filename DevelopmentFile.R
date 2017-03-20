
## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd('~/Rclass/PS5') # change as appropriate
current.code <- as.package('fitStatR')
load_all(current.code)
document(current.code)
