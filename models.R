# setwd as appropriate
## Read in the data:
library(foreign)
anes <- read.dta('anes_timeseries_2012_stata12.dta')
## Deal with missingness:
# I plan to listwise delete. Although this is not generally advisable, it will
# not affect the purpose of this exercise.
codeNAsForANES <- function(variable){
  variable[grepl('-[1-9]', variable)] <- NA
  return(variable)
}
anes <- as.data.frame(lapply(anes, codeNAsForANES))
## Subset to just the questions I will use in the models:
financeQs <- c(141, 144, 147)
trustQs <- 304:307
usWorldQs <- 226:227
anes <- anes[ , c(125, financeQs, trustQs, usWorldQs)]
## It will be helpful to recode the variables a little:
recodeANESvalues <- function(x){
  x <- as.factor(as.character(x))
  if (length(levels(x)) == 2 | length(levels(x)) == 6) {
    return(as.numeric(x) - 1)
  }
  return(as.numeric(x))
}
anes <- as.data.frame(lapply(anes, recodeANESvalues))
## Split the data into two subsets:
indices <- sample(1:nrow(anes), nrow(anes)/2)
training <- anes[indices, ]
testing <- anes[-indices, ]
## Run three models on the training set:
mod1 <- lm(ft_dpc ~ ., training[ , 1:4])
mod2 <- lm(ft_dpc ~ ., training[ , c(1, 5:8)])
mod3 <- lm(ft_dpc ~ ., training[ , c(1, 9:10)])
summary(mod1)
summary(mod2)
summary(mod3)
## Make predictions:
mod1predictions <- predict(mod1, testing)
mod2predictions <- predict(mod2, testing)
mod3predictions <- predict(mod3, testing)
