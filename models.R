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
anes[ , 2:ncol(anes)] <- lapply(anes[ , 2:ncol(anes)], recodeANESvalues)
## Split the data into two subsets:
indices <- sample(1:nrow(anes), nrow(anes)/2)
training <- anes[indices, ]
testing <- anes[-indices, ]
## Run three models on the training set:
# How does the respondents' personal financial situation affect ft_dpc?
mod1 <- lm(ft_dpc ~ ., training[ , 1:4])
# How does the respondents' trust in government and social trust affect ft_dpc?
mod2 <- lm(ft_dpc ~ ., training[ , c(1, 5:8)])
# How does the respondents' view of US position in the world affect ft_dpc?
mod3 <- lm(ft_dpc ~ ., training[ , c(1, 9:10)])
# BONUS: How is ft_dpc affected by all three of these sets of variables?
mod4 <- lm(ft_dpc ~ ., training)
summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)
## Make predictions:
mod1preds <- predict(mod1, testing)
mod2preds <- predict(mod2, testing)
mod3preds <- predict(mod3, testing)
mod4preds <- predict(mod4, testing)
## Check the accuracy of the models using fitStat:
P <- matrix(c(mod1preds, mod2preds, mod3preds, mod4preds), ncol=4)
r <- rep(mean(na.omit(testing$ft_dpc)), nrow(testing)) # naive forecast (mean)
library(fitStatR)
fitStat(testing$ft_dpc, P, r) # Note the warnings (due to nature of this data)
# They do not perform well at all, though (predictably) mod4 is slightly better
