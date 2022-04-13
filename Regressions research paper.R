# Research Seminar Financial Economics
# Title of the seminar paper by Felix Boeltz and Fabio Ramsperger

## Setting the working directory
setwd("C:/Users/felix/OneDrive/Desktop/St. Gallen/05_Research Seminar Financial Economics/Data and models")

## Loading required packages
library(readxl)
library(xts)
library(sandwich)
library(stargazer)
library(systemfit)
library(ggplot2)
library(IndexConstruction)

# Replication of the Crix index
#Since the data publicly available on the crix index covers only weekdays, we replicate the index using prices and market cap of the current top 30 coins by market cap
crix_data<- read.csv("topcoins.csv")
crix_data <- crix_data[colSums(!is.na(crix_data)) > 0]
crix_data <- crix_data [!apply(crix_data  == "", 1, all), ]  
crix_data <- crix_data[rowSums(is.na(crix_data)) != ncol(crix_data),]
crix_data[is.na(crix_data)] <- 0
crix_mcap <-  cbind(crix_data[1], crix_data[seq(3, ncol(crix_data), 3)])
crix_price <-  cbind(crix_data[1], crix_data[seq(2, ncol(crix_data), 3)])
crix_volume <-  cbind(crix_data[1], crix_data[seq(4, ncol(crix_data), 3)])

#convert dataframes to timeseries
crix_mcap_ts <- xts(crix_mcap [,-1], order.by=as.Date(crix_mcap$date))
crix_price_ts  <- xts(crix_price [,-1], order.by=as.Date(crix_price[,1], "%Y-%m-%d"))
crix_volume_ts <-  xts(crix_volume [,-1], order.by=as.Date(crix_volume[,1], "%Y-%m-%d"))

days.line = switchDates(crix_price_ts , specificDate = "1")
crix <- indexComp(crix_mcap_ts, crix_price_ts, weighting = "market", eval.seq = "all.together", optimum = "local", base.value = 50, days.line = days.line)
#Note: You have to indicate what index to plot by entering 1, 2 or 3 in the chat. To get the price levels of the Crix chose 1
plot(crix)
weights(crix)

#export crix index to csv (to be later used in regressions)
write.csv(index$results$index, "index_returns.csv", row.names = TRUE)

# Creation of the gaming index
gaming_fund_data <- read.csv("Gaming_Fundamentals_500d.csv")
gaming_fund_data <- gaming_fund_data[colSums(!is.na(gaming_fund_data)) > 0]
gaming_fund_data <- gaming_fund_data [!apply(gaming_fund_data  == "", 1, all), ]  
gaming_fund <- gaming_fund_data[rowSums(is.na(gaming_fund_data)) != ncol(gaming_fund_data),]
gaming_fund_data[is.na(gaming_fund_data)] <- 0
gaming_fund_mcap <-  cbind(gaming_fund_data[1], gaming_fund_data[seq(3, ncol(gaming_fund_data), 3)])
gaming_fund_price <-  cbind(gaming_fund_data[1], gaming_fund_data[seq(2, ncol(gaming_fund_data), 3)])
gaming_fund_volume <-  cbind(gaming_fund_data[1], gaming_fund_data[seq(4, ncol(gaming_fund_data), 3)])

gaming_fund_mcap_ts <- xts(gaming_fund_mcap [,-1], order.by=as.Date(gaming_fund_mcap[,1], "%Y-%m-%d"))
gaming_fund_price_ts  <- xts(gaming_fund_price [,-1], order.by=as.Date(gaming_fund_price[,1], "%Y-%m-%d"))
gaming_fund_volume_ts <-  xts(gaming_fund_volume [,-1], order.by=as.Date(gaming_fund_volume[,1], "%Y-%m-%d"))

days.line = switchDates(gaming_fund_price_ts , specificDate = "1")
gaming_fund <- indexComp(gaming_fund_mcap_ts, gaming_fund_price_ts, gaming_fund_volume_ts, weighting = "market", weighting.all = "market", eval.seq = "all.together", optimum = "local", start.const=1, base.value = 50, days.line = days.line, derivation.period.ic = 3)
#Note: You have to indicate what index to plot by entering 1, 2 or 3 in the chat. To get the price levels of the gaming index chose 1
plot(gaming_fund)
weights(gaming_fund)

#export gaming index to csv (to be later used in regressions)
write.csv(gaming_fund$results$index, "gaming_fund_returns.csv", row.names = TRUE)

# For the later parts of this code we use price data on the top50 GameFi coins, excluding coins that don't have enough data to be analyzed and the in-game currencies of the games (we only focused on governance tokens). We pulled the same data for general crypto coins that have about the same market cap to allow for comparability
# We excluded coins from the analysis that were part of either gaming index or CRIX since they would obviously have a very high correlation with the index
# We pulled the price data from Coingecko via their API and then converted the prices to continuous returns (=ln(price_t+1 / price t))
# From the index prices for both CRIX and gaming index given through the code above, we also calculated continuous returns
# We also pulled the "Germany 3 month Historische Daten der Anleiherenditen", which is a proxy for the german risk-free rate. This makes sense since all our data is in UTC time. We then converted the annualized yield to daily yields applying the formula: i_daily = (1 + i_annual) ^ (1/365) - 1
# We then calculated excess returns for all the coins by subtracting the risk free rate from the continous returns
# We then also added dummies to the data to identify whether a coin is a GameFi coin or a general crypto coin
# The resulting Excel sheet is called "Clean_data_reg.xlsx" and will be used for the further analysis

## Loading the data
data_1 <- read_excel("Clean_data_reg.xlsx", sheet = "Excess returns")

## Converting the data into time series and removing the original data
date <- as.Date(data_1$date)
ts <- xts(x = data_1[-1], order.by = date)
remove(data_1, date)

## Creating a list that contains the "is GameFi" dummy variable for each coin (required for the next step)
dummies <- c()
for(x in 1:100) {
  new_element <- paste0("D", x)
  dummies <- c(dummies, new_element)
}

## Getting the column names of all variables on which we run regressions
depVarList_1 = setdiff(colnames(ts), c("rf", "crix_index", "crypto_gaming_index", "gaming_index", dummies))

# Analysis 1: Initial market models for GameFi coins and general-purpose cryptos using the Crix index

## Looping over all dependent variables used for the first analysis and creating an initial market model for each one, saving the models as "allModels_1_1"
allModels_1_1 = lapply(depVarList_1, function(x){
  summary(lm(formula = paste0("`", x, "` ~ crix_index"),
             data = ts, na.action = na.omit))
})

## Re-creating the same initial market models using HAC standard errors, saving the models as "allModels_1_2"
allModels_1_2 = lapply(depVarList_1, function(x){
  summary(lm(formula = paste0("`", x, "` ~ crix_index"),
             data = ts, na.action = na.omit), vcov = sandwich)
})

## Naming the created models after the coin / dependent variable used in each model
names(allModels_1_1) = depVarList_1
names(allModels_1_2) = depVarList_1

## Creating two matrices to store the regression results
val_matrix_GF_1 <- matrix(0, nrow = 50, ncol = 9)
rownames(val_matrix_GF_1) <- depVarList_1[1:50]
colnames(val_matrix_GF_1) <- c("Value Intercept", "Std. Error Intercept", "t-stat Intercept", "p-value Intercept", "Value Market Factor", "Std. Error Market Factor", "t-stat Market Factor", "p-value Market Factor", "Adjusted R-squared")
val_matrix_GE_1 <- matrix(0, nrow = 50, ncol = 9)
rownames(val_matrix_GE_1) <- depVarList_1[51:100]
colnames(val_matrix_GE_1) <- c("Value Intercept", "Std. Error Intercept", "t-stat Intercept", "p-value Intercept", "Value Market Factor", "Std. Error Market Factor", "t-stat Market Factor", "p-value Market Factor", "Adjusted R-squared")

## Storing the results of the initial market models run on GameFi coins in the first matrix
for(x in 1:50) {
  val_matrix_GF_1[x, 1] <- allModels_1_2[[x]]$coef[1, 1]
  val_matrix_GF_1[x, 2] <- allModels_1_2[[x]]$coef[1, 2]
  val_matrix_GF_1[x, 3] <- allModels_1_2[[x]]$coef[1, 3]
  val_matrix_GF_1[x, 4] <- allModels_1_2[[x]]$coef[1, 4]
  val_matrix_GF_1[x, 5] <- allModels_1_2[[x]]$coef[2, 1]
  val_matrix_GF_1[x, 6] <- allModels_1_2[[x]]$coef[2, 2]
  val_matrix_GF_1[x, 7] <- allModels_1_2[[x]]$coef[2, 3]
  val_matrix_GF_1[x, 8] <- allModels_1_2[[x]]$coef[2, 4]
  val_matrix_GF_1[x, 9] <- allModels_1_2[[x]]$adj.r.squared
}

#quick test whether there's a difference btw. small, mid and large cap
#Note: Categorization is based on the market cap on the 06.04.22 -> for an exact analysis one would need to include the market cap categories over time. However a regression of excess returns on market caps could be highly problematic due to very high correlation btw. the two (price and dilution are the two components of market cap)
val_matrix_GF_large_cap <- val_matrix_GF_1[1:8, 1:9]
val_matrix_GF_mid_cap <- val_matrix_GF_1[9:24, 1:9]
val_matrix_GF_small_cap <- val_matrix_GF_1[25:50, 1:9]

#r-squared seems to increase and the p-value to decrease from small to large cap -> interesting for further research
val_matrix_GF_large_cap_mean <- colMeans(val_matrix_GF_large_cap)
val_matrix_GF_mid_cap_mean <- colMeans(val_matrix_GF_mid_cap)
val_matrix_GF_small_cap_mean <- colMeans(val_matrix_GF_small_cap)

## Storing the results of the initial market models run on general-purpose cryptos in the second matrix
for(x in 1:50) {
  val_matrix_GE_1[x, 1] <- allModels_1_2[[x + 50]]$coef[1, 1]
  val_matrix_GE_1[x, 2] <- allModels_1_2[[x + 50]]$coef[1, 2]
  val_matrix_GE_1[x, 3] <- allModels_1_2[[x + 50]]$coef[1, 3]
  val_matrix_GE_1[x, 4] <- allModels_1_2[[x + 50]]$coef[1, 4]
  val_matrix_GE_1[x, 5] <- allModels_1_2[[x + 50]]$coef[2, 1]
  val_matrix_GE_1[x, 6] <- allModels_1_2[[x + 50]]$coef[2, 2]
  val_matrix_GE_1[x, 7] <- allModels_1_2[[x + 50]]$coef[2, 3]
  val_matrix_GE_1[x, 8] <- allModels_1_2[[x + 50]]$coef[2, 4]
  val_matrix_GE_1[x, 9] <- allModels_1_2[[x + 50]]$adj.r.squared
}

#count significant alphas / betas at 5% level for GameFi
significant_alphas_GF <- sum(val_matrix_GF_1[,4] < 0.05)
significant_betas_GF <- sum(val_matrix_GF_1[,8] < 0.05)

#count significant alphas / betas at 5% level for general crypto coins
significant_alphas_GE <- sum(val_matrix_GE_1[,4] < 0.05)
significant_betas_GE <- sum(val_matrix_GE_1[,8] < 0.05)

#calculate average statistics for the two models
avg_val_matrix_GF_1 <- colMeans(val_matrix_GF_1)
avg_val_matrix_GE_1 <- colMeans(val_matrix_GE_1)

## Creating a data frame containing all alphas and dummy variables
df <- data.frame(alpha = numeric(0), dummy = numeric(0))
for(x in 1:50) {
  df[nrow(df) + 1,] = c(allModels_1_2[[x]]$coef[1, 1], 1)
}

for(x in 51:100) {
  df[nrow(df) + 1,] = c(allModels_1_2[[x]]$coef[1, 1], 0)
}

## Regression the alphas of the initial market models on the dummy variables and returning the results
alpha_reg <- lm(alpha ~ dummy, data = df)
summary(alpha_reg)
#Note: coefficient on the dummy is as expected the same as the differences between the alphas of GameFi and notGameFi from above (=D1)

# Analysis 2: Market models for GameFi coins using the Crix index, a newly created GameFi index, and a gaming stock index

# Analysis 2.1: Market models for GameFi coins using a newly created GameFi index
## Drawing a sub-set of the previously used time series, containing all the data required for the second part of our analysis
ts_gamefi <- ts[, 0:104]
depVarList_2 = setdiff(colnames(ts_gamefi), c("rf", "crix_index", "crypto_gaming_index", "gaming_index", dummies))

## Looping over all dependent variables used for the second analysis and creating an new market model for each one, saving the models as "allModels_2_1"
allModels_2_1 = lapply(depVarList_2, function(x){
  summary(lm(formula = paste0("`", x, "` ~ crypto_gaming_index"),
             data = ts_gamefi, na.action = na.omit))
})

## Re-creating the same new market models using HAC standard errors, saving the models as "allModels_2_2"
allModels_2_2 = lapply(depVarList_2, function(x){
  coeftest(lm(formula = paste0("`", x, "` ~ crypto_gaming_index"),
             data = ts_gamefi, na.action = na.omit), vcov = sandwich)
})

## Naming the created models after the coin / dependent variable used in each model
names(allModels_2_1) = depVarList_2
names(allModels_2_2) = depVarList_2

## Creating a matrix to store the regression results
val_matrix_GF_2 <- matrix(0, nrow = 50, ncol = 9)
rownames(val_matrix_GF_2) <- depVarList_2[1:50]
colnames(val_matrix_GF_2) <- c("Value Intercept", "Std. Error Intercept", "t-stat Intercept", "p-value Intercept", "Value Market Factor", "Std. Error Market Factor", "t-stat Market Factor", "p-value Market Factor", "Adjusted R-squared")

## Storing the results of the new market models run on GameFi coins in the matrix
for(x in 1:50) {
  val_matrix_GF_2[x, 1] <- allModels_2_2[[x]][1, 1]
  val_matrix_GF_2[x, 2] <- allModels_2_2[[x]][1, 2]
  val_matrix_GF_2[x, 3] <- allModels_2_2[[x]][1, 3]
  val_matrix_GF_2[x, 4] <- allModels_2_2[[x]][1, 4]
  val_matrix_GF_2[x, 5] <- allModels_2_2[[x]][2, 1]
  val_matrix_GF_2[x, 6] <- allModels_2_2[[x]][2, 2]
  val_matrix_GF_2[x, 7] <- allModels_2_2[[x]][2, 3]
  val_matrix_GF_2[x, 8] <- allModels_2_2[[x]][2, 4]
  val_matrix_GF_2[x, 9] <- allModels_2_1[[x]]$adj.r.squared
}

## Calculating the averages of the alphas, the market betas, the corresponding p-values, and the adjusted R-squareds across the 50 market models
avg_val_matrix_GF_2 <- colMeans(val_matrix_GF_2)

#quick test whether there's a difference btw. small, mid and large cap
#Note: Categorization is based on the market cap on the 06.04.22 -> for an exact analysis one would need to include the market cap categories over time. However a regression of excess returns on market caps could be highly problematic due to very high correlation btw. the two (price and dilution are the two components of market cap)
val_matrix_GF_2_large_cap <- val_matrix_GF_2[1:8, 1:9]
val_matrix_GF_2_mid_cap <- val_matrix_GF_2[9:24, 1:9]
val_matrix_GF_2_small_cap <- val_matrix_GF_2[25:50, 1:9]

#r-squared once again seems to increase and the p-value to decrease from small to large cap -> interesting for further research
val_matrix_GF_2_large_cap_mean <- colMeans(val_matrix_GF_2_large_cap)
val_matrix_GF_2_mid_cap_mean <- colMeans(val_matrix_GF_2_mid_cap)
val_matrix_GF_2_small_cap <- colMeans(val_matrix_GF_2_small_cap)

#once again count significant alphas / betas at 5% level 
significant_alphas_GF_2 <- sum(val_matrix_GF_2[,4] < 0.05)
significant_betas_GF_2 <- sum(val_matrix_GF_2[,8] < 0.05)

#Analysis 2.2: Regress excess returns of gamefi coins on general gaming index. DE000SLA5J17 (MVIS Global Video Gaming and eSports Index) is used as an index (proxy) for the traditional gaming industry.
# Analysis 2.2: Market models for GameFi coins using a gaming stock index

## Looping over all dependent variables used for the second analysis and creating an new market model for each one, saving the models as "allModels_3_1"
allModels_3_1 = lapply(depVarList_2, function(x){
  summary(lm(formula = paste0("`", x, "` ~ gaming_index"),
             data = ts_gamefi, na.action = na.omit))
})

## Re-creating the same new market models using HAC standard errors, saving the models as "allModels_3_2"
allModels_3_2 = lapply(depVarList_2, function(x){
  coeftest(lm(formula = paste0("`", x, "` ~ gaming_index"),
              data = ts_gamefi, na.action = na.omit), vcov = sandwich)
})

## Naming the created models after the coin / dependent variable used in each model
names(allModels_3_1) = depVarList_2
names(allModels_3_2) = depVarList_2

## Creating a matrix to store the regression results
val_matrix_GF_3 <- matrix(0, nrow = 50, ncol = 9)
rownames(val_matrix_GF_3) <- depVarList_2[1:50]
colnames(val_matrix_GF_3) <- c("Value Intercept", "Std. Error Intercept", "t-stat Intercept", "p-value Intercept", "Value Market Factor", "Std. Error Market Factor", "t-stat Market Factor", "p-value Market Factor", "Adjusted R-squared")

## Storing the results of the new market models run on GameFi coins in the matrix
for(x in 1:50) {
  val_matrix_GF_3[x, 1] <- allModels_3_2[[x]][1, 1]
  val_matrix_GF_3[x, 2] <- allModels_3_2[[x]][1, 2]
  val_matrix_GF_3[x, 3] <- allModels_3_2[[x]][1, 3]
  val_matrix_GF_3[x, 4] <- allModels_3_2[[x]][1, 4]
  val_matrix_GF_3[x, 5] <- allModels_3_2[[x]][2, 1]
  val_matrix_GF_3[x, 6] <- allModels_3_2[[x]][2, 2]
  val_matrix_GF_3[x, 7] <- allModels_3_2[[x]][2, 3]
  val_matrix_GF_3[x, 8] <- allModels_3_2[[x]][2, 4]
  val_matrix_GF_3[x, 9] <- allModels_3_1[[x]]$adj.r.squared
}

## Calculating the averages of the alphas, the market betas, the corresponding p-values, and the adjusted R-squareds across the 50 market models
avg_val_matrix_GF_3 <- colMeans(val_matrix_GF_3)

# Analysis 2.3: Market models for GameFi coins using all three indices

## Looping over all dependent variables used for the second analysis and creating an new market model for each one, saving the models as "allModels_4_1"
allModels_4_1 = lapply(depVarList_2, function(x){
  summary(lm(formula = paste0("`", x, "` ~ crix_index + crypto_gaming_index + gaming_index"),
             data = ts_gamefi, na.action = na.omit))
})

## Re-creating the same new market models using HAC standard errors, saving the models as "allModels_4_2"
allModels_4_2 = lapply(depVarList_2, function(x){
  coeftest(lm(formula = paste0("`", x, "` ~ crix_index + crypto_gaming_index + gaming_index"),
             data = ts_gamefi, na.action = na.omit), vcov = sandwich)
})

## Naming the created models after the coin / dependent variable used in each model
names(allModels_4_1) = depVarList_2
names(allModels_4_2) = depVarList_2

## Creating a matrix to store the regression results
val_matrix_GF_4 <- matrix(0, nrow = 50, ncol = 17)
rownames(val_matrix_GF_4) <- depVarList_2[1:50]
colnames(val_matrix_GF_4) <- c("Value Intercept", "Std. Error Intercept", "t-stat Intercept", "p-value Intercept", "Value Crix Market Factor", "Std. Error Crix Market Factor", "t-stat Crix Market Factor", "p-value Crix Market Factor", "Value GameFi Market Factor", "Std. Error GameFi Market Factor", "t-stat GameFi Market Factor", "p-value GameFi Market Factor", "Value Gaming Stock Market Factor", "Std. Error Gaming Stock Market Factor", "t-stat Gaming Stock Market Factor", "p-value Gaming Stock Market Factor", "Adjusted R-squared")

## Storing the results of the new market models run on GameFi coins in the matrix
for(x in 1:50) {
  val_matrix_GF_4[x, 1] <- allModels_4_2[[x]][1, 1]
  val_matrix_GF_4[x, 2] <- allModels_4_2[[x]][1, 2]
  val_matrix_GF_4[x, 3] <- allModels_4_2[[x]][1, 3]
  val_matrix_GF_4[x, 4] <- allModels_4_2[[x]][1, 4]
  val_matrix_GF_4[x, 5] <- allModels_4_2[[x]][2, 1]
  val_matrix_GF_4[x, 6] <- allModels_4_2[[x]][2, 2]
  val_matrix_GF_4[x, 7] <- allModels_4_2[[x]][2, 3]
  val_matrix_GF_4[x, 8] <- allModels_4_2[[x]][2, 4]
  val_matrix_GF_4[x, 9] <- allModels_4_2[[x]][3, 1]
  val_matrix_GF_4[x, 10] <- allModels_4_2[[x]][3, 2]
  val_matrix_GF_4[x, 11] <- allModels_4_2[[x]][3, 3]
  val_matrix_GF_4[x, 12] <- allModels_4_2[[x]][3, 4]
  val_matrix_GF_4[x, 13] <- allModels_4_2[[x]][4, 1]
  val_matrix_GF_4[x, 14] <- allModels_4_2[[x]][4, 2]
  val_matrix_GF_4[x, 15] <- allModels_4_2[[x]][4, 3]
  val_matrix_GF_4[x, 16] <- allModels_4_2[[x]][4, 4]
  val_matrix_GF_4[x, 17] <- allModels_4_1[[x]]$adj.r.squared
}

## Calculating the averages of the alphas, the market betas, the corresponding p-values, and the adjusted R-squareds across the 50 market models
avg_val_matrix_GF_4 <- colMeans(val_matrix_GF_4)

#test variables for multicollinearity -> high correlation of 68% as expected btw. crypto_gaming_index and crix_index -> multicollinearity
#leads to higher standard errors and therefore less reliable statistical inferences
#for the regression to work you would need to drop one independent variable, combine the two or chose a different procedure. For the purpose of this study we will just split the regression up into two regressions
cor_mat <- cor(ts[,2:4], method ="pearson")

#creating two more models, where we test the significance of a general gaming index by regressing excess returns of gamefi coins on the crypto_gaming and the general_index separately in combination with the general gaming index
allModels_4_3 = lapply(depVarList_2, function(x){
  summary(lm(formula= paste0("`", x, "` ~ crix_index + gaming_index"), 
             data= ts_gamefi ,na.action = na.omit))
})

allModels_4_4 = lapply(depVarList_2, function(x){
  coeftest(lm(formula= paste0("`", x, "` ~ crix_index + gaming_index"), 
              data= ts_gamefi ,na.action = na.omit))
})

allModels_4_5 = lapply(depVarList_2, function(x){
  summary(lm(formula= paste0("`", x, "` ~ crypto_gaming_index + gaming_index"), 
             data= ts_gamefi ,na.action = na.omit))
})

allModels_4_6 = lapply(depVarList_2, function(x){
  coeftest(lm(formula= paste0("`", x, "` ~ crypto_gaming_index + gaming_index"), 
              data= ts_gamefi ,na.action = na.omit))
})


#Name the list of models to the column name
names(allModels_4_3) = depVarList_2
names(allModels_4_4) = depVarList_2
names(allModels_4_5) = depVarList_2
names(allModels_4_6) = depVarList_2

## Creating a matrix to store the regression results
val_matrix_GF_5 <- matrix(0, nrow = 50, ncol = 13)
rownames(val_matrix_GF_5) <- depVarList_2
colnames(val_matrix_GF_5) <- c("Value Intercept", "Std. Error Intercept", "t-stat Intercept", "p-value Intercept", "Value Crix Market Factor", "Std. Error Crix Market Factor", "t-stat Crix Market Factor", "p-value Crix Market Factor", "Value Gaming Stock Market Factor", "Std. Error Gaming Stock Market Factor", "t-stat Gaming Stock Market Factor", "p-value Gaming Stock Market Factor", "Adjusted R-squared")

## Storing the results of the new market models run on GameFi coins in the matrix
for(x in 1:50) {
  val_matrix_GF_5[x, 1] <- allModels_4_4[[x]][1, 1]
  val_matrix_GF_5[x, 2] <- allModels_4_4[[x]][1, 2]
  val_matrix_GF_5[x, 3] <- allModels_4_4[[x]][1, 3]
  val_matrix_GF_5[x, 4] <- allModels_4_4[[x]][1, 4]
  val_matrix_GF_5[x, 5] <- allModels_4_4[[x]][2, 1]
  val_matrix_GF_5[x, 6] <- allModels_4_4[[x]][2, 2]
  val_matrix_GF_5[x, 7] <- allModels_4_4[[x]][2, 3]
  val_matrix_GF_5[x, 8] <- allModels_4_4[[x]][2, 4]
  val_matrix_GF_5[x, 9] <- allModels_4_4[[x]][3, 1]
  val_matrix_GF_5[x, 10] <- allModels_4_4[[x]][3, 2]
  val_matrix_GF_5[x, 11] <- allModels_4_4[[x]][3, 3]
  val_matrix_GF_5[x, 12] <- allModels_4_4[[x]][3, 4]
  val_matrix_GF_5[x, 13] <- allModels_4_3[[x]]$adj.r.squared
}

## Calculating the averages of the alphas, the market betas, the corresponding p-values, and the adjusted R-squareds across the 50 market models
avg_val_matrix_GF_5 <- colMeans(val_matrix_GF_5)

coef <- list(c(avg_val_matrix_GF_5[["Value Crix Market Factor"]], avg_val_matrix_GF_5[["Value Gaming Stock Market Factor"]], avg_val_matrix_GF_5[["Value Intercept"]]))
se <- c(avg_val_matrix_GF_5[["Std. Error Crix Market Factor"]], avg_val_matrix_GF_5[["Std. Error Gaming Stock Market Factor"]],avg_val_matrix_GF_5[["Std. Error Intercept"]])
names <- c("crypto_gaming_index", "gaming_index", "Constant")
se <- list(names, se)
t <- c(avg_val_matrix_GF_5[["t-stat Crix Market Factor"]], avg_val_matrix_GF_5[["t-stat Gaming Stock Market Factor"]], avg_val_matrix_GF_5[["t-stat Intercept"]])
names <- c("crypto_gaming_index", "gaming_index", "Constant")
t <- list(names, t)
p <- c(avg_val_matrix_GF_5[["p-value Crix Market Factor"]], avg_val_matrix_GF_5[["p-value Gaming Stock Market Factor"]],avg_val_matrix_GF_5[["p-value Intercept"]])
names <- c("crypto_gaming_index", "gaming_index", "Constant")
p <- list(names, p)
covariate <- as.character(c("crypto_gaming_index", "gaming_index", "Constant"))
r.squared <- allModels_4_3[[x]]$adj.r.squared
stargazer(coef, se, t, p, dep.var.labels = "Excess returns", summary = FALSE,  title="Regression Results", align=TRUE, digits = 4, out= "avg_val_matrix_GF_5.htm", star.char = c(".", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), add.lines = list(c("r-squared", allModels_4_3[[x]]$adj.r.squared)))

create_table = function(table_)
{
  p_table = data.frame(Variables=NA, Values=NA)
  for(i in 1:dim(table_)[1])
  {
    est_error = table_[i,1:2] # Im assuming that you know that 2 first columns are the values that you want
    name = c(rownames(table_)[i], '') # The final table rownames
    name_values = cbind(Variables=name, Values=est_error)
    p_table = rbind(p_table, name_values)
  }
  rownames(p_table) = 1:dim(p_table)[1]
  return(na.omit(p_table))
}

ctable = data.frame(summary(lm(mpg~. , data = mtcars))$coefficients)
ctable %>% tidy %>% xtable()
output_table = create_table(ctable)




## Creating a matrix to store the regression results
val_matrix_GF_6 <- matrix(0, nrow = 50, ncol = 13)
rownames(val_matrix_GF_6) <- depVarList_2
colnames(val_matrix_GF_6) <- c("Value Intercept", "Std. Error Intercept", "t-stat Intercept", "p-value Intercept", "Value Crix Market Factor", "Std. Error Crix Market Factor", "t-stat Crix Market Factor", "p-value Crix Market Factor", "Value Gaming Stock Market Factor", "Std. Error Gaming Stock Market Factor", "t-stat Gaming Stock Market Factor", "p-value Gaming Stock Market Factor", "Adjusted R-squared")

## Storing the results of the new market models run on GameFi coins in the matrix
for(x in 1:50) {
  val_matrix_GF_6[x, 1] <- allModels_4_6[[x]][1, 1]
  val_matrix_GF_6[x, 2] <- allModels_4_6[[x]][1, 2]
  val_matrix_GF_6[x, 3] <- allModels_4_6[[x]][1, 3]
  val_matrix_GF_6[x, 4] <- allModels_4_6[[x]][1, 4]
  val_matrix_GF_6[x, 5] <- allModels_4_6[[x]][2, 1]
  val_matrix_GF_6[x, 6] <- allModels_4_6[[x]][2, 2]
  val_matrix_GF_6[x, 7] <- allModels_4_6[[x]][2, 3]
  val_matrix_GF_6[x, 8] <- allModels_4_6[[x]][2, 4]
  val_matrix_GF_6[x, 9] <- allModels_4_6[[x]][3, 1]
  val_matrix_GF_6[x, 10] <- allModels_4_6[[x]][3, 2]
  val_matrix_GF_6[x, 11] <- allModels_4_6[[x]][3, 3]
  val_matrix_GF_6[x, 12] <- allModels_4_6[[x]][3, 4]
  val_matrix_GF_6[x, 13] <- allModels_4_5[[x]]$adj.r.squared
}

## Calculating the averages of the alphas, the market betas, the corresponding p-values, and the adjusted R-squareds across the 50 market models
avg_val_matrix_GF_6 <- colMeans(val_matrix_GF_6)

#for both separate regressions the gaming_index wasn't significant

## Analysis 3: Axie Infinity Index
# For the third part of our analysis we try to identify idiosyncratic factors that influence excess returns of the biggest GameFi project Axie Infinity
# We chose Axie Infinity since it's the game with the most data available
# We get the data through MaxBrand99's Google sheet, where he aggregates hourly, daily and monthly data pulled through the Axie Infinity API (see https://docs.google.com/spreadsheets/d/1AoKqbrtFbuDKQ1UU2ESXZh980P61_wCg_p3j981TScM/edit)
# For the number of NFT holders, the total supply of NFTs, the floor price of the NFTs and the number of NFTs on the marketplace we calculated the delta and made it stationary by applying log, same as with the returns before
# The 24h trading volume of the NFTs we kept as an absolute number and scaled the data by dividing it with a million. This will provide for better looking (larger) coefficients and standard errors
# As a market index we include the CRIX index since Axie Infinity is historically by far the largest constituent of the gaming index and would therefore obviously have a very large correlation with the index, which wouldn't allow for an analysis of idiosyncratic factors
# We saved the data in "Clean_data_reg.xlsx" within the "Axie data 2" worksheet. We will use that data for the further analysis

data_reg_2 <- read_excel("Clean_data_reg.xlsx", sheet = "Axie data 2")
data_reg_2 <- na.omit(data_reg_2)

#convert to timeseries
date <- as.Date(data_reg_2$date)
ts_2 <- xts(x = data_reg_2[-1], order.by = date)
remove(data_reg_2, date)

#test variables for correlation -> high correlation of around 70% btw. axie total supply delta and axie holders delta. We therefore had to be careful not to include both variables in the same model
#returns correlation matrix while excluding rd and axie_24hvolume_usd
corr_matrix_axie <- cor(ts_2[, !names(ts_2) %in% c("rf", "axie_24hvolume_usd")] , method ="pearson")
stargazer(corr_matrix_axie, title="Correlation Matrix", out= "corr_matrix_axie.htm")

#testing the full model to see which variables might be of interest. In both models axie_24hvolume_scaled and axies_on_marketplace had at least some significance
reg_axie_1 <- lm (axie_exc_returns ~ crix_index + axie_24hvolume_scaled + axie_holders_delta  + axie_floor_price_delta + axies_on_marketplace, data =ts_2)
summary(reg_axie_1)

reg_axie_2 <- lm (axie_exc_returns ~ crix_index + axie_24hvolume_scaled + axie_total_supply_delta  + axie_floor_price_delta + axies_on_marketplace, data =ts_2)
summary(reg_axie_2)

#axie floor price delta seemed very insignificant in both models, we dropped the variable. Axie total supply delta is at least in the first model very close to significance at 10%, we test the model again
reg_axie_3 <- lm (axie_exc_returns ~ crix_index + axie_24hvolume_scaled + axie_total_supply_delta + axies_on_marketplace, data =ts_2)
#same as before. axie_total_supply delta didn't become more significant
summary(reg_axie_3)

#same regression as before but we exchange axie_total_supply delta with axie holders delta since the two are highly correlated
reg_axie_4 <- lm (axie_exc_returns ~ crix_index + axie_24hvolume_scaled + axie_holders_delta + axies_on_marketplace, data =ts_2)
#as before axie_holders_delta is not of significance
summary(reg_axie_4)

#we now test models with only 2 variables. ideally one variable that explains the supply side and one variable that explains the demand side of the gamefi project
#the supply variable is axies_on_marketplace, the demand variable is axie_24hvolume_scaled Axie holders delta is somehow a mix btw. the two (they represent supply since axie holders * axies per holder = total supply but also satisfied demand, since they're the ones buying the Axies)
reg_axie_5 <- lm (axie_exc_returns ~ crix_index + axie_24hvolume_scaled + axies_on_marketplace, data =ts_2)
#all three variables are significant at the 5% level
summary(reg_axie_5)

reg_axie_6 <- lm (axie_exc_returns ~ crix_index + axie_holders_delta + axies_on_marketplace, data =ts_2)
#also all three significant at 5% level
summary(reg_axie_6)

#the model with HAC standard errors suggests that axie_24hvolume_scaled is the only relevant variable. we therefore also test this one alone
reg_axie_7 <- lm (axie_exc_returns ~ crix_index + axie_24hvolume_scaled, data =ts_2)
#implies significance at the 5% level
summary(reg_axie_7)

#checking for linearity -> all the models look more or less linear (relationship btw. the variables can be expressed by a linear model)
plot(reg_axie_1, 1)
plot(reg_axie_2, 1)
plot(reg_axie_3, 1)
plot(reg_axie_4, 1)
plot(reg_axie_5, 1)
plot(reg_axie_6, 1)
plot(reg_axie_7, 1)

#test for heteroskestadicity -> all p-values are a lot lower than 0.01, the model therefore has heteroskedasticity, standard errors need to be corrected for that
bptest(reg_axie_1)
bptest(reg_axie_2)
bptest(reg_axie_3)
bptest(reg_axie_4)
bptest(reg_axie_5)
bptest(reg_axie_6)
bptest(reg_axie_7)

#test for autocorrelation of residuals
#residuals are not at all autocorrelated
durbinWatsonTest(reg_axie_1)
durbinWatsonTest(reg_axie_2)
durbinWatsonTest(reg_axie_3)
durbinWatsonTest(reg_axie_4)
durbinWatsonTest(reg_axie_5)
durbinWatsonTest(reg_axie_6)
durbinWatsonTest(reg_axie_7)

#calculate HAC (heteroskedasticity- and autocorrelation-consistent) SE to be added to stargazer table
reg_axie_1_hac <- coeftest(reg_axie_1, vcov. = sandwich)
reg_axie_2_hac <- coeftest(reg_axie_2, vcov. = sandwich)
reg_axie_3_hac <- coeftest(reg_axie_3, vcov. = sandwich)
reg_axie_4_hac <- coeftest(reg_axie_4, vcov. = sandwich)
reg_axie_5_hac <- coeftest(reg_axie_5, vcov. = sandwich)
reg_axie_6_hac <- coeftest(reg_axie_6, vcov. = sandwich)
reg_axie_7_hac <- coeftest(reg_axie_7, vcov. = sandwich)

#extract standard errors and p-values from coeftest
ses <- list(reg_axie_1_hac[,2], reg_axie_2_hac[,2], reg_axie_3_hac[,2], reg_axie_4_hac[,2], reg_axie_5_hac[,2], reg_axie_6_hac[,2], reg_axie_7_hac[,2])
ps <- list(reg_axie_1_hac[,4], reg_axie_2_hac[,4], reg_axie_3_hac[,4], reg_axie_4_hac[,4], reg_axie_5_hac[,4], reg_axie_6_hac[,4], reg_axie_7_hac[,4])

stargazer(reg_axie_1 , reg_axie_2, reg_axie_3, reg_axie_4, reg_axie_5, reg_axie_6, reg_axie_7, title="Results", align=TRUE, digits = 4, se = ses, p=ps, out= "axie_reg.htm", star.char = c(".", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001))

## After correcting all the regressions for heteroskedasticity and autocorrelation only the crix index and the axie_24hvolume_scaled remain as relevant variables  

