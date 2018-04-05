library(ISLR)
data("Carseats")
dim(Carseats)
# features of the data set
names(Carseats)
# Y = Sales; X = Price, Urban, US, ShelveLoc : only one
# of the covariates is continuous (Sales)
# US = 'No' and ShelveLoc = 'Bad' are the baseline level
# (see the dummies variables associated) R starts from the baseline level (starting level)
my.data <- Carseats[,c('Sales','Price','ShelveLoc','Urban','US')]
dim(my.data)
my.data[1:3,]
# the following boxplot gives the sales distribution (notice the 2 outliers)
boxplot(my.data$Sales)
# there is a kind of inverse relationship between sales and price
# the variability of sales is not related to price
plot(my.data$Price, my.data$Sales, xlab='Price', ylab='Sales')
# relationship between sales and urban: no significant relation
# R creates a dummy variable with 2 values
boxplot(my.data$Sales ~ my.data$Urban)
# US has 2 levels; R creates a dummy variables with 2 values
boxplot(my.data$Sales ~ my.data$US)
# ShelveLoc is significant for sales
# boxplot shows 3 different boxes with different locations
# R creates a dummy variable for the 3 levels with 3 values
# first has no significance, second has significance, third no significance.
boxplot(my.data$Sales ~ my.data$ShelveLoc)
# interaction plots are useful to see if there are some interactions between variables
# there is no significant interaction between price and urban
plot(my.data$Price, my.data$Sales)
points(my.data$Price[my.data$Urban=='Yes'], my.data$Sales[my.data$Urban=='Yes'], col='red')
legend('bottomleft',col=c('black', 'red'),legend=c('Urban=No', 'Urban=Yes'),pch=c(19,19))
# repeat for US
plot(my.data$Price, my.data$Sales, col=my.data$US)
legend('bottomleft',col=c('black','red'), legend=c('US=No', 'US=Yes'), pch=c(21,21))
# it seems that there is a pattern between sales and shelveloc
# (see the plot horizontally) so there is an interaction
# is there a pattern between price and shelveloc? No -> there is no interaction
plot(my.data$Price, my.data$Sales, col=my.data$ShelveLoc)
legend('bottomleft',col=c('black','red','green'), legend=c('Bad','Good','Medium'), pch=c(21,21))
# baseline levels are all insert into the intercept of the model
m <- lm(Sales ~ Price + Urban + US + ShelveLoc, data=Carseats)
# Urban is no relevant so the model can be simplified
m2 <- lm(Sales ~ Price + US + ShelveLoc, data=Carseats)
# or m2 <- update(m, . ~ . -Urban)
# analysis of the variance table
anova(m2,m)
# let's observe the interaction by adding a covariate (Price* ShelveLoc)
m3 <- lm(Sales ~ US + Price* ShelveLoc, data=my.data)
# or m3 <- update(m2, . ~ . + ShelveLoc: Price)
# the summary prints also the dummies variables
# the interaction between shelveLoc and price is not significant
summary(m3)


