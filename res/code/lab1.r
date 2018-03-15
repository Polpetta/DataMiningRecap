# Clear the workspace
rm(list = ls())
# Upload dataset Boston inside library MASS (506 rows and 14 columns/variables)
library(MASS)
data(Boston)
# Help of something
help(Boston)
?Boston
# Vectors, matrices, data frames access: []; lists access: $
# Shows the first line
Boston[1,]
# Shows the first three elements
Boston[c(1,2,3),]
Boston[1:3,]
# Dimension of the dataset (shows rows and columns)
dim(Boston)
# n = number of observations/subjects/houses
n <- dim(Boston)[1]
ls()
# Start with Y=medv
# Summary prints min, max, mean and median and quartiles
# The first quartile leaves on his left the 25% of the data data distribution
# The median leaves on his left and on his right the 50% of the data distribution
summary(Boston$medv)
# histogram and boxplot
hist(Boston$medv, prob=TRUE)
# Change labels of x-axis, y-axis and title of the graph
hist(Boston$medv, prob=TRUE, xlab='Median price of the houses', ylab='Density', main='Histogram')
# Boxplot
boxplot(Boston$medv)
# Y = medv; X = lstat
# dispersion plot; cex defines size of points, pch defines shape of points
plot(Boston$lstat, Boston$medv, cex =0.5, pch=19, main='Dispersion plot')
# We expect a negative correlation between lstat and medv
> cor(Boston$lstat, Boston$medv)
# Computes beta1 and beta0
beta1 <- cov(Boston$lstat, Boston$medv)/var(Boston$lstat)
beta0 <- mean(Boston$medv) - beta1*mean(Boston$lstat)
# No need to specify Boston$medv, Boston$lstat here:
model <- lm(medv ~ lstat, data=Boston)
model
# summary provides informations on the model (including beta0, beta1, RSE and R-squared/explained variance)
summary(model)
