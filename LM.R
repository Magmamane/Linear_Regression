# Simple Linear Regression

# Importing the dataset
d<-read.csv("C:/Users/magma/Documents/budget.csv")

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools) 
set.seed(123)
split = sample.split(d$Sales, SplitRatio = 2/3)

training_set = subset(d, split == TRUE)
training_set
test_set = subset(d, split == FALSE)
test_set
s_f=lm(training_set$Sales~training_set$Spend, data=training_set)
s_f
# Fitting Simple Linear Regression to the Training set
#lm(formula=dependent_variable or independent_variable, data = dataset)
sf = lm(Sales~Spend, data=d)
summary(sf)


#Performance Measures:
#Residual Multiple /Adjusted R-squared F-statistic:
#
# Visualising dataset
plot(sf$resid~d$Spend)
plot(sf$resid~d$Spend[order(d$Spend)],
     main="Spend x Residuals\nfor Simple Regression",
     xlab="Marketing Spend", ylab="Residuals")
abline(h=0,lty=2)
#Histogram of Residuals
hist(sf$resid, main="Histogram of Residuals",
     ylab="Residuals")
#Q-Q Plot
qqnorm(sf$resid)
qqline(sf$resid)
# Fitting Simple Linear Regression to the Training set
mf = lm(Sales~Spend+Month, data=d)
summary(mf)

layout(matrix(c(1,2,3,4),2,2,byrow=T))
plot(multi.fit$fitted, rstudent(multi.fit),
     main="Multi Fit Studentized Residuals",
     xlab="Predictions",ylab="Studentized Resid",
     ylim=c(-2.5,2.5))
abline(h=0, lty=2)
plot(dataset$Month, multi.fit$resid,
     main="Residuals by Month",
     xlab="Month",ylab="Residuals")
abline(h=0,lty=2)
hist(multi.fit$resid,main="Histogram of Residuals")
qqnorm(multi.fit$resid)
qqline(multi.fit$resid)

# Visualising the Training set results
install.packages('ggplot2')
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$Spend, y = training_set$Sales),
             colour = 'red') +
  geom_line(aes(x = training_set$Spend, y = predict(sf, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Salary vs Experience (Training set)') +
  xlab('Spend') +
  ylab('Sales')

