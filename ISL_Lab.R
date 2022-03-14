#1.a ISLR 2.4 Applied Problem 8
#setting the directory
setwd("/Users/bunnysai/Desktop")
#reading the file from the directory
college <- read.csv("/Users/bunnysai/Desktop/R/College.csv")
head(College)
#1.b Now you should see that the first data column is Private. Note that another column labeled row.names now appears before the
#Private column. However, this is not a data column but rather the name that R is giving to each row.
#Here we used the head () to know the variables before modification and we removed one variable $X and by using the fix () we fix the college data.
rownames (college) <- college[, 1]
View (college)
college <- college[, -1]
head(college)
View (college)
#1.c.i Use the summary() function to produce a numerical summary of the variables in the data set
#summary () it is used to produce a numerical summary of variables in the collage dataset.
summary(college)
#1.c.ii. Use the pairs() function to produce a scatterplot matrix of the first ten columns or variables of the data. 
#Recall that you can reference the first ten columns of a matrix A using A[,1:10].
#pairs () function to produce a scatterplot matrix of the first ten columns or variables of the college dataset. 
college$Private <- as.factor(college$Private)
#pairs(college[, 1:10])
head(college)


#1.c.iii. Use the plot() function to produce side-by-side boxplots of Outstate versus Private.
#plot () function to produce side-by-side boxplots of Outstate versus Private. 
plot(college$Private, college$Outstate, xlab = "Private", ylab = "Out-of-state tuition in dollars")


#iv. Create a new qualitative variable, called Elite, by binning the Top10perc variable. We are going to divide universities
#into two groups based on whether or not the proportion of students coming from the top 10 % of their high school classes exceeds 50 %.
#Creating a quantitative variable brilliant, by starting the Top10perc and boxplot of outstate versus brilliant
Elite <- rep("No", nrow(college))
Elite[college$Top10perc > 50] <- "Yes"
Elite <- as.factor(Elite)
college <- data.frame(college, Elite)
summary(Elite)
plot(college$Elite, college$Outstate, xlab = "Elite University", ylab = "Tuition in $")
#1.c.v. Use the hist() function to produce some histograms with differing numbers of bins for a few of the quantitative variables. You may find the command par(mfrow = c(2, 2))
#useful: it will divide the print window into four regions so that four plots can be made simultaneously. Modifying the
#arguments to this function will divide the screen in other ways.
par(mfrow=c(2,2))
hist(college$Apps, xlab = "Applications Received", col="red",main = "")
hist(college$perc.alumni, col=2, xlab = "Perc of alumni who donate",main = "")
hist(college$S.F.Ratio, col=3, breaks=10, xlab = "Student/Faculty ratio",main = "")
hist(college$Expend, breaks=100, xlab = "Instructional expenditure per student", col="green",main = "")
#1.c.vi. Continue exploring the data, and provide a brief summary of what you discover.
summary(college$Apps)
summary(college$PhD)
row.names(college)[which.max(college$Top10perc)]
acceptance_rate <- college$Accept / college$Apps
row.names(college)[which.min(acceptance_rate)] 
plot(college$Outstate, college$Grad.Rate)

#2.a Which of the predictors are quantitative, and which are qualitative?
# we are loading the Auto dataset and finding which are qualitative and quantitative. 
Auto <- read.csv("/Users/bunnysai/Desktop/R/Auto.csv",na.strings="?")
Auto <- na.omit(Auto)
str(Auto)
#Except NAME and HORSEPOWER remaining are quantitative 


#2.b What is the range of each quantitative predictor? You can answer this using the range() function.
#We are retrieving the range of each quantitative redictors by using range() function.
sapply(Auto[, -c(4, 9)], mean)
qualitative_columns <- which(names(Auto) %in% c("name", "origin", "origins"))
qualitative_columns
sapply(Auto[, -qualitative_columns], range)
#2.(c) What is the mean and standard deviation of each quantitative predictor?
sapply(Auto[, -qualitative_columns], mean)
sapply(Auto[, -qualitative_columns], sd)
#2.(d) Now remove the 10th through 85th observations. What is the range, mean, and standard deviation of each 
#predictor in the subset of the data that remains?
subset <- Auto[-c(10:85), -c(4,9)]
sapply(subset, range)
sapply(subset, mean)
sapply(subset, sd)
#2.e) Using the full data set, investigate the predictors graphically,using scatterplots or other tools of your choice.
#Create some plots highlighting the relationships among the predictors. Comment on your findings
#pairs(Auto)
pairs(Auto[, -qualitative_columns])
with(Auto, plot(mpg, weight))
with(Auto, plot(mpg, cylinders))
# for 20 observations
Auto.sample <- Auto[sample(1:nrow(Auto), 20), ]
# ordering them
Auto.sample <- Auto.sample[order(Auto.sample$mpg), ]
# plot by using "dotchart"
with(Auto.sample, dotchart(mpg, name, xlab = "mpg"))
with(Auto, plot(origin, mpg), ylab = "mpg")
#*****Findings**********
#With mpg we are seeing an inverse effect for horsepower, weight and displacement
#mpg has increased drastically over years and it has twice in a decade.
#European and US cars have lower mpg than Japanese cars
#2.f) Suppose that we wish to predict gas mileage (mpg) on the basis of the other variables. Do your plots suggest that any of the
#other variables might be useful in predicting mpg? Justify your answer.
#Based on the scatter plots I made in part 5 which relate miles per gallon to the predictor’s engine displacement, horsepower, car weight, and model year, it seems as if the first three factors would be most helpful in predicting mpg, with model year still potentially being helpful but less so. There are clear relationships that increasing engine displacement/horsepower/car weight results in decreased fuel efficiency. 
#There is also a weak relationship that fuel efficiency generally increased going from 1970 to 1982.
Auto$horsepower <- as.numeric(Auto$horsepower)
cor(Auto$weight, Auto$horsepower)
cor(Auto$weight, Auto$displacement)
cor(Auto$displacement, Auto$horsepower)
#For predictors we can use origin, cylinders, year and hoursepower
#We cannot use weight and displacement as they are correlated highly with each other and also with horsepower.


#3(a) To begin, load in the Boston data set. The Boston data set is part of the ISLR2 library.
#How many rows are in this data set? How many columns? What do the rows and columns represent?
#Loading Dataset
library(MASS)
Boston$chas <- as.factor(Boston$chas)
nrow(Boston)
ncol(Boston)
#rows indicate the U.S Census Tracts in Boston area
#column indicate the measures of census 
#3.(b) Make some pairwise scatterplots of the predictors (columns) in this data set. Describe your findings.
#Scatterplots of the predictors 
par(mfrow = c(2, 2))
plot(Boston$nox, Boston$crim)
plot(Boston$rm, Boston$crim)
plot(Boston$age, Boston$crim)
plot(Boston$dis, Boston$crim)
pairs(Boston)

#3.(c) Are any of the predictors associated with per capita crime rate? If so, explain the relationship.
par(mfrow = c(2, 2))
plot(Boston$crim ~ Boston$zn, log = 'xy', col = 'yellow')
plot(Boston$crim ~ Boston$age, log = 'xy', col = 'yellow')
plot(Boston$crim ~ Boston$dis, log = 'xy', col = 'blue')
plot(Boston$crim ~ Boston$lstat, log = 'xy', col = 'blue')

#3.d) Do any of the census tracts of Boston appear to have particularly high crime rates? Tax rates? 
hist(Boston$crim, breaks = 50)
nrow(Boston[Boston$crim > 20, ])
#tax rate
hist(Boston$tax, breaks = 50)
nrow(Boston[Boston$tax == 666, ])
#Pupil-teacher ratios
hist(Boston$ptratio, breaks = 50)
nrow(Boston[Boston$ptratio > 20, ])
#3.e) How many of the census tracts in this data set bound the Charles river?
nrow(Boston[Boston$chas == 1, ])
#3.f) What is the median pupil-teacher ratio among the towns in this data set?
median(Boston$ptratio)


#3.g) Which census tract of Boston has lowest median value of owneroccupied homes? What are the values of the other predictors
#for that census tract, and how do those values compare to the overall ranges for those predictors? Comment on your findings.
#In summary, these two tracts with the lowest median value of owner-occupied homes have predictors generally at the extreme ends of their respective ranges.
t(subset(Boston,medv==min(Boston$medv)))
row.names(Boston[min(Boston$medv), ])
range(Boston$tax)
Boston[min(Boston$medv), ]$tax

#3.h) In this data set, how many of the census tracts average more than seven rooms per dwelling? More than eight rooms per
#dwelling? Comment on the census tracts that average more than eight rooms per dwelling.
#In this data set, there are 64 tracts which average more than seven rooms per dwelling, and 13 of those tracts which average more than 8 rooms per dwelling.
nrow(Boston[Boston$rm > 7, ])
nrow(Boston[Boston$rm > 8, ])


#4.a.i Is there a relationship between the predictor and the response?
library(ISLR)
library(MASS)
data("Auto")
head(Auto)
lm.fit<-lm(mpg~horsepower,data=Auto)
summary(lm.fit)
#Yes we do have relation between predictor and response because p value is 2e-16
#4.a.ii. To calculate the leftover blunder relative to the reaction we utilize the cruel of the reaction and the RSE. The cruel of mpg is 23.4459184. The RSE of the lm.fit was 4.9057569 which demonstrates a rate mistake of 20.9237141%. We may moreover
#note that as the R2 is rise to 0.6059483, nearly 60.5948258% of the inconstancy in “mpg” can be clarified utilizing “horsepower”. 
#4.a.iii. Is the relationship between the predictor and the response positive or negative?
#As the coefficient of “horsepower” is negative, the relationship is additionally negative. The more drive a vehicle has the direct relapse shows the less mpg fuel proficiency the vehicle will have. 
#4.a.iv. What is the predicted mpg associated with a horsepower of 98? What are the associated 95 % confidence and prediction intervals?
predict(lm.fit,data.frame(horsepower=c(98)),interval="prediction")
predict(lm.fit,data.frame(horsepower=c(98)),interval="confidence")
#4.b Plot the response and the predictor. Use the abline() function to display the least squares regression line.
attach(Auto)
plot(horsepower,mpg)
abline(lm.fit,lwd=5,col="blue")
#4.c) Use the plot() function to produce diagnostic plots of the least squares regression fit. 
#Comment on any problems you see with the fit.
which.max(hatvalues(lm.fit))
par(mfrow = c(2,2))
plot(lm.fit)
#Looking at the Residuals vs. Fitted plot, there is a clear U-shape to the residuals, 
#which is a strong indicator of non-linearity in the data. This, when combined with an inspection of the plot in Part 2, 
#tells us that the simple linear regression model is not a good fit. In addition, when looking at the Residuals vs. Leverage plot, 
#there are some high leverage points (remember that after dropping the rows with null values, there are 392 observations in the data set, giving an average leverage value of 2/392≈0.00512/392≈0.0051) which also have high standardized residual values (greater than 2), which is also of concern for the simple linear regression model. There are also a number of observations with a standardized residual value of 3 or more, 
#which is evidence to suggest that they would be possible outliers if we didn't already have the suspicion that the data is non-linear.


#5.a) Produce a scatterplot matrix which includes all of the variables in the data set.
pairs(Auto)
#Since origin and name are categorical columns, I'm excluding them from the scatterplot matrix.
#5.b) Compute the matrix of correlations between the variables using
#the function cor(). You will need to exclude the name variable, cor() which is qualitative.
#Since the origin column is also qualitative, I excluded it along with the name column when computing the matrix of correlations.
Auto$name<-NULL
cor(Auto,method = c("pearson"))
#5.c.i. Is there a relationship between the predictors and the response?
lm.fit<-lm(mpg~.,data=Auto)
summary(lm.fit)
#The F-statistic vale shows that we have a relationship between predictors and the response.
#5.c.ii. Which predictors appear to have a statistically significant relationship to the response?
#Able to reply this address by checking the p-values related with each predictor’s t-statistic. We may conclude that all indicators are measurably noteworthy but “cylinders”, “horsepower” and “acceleration”. 
#5.c.iii. What does the coefficient for the year variable suggest?
#The coefficient to the “year” variable recommends that the normal impact of an increment of 1 year is an increment of 0.7507727 in “mpg” (all other indicators remaining steady).
#In other words, cars gotten to be more fuel effective each year by nearly 1 mpg / year. 
#5.d) Use the plot() function to produce diagnostic plots of the linear regression fit. Comment on any problems you see with the fit.
#Do the residual plots suggest any unusually large outliers? Does the leverage plot identify any observations with unusually high leverage?
which.max(hatvalues(lm.fit))
par(mfrow = c(2,2))
plot(lm.fit)
#Looking at the Residuals vs. Fitted plot, there appears to be moderate U-shape, which indicates that there might be non-linearity in the data. 
#In addition, when looking at the Residuals vs. Leverage plot we can observe a few things. First,
#there are a number of observations with standardized residual values with absolute value greater than or equal to 3. 
#Those are likely outliers. This is confirmed by looking at the Scale-Location plot, which has √|Standardized residual| as the y-axis. 
#Points with √Standardized residual≥1.732 have |Standardized residual|≥3, which again means that they are likely outliers. Going back the Residuals vs. Leverage plot, we also see that there are a couple points with unusually high leverage. Again, remember that after dropping the rows with null values, there are 392 observations in the data set, giving an average leverage value of 9/392≈0.0239/392≈0.023. There is one point with a leverage value of about 0.10, which is almost 5 times greater than the average.
#There is another point with a leverage of about 0.20, which is almost 10 times greater than the average.
#5 .e)
#Here, the interaction term between horsepower and originEuropean has the highest p-value, so we remove the interaction between horsepower and origin from the model to proceed with backward selection. We're going to do this until the adjusted R2 value no longer increases when we remove predictors. The adjusted R2 value is an adjustment of the regular R2 value to take into account the number of predictors and is a way to compare the fit of models with different numbers of predictors, 
#as the regular R2 always increases with the inclusion of more predictors.
lm.fit = lm(mpg ~.-name+displacement:weight, data = Auto)
summary(lm.fit)
#We have statistically significant interaction between displacement and weight for p-vales.
#But we do not have any interaction between displacement and cylinders
#5.f) Try a few different transformations of the variables, such as log(X), ???X, X2. Comment on your findings.
par(mfrow = c(2, 2))
plot(log(Auto$horsepower), Auto$mpg)
plot(sqrt(Auto$horsepower), Auto$mpg)
plot((Auto$horsepower)^2, Auto$mpg)
lm.fit = lm(mpg ~.-name+I((displacement)^2)+log(displacement)+displacement:weight, data = Auto)
summary(lm.fit)
#To get a sense of which transformations I want to try out for each quantitative variable, focusing on displacement, horsepower and weight, I'll look at the scatterplots of each one versus mpg.
#The book already explored nonlinear transformations of horsepower to predict mpg, so I will first look at transforms of acceleration.


#6.a) Fit a multiple regression model to predict Sales using Price, Urban, and US.
library("ISLR")
?Carseats
head(Carseats)
str(Carseats)
lm.fit = lm(Sales ~ Price+Urban+US, data= Carseats)
summary(lm.fit)
#6.b) Provide an interpretation of each coefficient in the model. Be careful-some of the variables in the model are qualitative!
#The coefficient of -0.054459 for Price means that, for a given location (i.e. fixed values of Urban and US), 
#increasing the price of a car seat by $1 results in a decrease of sales by approximately 54.46 units, on average, in the model.
#The coefficient of -0.021916 for UrbanYes means that, for a given carseat price point and value of US, 
#the model predicts urban areas to have approximately 22 fewer carseat sales on average compared to non-urban areas. 
#The coefficient of 1.200573 for USYes means that, for a given car seat price point and value of Urban,
#the model predicts that stores in the United States have 1201 more carseat sales on average than stores outside the United States.
#6.c) Write out the model in equation form, being careful to handle the qualitative variables properly.
#Here, y^ is the estimated carseat sales, in thousands of car seats; x1jx1j is the price of the carseat at the jth store, in dollars; and x2j and x3j are dummy variables to represent whether or not the jth store at is located in an urban area and in the United States, respectively. 
#More concretely, x2j and x3j use the following coding scheme.
#6.d) For which of the predictors can you reject the null hypothesis H0 : ??j = 0?
#The p-values for the intercept, Price, and US Yes are all essentially zero, which provides strong evidence to reject the null hypothesis H0:βj=0 for those predictors. 
#The p-value for UrbanYes, however, is 0.936, so there is no evidence to reject the null hypothesis that it has a non-zero coefficient in the true relationship between the predictors and Sales.
#6.e) On the basis of your response to the previous question, fit a smaller model that only uses the predictors for which there is
#evidence of association with the outcome.
fits <- lm(Sales ~ Price + US, data = Carseats)
summary(fits)
#6.f) How well do the models in (a) and (e) fit the data?
#The models in Part 1 and Part 5 both fit the data about equally well, with identical R2 values of 0.2393. In addition, when comparing the diagnostic plots between the two models, 
#there isn't any discernable visual differences that would strongly indicate that one model is a better fit than the other.
#6.g) Using the model from (e), obtain 95 % confidence intervals for the coefficient(s).
confint(fits)
#6.h) Is there evidence of outliers or high leverage observations in the model from (e)?
#When we look at the residuals vs. leverage plot for the model from Part 5 that I generated in Part 6, 
#we see that there are a number of observations with standardized residuals close to 3 in absolute value. Those observations are possible outliers. We can also see in the same plot that there are number of high leverage points with leverage values greatly exceeding the average leverage of 3/400=0.0075, 
#though those high leverage observations are not likely outliers, as they have studentized residual values with absolute value less than 2.
par(mfrow = c(2, 2))
plot(fits)

