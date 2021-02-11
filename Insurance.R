######################## INSURANCE PREDICTION DATA MODELS##############################

######################### LOAD LIBRARIES ##########################
library(magrittr)
library(car)
library(broom)
library(ggplot2)
library(gridExtra)
library(knitr)

getwd()
alldata <- read.csv(file = "insurance.csv") 
summary(alldata)

str(alldata)

####################### Data Distribution #######################

#Plotting raw data to see the distribution of the data

attach(alldata)
par(mfrow=c(3,4))
qq_plot1 <- qqnorm(alldata$age,pch = 1, frame = FALSE,xlab = "age")
qq_plot1 <- qqline(alldata$age, col = "steelblue", lwd = 2)

qq_plot2 <- qqnorm(as.numeric(alldata$sex), pch = 1, frame = FALSE,xlab = "sex")
qq_plot2 <- qqline(as.numeric(alldata$sex), col = "steelblue", lwd = 2)

qq_plot3 <- qqnorm(alldata$bmi, pch = 1, frame = FALSE,xlab = "bmi")
qq_plot3 <- qqline(alldata$bmi, col = "steelblue", lwd = 2)

qq_plot4 <- qqnorm(alldata$children, pch = 1, frame = FALSE,xlab = "children")
qq_plot4 <- qqline(alldata$children, col = "steelblue", lwd = 2)

qq_plot5 <- qqnorm(as.numeric(alldata$smoker), pch = 1, frame = FALSE,xlab = "smoker")
qq_plot5 <- qqline(as.numeric(alldata$smoker), col = "steelblue", lwd = 2)

qq_plot6 <- qqnorm(as.numeric(alldata$region), pch = 1, frame = FALSE,xlab = "region")
qq_plot6 <- qqline(as.numeric(alldata$region), col = "steelblue", lwd = 2)

qq_plot7 <- qqnorm(alldata$charges, pch = 1, frame = FALSE,xlab = "charges")
qq_plot7 <- qqline(alldata$charges, col = "steelblue", lwd = 2)

####################### Data Cleaning ##########################

#find columns which have NA and need to be imputed
x <- unlist(lapply(alldata, function(x)
  any(is.na(x))))
x


# Plotting Boxplots to View Outliers in Important Variables

boxplot_column1 <-
  qplot(
    x = alldata$age,
    y = alldata[, 1],
    geom = "boxplot" ,
    xlab = "",
    ylab = "age"
  )
boxplot_column2 <-
  qplot(
    x = alldata$sex,
    y = alldata[, 2],
    geom = "boxplot" ,
    xlab = "",
    ylab = "sex"
  )
boxplot_column3 <-
  qplot(
    x = alldata$bmi,
    y = alldata[, 3],
    geom = "boxplot" ,
    xlab = "",
    ylab = "bmi"
  )
boxplot_column4 <-
  qplot(
    x = alldata$children,
    y = alldata[, 4],
    geom = "boxplot" ,
    xlab = "",
    ylab = "Children"
  )
boxplot_column5 <-
  qplot(
    x = alldata$smoker,
    y = alldata[, 5],
    geom = "boxplot" ,
    xlab = "",
    ylab = "Smoker"
  )
boxplot_column6 <-
  qplot(
    x = alldata$region,
    y = alldata[, 6],
    geom = "boxplot" ,
    xlab = "",
    ylab = "Region"
  )

grid.arrange(
  boxplot_column1,
  boxplot_column2,
  boxplot_column3,
  boxplot_column4,
  boxplot_column5,
  boxplot_column6,
  ncol = 6
)

#Removing Ouliers determined using the Boxplot#
outliers <- boxplot(alldata$bmi, plot=FALSE)$out
print(outliers)

alldata<-alldata[-which(alldata$bmi %in% outliers),]

qplot(
  x = alldata$bmi,
  y = alldata[, 3],
  geom = "boxplot" ,
  xlab = "",
  ylab = "bmi"
)

#Running initial Linear Regression model #

lm.fit <- lm(formula = charges~., data = alldata) 
#Here '.' means we are using all the predictors in the dataset.

summary(lm.fit)

#Performing mixed selection for performing feature selection
step.lm.fit <- MASS::stepAIC(lm.fit, direction = "both", 
                             trace = FALSE)
#Perform Multicollinearity check
vif(step.lm.fit) %>%    
  knitr::kable()

#Standardized residual plot to determine the improvement in the predicted model
residualPlot(step.lm.fit, type = "rstandard")

#Non-linearity check
ceresPlots(step.lm.fit)

#Transforming Non-linearity of BMI
step.lm.fit.new <- update(step.lm.fit, .~.+I(bmi^1.25))  
ceresPlots(step.lm.fit.new)

####################### Significance Tests #######################

####### ANOVA #####

#ANOVA test to verify the statistcial importance of the variables used
anova(step.lm.fit, step.lm.fit.new, test = "F")

#Residual Plot verification
residualPlot(step.lm.fit.new, type = "rstandard")

#Modifying the step fit to fit the data better
lm.fit1 <- update(step.lm.fit.new, ~ .+bmi*smoker)
residualPlot(lm.fit1, type = "rstandard", id=TRUE)

#ANOVA test
anova(step.lm.fit.new, lm.fit1, test = "F")

#checking adjusted R-square value
summary(lm.fit1)$adj.r.squared

####### F-Test #####



######################fitted values of initial model########################################

#Creating function for applying ggplot on intial and predicted mdoel
fitted_vs_actual <- function(predictions, title){
  ggplot(predictions, aes(x=alldata$charges, y=fit))+
    geom_point()+
    geom_smooth(aes(color = 'model'))+
    geom_line(aes(x=seq(min(alldata$charges),max(alldata$charges), length.out = 1329), 
                  y=seq(min(alldata$charges),max(alldata$charges), length.out = 1329), 
                  color = 'ideal'))+
    labs(x="actual charges", y="fitted values") + 
    scale_color_manual('linear relation', values = c('red', 'blue')) +
    theme(legend.position = c(0.25, 0.8))+
    ggtitle(title)
}

#Predicting the intitally fitted linear regressions model
fitted_init <- predict(lm.fit, alldata, 
                       interval = "confidence") %>%   
  tidy()

g1 <- fitted_vs_actual(fitted_init, "Initial Model")

#Predicting the final fitted linear regressions model after feature selection
fitted_final <- predict(lm.fit1, alldata,
                        interval = "confidence") %>% 
  tidy()
g2 <- fitted_vs_actual(fitted_final, "Final Model")

#Plot initial and final predicted model
grid.arrange(g1,g2, ncol = 2)

