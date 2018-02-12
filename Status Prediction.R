mydata <- read.csv(file.choose(), header=TRUE)

str(mydata)

# keep purchase as an integer

#####################
# DATA EXPLORATION
#####################
# How many "yes" to purchase out of the total?
table(mydata$disease.status) # 10 "yes", where the variable is = 1

frequen <- table(mydata$disease.status)/nrow(mydata) # 0.5 = proportion of "yes" people have purchased this item

var(mydata$disease.status)

table(subset(mydata,mydata$socioeconomic.status =="low" )$disease.status)/nrow(subset(mydata,mydata$socioeconomic.status =="low" ))
table(subset(mydata,mydata$socioeconomic.status =="middle" )$disease.status)/nrow(subset(mydata,mydata$socioeconomic.status =="middle" ))
table(subset(mydata,mydata$socioeconomic.status =="upper" )$disease.status)/nrow(subset(mydata,mydata$socioeconomic.status =="upper" ))
table(subset(mydata,mydata$sector =="sector 1" )$disease.status)/nrow(subset(mydata,mydata$sector =="sector 1" ))
table(subset(mydata,mydata$sector =="sector 2" )$disease.status)/nrow(subset(mydata,mydata$sector =="sector 2" ))
#################
# PLOT THE DATA
#################

plot(jitter(disease.status) ~ age, data=mydata, pch=16, main="Population disease status with their age")
lines(lowess(mydata$age, mydata$disease.status))

#####################################
# NULL MODEL / INTERCEPT ONLY MODEL
#####################################

z.null <- glm(disease.status ~ 1, data=mydata, family="binomial"(link="logit"))
summary(z.null)

###############################
# INTERPRETING CO-EFFICIENTS
###############################
# Fit the model
z1 <- glm(disease.status ~ age, data=mydata, family="binomial"(link="logit"))

summary(z1)



#################################
# LIKELIHOOD RATIO TEST
#################################

# You can compare the null model and your model using a likelihood ratio test
anova(z.null, z1, test="Chi")

# Log likelihood, larger is better
logLik(z1) 
logLik(z.null)


-2*(logLik(z.null) - logLik(z1))

# How to find chi-squared critical value:
qchisq(0.95, 1)

# How to find the p-value:
pchisq(8.666812, 1, lower.tail=FALSE)


#####################################
# ADD THE MODEL FIT TO THE GRAPH
#####################################

# Notes:
predict(z1) # will give predicted values on the logit scale

fitted(z1) # will give predicted values on the original scale (as predicted probabilities)
predict(z1, type="response") # will also give predicted values on the original scale 

# Create new data so that you can plot using it
xnew <- seq(min(mydata$age), max(mydata$age), length.out = 100)
xnew
ynew <- predict(z1, data.frame(age = xnew), type="response")
ynew

# PLOT WITH MODEL FIT AND PREDICTED VALUES
plot(jitter(disease.status) ~ age, data=mydata, pch=16,main="Model A fit")
lines(xnew, ynew, lty=1,col="blue")

zhat <- predict(z1, data.frame(age = xnew), se.fit=TRUE)  # result on logit or log scale
zupper <- zhat$fit + 1.96 * zhat$se.fit
zlower <- zhat$fit - 1.96 * zhat$se.fit

yupper <- exp(zupper)/(1 + exp(zupper)) # for logit link, backtransform to units of response variable
ylower <- exp(zlower)/(1 + exp(zlower))


lines(xnew, yupper, lty=2,col="blue")
lines(xnew, ylower, lty=2,col="blue")
abline(h=mean(mydata$disease.status),lty=2)

mean(mydata$disease.status)

(log(0.2908163/(1-0.2908163))+1.659043)/0.028464


###################################
# CREATE A CLASSIFICATION TABLE
###################################

# This will try out many different cut-off points to give you an idea of how to maximize or minimize different values.
# For example, you might want to maximize percentage of correct predictions.
# For example, you might want to minimize false negatives.

# Create an empty dataframe that you will fill with 
df <- data.frame(matrix(ncol = 9, nrow = 51))
colnames(df) <- c("correct.event", "correct.non.event", "incorrect.event", "incorrect.non.event", "correct.percent", "sensitivity", "specificity", "false.pos", "false.neg")
df


prob.level <- seq(0, 1, length.out=51) # create a vector with different possible probabilities
prob.level
class.table.data <- cbind(prob.level, df) # combine your vector of probabilities and your empty dataframe
class.table.data # Your dataframe has one row for each probability cut-off

# fill empty cells in your dataframe with 0
class.table.data$correct.non.event <- rep(c(0), c(51))
class.table.data$correct.event <- rep(c(0), c(51))
class.table.data$incorrect.non.event <- rep(c(0), c(51))
class.table.data$incorrect.event <- rep(c(0), c(51))
class.table.data



# This loop will try out the different probability cut-off values and fill in how many correct and incorrect events and non-events you have based on your data.
for (i in 1:51) {
  class.table <- table(mydata$disease.status, fitted(z1) > class.table.data$prob.level[i])
  
  col.true.num <- grep("TRUE", colnames(class.table))
  col.false.num <- grep("FALSE", colnames(class.table))
  
  if (length(col.true.num) > 0) {
    class.table.data$incorrect.non.event [i] <- class.table[1, col.true.num]
    class.table.data$correct.event [i] <- class.table[2, col.true.num] }
  
  if (length(col.false.num) > 0) {
    class.table.data$correct.non.event [i] <- class.table[1, col.false.num]
    class.table.data$incorrect.event [i] <- class.table[2, col.false.num] }  }

class.table.data

# You will use this information to fill in the rest of your classification table.
class.table.data$correct.percent <- (class.table.data$correct.event + class.table.data$correct.non.event)/nrow(mydata)
class.table.data$sensitivity <- (class.table.data$correct.event)/sum(mydata$disease.status)

class.table.data$specificity <- (class.table.data$correct.non.event)/(nrow(mydata) - sum(mydata$disease.status))

class.table.data$false.neg <- (class.table.data$incorrect.event)/sum(mydata$disease.status)

class.table.data$false.pos <- (class.table.data$incorrect.non.event)/(nrow(mydata) - sum(mydata$disease.status))

class.table.data

write.csv(class.table.data, file="4dataset.csv")

# Subset the data into purchase and no purchase 
mydata.sector1 <- subset(mydata, mydata$sector == "sector 1")
mydata.sector2 <- subset(mydata, mydata$sector == "sector 2")


# Plot the data and model fit with color coding
plot(jitter(disease.status) ~ age, data=mydata, pch=16, col = c("red", "blue")[as.factor(mydata$sector)], main="Disease status against age with each sector")
lines(lowess(mydata.sector1$age, mydata.sector1$disease.status),col="red")
lines(lowess(mydata.sector2$age, mydata.sector2$disease.status),col="blue")
legend(60,0.65,lty = 1,col=c("red","blue"), legend=c("Sector 1","Sector 2"))

mydata.sociolow <- subset(mydata, mydata$socioeconomic.status == "low")
mydata.sociomiddle <- subset(mydata, mydata$socioeconomic.status == "middle")
mydata.sociohigh <- subset(mydata, mydata$socioeconomic.status == "upper")

plot(jitter(disease.status) ~ age, data=mydata, pch=16, xlim=c(0,102), col = c("red", "blue","black")[as.factor(mydata$socioeconomic.status)], main="Disease status against age with each socioeconomic")
lines(lowess(mydata.sociolow$age, mydata.sociolow$disease.status),col="red")
lines(lowess(mydata.sociomiddle$age, mydata.sociomiddle$disease.status),col="blue")
lines(lowess(mydata.sociohigh$age, mydata.sociohigh$disease.status),col="black")
legend(76,0.73,lty = 1,col=c("red","blue","black"), legend=c("Low","Middle","High"))


z.full <- glm(disease.status ~ age + socioeconomic.status + sector + socioeconomic.status*age + sector*age, data=mydata, family="binomial"(link="logit"))
summary(z.full)
logLik(z.full)

anova(z.null, z.full, test="Chi")

z.reduceage <- glm(disease.status ~ socioeconomic.status + sector, data=mydata, family="binomial"(link="logit"))
logLik(z.reduceage)
-2*(-110.6313+104.5699)
anova(z.reduceage, z.full, test="Chi")

z.reducesector <- glm(disease.status ~ socioeconomic.status + age + socioeconomic.status*age, data=mydata, family="binomial"(link="logit"))
logLik(z.reducesector)
anova(z.reducesector, z.full, test="Chi")

z.reducesocial <- glm(disease.status ~ sector + age + sector*age, data=mydata, family="binomial"(link="logit"))
logLik(z.reducesocial)
anova(z.reducesocial, z.full, test="Chi")

z.reduceinteract <- glm(disease.status ~ sector + age, data=mydata, family="binomial"(link="logit"))
logLik(z.reduceinteract)
anova(z.reduceinteract, z.reducesocial, test="Chi")
-2*(-105.8196+105.6882)

z.final <- glm(disease.status ~ sector + age, data=mydata, family="binomial"(link="logit"))
summary(z.final)
logLik(z.final)

-2* (-105.8196)+2*(1 + 2)


###########################
# Visualizing the model
###########################


# Create a plot of the model fit in the original units


age<- seq(min(mydata$age), max(mydata$age), length.out = 100) # create 100 new x-values

sector<- rep('sector 1', 100)
sector

xnew <- data.frame(age, sector)
xnew

str(xnew)

ynew.pred.1 <- data.frame(predict(z.final, newdata =  xnew, type="response")) # get the predicted values, not that the "newdata" is "xnew" because "xnew" is the name of the dataframe. The variable names in xnew MUST match the variable names in the original dataset.
ynew.pred.1
is.data.frame(ynew.pred.1)

names(ynew.pred.1) <- c("response")
ynew.pred.1
ynew.pred.1$response



sector<- rep('sector 2', 100)
sector

xnew <- data.frame(age, sector)
xnew

str(xnew)

ynew.pred.2 <- data.frame(predict(z.final, newdata =  xnew, type="response")) # get the predicted values, not that the "newdata" is "xnew" because "xnew" is the name of the dataframe. The variable names in xnew MUST match the variable names in the original dataset.
ynew.pred.2

names(ynew.pred.2) <- c("response")
ynew.pred.2
ynew.pred.2$response

# PLOT

plot(jitter(disease.status) ~ age, data=mydata, pch=16, col = c("red", "blue")[as.factor(mydata$sector)], xlim=c(0,115),main="Disease status against age with each sector")
lines(ynew.pred.1$response ~ xnew$age, col="red")
lines(ynew.pred.2$response ~ xnew$age, col="blue")
legend(85,0.65,lty = 1,col=c("red","blue"), legend=c("Sector 1","Sector 2"))



###################################
# CREATE A CLASSIFICATION TABLE
###################################

# This will try out many different cut-off points to give you an idea of how to maximize or minimize different values.
# For example, you might want to maximize percentage of correct predictions.
# For example, you might want to minimize false negatives.

# Create an empty dataframe that you will fill with 
df <- data.frame(matrix(ncol = 9, nrow = 51))
colnames(df) <- c("correct.event", "correct.non.event", "incorrect.event", "incorrect.non.event", "correct.percent", "sensitivity", "specificity", "false.pos", "false.neg")
df


prob.level <- seq(0, 1, length.out=51) # create a vector with different possible probabilities
prob.level
class.table.data <- cbind(prob.level, df) # combine your vector of probabilities and your empty dataframe
class.table.data # Your dataframe has one row for each probability cut-off

# fill empty cells in your dataframe with 0
class.table.data$correct.non.event <- rep(c(0), c(51))
class.table.data$correct.event <- rep(c(0), c(51))
class.table.data$incorrect.non.event <- rep(c(0), c(51))
class.table.data$incorrect.event <- rep(c(0), c(51))
class.table.data



# This loop will try out the different probability cut-off values and fill in how many correct and incorrect events and non-events you have based on your data.
for (i in 1:51) {
  class.table <- table(mydata$disease.status, fitted(z.final) > class.table.data$prob.level[i])
  
  col.true.num <- grep("TRUE", colnames(class.table))
  col.false.num <- grep("FALSE", colnames(class.table))
  
  if (length(col.true.num) > 0) {
    class.table.data$incorrect.non.event [i] <- class.table[1, col.true.num]
    class.table.data$correct.event [i] <- class.table[2, col.true.num] }
  
  if (length(col.false.num) > 0) {
    class.table.data$correct.non.event [i] <- class.table[1, col.false.num]
    class.table.data$incorrect.event [i] <- class.table[2, col.false.num] }  }

class.table.data

# You will use this information to fill in the rest of your classification table.
class.table.data$correct.percent <- (class.table.data$correct.event + class.table.data$correct.non.event)/nrow(mydata)
class.table.data$sensitivity <- (class.table.data$correct.event)/sum(mydata$disease.status)

class.table.data$specificity <- (class.table.data$correct.non.event)/(nrow(mydata) - sum(mydata$disease.status))

class.table.data$false.neg <- (class.table.data$incorrect.event)/sum(mydata$disease.status)

class.table.data$false.pos <- (class.table.data$incorrect.non.event)/(nrow(mydata) - sum(mydata$disease.status))

class.table.data

write.csv(class.table.data, file="5dataset.csv")

