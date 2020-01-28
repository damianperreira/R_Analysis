# MPG Regression
mecha <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
library(ggplot2)
head(mecha)
names(mecha)
names(mecha)<-str_replace_all(names(mecha), c(" " = "." , "," = "" ))
str(mecha)
tail(mecha)
mechsample <- sample(mecha)
ggplot(mecha, aes(mpg)) +
  geom_histogram(binwidth = 4) + xlab('Miles per Gallon') + ylab('Number of Cars') + 
  ggtitle('Distribution of Cars by Mileage')
head(mechsample)

# generate multiple linear regression model
lm(formula = mpg ~ +AWD + vehicle.weight + ground.clearance + spoiler.angle + vehicle.length, data = mechsample)

summary(lm(formula = mpg ~ +AWD + vehicle.weight + ground.clearance + spoiler.angle + vehicle.length, data = mechsample))

mechmodel <- summary(lm(formula = mpg ~ +AWD + vehicle.weight + ground.clearance + spoiler.angle + vehicle.length, data = mechsample))

summary(mechmodel)

# Suspension Coil Summary Statistics

coildata <- read.csv(file='Suspension_Coil.csv', check.names = F,stringsAsFactors = F)
head(coildata)
tail(coildata)

#create a summary statistics table for the suspension coil’s pounds-per-inch continuous variable

mean(coildata$PSI)
median(coildata$PSI)
var(coildata$PSI)
sd(coildata$PSI)

summary(coildata)


# Suspension Coil T-Test

t.test(log10(coildata$PSI),mu=mean(log10(coildata$PSI))) 

