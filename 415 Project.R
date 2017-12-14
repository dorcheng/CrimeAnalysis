crimedata = read.csv(file.choose()) #Crime Data Updated

## Frequency of Different Types of Crime in 2015 ##
type = count(crimedata$Primary.Type)
yearplot = barplot(type$freq, names.arg = c("Assault", "Criminal Violations", "Offense Involving Children", "Sex Offense", "Substance Abuse", "Theft", "Weapons Violation"), main = "Frequency of Different Types of Crime in 2015")
text(x = yearplot, y=type$freq - 1000, labels = as.character(type$freq))


## Types of Crime Per Day ##
dailytype = read.csv(file.choose()) #type of crime per day
assaultfreq = as.numeric(dailytype$ASSAULT)
crimviolfreq = as.numeric(dailytype$CRIMINAL.VIOLATIONS)
offwchilfreq = as.numeric(dailytype$OFFENSE.INVOLVING.CHILDREN)
sexofffreq = as.numeric(dailytype$SEX.OFFENSE)
subabusefreq = as.numeric(dailytype$SUBSTANCE.ABUSE)
theftfreq = as.numeric(dailytype$THEFT)
weaponsfreq = as.numeric(dailytype$WEAPONS.VIOLATION)

plot(dailytype$Day, assaultfreq, xlab = "Day", ylab = "Frequency of Assault", main = "Frequency of Assault Per Day")
fit = lm(dailytype$ASSAULT ~ poly(dailytype$Day, 2))
summary(fit)

Call:
  lm(formula = dailytype$ASSAULT ~ poly(dailytype$Day, 2))

Residuals:
  Min     1Q Median     3Q    Max 
-78.39 -17.72  -3.58  15.78 114.68 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)              180.532      1.439 125.420   <2e-16 ***
  poly(dailytype$Day, 2)1   36.525     27.500   1.328    0.185    
poly(dailytype$Day, 2)2 -379.852     27.500 -13.813   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 27.5 on 362 degrees of freedom
Multiple R-squared:  0.3472,	Adjusted R-squared:  0.3436 
F-statistic: 96.28 on 2 and 362 DF,  p-value: < 2.2e-16

lines(predict(fit), col = "blue")

crimviolfreq = as.numeric(dailytype$CRIMINAL.VIOLATIONS)
plot(dailytype$Day, crimviolfreq, xlab = "Day", ylab = "Frequency of Criminal Violations", main = "Frequency of Criminal Violations Per Day")
fit1 = lm(dailytype$CRIMINAL.VIOLATIONS ~ poly(dailytype$Day, 2))
summary(fit1)
Call:
  lm(formula = dailytype$CRIMINAL.VIOLATIONS ~ poly(dailytype$Day, 
                                                    2))

Residuals:
  Min      1Q  Median      3Q     Max 
-37.131  -9.712  -0.572   8.632  59.128 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)               96.0055     0.7826 122.669  < 2e-16 ***
  poly(dailytype$Day, 2)1  103.2272    14.9523   6.904 2.28e-11 ***
  poly(dailytype$Day, 2)2 -149.8105    14.9523 -10.019  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 14.95 on 362 degrees of freedom
Multiple R-squared:  0.2903,	Adjusted R-squared:  0.2863 
F-statistic: 74.02 on 2 and 362 DF,  p-value: < 2.2e-16
lines(predict(fit1), col = "blue")

offwchilfreq = as.numeric(dailytype$OFFENSE.INVOLVING.CHILDREN)
plot(dailytype$Day, offwchilfreq, xlab = "Day", ylab = "Frequency of Offense Involving Children", main = "Frequency of Offense Involving Chidren Per Day")
fit2 = lm(dailytype$OFFENSE.INVOLVING.CHILDREN ~ poly(dailytype$Day, 2))
summary(fit2)
Call:
  lm(formula = dailytype$OFFENSE.INVOLVING.CHILDREN ~ poly(dailytype$Day, 
                                                           2))

Residuals:
  Min     1Q Median     3Q    Max 
-5.698 -1.996 -0.516  1.480 43.250 

Coefficients:
  Estimate Std. Error t value
(Intercept)               5.8863     0.1834  32.100
poly(dailytype$Day, 2)1  -6.7856     3.4897  -1.944
poly(dailytype$Day, 2)2   2.1567     3.5051   0.615
Pr(>|t|)    
(Intercept)               <2e-16 ***
  poly(dailytype$Day, 2)1   0.0526 .  
poly(dailytype$Day, 2)2   0.5387    
---
  Signif. codes:  
  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3.489 on 359 degrees of freedom
(3 observations deleted due to missingness)
Multiple R-squared:  0.01146,	Adjusted R-squared:  0.005956 
F-statistic: 2.081 on 2 and 359 DF,  p-value: 0.1263

abline(a = 5.8863, b = 0, col = "blue")

sexofffreq = as.numeric(dailytype$SEX.OFFENSE)
plot(dailytype$Day, sexofffreq, xlab = "Day", ylab = "Frequency of Sex Offense", main = "Frequency of Sex Offense")
fit3 = lm(dailytype$SEX.OFFENSE ~ poly(dailytype$Day, 2))
summary(fit3)

Call:
  lm(formula = dailytype$SEX.OFFENSE ~ poly(dailytype$Day, 2))

Residuals:
  Min      1Q  Median      3Q     Max 
-8.6250 -3.2617 -0.6337  2.6466 22.2532 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)               9.5315     0.2447  38.949  < 2e-16 ***
  poly(dailytype$Day, 2)1 -12.9582     4.6754  -2.772 0.005867 ** 
  poly(dailytype$Day, 2)2 -16.8521     4.6754  -3.604 0.000357 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.675 on 362 degrees of freedom
Multiple R-squared:  0.05402,	Adjusted R-squared:  0.0488 
F-statistic: 10.34 on 2 and 362 DF,  p-value: 4.308e-05

subabusefreq = as.numeric(dailytype$SUBSTANCE.ABUSE)
plot(dailytype$Day, subabusefreq, xlab = "Day", ylab = "Frequency of Substance Abuse", main = "Frequency of Substance Abuse Per Day")
fit4 = lm(dailytype$SUBSTANCE.ABUSE ~ dailytype$Day)
summary(fit4)

Call:
  lm(formula = dailytype$SUBSTANCE.ABUSE ~ dailytype$Day)

Residuals:
  Min      1Q  Median      3Q     Max 
-50.669  -8.435   0.387   8.921  48.934 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)   76.299262   1.441527   52.93   <2e-16 ***
  dailytype$Day -0.082195   0.006827  -12.04   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 13.74 on 363 degrees of freedom
Multiple R-squared:  0.2854,	Adjusted R-squared:  0.2834 
F-statistic:   145 on 1 and 363 DF,  p-value: < 2.2e-16

lines(predict(fit4), col = "blue")

theftfreq = as.numeric(dailytype$THEFT)
plot(dailytype$Day, theftfreq, xlab = "Day", ylab = "Frequency of Theft", main = "Frequency of Theft Per Day")
fit5 = lm(dailytype$THEFT ~ poly(dailytype$Day, 4))
summary(fit5)

Call:
  lm(formula = dailytype$THEFT ~ poly(dailytype$Day, 4))

Residuals:
  Min       1Q   Median       3Q      Max 
-100.302  -14.857   -1.504   17.698   91.594 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)              246.616      1.428 172.681  < 2e-16 ***
  poly(dailytype$Day, 4)1  405.632     27.285  14.867  < 2e-16 ***
  poly(dailytype$Day, 4)2 -216.508     27.285  -7.935 2.69e-14 ***
  poly(dailytype$Day, 4)3 -145.033     27.285  -5.315 1.87e-07 ***
  poly(dailytype$Day, 4)4  169.557     27.285   6.214 1.43e-09 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 27.28 on 360 degrees of freedom
Multiple R-squared:  0.4936,	Adjusted R-squared:  0.4879 
F-statistic: 87.71 on 4 and 360 DF,  p-value: < 2.2e-16

lines(predict(fit5), col = "blue")

weaponsfreq = as.numeric(dailytype$WEAPONS.VIOLATION)
plot(dailytype$Day, weaponsfreq, xlab = "Day", ylab = "Frequency of Weapons Violation", main = "Frequency of Weapons Violations Per Day")
fit6 = lm(dailytype$WEAPONS.VIOLATION~ poly(dailytype$Day, 2))
lines(predict(fit6), col = "blue")

### Combined Types of Crime Per Day ###
dailytype2 = read.csv(file.choose(), header = FALSE)
colnames(dailytype2) = c("Type", as.character(seq(1:365)))
barplot(as.matrix(dailytype2[,2:366]), col = c("darkorchid1", "deepskyblue", "darkolivegreen", "darkturquoise", "forestgreen", "blue", "green"), xlab = "Day", main = "Frequency of Type of Crime Per Day", ylab = "Frequency", legend = c("Assault", "Criminal Violations", "Offense Involving Children", "Sex Offense", "Substance Abuse", "Theft", "Weapons Violation"))

### Type of Crime Per Hour ##
crimetime = read.csv(file.choose())
assaultfreq = as.numeric(crimetime$ASSAULT)
crimviolfreq = as.numeric(crimetime$CRIMINAL.VIOLATIONS)
offwchilfreq = as.numeric(crimetime$OFFENSE.INVOLVING.CHILDREN)
sexofffreq = as.numeric(crimetime$SEX.OFFENSE)
subabusefreq = as.numeric(crimetime$SUBSTANCE.ABUSE)
theftfreq = as.numeric(crimetime$THEFT)
weaponsfreq = as.numeric(crimetime$WEAPONS.VIOLATION)

plot(crimetime$Hour, assaultfreq, xlab = "Hours", ylab = "Frequency of Assault", main = "Frequency of Assault Per Hour")
plot(crimetime$Hour, crimviolfreq, xlab = "Hours", ylab = "Frequency of Criminal Violations", main = "Frequency of Criminal Violations Per Hour")
plot(crimetime$Hour, offwchilfreq, xlab = "Hours", ylab = "Frequency of Offense Involving Children", main = "Frequency of Offense Involving Children Per Hour")
plot(crimetime$Hour, sexofffreq, xlab = "Hours", ylab = "Frequency of Sex Offense", main = "Frequency of Sex Offense Per Hour")
plot(crimetime$Hour, subabusefreq, xlab = "Hours", ylab = "Frequency of Substance Abuse", main = "Frequency of Substance Abuse Per Hour")
plot(crimetime$Hour, theftfreq, xlab = "Hours", ylab = "Frequency of Theft", main = "Frequency of Theft Per Hour")
plot(crimetime$Hour, weaponsfreq, xlab = "Hours", ylab = "Frequency of Weapons Violation", main = "Frequency of Weapons Violation Per Hour")

### Combined Type of Crime Per Hour ##
crimetime2 = read.csv(file.choose(), header = FALSE)
colnames(crimetime2) = c("Hour", as.character(seq(1:24)))
barplot(as.matrix(crimetime2[,2:25]), col = c("darkorchid1", "deepskyblue", "darkolivegreen", "darkturquoise", "forestgreen", "blue", "green"), xlab = "Hour", main = "Frequency of Type of Crime Per Hour", ylab = "Frequency", legend = c("Assault", "Criminal Violations", "Offense Involving Children", "Sex Offense", "Substance Abuse", "Theft", "Weapons Violation"))

## Arrest Based on Type of Crime Per Hour ##
colnames(assaulthour) = c("Arrest Type", as.character(seq(1:24)))
barplot(as.matrix(assaulthour[,2:25]), xlab = "Hour", main = "Arrest Frequency Per Hour Based on Assault", ylab = "Frequency", legend = c("Arrest", "No Arrest"))

colnames(crimviolhour) = as.character(seq(1:24))
barplot(as.matrix(crimviolhour), xlab = "Hour", main = "Arrest Frequency Per Hour Based on Criminal Violations", ylab = "Frequency", legend = c("Arrest", "No Arrest"))

colnames(childrenhour) = as.character(seq(1:24))
barplot(as.matrix(childrenhour), xlab = "Hour", main = "Arrest Frequency Per Hour Based on Offense Involving Children", ylab = "Frequency", legend = c("Arrest", "No Arrest"))

colnames(sexhour) = as.character(seq(1:24))
barplot(as.matrix(sexhour), xlab = "Hour", main = "Arrest Frequency Per Hour Based on Sex Offense", ylab = "Frequency", legend = c("Arrest", "No Arrest"))

colnames(subhour) = as.character(seq(1:24))
barplot(as.matrix(subhour), xlab = "Hour", main = "Arrest Frequency Per Hour Based on Substance Abuse", ylab = "Frequency", legend = c("Arrest", "No Arrest"))

colnames(thefthour) = as.character(seq(1:24))
barplot(as.matrix(thefthour), xlab = "Hour", main = "Arrest Frequency Per Hour Based on Theft", ylab = "Frequency", legend = c("Arrest", "No Arrest"))

colnames(weaponshour) = as.character(seq(1:24))
barplot(as.matrix(weaponshour), xlab = "Hour", main = "Arrest Frequency Per Hour Based on Weapons Violation", ylab = "Frequency", legend = c("Arrest", "No Arrest"))

##### Classification Tree ####
library(rpart)

#grow tree
tree.crimedata = rpart(Arrest ~ Time + Day + District + Primary.Type, method = "class", data = crimedata)

#summary of splits
summary(tree.crimedata)

Call:
  rpart(formula = Arrest ~ Time + Day + District + Primary.Type, 
        data = crimedata, method = "class")
n= 222291 

CP nsplit rel error    xerror        xstd
1 0.4212187      0 1.0000000 1.0000000 0.003583662
2 0.0100000      1 0.5787813 0.5787813 0.002920583

Variable importance
Primary.Type 
100 

Node number 1: 222291 observations,    complexity param=0.4212187
predicted class=FALSE  expected loss=0.2594167  P(node) =1
class counts: 164625 57666
probabilities: 0.741 0.259 
left son=2 (196561 obs) right son=3 (25730 obs)
Primary splits:
  Primary.Type splits as  LLLLRLR,    improve=29551.9700, (0 missing)
Time         < 1000.5 to the left,  improve=  839.8772, (0 missing)
District     < 11.5   to the right, improve=  295.7742, (0 missing)
Day          splits as  RLLLRRR,    improve=   10.3008, (0 missing)

Node number 2: 196561 observations
predicted class=FALSE  expected loss=0.1661367  P(node) =0.8842508
class counts: 163905 32656
probabilities: 0.834 0.166 

Node number 3: 25730 observations
predicted class=TRUE   expected loss=0.0279829  P(node) =0.1157492
class counts:   720 25010
probabilities: 0.028 0.972 

#plot tree
plot(tree.crimedata, uniform=TRUE, main="Classification Tree for Arrest Type")
text(tree.crimedata, use.n=TRUE, all=TRUE, cex=.8)

#Random Forest#
rf.fit <- randomForest(Primary.Type ~ Time + Day + District + Arrest, data = crimedata)
summary(rf.fit)
Length  Class  Mode     
call                  3 -none- call     
type                  1 -none- character
predicted        222291 factor numeric  
err.rate           4000 -none- numeric  
confusion            56 -none- numeric  
votes           1556037 matrix numeric  
oob.times        222291 -none- numeric  
classes               7 -none- character
importance            4 -none- numeric  
importanceSD          0 -none- NULL     
localImportance       0 -none- NULL     
proximity             0 -none- NULL     
ntree                 1 -none- numeric  
mtry                  1 -none- numeric  
forest               14 -none- list     
y                222291 factor numeric  
test                  0 -none- NULL     
inbag                 0 -none- NULL     
terms                 3 terms  call


importance(rf.fit)

MeanDecreaseGini
Time            13057.249
Day              3067.456
District         8121.076
Arrest          11603.652

#A low Gini (i.e. higher descrease in Gini) means that a particular
##predictor variable plays a greater role in partitioning the data
##into the defined classes
