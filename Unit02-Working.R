setwd("C:/C/Education/edX MIT 15.071 - The Analytics Edge/Unit 02 Data Files/")
getwd()

### Section 1 - Statistical Sommelier
## Video 4 Linear Regression in R

wine = read.csv("wine.csv")
str(wine)
summary(wine)

#syntax: lm(dependent_var ~ independent_var, data = data_frame)
model_temperature = lm(Price ~ AGST, data = wine)
model_temperature
summary(model_temperature)
model_temperature$residuals
SSE = sum(model_temperature$residuals^2)

#syntax: lm(dependent_var ~ independent_var_0 + ... + independent_var_n, data = data_frame)
model2 = lm(Price ~ AGST + HarvestRain, data = wine)
summary(model2)
SSE = sum(model2$residuals^2)

model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)
SSE = sum(model3$residuals^2)

plot(model3)

# Quick Question 

model_qq = lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(model_qq)

# Video 5

summary(model3)

model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)
# Note adjusted R² improved, and that Age has become more significant after we removed FrancePopulation



#Video 6 = Correlation and Multicollinearity

plot(wine$Age, wine$FrancePop) #highly linearly correlated
plot(wine$WinterRain, wine$Price) #not very linearly correlated
cor(wine$Age, wine$FrancePop) 	# -0.9944851 ==> strong negative correlation
cor(wine$WinterRain, wine$Price) # 0.1366505 ==> not correlated

cor(wine)

model5 = lm(Price ~ AGST + HarvestRain + WinterRain , data=wine)
summary(model5)	#R² has decreased with the removal of Age. Must remove highly correlated variables one at a time instead of all at once to avoid missing important relationships

#QQ
cor(wine$HarvestRain, wine$WinterRain)

#Video 7 - Making predictions
#using model4
# Created using "Training Data"
# Test "Out of Sample Accuracy" by checking predictive power on data not in the Training Data
summary(model4)
winetest = read.csv("wine_test.csv")
summary(winetest)
str(winetest)

predict_test = predict(model4, newdata=winetest)
predict_test #Pretty close!
tSSE = sum((winetest$Price - predict_test)^2)
tSST = sum((winetest$Price - mean(wine$Price))^2)
R2 = 1 - (tSSE/tSST)
c(tSSE, tSST, R2)
#R² = 0.79442776, pretty good for an out-of-sample Test
#  HOWEVER, small sample
#We want a model with a good R² *and* a good R² on the out-of-sample test data


### Section 2 - Moneyball - Sports Analytics

baseball = read.csv("baseball.csv")
str(baseball)
summary(baseball)

unique(baseball$Team)
Oakland = subset(baseball, baseball$Team=="OAK")
plot(Oakland$Year, Oakland$OBP)

plot(baseball$W, baseball$Team, col=ifelse(baseball$Playoffs==1,"red", "black"))
abline(v=95, col="blue", lwd="5")	#abline(v=95)

#Video 2

moneyball = subset(baseball, baseball$Year < 2002)
str(moneyball)

moneyball$RD = moneyball$RS - moneyball$RA # Run Difference = Runs Scored - Runs Allowed

plot(moneyball$RD, moneyball$W)

winsreg = lm(W ~ RD, data=moneyball)
summary(winsreg) #Looking good R² = 0.88 AND RD very significant (***)
# Wins =  80.881375 + 0.105766 * RD
#  We want Wins >= 95
#  80.881375 + 0.105766 * RD >= 95
#  RD >= 133.489

# qq  - If a baseball team scores 713 runs and allows 614 runs, how many games do we expect the team to win?
winsreg$coefficients[1] + winsreg$coefficients[2] * (713 - 614)

#Video 3
#How do we score these runs?
# OBP - on base percentage - % of time getting on base
# SLG - slugging percentage - how far player gets on turn
#  Oakland A's showed BA batting average was overvalued

str(moneyball)

runsreg = lm(RS~ OBP + SLG + BA, data=moneyball)
summary(runsreg)
#Looks pretty good - R² 0.93 but notice coefficient of BA is negative. This is counterintuitive.
# Maybe collinearity?
# Try removing BA (least significant)
runsreg = lm(RS~ OBP + SLG, data=moneyball)
summary(runsreg)
#Looks pretty good - R² still ~ 0.93. All independent variables are now significant(***)
#Try removing OBP instead:
runsreg = lm(RS~ SLG + BA, data=moneyball)
summary(runsreg)
# R² has now dropped to 0.88, so model got worse. Go back to prior model
runsreg = lm(RS~ OBP + SLG, data=moneyball)
summary(runsreg)
# Since coefficient for OBP is higher than SLG and they are on comparable scales, OBP's higher
#  coefficient suggests it is more important for predicting runs scored

#
runsoppreg = lm(RA ~ OOBP + OSLG, data = moneyball)
summary(runsoppreg)
#R² ~ 0.91, notice coefficients pretty similar to runsreg

# qq

runsreg$coefficients[1] + runsreg$coefficients[2]  *  0.311 + runsreg$coefficients[3] * 0.405

runsoppreg$coefficients[1] + runsoppreg$coefficients[2] * 0.297  + runsoppreg$coefficients[3] * 0.370

(runsreg$coefficients[1] + runsreg$coefficients[2]  *  0.311 + runsreg$coefficients[3] * 0.405) - 
(runsoppreg$coefficients[1] + runsoppreg$coefficients[2] * 0.297  + runsoppreg$coefficients[3] * 0.370)

#Video 4 - Making predictions

#Want to predict 2002 performance
# BUT each year, team is different
# Need to estimate new team stats using past player performance
#  - Assumes past performance correlates with future performance
#  - Assumes few injuries
#Estimate 2002 team stats using 2001 player stats
#At start of 2002 season, A's had 24 batters on roster. 
# Using the 2001 regular season stats for these players, 
#  Team OBP is 0.339
#  Team SLG is 0.430
# Putting these values into our regression:
rScored = function(OBP, SLG){
	return (-804.6  + 2737.8 * OBP + 1584.9  * SLG)
}
rAllowed = function(OOBP, OSLG){
	return (-837.38  + 2913.60 * OOBP + 1514.29 * OSLG)
}
rScored(0.311,0.405); rAllowed(0.297, 0.370)
rScored(0.339, 0.43) #  ~805 runs scored is prediction for 2002
# Similar method for runs allowed, using 2001 player stats:
rAllowed(0.307, 0.373) # ~ 622 runs allowed prediction for 2002

RunDifferential = rScored(0.339, 0.43) - rAllowed(0.307, 0.373) # ~183

wins = function(rDifferential){
	return(80.8814 + 0.1058 * (rDifferential))
}
wins(RunDifferential) # ~ 100 wins in 2002

#                 Actual results (Paul DePodesta predictions)
#  runs scored    800  (800-820)
#  runs allowed   653  (650-670)
#  wins           103  (93-97)

#qq

newplayers = read.csv("qq4-players.csv")
newplayers
newplayers$RunsScored = rScored(newplayers$OBP, newplayers$SLG)
#Therefore, select Carlos Pena and Jeremy Giambi

#Video 5 - is playoff performance predictable?
# (5 games - small sample size)
#  Correlation is 0.03 between regular season wins and world series wins.
#  Linear regression can't predict world series wins well.

#qq

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)

cor(teamRank, wins2012)
cor(teamRank, wins2013)

