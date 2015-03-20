setwd("C:/C/Education/edX MIT 15.071 - The Analytics Edge/Unit 02 Data Files")
getwd()

##### CLIMATE CHANGE #####

climate = read.csv("climate_change.csv")
summary(climate)
str(climate)
cTrain = subset(climate, climate$Year <= 2006)
cTest = subset(climate, climate$Year > 2006)
str(cTrain); str(cTest)	#check record count adds to original 308
summary(cTrain)
summary(cTest)		#check year boundaries

climReg = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=cTrain)
summary(climReg)

# 2.2
cor(cTrain)

# 3
climReg = lm(Temp ~ MEI  + TSI + Aerosols + N2O, data=cTrain)
summary(climReg)

# 4

climReg = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data=cTrain)
summary(climReg)
climRegStep = step(climReg)
summary(climRegStep)

# 5

predict_test = predict(climRegStep, newdata=cTest)
tSSE = sum((cTest$Temp - predict_test)^2)
tSST = sum((cTest$Temp - mean(cTest$Temp))^2)
R2 = 1 - (tSSE/tSST)
c(tSSE, tSST, R2)
#Hmm... Something has gone awry. R² is not 0.1864065

predict_test = predict(climRegStep, newdata=cTest)
R2 = cor(cTest$Temp, predict_test) ^2
R2
#Hmm... Nope... Also not 0.2245625

Sanity check:
predict_test = predict(climRegStep, newdata=cTrain)
tSSE = sum((cTrain$Temp - predict_test)^2)
tSST = sum((cTrain$Temp - mean(cTrain$Temp))^2)
R2 = 1 - (tSSE/tSST)
c(tSSE, tSST, R2)
# Yes, produces 0.7508409, an expected result, same as climRegStep, based on cTrain.

predict_test = predict(climRegStep, newdata=cTest)
tSSE = sum((cTest$Temp - predict_test)^2)
tSST = sum((cTest$Temp - mean(cTrain$Temp))^2)
R2 = 1 - (tSSE/tSST)
c(tSSE, tSST, R2)
#Tried truncating to 4 decimal places - also not 0.1864.
#Going to leave this one as is for now and come back to it.
# 2 more submission attempts available.
## OK, solved it:
## Problem was for SST, we are supposed to use the 'baseline model,' 
##   which is the mean of the training data.
## I was incorrectly using  SST = sum((cTest$Temp - mean(cTest$Temp))^2)
## when it should have been SST = sum((cTest$Temp - mean(cTrain$Temp))^2)


##### READING TEST SCORES #####
#1.1

pisaTrain = read.csv("pisa2009train.csv")
pisaTest =  read.csv("pisa2009test.csv")
str(pisaTrain)

tapply(pisaTrain$readingScore, pisaTrain$male ==1, mean)

length(which(is.na(pisaTrain$grade)))
length(which(is.na(pisaTrain$male)))
length(which(is.na(pisaTrain$raceeth)))
length(which(is.na(pisaTrain$preschool)))
length(which(is.na(pisaTrain$expectBachelors)))
length(which(is.na(pisaTrain$motherHS)))
length(which(is.na(pisaTrain$motherBachelors)))
length(which(is.na(pisaTrain$motherWork)))
length(which(is.na(pisaTrain$fatherHS)))
length(which(is.na(pisaTrain$fatherBachelors)))
length(which(is.na(pisaTrain$fatherWork)))
length(which(is.na(pisaTrain$selfBornUS)))
length(which(is.na(pisaTrain$motherBornUS)))
length(which(is.na(pisaTrain$fatherBornUS)))
length(which(is.na(pisaTrain$englishAtHome)))
length(which(is.na(pisaTrain$computerForSchoolwork)))
length(which(is.na(pisaTrain$read30MinsADay)))
length(which(is.na(pisaTrain$minutesPerWeekEnglish)))
length(which(is.na(pisaTrain$studentsInEnglish)))
length(which(is.na(pisaTrain$schoolHasLibrary)))
length(which(is.na(pisaTrain$publicSchool)))
length(which(is.na(pisaTrain$urban)))
length(which(is.na(pisaTrain$schoolSize)))
length(which(is.na(pisaTrain$readingScore)))

# 1.4
pisaTrain = na.omit(pisaTrain)
str(pisaTrain)
pisaTest = na.omit(pisaTest)
str(pisaTest)

# 2.2

unique(pisaTrain$raceeth)


# 3.1

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore ~ . , data=pisaTrain)
summary(lmScore)

predict_test = predict(lmScore, newdata=pisaTrain) #Predict on itself
RMSE = sqrt( mean( (predict_test-pisaTrain$readingScore)^2 , na.rm = TRUE ) ) #Compute root mean square error

summary(lmScore)

# 4.1

predTest = predict(lmScore, newdata=pisaTest)
summary(predTest)
max(predTest)-min(predTest)

# 4.2
tSSE = sum((pisaTest$readingScore - predTest)^2)
tSST = sum((pisaTest$readingScore - mean(pisaTrain$readingScore))^2)
R2 = 1 - (tSSE/tSST)
c(tSSE, tSST, R2)
RMSE = sqrt( mean( (predTest-pisaTest$readingScore)^2 , na.rm = TRUE ) )
RMSE
mean(pisaTrain$readingScore)


##### DETECTING FLU EPIDEMICS VIA SEARCH ENGINE QUERY DATA  #####

setwd("C:/C/Education/edX MIT 15.071 - The Analytics Edge/Unit 02 Data Files")
getwd()
FluTrain = read.csv("FluTrain.csv")
str(FluTrain)
summary(FluTrain)

plot(FluTrain$ILI ~ FluTrain$Queries)
plot(FluTrain$Queries ~ FluTrain$ILI)

subset(FluTrain, FluTrain$ILI == max(FluTrain$ILI))
subset(FluTrain, FluTrain$Queries == max(FluTrain$Queries))

subset(FluTrain, FluTrain$ILI == max(FluTrain$ILI))$Week
subset(FluTrain, FluTrain$Queries == max(FluTrain$Queries))$Week

#1.2
hist(FluTrain$ILI) #unexpectedly, this is called "skew right," due to long right tail.

#1.3
plot(log(FluTrain$ILI) ~ FluTrain$Queries)

#2.2
FluTrend1 = lm(log(FluTrain$ILI) ~ FluTrain$Queries)
summary(FluTrend1)

# 3.1
FluTest = read.csv("FluTest.csv")
PredTest1 = predict(FluTrend1, newdata = FluTest)
PredTest1 = exp(predict(FluTrend1, newdata = FluTest))

which(FluTest$Week == "2012-03-11 - 2012-03-17")
FluTest[which(FluTest$Week == "2012-03-11 - 2012-03-17"),]

FluPredict = function(nQueries){
	return (exp(-0.49934 + 2.96129 * nQueries))
}

FluTest[which(FluTest$Week == "2012-03-11 - 2012-03-17"),]
FluPredict( FluTest[which(FluTest$Week == "2012-03-11 - 2012-03-17"),]$Queries )

#3.2

obsILI = FluTest[which(FluTest$Week == "2012-03-11 - 2012-03-17"),]$ILI
estILI = FluPredict( FluTest[which(FluTest$Week == "2012-03-11 - 2012-03-17"),]$Queries )
relErr = (obsILI - estILI)/obsILI
relErr

#3.3
#Using the predict function, as in:
#    PredTest1 = predict(FluTrend1, newdata = FluTest), or
#    PredTest1 = exp(predict(FluTrend1, newdata = FluTest))
# seems to be producing the wrong result, and also an interesting warning:
#    Warning message: 'newdata' had 52 rows but variables found have 417 rows
# Unsure how to fix that; R syntax is weird. Will compute manually instead:
#
PredTest1 = FluPredict(FluTest$Queries)
RMSE = sqrt( mean( (FluPredict(FluTest$Queries)-FluTest$ILI)^2 , na.rm = TRUE ) )
RMSE

#4.1
install.packages("zoo")
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
sum(ifelse(is.na(FluTrain$ILILag2),1,0))

plot(log(FluTrain$ILILag2) ~ log(FluTrain$ILI))

#4.3

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2) ,data=FluTrain)
summary(FluTrend2)

summary(FluTrend1)

testILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(testILILag2)
sum(ifelse(is.na(FluTest$ILILag2),1,0))

FluTest$ILILag2[1] = FluTrain$ILI[416]   #Update from prior time series
FluTest$ILILag2[2] = FluTrain$ILI[417]	#Update from prior time series
#5.4
summary(FluTrend2)
#FluPredict2 = function(nQueries, nILILag2){
#	return (exp(-0.24064  + 1.25578 * nQueries + 0.65569 * log(nILILag2)))
#}

PredTest2 = exp(predict(FluTrend2, newdata=FluTest))
RMSE = sqrt( mean( (PredTest2-FluTest$ILI)^2 , na.rm = TRUE ) )
RMSE		#0.2942029

