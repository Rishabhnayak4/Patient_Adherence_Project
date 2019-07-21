getwd()
setwd("C:/Users/Risha/Documents")
# Read the data
rawData <- read.csv("Patient Adherence-Data.csv", header=T)
rawData$PatientID <- as.factor(rawData$PatientID)

rawData$Date <- as.Date(rawData$Date)
str(rawData)
head(rawData)
summary(rawData)

library(DMwR)
central<-centralImputation(rawData)
central
summary(central)
# There is only one NA. Eyeballing the data shows that it has to be 205.
rawData[which(rawData$PatientID == "1015" & rawData$Medication == "Med423" & rawData$Pharmacy == "PHARMACY 1558"), ]$AmountPaid = 205

rawData$Age <- as.factor(floor(central$Age/7))

# Let's create lookup data just in case we need it later
Patients <- unique(central$PatientID)
Medications <- unique(central$Medication)
Pharmacies <- unique(central$Pharmacy)
View(central)
# Let's try to calculate the number of days between the purchases
PurchaseBehavior <- data.frame()
for (i in 1:length(Patients)){
  for (j in 1:length(Medications)){
    patientMedData <- central[which(central$PatientID == Patients[i]
                                    & central$Medication == Medications[j]), ]

    if (nrow(patientMedData) > 0){
      prevDate <- patientMedData[1, "Date"]
      PurchasedAfter <- vector(length=nrow(patientMedData))
      PurchaseLapseDays <- vector(length=nrow(patientMedData))
      Adherence <- vector(length=nrow(patientMedData))
      for (k in 1:nrow(patientMedData)){
        PurchasedAfter[k] <- patientMedData[k, "Date"] - prevDate
        PurchaseLapseDays[k] <- PurchasedAfter[k] - patientMedData[k, "For_How_Many_Days"]
       
         if (PurchaseLapseDays[k] > 0){
          Adherence[k] <- 0
        } else{
          Adherence[k] <- 1
        }
        prevDate <- patientMedData[k, "Date"]
      }

      PatientPurchaseBehavior <- cbind(patientMedData, PurchasedAfter, PurchaseLapseDays, Adherence)
      PurchaseBehavior <- rbind(PurchaseBehavior, PatientPurchaseBehavior)
    }
  }
}

View(patientMedData)
View(PatientPurchaseBehavior)
View(PurchaseBehavior)

# Clean the slate, keep what is needed only
rm(PatientPurchaseBehavior, patientMedData, i, j, k, PurchasedAfter, prevDate, Adherence, PurchaseLapseDays)

PurchaseBehavior$Adherence <- as.factor(PurchaseBehavior$Adherence)

Med.Count <- as.data.frame(table(PurchaseBehavior$Medication))
Outlier.Meds <- Med.Count[which(Med.Count$Freq == 1), ]

PurchaseBehaviorTrend <- PurchaseBehavior[-which(PurchaseBehavior$Medication %in% Outlier.Meds$Var1), ]

library(plyr)
Med.Distribution <- ddply(PurchaseBehaviorTrend, ~PatientID, summarise, No.Of.Meds=length(unique(Medication)))

table(PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$Purchased.By == "Patient Only"), "Adherence"])

table(PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$Purchased.By == "Others"), "Adherence"])

table(PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$Sex == "M"), "Adherence"])

table(PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$Sex == "F"), "Adherence"])

PurchaseBehaviorTrend <- data.frame(PurchaseBehaviorTrend, Months=months(PurchaseBehaviorTrend$Date))

# Let's keep only thos columns that matter now
AdherenceTrend <- PurchaseBehaviorTrend[ , -c(9, 10, 12)]

Month.Wise.Pattern <- ddply(PurchaseBehaviorTrend, ~Months, summarise, Adherent=length(PurchaseBehaviorTrend$Adherence == 1), Non.Adherent=length(PurchaseBehaviorTrend$Adherence == 0))

Month.Wise.Pattern <- ddply(PurchaseBehaviorTrend[ , c("Months", "Adherence")], ~Months, summarise, Adherence=length(Adherence))

par(mfrow=c(1, 2))
plot(PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$PurchaseLapseDays > 0), "AmountPaid"], PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$PurchaseLapseDays > 0), "PurchaseLapseDays"])

plot(PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$PurchaseLapseDays > 0), "QTY"], PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$PurchaseLapseDays > 0), "PurchaseLapseDays"])

plot(PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$PurchaseLapseDays > 0), "For_How_Many_Days"], PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$PurchaseLapseDays > 0), "PurchaseLapseDays"])

plot(PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$PurchaseLapseDays > 0), "Pharmacy"], PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$PurchaseLapseDays > 0), "PurchaseLapseDays"])

plot(PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$PurchaseLapseDays > 0), "Medication"], PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$PurchaseLapseDays > 0), "PurchaseLapseDays"])
plot(PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$PurchaseLapseDays > 0), "Age"], PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$PurchaseLapseDays > 0), "PurchaseLapseDays"])
plot(PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$PurchaseLapseDays > 0), "Sex"], PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$PurchaseLapseDays > 0), "PurchaseLapseDays"])

plot(PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$Purchased.By == "Patient Only"), "Date"], PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$Purchased.By == "Patient Only"), "PurchaseLapseDays"])
plot(PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$Purchased.By == "Others"), "Date"], PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$Purchased.By == "Others"), "PurchaseLapseDays"])

par(mfrow=c(1,1))
QPlot <- qplot(Medication, PurchaseLapseDays, data=AdherenceTrend,
               color=Adherence, facets=Age~Sex, shape=Purchased.By,
               size=2)
QPlot

library(ggplot2)
TrendF <- ggplot(data=PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$Sex == "F"), ], aes(x=Date, y=PurchaseLapseDays, group=interaction(PatientID,Medication)))
TrendF + geom_line(aes(color=PatientID))
TrendM <- ggplot(data=PurchaseBehaviorTrend[which(PurchaseBehaviorTrend$Sex == "M"), ], aes(x=Date, y=PurchaseLapseDays, group=interaction(PatientID,Medication)))
TrendM + geom_line(aes(color=PatientID))

#adding a category for low risk people with purchase lapse days < = 7
Adherencenew = within(AdherenceTrend, {
  lowrisk = ifelse(PurchaseLapseDays <= 7, 0, 1)
})

#plottting for low risk people

Trend1 <- ggplot(data=Adherencenew[which(Adherencenew$lowrisk == '0'), ], aes(x=Date, y=PurchaseLapseDays, group=interaction(PatientID,Medication)))
Trend1 + geom_line(aes(color=PatientID))


#Building a Linear Regression Model to Predict the next purchase Lapse Days for each patient

#Splitting the data into Train and Test

rows=seq(1,nrow(Adherencenew),1)
set.seed(123)
trainrows=sample(rows,(70*nrow(Adherencenew))/100)
train=Adherencenew[trainrows,]
test=Adherencenew[-trainrows,]
names(train)
#Applying linear regression to the data and looking at the summary
LinReg_All<-lm(PurchaseLapseDays~.,data=train)
summary(LinReg_All)
par(mfrow=c(2,2))

#Diagnostic plots
windows()
par(mfrow=c(2,2))
plot(LinReg_All)


#Evaluation using all the attributes
library(DMwR)
#Evaluation on the train data with the model developed using all attributes
regr.eval(train$PurchaseLapseDays,LinReg_All$fitted.values)


#Predicting the PurchaseLapseDays in the test data using the model developed and evaluating the fit
pred<-predict(LinReg_All,test[,],interval='confidence')
regr.eval(test$PurchaseLapseDays,pred[1])




