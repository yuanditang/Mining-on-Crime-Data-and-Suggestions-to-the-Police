#data entry
data = read.csv("C:/Users/tyd92/Downloads/Fall 2022/699/PROJECT/crimedata.csv")
#data cleanig-remove NA
b <- which(is.na(data$ViolentCrimesPerPop))
length(b)
data = data[-b,]
e <- which(is.na(data$PctUnemployed))
length(e)
f <- which(is.na(data$PctImmigRec5))
length(f)
g <- which(is.na(data$TotalPctDiv))
length(g)
h <- which(is.na(data$PopDens))
length(h)
i <- which(is.na(data$PctLess9thGrade))
length(i)
j <- which(is.na(data$medIncome))
length(j)


#linear regression
fivenum(data$ViolentCrimesPerPop)
boxplot(data$ViolentCrimesPerPop)
##The dataset is highly screwed

##In 2021, the violent crime rate in the United States was 395.7 cases per 100,000 of the population.
##We use 395.7 as a threshold to classify ViolentCrimesPerPop, the higher will be classified as "1"(High), the lower will be classified as "0"(Low).
data$levelofcrime <- ifelse(data$ViolentCrimesPerPop>395.7,1,0)
data <- subset(data, select=c(PctUnemployed,PctImmigRec5,TotalPctDiv,PopDens,PctLess9thGrade,medIncome,levelofcrime))
cor(data$levelofcrime,data$PctUnemployed)
cor(data$levelofcrime,data$PctImmigRec5)
cor(data$levelofcrime,data$TotalPctDiv)
cor(data$levelofcrime,data$PopDens)
cor(data$levelofcrime,data$PctLess9thGrade)
cor(data$levelofcrime,data$medIncome)


#anova test
anova_table<- anova(lm(data$levelofcrime~data$PctUnemployed+data$PctImmigRec5+data$TotalPctDiv+data$PopDens+data$PctLess9thGrade+data$medIncome))
anova_table
##all attributes are significant

#foward selection
FwModel = step(lm(data$levelofcrime~1), direction="forward", scope=(~data$PctUnemployed+data$PctImmigRec5+data$TotalPctDiv+data$PopDens+data$PctLess9thGrade+data$medIncome))
#backward selection
b1 = step(lm(data$levelofcrime~data$PctUnemployed+data$PctImmigRec5+data$TotalPctDiv+data$PopDens+data$PctLess9thGrade+data$medIncome),direction="backward")
##the best model is with all the attributes

data$levelofcrime <- ifelse(data$levelofcrime==1,"High","Low")
write.csv(x = data,file = "crimedata_selected.csv")

#Split train and test dataset
set.seed(699)
temp <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.66,0.34))
train  <- data[temp, ]
test <- data[!temp,]
train
test
write.csv(x = train,file = "crimedata_selected_train.csv")
write.csv(x = test,file = "crimedata_selected_test.csv")
