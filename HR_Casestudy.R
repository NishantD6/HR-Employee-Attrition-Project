#install.packages("rattle", repos="https://rattle.togaware.com", type="source")

library(readr)
HR_Casestudy <- read_csv("~/Documents/HR_Casestudy.csv")
HR <- tbl_df(HR_Casestudy)

glimpse(HR)

HR$Attrition <- as.factor(HR$Attrition)
HR$BusinessTravel <- as.factor(HR$BusinessTravel)
HR$Department <- as.factor(HR$Department)
HR$EducationField <- as.factor(HR$EducationField)
HR$Gender <- as.factor(HR$Gender)
HR$JobRole <- as.factor(HR$JobRole)
HR$MaritalStatus <- as.factor(HR$MaritalStatus)
HR$Over18 <- as.factor(HR$Over18)
HR$OverTime <- as.factor(HR$OverTime)
HR$StockOptionLevel <- as.factor(HR$StockOptionLevel)

HR$Education <- factor(HR$Education, levels= c(1,2,3,4,5), labels=c("Below College", "College", "Bachelor", "Master", "Doctor"))
HR$EnvironmentSatisfaction <- factor(HR$EnvironmentSatisfaction, levels= c(1,2,3,4), labels=c("Low","Medium","High","Very High") )
HR$JobInvolvement <- factor(HR$JobInvolvement, levels= c(1,2,3,4), labels=c("Low","Medium","High","Very High") )
HR$PerformanceRating <- factor(HR$PerformanceRating, levels= c(1,2,3,4), labels=c("Low","Good","Excellent","Outstanding") )
HR$RelationshipSatisfaction <- factor(HR$RelationshipSatisfaction, levels= c(1,2,3,4), labels=c("Low","Medium","High","Very High"))
HR$WorkLifeBalance <- factor(HR$WorkLifeBalance, levels= c(1,2,3,4), labels=c("Bad","Good","Better","Best"))

glimpse(HR)

summary(HR)

levels(HR$Education)
summary(HR$Education)


HR$EmployeeNumber <- NULL
HR$EmployeeCount <- NULL


# Analtics
glimpse(HR)

HR%>%
  na.omit()%>%
  select(MonthlyIncome, YearsAtCompany, Attrition, Gender)%>%
  ggplot(aes(x=YearsAtCompany, y= MonthlyIncome, col=Attrition, alpha=0.4))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  facet_grid(.~Gender)

#Conduct Analysis & Find Trends & Correlations
HR%>%
  na.omit()%>%
  select(JobLevel, Attrition)%>%
  mutate(JobLevel=as.factor(JobLevel))%>%
  group_by(JobLevel)%>%
  summarise(patt=sum(Attrition=="Yes")/n()*100)%>%
  ggplot(aes(JobLevel, patt))+
  geom_bar(stat="identity", position="dodge")

HR%>%
  na.omit()%>%
  select(JobLevel, Attrition, WorkLifeBalance)%>%
  mutate(JobLevel=as.factor(JobLevel))%>%
  group_by(JobLevel, WorkLifeBalance)%>%
  summarise(patt=sum(Attrition=="Yes")/n()*100)%>%
  ggplot(aes(JobLevel, patt))+
  geom_bar(stat="identity", position="dodge")+
  facet_grid(.~WorkLifeBalance)

#Creating Classification Model
set.seed(100)
# Shuffle the dataset, call the result shuffled
n<-nrow(HR)
shuffled<-HR[sample(n),]

# Split the data in train and test
train_indices <- 1:round(0.7*n)
test_indices<-(round(0.7*n)+1):n

#Making the New Data Set
train<-shuffled[train_indices,]
test<- shuffled[test_indices,]

# Print the structure of train and test
str(train) 
str(test)

#Create the Classifcation Model & Predict on Test set
my_mod <- rpart(Attrition ~ ., train, method = "class", maxdepth = 3)
pred<-predict(my_mod, test, type="class")

#Create a Confustion Matrix
conf<-table(HR$Attrition, pred)
TP<-conf[1, 1]
FN<-conf[1, 2]
FP<-conf[2, 1]
TN<-conf[2, 2]

print(conf)

# Calculate and print the accuracy:
acc<-(TP+TN)/(TP+FN+FP+TN)
acc*100

# Calculate and print out the precision: prec
prec<-TP/(TP+FP)
prec*100

#Calculate and print out the recall: rec
rec<-TP/(TP+FN)
rec*100


# Draw the decision tree
install.packages("rpart.plot")
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


rpart.plot(my_mod, 3)

summary(rpart(Attrition ~ ., train, method = "class"))

glimpse(HR)
summary(HR)

#Trim The Tree
glimpse(HR)

summary(HR$Attrition)


#Improve Classfication Model
HR$EducationField <- NULL
HR$EnvironmentSatisfaction<-NULL
HR$Department <-NULL
HR$BusinessTravel <-NULL
HR$HourlyRate <-NULL
HR$Gender <-NULL
HR$JobInvolvement <-NULL
HR$MaritalStatus  <-NULL
HR$NumCompaniesWorked  <-NULL
HR$Over18    <-NULL
HR$JobRole   <-NULL

#What Is relevent from the data?
#Job Level of above 1.5 , attirton rate is 100-63=37%. Hence, to reduce attrition rate, focus on employees below 1.5Job LEvel.
#Focus on those who do over time and are below 1.5 Job Level.





