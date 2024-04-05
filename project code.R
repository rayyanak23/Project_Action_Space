install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("MASS")
library(MASS)
install.packages("corrplot")
library(corrplot)
install.packages("Epi")
library(Epi)





data=read.csv("heart.csv",header=TRUE )

#View(data)
colnames(data)
summary(data)
#############################R code for Data Visualization:#########################
data$Sex <- as.factor(data$Sex)
data$ChestPainType <- as.factor(data$ChestPainType)
data$FastingBS<-as.factor(data$FastingBS)
data$RestingECG<-as.factor(data$RestingECG)
data$ExerciseAngina<-as.factor(data$ExerciseAngina)
data$ST_Slope<-as.factor(data$ST_Slope)
str(data)
data1<- data %>% dplyr::select(Age, Sex,ChestPainType,
                      FastingBS, RestingECG,
                      ExerciseAngina,ST_Slope,
                      HeartDisease) %>%
                      mutate(Sex = ifelse(Sex == "F", "FEMALE", "MALE"),
                            FastingBS = ifelse(FastingBS==1, ">120", "<=120"),
                            ExerciseAngina=ifelse(ExerciseAngina==1, "YES", "NO"),
                            ChestPainType=ifelse(ChestPainType==0, "ATYPICAL ANGINA",
                                                 ifelse(ChestPainType==1,"NON-ANGINAL PAIN",
                                                        ifelse(ChestPainType==2,"ASYMPTOMATIC",
                                                               "TYPICAL ANGINA"))),
                            RestingECG=ifelse(RestingECG==0,"NORMAL",
                                              ifelse(RestingECG==1,"ST","ABNORMALITY")),
                            ST_Slope= ifelse(ST_Slope==0,"Up",
                                             ifelse(ST_Slope==1,"Flat","Down")),
                            HeartDisease=ifelse(HeartDisease==1, "YES", "NO")) %>%
                          mutate_if(is.character, as.factor)%>%
                          dplyr::select(HeartDisease,Sex,FastingBS,ExerciseAngina,ChestPainType,
                            RestingECG,ST_Slope,everything())
                            
# Bar plot for heart diease
ggplot(data,aes(x=data1$HeartDisease), fill=data1$HeartDisease)+
  geom_bar()+
  xlab("Heart Disease")+
  ylab("count")+
  ggtitle("Presence and absence of heart disease")


prop.table(table(data$HeartDisease))

#Count the frequency of values of age
data %>%
  group_by(Age) %>%
  count() %>%
  filter(n>10)%>%
  ggplot()+
  geom_col(aes(Age,n), fill='magenta')+
  xlab("Age")+
  ylab("Age count")

#Count the frequency of values of BP
data %>%
  group_by(RestingBP) %>%
  count() %>%
  filter(n>12)%>%
  ggplot()+
  geom_col(aes(RestingBP,n), fill='blue')+
  xlab("Resting Blood Pressure")+
  ylab("count")
#Count the frequency of values of Cholesterol
data %>%
  group_by(Cholesterol) %>%
  count() %>%
  filter(n>9)%>%
  ggplot()+
  geom_col(aes(Cholesterol,n), fill='blue')+
  xlab("Cholesterol")+
  ylab("count")
# Compare bp across chest pain type
data %>%
  ggplot(aes(x=Sex,y=RestingBP))+
  geom_boxplot(fill='blue')+
  xlab("Sex")+
  ylab("BP")+
  facet_grid(~ChestPainType)
## Compare Cholesterol across chest pain
data %>%
  ggplot(aes(x=Sex,y=Cholesterol))+
  geom_boxplot(fill='blue')+
  xlab("Sex")+
  ylab("Cholesterol")+
  facet_grid(~ChestPainType)

## Correlation plot
data3 <- data %>% dplyr::select(Age,RestingBP,Cholesterol,MaxHR,Oldpeak)
cor_hd<-cor(data3)
cor_hd
corrplot(cor_hd, method = 'square', type = 'upper')


#######################R codes for analysing the data:########################


data=read.csv("heart.csv",header=TRUE )
#View(data)
colnames(data)
summary(data)


data1<- data %>%dplyr::select(Age,Sex,ChestPainType,
                       FastingBS, RestingECG,
                       ExerciseAngina,ST_Slope,
                       HeartDisease) %>%
  mutate(Sex = ifelse(Sex == "F", "FEMALE", "MALE"),
         FastingBS = ifelse(FastingBS==1, ">120", "<=120"),
         ExerciseAngina=ifelse(ExerciseAngina==1, "YES", "NO"),
         ChestPainType=ifelse(ChestPainType==0, "ATYPICAL ANGINA",
                              ifelse(ChestPainType==1,"NON-ANGINAL PAIN",
                                     ifelse(ChestPainType==2,"ASYMPTOMATIC",
                                            "TYPICAL ANGINA"))),
         RestingECG=ifelse(RestingECG==0,"NORMAL",
                           ifelse(RestingECG==1,"ST","ABNORMALITY")),
         ST_Slope= ifelse(ST_Slope==0,"Up",
                          ifelse(ST_Slope==1,"Flat","Down")),
         HeartDisease=ifelse(HeartDisease==1, "YES", "NO"))%>%
mutate_if(is.character, as.factor)%>%
  dplyr::select(HeartDisease,Sex,FastingBS,ExerciseAngina,ChestPainType,
                RestingECG,ST_Slope,everything())
# TWO BY TWO TABLE ANALYSIS

m=xtabs(~ HeartDisease + Sex, data=data1)
m
xtabs(~ HeartDisease + ChestPainType, data=data)
xtabs(~ HeartDisease + FastingBS, data=data1)
xtabs(~ HeartDisease + RestingECG, data=data)
xtabs(~ HeartDisease + ChestPainType, data=data)
xtabs(~ HeartDisease + ST_Slope, data=data)
xtabs(~ HeartDisease + ExerciseAngina, data=data)

twoby2(m)
##ANALYSIS USING LOGISTIC REGRESSION
logistic=glm(HeartDisease~Sex , data=data1,family=binomial(link=logit))
summary(logistic)
predicted.data <- data.frame(
  probability.of.hd=logistic$fitted.values,
  Sex=data1$Sex)
ggplot(data=predicted.data, aes(x=Sex, y=probability.of.hd)) +
  geom_point(aes(color=Sex), size=5) +
  xlab("Sex") +
  ylab("Predicted probability of getting heart disease")
## Predicted probabilities of getting heart disease
xtabs(~ probability.of.hd + Sex, data=predicted.data)

prop.table(table(data1$HeartDisease))
####################model 1##########################
logistic1=glm(HeartDisease~Age+ Sex, data=data,family=binomial(link=logit))
summary(logistic1)
##########################model2####################### 
logistic2=glm(HeartDisease~RestingBP+ Cholesterol+
                Age+ Sex , data=data,family=binomial(link=logit))
summary(logistic2)
################model3#######################
logistic3=glm(HeartDisease~ RestingBP+Cholesterol+Age+Sex+
                MaxHR+Oldpeak , data=data,family=binomial(link=logit))
summary(logistic3)
###############model 4##########################
logistic4=glm(HeartDisease~RestingBP+ Cholesterol+
                Sex , data=data,family=binomial(link=logit))
summary(logistic4)

predicted.data1 <- data.frame(
  probability.of.hd=logistic3$fitted.values,
  hd=data1$HeartDisease)
predicted.data1 <- predicted.data1[
  order(predicted.data1$probability.of.hd, decreasing=FALSE),]
predicted.data1$rank <- 1:nrow(predicted.data1)

ggplot(data=predicted.data1, aes(x=rank, y=probability.of.hd)) +
  geom_point(aes(color=hd), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting heart disease")
cor(data$MaxHR,data$RestingBP)
############################ extracted data set###################
data3 <- data %>% dplyr::select(Age,RestingBP,Cholesterol,MaxHR,Oldpeak)
## plot of extracted data set
plot(data3)



