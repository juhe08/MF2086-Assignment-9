# MF2086 group6a

#1. Setting up

#1.1 Installing packages (you just need to run these for the first time)
install.packages("ggplot2")
install.packages("dplyr")
install.packages("forcats")
install.packages("readxl")

#1.2 Loading necessary libraries
library(ggplot2)
library(dplyr)
library(forcats)
library(readxl)
library(scatterplot3d)

#1.3 Setting your working directory
#getwd() #see what your current work directory is
#setwd("//Users/rafa/Box/KTH/2018 IPD-ITM postdoc/Lectures/2019/R/data") #Change "path"

#1.4 Loading the data
df <- read_excel("Survey.xlsx", col_types = c("text", 
                                              "text", "text", "text", "numeric", "numeric", 
                                              "numeric", "numeric"))
View(df)

#1.5 Looking at the data
str(df)

#1.6 Clear data for df
df <- df[-which(df$Gender=="Other"),]
df[which(df$Country%in%c("China","India","chinese","Indian")),]$Country <- "nonEU"
df[which(df$Country%in%c("Icelandic","Spain","Denmark","Italy")),]$Country <- "EU"
df[which(df$Country%in%c("sweden","Sweden","Swe","Swedish","Sweden, Switzerland")),]$Country <- "Swe"

# 1.7 Create "external" dataset and "internal" dataset
external <- df[df$EorI == "Master's programme and I have a Bachelor from another university", ]
internal <- df[-which(df$EorI == "Master's programme and I have a Bachelor from another university"), ]

#1.8 Exploring the data

#1.8.1 Number of objections
nrow(df)
group_by(df, Gender) %>% summarize( count=n())
group_by(df, Country) %>% summarize( count=n())
nrow(external)
group_by(external, Gender) %>% summarize( count=n())
group_by(external, Country) %>% summarize( count=n())

#1.8.2 mean of hypothesis
mean(df$Coursehard)
mean(df$Workloadheavy)
mean(df$Grades)
mean(df$Lesschallenging)

mean(external$Coursehard)
mean(external$Workloadheavy)
mean(external$Grades)
mean(external$Lesschallenging)

#1.8.3 Visualising the data

ggplot(df, aes(Coursehard)) + 
  geom_bar(aes(fill = ..count..))+xlim("1","2","3","4","5","6","7")
ggplot(df, aes(Workloadheavy)) + 
  geom_bar(aes(fill = ..count..))+xlim("1","2","3","4","5","6","7")
ggplot(df, aes(Grades)) + 
  geom_bar(aes(fill = ..count..))+xlim("1","2","3","4","5","6","7")
ggplot(df, aes(Lesschallenging)) + 
  geom_bar(aes(fill = ..count..))+xlim("1","2","3","4","5","6","7")

#2. Hypothesis  
#2.1 Men tend to base their master choice on grades/achievements from previous courses more than women. 

boxplot (Grades ~ Gender, 
         data = df, 
         # main = "Grade influence distribution for different genders ", 
         xlab = "Genders", 
         ylab = "Grade influence", 
         col = "blue")

t.test(Grades~Gender,data=df, paired=F,var.equal = F)

boxplot (Grades ~ Gender, 
         data = external, 
      #  main = "Grade influence distribution for different genders (externals)", 
         xlab = "Genders", 
         ylab = "Grade influence", 
         col = "blue")

t.test(Grades~Gender, data=external,paired=F, var.equal=F)

#2.2 External students based on where they are from (Sweden, another country in Europe, a country in Asia) want different levels of challenges in their master 

dt.aov <- aov(Lesschallenging ~ Country, df)
summary(dt.aov)
all <- data.frame(Lesschallenging=c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7), Country=c("Swe","EU","nonEU","Swe","EU","nonEU","Swe","EU","nonEU","Swe","EU","nonEU","Swe","EU","nonEU","Swe","EU","nonEU","Swe","EU","nonEU"), Count
                  =c(17, 3, 2, 8, 0, 3, 5, 0,2, 9, 0, 1,6,1,2,8,0,1,10,0,0))
ggplot(all,aes(Lesschallenging,Count,fill=Country))+geom_bar(stat="identity",position="dodge")+xlim("1","2","3","4","5","6","7")

ex.aov <- aov(Lesschallenging ~ Country, external)
summary(ex.aov)
ex <- data.frame(Lesschallenging=c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7), Country=c("Swe","EU","nonEU","Swe","EU","nonEU","Swe","EU","nonEU","Swe","EU","nonEU","Swe","EU","nonEU","Swe","EU","nonEU","Swe","EU","nonEU"), Count=c(4,2,2,0,0,3,0,0,2,0,0,1,0,0,2,0,0,1,0,0,0))
ggplot(ex,aes(Lesschallenging,Count,fill=Country))+geom_bar(stat="identity",position="dodge")+xlim("1","2","3","4","5","6","7")

#2.3 Students prefer to choose a less challenging master due to previous experiences of course content rather than the workload in their bachelor’s.  

fit1 <- lm(Lesschallenging ~ Coursehard, df)
summary(fit1)

fit2 <- lm(Lesschallenging ~ Workloadheavy, df)
summary(fit2)

fit3 <- lm(Coursehard ~Workloadheavy, df)
summary(fit3)

sfit2 <- lm(Lesschallenging ~ Coursehard + Workloadheavy, df)  
summary(sfit2)

ggplot(df, aes(x = Coursehard, y = Lesschallenging)) +  geom_point()+
  geom_jitter(width=0.1,alpha=0.2) + geom_smooth(method="lm",se=F)+
  xlim("1","2","3","4","5","6","7")+ylim("1","2","3","4","5","6","7")

ggplot(df, aes(x = Workloadheavy, y = Lesschallenging)) +  geom_point()+
  geom_jitter(width=0.1,alpha=0.2) + geom_smooth(method="lm",se=F)+
  xlim("1","2","3","4","5","6","7")+ylim("1","2","3","4","5","6","7")

ggplot(df, aes(x = Coursehard, y = Workloadheavy)) +  geom_point()+
  geom_jitter(width=0.1,alpha=0.2) + geom_smooth(method="lm",se=F)+
  xlim("1","2","3","4","5","6","7")+ylim("1","2","3","4","5","6","7")

efit1 <- lm(Lesschallenging ~ Coursehard, external)
summary(efit1)

efit2 <- lm(Lesschallenging ~ Workloadheavy, external)
summary(efit2)

efit3 <- lm(Workloadheavy ~ Coursehard, external)
summary(efit3)

esfit2 <- lm(Lesschallenging ~ Coursehard + Workloadheavy, external)  
summary(esfit2)

ggplot(external, aes(x = Coursehard, y = Lesschallenging)) +  geom_point()+
  geom_jitter(width=0.1,alpha=0.2) + geom_smooth(method="lm",se=F)+
  xlim("1","2","3","4","5","6","7")+ylim("1","2","3","4","5","6","7")

ggplot(external, aes(x = Workloadheavy, y = Lesschallenging)) +  geom_point()+
  geom_jitter(width=0.1,alpha=0.2) + geom_smooth(method="lm",se=F)+
  xlim("1","2","3","4","5","6","7")+ylim("1","2","3","4","5","6","7")

ggplot(external, aes(x = Coursehard, y = Workloadheavy)) +  geom_point()+
  geom_jitter(width=0.1,alpha=0.2) + geom_smooth(method="lm",se=F)+
  xlim("1","2","3","4","5","6","7")+ylim("1","2","3","4","5","6","7")
#3. Analysis 

#3.2 t.test
# Normal distribution  <0.05, then non-normal distributed
shapiro.test(df[df$Gender=="Male",]$Grades)
shapiro.test(df[df$Gender=="Female",]$Grades)
shapiro.test(external[external$Gender=="Female",]$Grades)
shapiro.test(external[external$Gender=="Male",]$Grades)

#3.3 lm
cor ( df[, c("Coursehard", "Workloadheavy")])
cor ( external[, c("Coursehard", "Workloadheavy")])

# s3d <- scatterplot3d(data=df, Coursehard,Workloadheavy,Lesschallenging, highlight.3d=T)
# attach(df)
# my.lm <- lm(Lesschallenging ~ Coursehard + Workloadheavy,df )
# s3d$plane3d(my.lm, lty.box = "solid")
# detach(df)

# e3d <- scatterplot3d(data=external,Coursehard,Workloadheavy,Lesschallenging, highlight.3d=T)
# attach(external)
# ex.lm <- lm(Lesschallenging ~ Coursehard + Workloadheavy,external)
# e3d$plane3d(ex.lm, lty.box = "solid")
