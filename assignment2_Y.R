Data <- read.csv("ProjectData.csv", header=TRUE, sep= ",")
# str(Data)
#attach create data frame
# attach(Data)
#View(Data)
# str(Data)
library("rpart")
library(tidyverse)

#Data1 <- filter(Data, Group == 1)
Data1<-Data
#View(Data1)
library(mice)
Data1 <- mice(Data1,m=5,maxit=50,meth="pmm",seed=500)
Data1 <- complete(Data1,1)
#view(Data1)


# Data1$Y7[is.na(Data1$Y7)] <- mean(Data1$Y7[!is.na(Data1$Y7)])
# Data1$Y6[is.na(Data1$Y6)] <- mean(Data1$Y6[!is.na(Data1$Y6)])
# Data1$Y5[is.na(Data1$Y5)] <- mean(Data1$Y5[!is.na(Data1$Y5)])
# Data1$Y4[is.na(Data1$Y4)] <- mean(Data1$Y4[!is.na(Data1$Y4)])
# Data1$Y3[is.na(Data1$Y3)] <- mean(Data1$Y3[!is.na(Data1$Y3)])
# Data1$Y2[is.na(Data1$Y2)] <- mean(Data1$Y2[!is.na(Data1$Y2)])
# Data1$Y1[is.na(Data1$Y1)] <- mean(Data1$Y1[!is.na(Data1$Y1)])

#to reorder the datatset if it is in some particular grouped fashon becoz we must not feed a ordered dataset to ML algorithm
#set.seed(9850)
#g <- runif(nrow(iris))
#irisr<-iris[order(g),]

################################
DT_Model <- rpart(Response~Y1+Y2+Y3+Y4+Y5+Y6+Y7,data=Data1,control=rpart.control(minsplit=60,minbucket=30, maxdepth=4 ))
plot(DT_Model)

library("partykit")
plot(as.party(DT_Model))
# attach(Data1)
Target=ifelse(Response==1,'Y','N')
# View(Target)
Data2 <- data.frame(Data1,Target)
####################################
# dim(Data1) #or dim(Data)
# dim(Data2)

s <- sample(296,236)
train <- Data2[s,]
test <- Data2[-s,]
# dim(test)

# View(Data)
# View(Data1)
 DT_Model2<-rpart(Target~Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=train, control=rpart.control(minsplit=60, minbucket=30,maxdepth=10))
 plot(as.party(DT_Model2))
# 
 print(DT_Model2$cptable)
#
 opt <- which.min(DT_Model2$cptable [, "xerror"])
#
 cp <- DT_Model2$cptable [opt,"CP"]
 DT_Model2_pruned <- prune(DT_Model2, cp=cp)
 plot(as.party(DT_Model2_pruned))
# 
# DT_Model2
# 
# DT_Model3<-rpart(Target~Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=train, method = "class")
# p <- predict(DT_Model3,test,type="class")
p <- predict(DT_Model_pruned,test,type="class")
table(test[,18],p)
#################################
DT_Model3<-rpart(Target~Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=Data2, control=rpart.control(minsplit=60, minbucket=30,maxdepth=10))
plot(as.party(DT_Model3))
# 
print(DT_Model3$cptable)
#
opt <- which.min(DT_Model3$cptable [, "xerror"])
#
cp <- DT_Model3$cptable [opt,"CP"]
DT_Model3_pruned <- prune(DT_Model3, cp=cp)
plot(as.party(DT_Model3_pruned))
