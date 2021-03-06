Data <- read.csv("ProjectData.csv", header=TRUE, sep= ",")

# str(Data)
# #attach create data frame
# attach(Data)
#View(Data)
# str(Data)
library("rpart")
library(tidyverse)

library(mice)

#Data1 <- filter(Data, Group == 0)
Data1 <- Data
 #View(Data1)
Data1 <- mice(Data1,m=5,maxit=50,meth="pmm",seed=500)
Data1 <- complete(Data1,1)
#view(Data1)
# Data1$X7[is.na(Data1$X7)] <- mean(Data1$X7[!is.na(Data1$X7)])
# Data1$X6[is.na(Data1$X6)] <- mean(Data1$X6[!is.na(Data1$X6)])
# Data1$X5[is.na(Data1$X5)] <- mean(Data1$X5[!is.na(Data1$X5)])
# Data1$X4[is.na(Data1$X4)] <- mean(Data1$X4[!is.na(Data1$X4)])
# Data1$X3[is.na(Data1$X3)] <- mean(Data1$X3[!is.na(Data1$X3)])
# Data1$X2[is.na(Data1$X2)] <- mean(Data1$X2[!is.na(Data1$X2)])
# Data1$X1[is.na(Data1$X1)] <- mean(Data1$X1[!is.na(Data1$X1)])

##############################################################
DT_Model <- rpart(Response~X1+X2+X3+X4+X5+X6+X7,data=Data1,control=rpart.control(minsplit=60,minbucket=30, maxdepth=4 ))
plot(DT_Model)

library("partykit")
plot(as.party(DT_Model))
print(DT_Model)
#attach(Data1)
Target=ifelse(Response==1,'Y','N')
 #View(Target)
Data2 <- data.frame(Data1,Target)
#view(Data2)

###############################################################
s <- sample(296,236)
train <- Data2[s,]
test <- Data2[-s,]

DT_Model2<-rpart(Target~X1+X2+X3+X4+X5+X6+X7, data=train, control=rpart.control(minsplit=60, minbucket=30,maxdepth=10 ))
plot(as.party(DT_Model2))

 print(DT_Model2$cptable)
# 
 opt <- which.min(DT_Model2$cptable [, "xerror"])
# 
 cp <- DT_Model2$cptable [opt,"CP"]
 DT_Model2_pruned <- prune(DT_Model2, cp=cp)
 plot(as.party(DT_Model2_pruned))
# dim(Data1) #or dim(Data)
# dim(Data2)

# dim(test)


# DT_Model3<-rpart(Target~X1+X2+X3+X4+X5+X6+X7, data=train, method = "class")
p <- predict(DT_Model2_pruned,test,type="class")
table(test[,18],p)

#############################################################

DT_Model3<-rpart(Target~X1+X2+X3+X4+X5+X6+X7, data=Data2, control=rpart.control(minsplit=60, minbucket=30,maxdepth=10 ))
plot(as.party(DT_Model3))

print(DT_Model3$cptable)
# 
opt <- which.min(DT_Model3$cptable [, "xerror"])
# 
cp <- DT_Model3$cptable [opt,"CP"]
DT_Model3_pruned <- prune(DT_Model3, cp=cp)
plot(as.party(DT_Model3_pruned))



