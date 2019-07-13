library(MASS)
library(lmtest)
library(neuralnet)
library(pscl)

getwd()

mydata = read.csv("PL_XSELL.csv", header = TRUE)
attach(mydata)
names(mydata)
mydata = mydata[,-c(1,11)]
names(mydata)
View(mydata)

OCC.MATRIX = model.matrix(~ OCCUPATION - 1, data = mydata)
mydata = data.frame(mydata, OCC.MATRIX)

OCC.MATRIX1 = model.matrix(~ GENDER - 1, data = mydata)
mydata = data.frame(mydata, OCC.MATRIX1)

OCC.MATRIX2 = model.matrix(~ ACC_TYPE - 1, data = mydata)
mydata = data.frame(mydata, OCC.MATRIX2)
View(mydata)
mydata = mydata[,-c(3,5,9)]
View(mydata)

### We are using Logistic Regression to identify the significant variables based on p values to be used finally for Neural Network Model

logit = glm(TARGET~.,data = mydata, family = binomial)

lrtest(logit)

## Overall Model Logit is ok as Fstat p value is less than 0.05

summary(logit)

pR2(logit)

##Mcfadden R2 is 12% which is just modest


### Now starting Neural Network

### Dividing data into Train and test 70% vs 30%

dt = sort(sample(nrow(mydata), nrow(mydata)*.7))
dt

train<-mydata[dt,]
test<-mydata[-dt,]

View(train)
View(test)

c(nrow(train), nrow(test))
str(train)
      
## Response Rate   TRAIN IS 12%  AND TEST IS 12%
sum(train$TARGET) / nrow(train)
sum(test$TARGET) / nrow(test)

nn1 <- neuralnet(formula = TARGET ~ BALANCE+SCR+HOLDING_PERIOD+LEN_OF_RLTN_IN_MNTH+TOT_NO_OF_L_TXNS+FLG_HAS_CC+FLG_HAS_ANY_CHGS+AVG_AMT_PER_ATM_TXN+AVG_AMT_PER_NET_TXN++OCCUPATIONSAL+OCCUPATIONSELF.EMP
 ,    data = train, 
      hidden = c(3,2),
      linear.output = FALSE,
      lifesign = "full",
      lifesign.step = 10,
      threshold = 0.1,
      ##stepmax = 2000
      ##startweights = startweightsObj
)

plot (nn1)


## Assigning the Probabilities to Dev Sample
train$Prob = nn1$net.result[[1]] 

View(train)

## The distribution of the estimated probabilities
quantile(train$Prob, c(0,1,5,10,25,50,75,90,95,99,100)/100)
hist(train$Prob)

## deciling code
decile <- function(x){
deciles <- vector(length=10)
for (i in seq(0.1,1,.1)){
deciles[i*10] <- quantile(x, i, na.rm=T)
}
return (
ifelse(x<deciles[1], 1,
ifelse(x<deciles[2], 2,
ifelse(x<deciles[3], 3,
ifelse(x<deciles[4], 4,
ifelse(x<deciles[5], 5,
ifelse(x<deciles[6], 6,
ifelse(x<deciles[7], 7,
ifelse(x<deciles[8], 8,
ifelse(x<deciles[9], 9, 10
))))))))))
}

## deciling
train$deciles <- decile(train$Prob)


## Ranking code
##install.packages("data.table")
library(data.table)
library(scales)

tmp_DT = data.table(train)
rank <- tmp_DT[, list(
cnt = length(TARGET), 
cnt_resp = sum(TARGET), 
cnt_non_resp = sum(TARGET == 0)) , 
by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)


==========================================================================

## Rebuilding the model by Scaling the Independent Variables

## build the neural net model by scaling the variables

View(train)



x <- subset(train, 
            select = c("BALANCE","SCR","HOLDING_PERIOD","LEN_OF_RLTN_IN_MNTH","TOT_NO_OF_L_TXNS","AMT_ATM_DR","AVG_AMT_PER_NET_TXN","FLG_HAS_CC","FLG_HAS_ANY_CHGS","AVG_AMT_PER_ATM_TXN","OCCUPATIONSAL","OCCUPATIONSELF.EMP"
                      ))
 
trainscaled <- scale(x)
trainscaled <- cbind(train[1], trainscaled)
View(trainscaled)

nn2 <- neuralnet(formula = TARGET ~ BALANCE+SCR+HOLDING_PERIOD+LEN_OF_RLTN_IN_MNTH+TOT_NO_OF_L_TXNS+AMT_ATM_DR+AVG_AMT_PER_NET_TXN+FLG_HAS_CC+FLG_HAS_ANY_CHGS+AVG_AMT_PER_ATM_TXN+OCCUPATIONSAL+OCCUPATIONSELF.EMP,
data = trainscaled, 
hidden = c(3,2),
linear.output = FALSE,
lifesign = "full",
lifesign.step = 10,
threshold = 0.2,
##stepmax = 2000
)

plot(nn2)



## Assigning the Probabilities to Dev Sample scaled
train$Prob = nn2$net.result[[1]]

View(train)

## The distribution of the estimated probabilities
quantile(train$Prob, c(0,1,5,10,25,50,75,90,95,99,100)/100)
hist(train$Prob)



## deciling code
decile <- function(x){
deciles <- vector(length=10)
for (i in seq(0.1,1,.1)){
deciles[i*10] <- quantile(x, i, na.rm=T)
}
return (
ifelse(x<deciles[1], 1,
ifelse(x<deciles[2], 2,
ifelse(x<deciles[3], 3,
ifelse(x<deciles[4], 4,
ifelse(x<deciles[5], 5,
ifelse(x<deciles[6], 6,
ifelse(x<deciles[7], 7,
ifelse(x<deciles[8], 8,
ifelse(x<deciles[9], 9, 10
))))))))))
}

## deciling
train$deciles <- decile(train$Prob)


## Ranking code
##install.packages("data.table")
library(data.table)
library(scales)

tmp_DT = data.table(train)
rank <- tmp_DT[, list(
cnt = length(TARGET), 
cnt_resp = sum(TARGET), 
cnt_non_resp = sum(TARGET == 0)) , 
by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
library(scales)
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)

# gOT RANK ORDERING AND A KS OF 0.32 with all variables scaled

##Y = scale(train$BALANCE)
##View(Y)

## Other Model Performance Measures

library(ROCR)


library(ineq)
gini = ineq(train$Prob, type="Gini")

KS  ## 0.34
gini  ## 0.4

### Now applying the model nn2 in testing , note this was all scaled

## Scoring another dataset using the Neural Net Model Object
## To score we will use the compute function
?compute

z <- subset(test, 
            select = c("BALANCE","SCR","HOLDING_PERIOD","LEN_OF_RLTN_IN_MNTH","TOT_NO_OF_L_TXNS","AMT_ATM_DR","AVG_AMT_PER_NET_TXN","FLG_HAS_CC","FLG_HAS_ANY_CHGS","AVG_AMT_PER_ATM_TXN","OCCUPATIONSAL","OCCUPATIONSELF.EMP"
))

View(z)


z.scaled <- scale(z)

compute.output = compute(nn2, z.scaled)
test$Predict.score = compute.output$net.result
View(test)

quantile(test$Predict.score, c(0,1,5,10,25,50,75,90,95,99,100)/100)
test$deciles <- decile(test$Predict.score)

library(data.table)
tmp_DT = data.table(test)
h_rank <- tmp_DT[, list(
cnt = length(TARGET), 
cnt_resp = sum(TARGET), 
cnt_non_resp = sum(TARGET == 0)) , 
by=deciles][order(-deciles)]
h_rank$rrate <- round (h_rank$cnt_resp / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),2);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp);


library(scales)
h_rank$rrate <- percent(h_rank$rrate)
h_rank$cum_rel_resp <- percent(h_rank$cum_rel_resp)
h_rank$cum_rel_non_resp <- percent(h_rank$cum_rel_non_resp)

View(h_rank)


### KS is 0.28 in test whereas in train it was 0.33 

###========================================================================

####Third Iteration , here we will increase the error threshold t0 0.2 to minimise overfitting and we will only scale the following variables
### BALANCE , AMT_ATM_DR, AVG_AMT_PER_ATM_TXN","AVG_AMT_PER_NET_TXN


## Rebuilding the model by Scaling the Independent Variables

## build the neural net model by scaling the variables

View(train)

x <- subset(train, 
select = c("BALANCE","SCR" ,"HOLDING_PERIOD","LEN_OF_RLTN_IN_MNTH","FLG_HAS_CC","AMT_ATM_DR","FLG_HAS_ANY_CHGS","AVG_AMT_PER_ATM_TXN","AVG_AMT_PER_NET_TXN","FLG_HAS_NOMINEE","OCCUPATIONSAL","OCCUPATIONSELF.EMP","GENDERF"
))

x$BALANCE = scale(x$BALANCE)
x$AMT_ATM_DR = scale(x$AMT_ATM_DR)
x$AVG_AMT_PER_ATM_TXN = scale(x$AVG_AMT_PER_ATM_TXN)
x$AVG_AMT_PER_NET_TXN = scale(x$AVG_AMT_PER_NET_TXN)

trainpartialscaled <- cbind(train[1], x)
View(trainpartialscaled)

nn3 <- neuralnet(formula = TARGET ~ BALANCE+SCR+HOLDING_PERIOD+LEN_OF_RLTN_IN_MNTH+FLG_HAS_CC+AMT_ATM_DR+FLG_HAS_ANY_CHGS+AVG_AMT_PER_ATM_TXN+AVG_AMT_PER_NET_TXN+FLG_HAS_NOMINEE+OCCUPATIONSAL+OCCUPATIONSELF.EMP+GENDERF,
data = trainpartialscaled, 
hidden = 3,
linear.output = FALSE,
lifesign = "full",
lifesign.step = 10,
threshold = 0.1,
##stepmax = 2000
)

plot(nn3)




## Assigning the Probabilities to Dev Sample scaled
train$Prob = nn3$net.result[[1]]

View(train)

## The distribution of the estimated probabilities
quantile(train$Prob, c(0,1,5,10,25,50,75,90,95,99,100)/100)
hist(train$Prob)



## deciling code
decile <- function(x){
deciles <- vector(length=10)
for (i in seq(0.1,1,.1)){
deciles[i*10] <- quantile(x, i, na.rm=T)
}
return (
ifelse(x<deciles[1], 1,
ifelse(x<deciles[2], 2,
ifelse(x<deciles[3], 3,
ifelse(x<deciles[4], 4,
ifelse(x<deciles[5], 5,
ifelse(x<deciles[6], 6,
ifelse(x<deciles[7], 7,
ifelse(x<deciles[8], 8,
ifelse(x<deciles[9], 9, 10
))))))))))
}

## deciling
train$deciles <- decile(train$Prob)


## Ranking code
##install.packages("data.table")
library(data.table)
library(scales)

tmp_DT = data.table(train)
rank <- tmp_DT[, list(
cnt = length(TARGET), 
cnt_resp = sum(TARGET), 
cnt_non_resp = sum(TARGET == 0)) , 
by=deciles][order(-deciles)]
rank$rrate <- round (rank$cnt_resp / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_rel_resp <- round(rank$cum_resp / sum(rank$cnt_resp),2);
rank$cum_rel_non_resp <- round(rank$cum_non_resp / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_rel_resp - rank$cum_rel_non_resp);
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

library(scales)
rank$rrate <- percent(rank$rrate)
rank$cum_rel_resp <- percent(rank$cum_rel_resp)
rank$cum_rel_non_resp <- percent(rank$cum_rel_non_resp)

View(rank)

### here with partial scaling and threshold of 0.1 we got KS of 0.13



## Other Model Performance Measures

library(ROCR)


library(ineq)
gini = ineq(train$Prob, type="Gini")

KS  ## 0.30
gini  ## 0.36


### Now applying the model nn3 in testing , note this was partial scaled

## Scoring another dataset using the Neural Net Model Object
## To score we will use the compute function
?compute

x <- subset(test, 
select = c("BALANCE","SCR","HOLDING_PERIOD","LEN_OF_RLTN_IN_MNTH","FLG_HAS_CC","AMT_ATM_DR","FLG_HAS_ANY_CHGS","AVG_AMT_PER_ATM_TXN","AVG_AMT_PER_NET_TXN","FLG_HAS_NOMINEE","OCCUPATIONSAL","OCCUPATIONSELF.EMP","GENDERF"
))

x$BALANCE = scale(x$BALANCE)
x$AMT_ATM_DR = scale(x$AMT_ATM_DR)
x$AVG_AMT_PER_ATM_TXN = scale(x$AVG_AMT_PER_ATM_TXN)
x$AVG_AMT_PER_NET_TXN = scale(x$AVG_AMT_PER_NET_TXN)

testpartialscaled <- cbind(test[1], x)
View(testpartialscaled)

testpartialscaled1 = testpartialscaled[,-1]

compute.output = compute(nn3, testpartialscaled1)
test$Predict.score = compute.output$net.result
View(test)

quantile(test$Predict.score, c(0,1,5,10,25,50,75,90,95,99,100)/100)
test$deciles <- decile(test$Predict.score)

library(data.table)
tmp_DT = data.table(test)
h_rank <- tmp_DT[, list(
cnt = length(TARGET), 
cnt_resp = sum(TARGET), 
cnt_non_resp = sum(TARGET == 0)) , 
by=deciles][order(-deciles)]
h_rank$rrate <- round (h_rank$cnt_resp / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_rel_resp <- round(h_rank$cum_resp / sum(h_rank$cnt_resp),2);
h_rank$cum_rel_non_resp <- round(h_rank$cum_non_resp / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_rel_resp - h_rank$cum_rel_non_resp);


library(scales)
h_rank$rrate <- percent(h_rank$rrate)
h_rank$cum_rel_resp <- percent(h_rank$cum_rel_resp)
h_rank$cum_rel_non_resp <- percent(h_rank$cum_rel_non_resp)

View(h_rank)



=================================eda

x$BALANCE = scale(x$BALANCE)
x$AMT_ATM_DR = scale(x$AMT_ATM_DR)
x$AVG_AMT_PER_ATM_TXN = scale(x$AVG_AMT_PER_ATM_TXN)
x$AVG_AMT_PER_NET_TXN = scale(x$AVG_AMT_PER_NET_TXN)

summary(BALANCE)
boxplot(BALANCE)
hist(BALANCE)
sd(BALANCE)
