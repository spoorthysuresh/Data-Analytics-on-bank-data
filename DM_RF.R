getwd()
mydata <- read.csv("PL_XSELL.csv", header = TRUE)
View(mydata)
attach(mydata)
names(mydata)


##split the data dev1(70%) and hold1(30%) as holdout sample has to be minimun 20%
library(caTools)
set.seed(100)
splitdata1 <- sample(2, nrow(mydata), replace = TRUE, prob = c(0.7,0.3))
dev1 <- mydata[splitdata1 == 1,]
hold1 <- mydata[splitdata1 ==2,]
View(dev1)
View(hold1)
nrow(dev1)
nrow(hold1)

## Check my data splitting accuracy
table(mydata$TARGET)
table(dev1$TARGET)
table(hold1$TARGET)

##Random Forest
#install.packages("randomForest")
library(randomForest)
?randomForest
View(dev1)
## Calling syntax to build the Random Forest
RF <- randomForest(as.factor(TARGET) ~ ., data = dev1[,-c(1,11)], 
                   ntree=501, mtry = 6, nodesize = 10,
                   importance=TRUE)
print(RF)


plot(RF, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest dev1")
RF$err.rate


## List the importance of the variables.
impVar <- round(randomForest::importance(RF), 2)
impVar[order(impVar[,4], decreasing=TRUE),]


?tuneRF
## Tuning Random Forest
tRF <- tuneRF(x = dev1[,-c(1,2,11)], 
              y=as.factor(dev1$TARGET),
              mtryStart = 3, 
              ntreeTry=101, 
              stepFactor = 1.5, 
              improve = 0.001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 300, 
              importance=TRUE
)

tRF$importance

## List the importance of the variables after tuning.
impVar1 <- round(randomForest::importance(tRF), 2)
impVar1[order(impVar1[,4], decreasing=TRUE),]

## Scoring syntax
dev1$predict.class <- predict(tRF, dev1, type="class")
dev1$predict.score <- predict(tRF, dev1, type="prob")
head(dev1)
class(dev1$predict.score)


## deciling
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

dev1$deciles <- decile(dev1$predict.score[,2])


##Ranking for Dev sample
#install.packages("data.table")
library(data.table)
tmp_DT = data.table(dev1)
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

sum(dev1$TARGET) / nrow(dev1)


## Classification Error
with(dev1, table(TARGET, predict.class))


## Scoring syntax
hold1$predict.class <- predict(tRF, hold1, type="class")
hold1$predict.score <- predict(tRF, hold1, type="prob")

hold1$deciles <- decile(hold1$predict.score[,2])


## Ranking for holdout sample
tmp_DT = data.table(hold1)
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

## Gini Coefficient
library(ineq)
gini = ineq(dev1$predict.score[,2], type="Gini")
gini


library(ROCR)
pred <- prediction(dev1$predict.score[,2], dev1$TARGET)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS

## Area Under Curve calculation
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc
##PLot lift chart
perf <- performance(pred,"lift","rpp")
plot(perf, main="LIFT CURVE", colorize=T)

