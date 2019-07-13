
########################################################################################
#Random Forest############
#install.packages("randomForest")
library(randomForest)

# Creating Development and Validation Sample
getwd()
PL_Loan_RF1<- read.csv("PL_XSELL.csv", header = TRUE)
library(caTools)
set.seed(100)
splitdata1 <- sample(2, nrow(PL_Loan_RF1), replace = TRUE, prob = c(0.7,0.3))
PL_Loan_RF1.Dev <- PL_Loan_RF1[splitdata1 == 1,]
PL_Loan_RF1.Holdout <- PL_Loan_RF1[splitdata1 ==2,]
View(PL_Loan_RF1.Dev)
View(PL_Loan_RF1.Holdout)
nrow(PL_Loan_RF1.Dev)
nrow(PL_Loan_RF1.Holdout)

##check table split
table(PL_Loan_RF1$TARGET)
table(PL_Loan_RF1.Dev$TARGET)
table(PL_Loan_RF1.Holdout$TARGET)

##Build Random Forest Model
RF_Model1 <- randomForest(as.factor(TARGET) ~ ., data = PL_Loan_RF1.Dev[,c(-1,-11)], 
                   ntree=501, mtry = 6, nodesize = 10,
                   importance=TRUE)
#?randomForest
print(RF_Model1)

##OOB error plot

plot(RF_Model1, main="")
legend("topright", c("OOB", "0", "1"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest PL_Loan_RF1.Dev")


RF_Model1$err.rate

## List the importance of the variables.
impVar <- round(randomForest::importance(RF_Model1), 2)
impVar[order(impVar[,4], decreasing=TRUE),]


#?tuneRF
## Tuning Random Forest
tRF <- tuneRF(x = PL_Loan_RF1.Dev[,-c(1,2,11)], 
              y=as.factor(PL_Loan_RF1.Dev$TARGET),
              mtryStart = 3, 
              ntreeTry=75, 
              stepFactor = 1.2, 
              improve = 0.001, 
              trace=TRUE, 
              plot = TRUE,
              doBest = TRUE,
              nodesize = 350, 
              importance=TRUE
)

tRF$importance


View(PL_Loan_RF1.Dev)
## Scoring syntax
PL_Loan_RF1.Dev$predict.class <- predict(tRF, PL_Loan_RF1.Dev, type="class")
PL_Loan_RF1.Dev$predict.score <- predict(tRF, PL_Loan_RF1.Dev, type="prob")
#head(PL_Loan_RF1.Dev)
class(PL_Loan_RF1.Dev$predict.score)

## deciling
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


PL_Loan_RF1.Dev$deciles <- decile(PL_Loan_RF1.Dev$predict.score[,2])


library(data.table)
tmp_DT = data.table(PL_Loan_RF1.Dev)
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

sum(PL_Loan_RF1.Dev$TARGET) / nrow(PL_Loan_RF1.Dev)


library(ROCR)
pred <- prediction(PL_Loan_RF1.Dev$predict.score[,2], PL_Loan_RF1.Dev$TARGET)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
KS

## Area Under Curve
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)
auc

## Gini Coefficient
library(ineq)
gini = ineq(PL_Loan_RF1.Dev$predict.score[,2], type="Gini")
gini

## Classification Error
with(PL_Loan_RF1.Dev, table(TARGET, predict.class))

View(PL_Loan_RF1.Holdout)
## Scoring syntax
PL_Loan_RF1.Holdout$predict.class <- predict(tRF, PL_Loan_RF1.Holdout, type="class")
PL_Loan_RF1.Holdout$predict.score <- predict(tRF, PL_Loan_RF1.Holdout, type="prob")

PL_Loan_RF1.Holdout$deciles <- decile(PL_Loan_RF1.Holdout$predict.score[,2])


## Ranking for holdout sample
tmp_DT = data.table(PL_Loan_RF1.Holdout)
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





##PLot lift chart
perf <- performance(pred,"lift","rpp")
plot(perf, main="lift curve", colorize=T)

#install.packages("InformationValue")
library(InformationValue)
##Concordance
Concordance(actuals=PL_Loan_RF1.Holdout$TARGET, predictedScores=PL_Loan_RF1.Holdout$predict.score)


