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

## CART Technique
library(rpart)
library(rpart.plot)

## Target Rate 
sum(dev1$TARGET)/14076


## setting the control paramter inputs for rpart
r.ctrl = rpart.control(minsplit=100, minbucket = 10, cp = 0, xval = 10)

## calling the rpart function to build the tree
m1 <- rpart(formula = TARGET ~ ., 
            data = dev1[,-c(1,11)], method = "class", 
            control = r.ctrl)
m1


#install.packages("rattle")
#install.packages("RcolorBrewer")
library(rattle)
library(RColorBrewer)
fancyRpartPlot(m1)

## to find how the tree performs
printcp(m1)
plotcp(m1)


## Pruning Code
ptree<- prune(m1, cp= 0.0018 ,"CP")
printcp(ptree)
fancyRpartPlot(ptree, uniform=TRUE,  main="Pruned Classification Tree")

## Let's use rattle to see various model evaluation measures

View(dev1)
## Scoring syntax
dev1$predict.class <- predict(ptree, dev1, type="class")
dev1$predict.score <- predict(ptree, dev1)
head(dev1)


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


class(dev1$predict.score)

## deciling
dev1$deciles <- decile(dev1$predict.score[,2])
View(dev1)

##Ranking Code
#install.packages("data.table")
library(data.table)
tmp_DT = data.table(dev1)
rank <- tmp_DT[, list(
  cnt = length(TARGET), 
  cnt_resp = sum(TARGET), 
  cnt_non_resp = sum(TARGET == 0)) , 
  by=deciles][order(-deciles)]
rank$rrate <- round(rank$cnt_resp * 100 / rank$cnt,2);
rank$cum_resp <- cumsum(rank$cnt_resp)
rank$cum_non_resp <- cumsum(rank$cnt_non_resp)
rank$cum_perct_resp <- round(rank$cum_resp * 100 / sum(rank$cnt_resp),2);
rank$cum_perct_non_resp <- round(rank$cum_non_resp * 100 / sum(rank$cnt_non_resp),2);
rank$ks <- abs(rank$cum_perct_resp - rank$cum_perct_non_resp);
View(rank)


## Scoring Holdout sample
hold1$predict.class <- predict(ptree, hold1, type="class")
hold1$predict.score <- predict(ptree, hold1)

hold1$deciles <- decile(hold1$predict.score[,2])
View(hold1)

## Ranking code
##install.packages("data.table")
library(data.table)
tmp_DT = data.table(hold1)
h_rank <- tmp_DT[, list(
  cnt = length(TARGET), 
  cnt_resp = sum(TARGET), 
  cnt_non_resp = sum(TARGET == 0)) , 
  by=deciles][order(-deciles)]
h_rank$rrate <- round(h_rank$cnt_resp * 100 / h_rank$cnt,2);
h_rank$cum_resp <- cumsum(h_rank$cnt_resp)
h_rank$cum_non_resp <- cumsum(h_rank$cnt_non_resp)
h_rank$cum_perct_resp <- round(h_rank$cum_resp * 100 / sum(h_rank$cnt_resp),2);
h_rank$cum_perct_non_resp <- round(h_rank$cum_non_resp * 100 / sum(h_rank$cnt_non_resp),2);
h_rank$ks <- abs(h_rank$cum_perct_resp - h_rank$cum_perct_non_resp);
View(h_rank)



##To calculate KS, AUC, GINI
#install.packages("ROCR")
library(ROCR)
pred <- prediction(dev1$predict.score[,2], dev1$TARGET)
perf <- performance(pred, "tpr", "fpr")
plot(perf)
KS <- max(attr(perf, 'y.values')[[1]]-attr(perf, 'x.values')[[1]])
auc <- performance(pred,"auc"); 
auc <- as.numeric(auc@y.values)

#install.packages("ineq")
library(ineq)
gini = ineq(dev1$predict.score[,2], type="Gini")

with(dev1, table(TARGET, predict.class))
with(hold1, table(TARGET, predict.class))
auc
KS
gini

## Concordance and Discordance

