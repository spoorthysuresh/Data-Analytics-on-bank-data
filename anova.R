getwd()
mydata <- read.csv("PL_XSELL.csv", header = TRUE)
View(mydata)
attach(mydata)

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

#principal component analysis
##dev1 <- dev1[,-c(1,4,6,10,11)]
#dev1 <- dev1[,-c(1,c(7:14),c(16:31),34)]
#prin_comp <- prcomp(dev1, scale. = T)
#names(prin_comp)


#outputs the mean of variables
#prin_comp$center

#outputs the standard deviation of variables
#prin_comp$scale

#prin_comp$rotation
#round(prin_comp$rotation,digits = 4)
#prin_comp$rotation[1:5,1:4]

#biplot(prin_comp, scale = 0)

#compute standard deviation of each principal component
#std_dev <- prin_comp$sdev
#std_dev
#compute variance
#pr_var <- std_dev^2
#pr_var

#hypothesis testing
library(dplyr)
library(ggpubr)
library(multcomp)
library(MASS)
dev1 <- dev1[,-c(1,4,6,7,10,11)]
anova1 <- aov(TARGET ~ ., data = dev1)
summary(anova1)
plot(anova1)
levene1 <- leveneTest(TARGET ~ ., data = dev1)
summary(levene1)
dev1 <- dev1[,-c(1)]
names(dev1)
chisq.test(dev1)
