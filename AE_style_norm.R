## 3.3.2 Emergence of a Style Norm

library(e1071)
library(caret)
library(randomForest)
library(party)

## Predict Condition based on features of images.
doRandomForest2 <- function(names, party=F, PSAMPLE=0.6, ntree=2000, mtry=NA,
                            conditional=TRUE, var.predicted="ex", proximity=FALSE,
                            data=p1pramt, seed=NA) {

    if (!is.na(seed)) set.seed(seed)

    data <- data[!is.na(data[,var.predicted]),]

    PSAMPLE <- PSAMPLE
    len <- nrow(data)
    sample.idx <- sample(len, len * PSAMPLE)

    train <- data[sample.idx,]
    test <- data[-sample.idx,]


    ## Add + sign between exploratory variables.
    varNames <- paste(names, collapse = "+")
    myformula <- as.formula(paste(var.predicted, " ~ ", varNames),)

    if (!party) {
        model <- randomForest(myformula,
                              train,
                              ntree=ntree,
                              proximity=proximity,
                              importance=T, na.action=na.omit)


        ## Variable Importance Table.
        var.imp <- importance(model, type=1)
        ## Make sure the name of the column is correct.
        colnames(var.imp) <- "MeanDecreaseAccuracy"
        ## Make row names as columns.
        var.imp <- data.frame(Variable=row.names(var.imp),
                              MeanDecreaseAccuracy=var.imp)

        ## Predicting response variable in train.
        train$predicted.response <- predict(model, train)

        ## Predicting response variable in validation (test).
        test$predicted.response <- predict(model, test)

    } else {

        model <- cforest(myformula,
                         train,
                         controls = cforest_unbiased(ntree=ntree))


        ## Predicting response variable in train.
        train$predicted.response <- predict(model, train, OOB=TRUE,
                                            type = "response")

        ## Predicting response variable in validation (test).
        test$predicted.response <- predict(model, test, OOB=TRUE,
                                           type = "response")

        var.imp <- varimp(model, conditional=conditional)
        var.imp <- data.frame(Variable=row.names(as.matrix(var.imp)),
                              MeanDecreaseAccuracy=var.imp)
    }

    ## Sort importance.
    var.imp <- var.imp[order(var.imp$MeanDecreaseAccuracy, decreasing = T),]
    row.names(var.imp) <- seq(1, nrow(var.imp))

    ## Create Confusion Matrix with trai.n
    cm.train <- confusionMatrix(data=train$predicted.response,
                                reference=train[,var.predicted])


    ## Create Confusion Matrix with train.
    cm.test <- confusionMatrix(data=test$predicted.response,
                               reference=test[,var.predicted])


    return(list(model=model, cm.train=cm.train,
                cm.test=cm.test, importance=var.imp))
}

allFeaturesNames <- names(p1pramt)[8:20]

###################### Predicting Condition based on features all rounds.

aa <- NULL
for (i in 1:100) {
    fit.loop <- doRandomForest2(allFeaturesNames, var.predicted = "condition",
                                data=p1pramt)
    tmp <- as.data.frame(t(fit.loop$cm.test$overall))
    ##
    if (!exists("aa")) {
        aa <- tmp
    } else {
        aa <- rbind(aa, tmp)
    }
}

summ <- summarySE(aa, "Accuracy") ## 78-80%


###################### Predicting Condition based on features by groups of rounds.

aa <- NULL
for (i in 1:100) {
    ## First 3 rounds = 68
    fit1.loop <- doRandomForest2(allFeaturesNames, var.predicted = "condition",
                            data=p1pramt[p1pramt$round <= 3,])
    ## Second 3 rounds = 70
    fit2.loop <- doRandomForest2(allFeaturesNames, var.predicted = "condition",
                            data=p1pramt[p1pramt$round > 3 & p1pramt$round <= 6,])
    ## Third 3 rounds = 79
    fit3.loop <- doRandomForest2(allFeaturesNames, var.predicted = "condition",
                            data=p1pramt[p1pramt$round > 6 & p1pramt$round <= 9,])
    ## Last 3 rounds = 82
    fit4.loop <- doRandomForest2(allFeaturesNames, var.predicted = "condition",
                            data=p1pramt[p1pramt$round > 9,])
    ## fit0 <- doRandomForest2(allFeaturesNames, var.predicted = "condition",
    ##                         data=p1pramt[p1pramt$round == 1,])
    ## fit.last <- doRandomForest2(allFeaturesNames, var.predicted = "condition",
    ##                             data=p1pramt[p1pramt$round == 12,])
    tmp <- rbind(fit1.loop$cm.test$overall, fit2.loop$cm.test$overall,
                fit3.loop$cm.test$overall, fit4.loop$cm.test$overall)
    tmp <- as.data.frame(tmp)
    tmp$Rounds <- c("1-3", "4-6", "7-9", "10-12")
    ##
    if (!exists("aa")) {
        aa <- tmp
    } else {
        aa <- rbind(aa, tmp)
    }
}

summ <- summarySE(aa, "Accuracy", "Rounds")
summ$Rounds <- factor(summ$Rounds, levels=c("1-3", "4-6", "7-9", "10-12"))

ggplot(summ, aes(Rounds, Accuracy, group=1)) + geom_point(size=5) + geom_line(alpha=0.6) +
    geom_errorbar(aes(ymin=Accuracy-ci,ymax=Accuracy+ci), width=0.4)


## Exhibitions Specialization.
##############################


mydata=p1pramt[p1pramt$round > 1 &
               p1pramt$age.group2 %in% c("young", "old") &
               p1pramt$svo.group %in% c("Individualistic", "Prosocial")
              ,]
mydatap <- mydata[mydata$published == 1,]

fit.abs.exA <- lmer(abstract ~ round + exA + (1|session/player), data=mydata)
summary(fit.abs.exA)

fit.abs.exA.pub <- lmer(abstract ~ round + exA + (1|session/player), data=mdp)
summary(fit.abs.exA.pub)



fit.A.nc.abs <- lmer(abstract ~ round +  exA +
                            (1|session/player),
                 data=mydata[mydata$stratified == 1,])
summary(fit.A.nc.abs)

fit.A.nc.abs.pub <- lmer(abstract ~ round + exA +
                            (1|session/player),
                     data=mydata[mydata$stratified == 1 &
                                 mydata$published == 1,])
summary(fit.A.nc.abs.pub)

fit.A.nc.crea <- lmer(creativity ~ round + exA +
                            (1|session/player),
                 data=mydata[mydata$stratified == 1,])
summary(fit.A.nc.crea)

fit.A.nc.crea.pub <- lmer(creativity ~ round +  exA +
                            (1|session/player),
                     data=mydata[mydata$stratified == 1 &
                                 mydata$published == 1,])
summary(fit.A.nc.crea.pub)


fit.A.nc.ove <- lmer(overall ~ round + exA +
                            (1|session/player),
                 data=mydata[mydata$stratified == 1,])
summary(fit.A.nc.ove)

fit.A.nc.ove.pub <- lmer(overall ~ round +  exA +
                            (1|session/player),
                     data=mydata[mydata$stratified == 1 &
                                 mydata$published == 1,])
summary(fit.A.nc.ove.pub)


fit.A.nc.face <- lmer(face ~ round + exA +
                            (1|session/player),
                 data=mydata[mydata$stratified == 1,])
summary(fit.A.nc.ove)

fit.A.nc.face.pub <- lmer(face ~ round +  exA +
                            (1|session/player),
                     data=mydata[mydata$stratified == 1 &
                                 mydata$published == 1,])
summary(fit.A.nc.ove.pub)

ttexreg(list(
    fit.A.nc.ove, fit.A.nc.ove.pub, fit.A.nc.face, fit.A.nc.face.pub,
    fit.A.nc.crea, fit.A.nc.crea.pub, fit.A.nc.abs, fit.A.nc.abs.pub
    ))



## Including controls.
######################

fit.A.nc.abs <- lmer(abstract ~ round +  exA +
                         r.mean.from + age.group + svo.group + skill +
                            (1|session/player),
                 data=mydata[mydata$stratified == 1,])
summary(fit.A.nc.abs)

fit.A.nc.abs.pub <- lmer(abstract ~ round + exA +
                         r.mean.from + age.group + svo.group + skill +
                            (1|session/player),
                     data=mydata[mydata$stratified == 1 &
                                 mydata$published == 1,])
summary(fit.A.nc.abs.pub)

fit.A.nc.crea <- lmer(creativity ~ round + exA +
                         r.mean.from + age.group + svo.group + skill +
                            (1|session/player),
                 data=mydata[mydata$stratified == 1,])
summary(fit.A.nc.crea)

fit.A.nc.crea.pub <- lmer(creativity ~ round +  exA +
                         r.mean.from + age.group + svo.group + skill +
                            (1|session/player),
                     data=mydata[mydata$stratified == 1 &
                                 mydata$published == 1,])
summary(fit.A.nc.crea.pub)


fit.A.nc.ove <- lmer(overall ~ round + exA +
                         r.mean.from + age.group + svo.group + skill +
                            (1|session/player),
                 data=mydata[mydata$stratified == 1,])
summary(fit.A.nc.ove)

fit.A.nc.ove.pub <- lmer(overall ~ round +  exA +
                         r.mean.from + age.group + svo.group + skill +
                            (1|session/player),
                     data=mydata[mydata$stratified == 1 &
                                 mydata$published == 1,])
summary(fit.A.nc.ove.pub)


fit.A.nc.face <- lmer(face ~ round + exA +
                         r.mean.from + age.group + svo.group + skill +
                            (1|session/player),
                 data=mydata[mydata$stratified == 1,])
summary(fit.A.nc.ove)

fit.A.nc.face.pub <- lmer(face ~ round +  exA +
                         r.mean.from + age.group + svo.group + skill +
                            (1|session/player),
                     data=mydata[mydata$stratified == 1 &
                                 mydata$published == 1,])
summary(fit.A.nc.ove.pub)


ttexreg(list(
    fit.A.nc.ove, fit.A.nc.ove.pub, fit.A.nc.face, fit.A.nc.face.pub,
    fit.A.nc.crea, fit.A.nc.crea.pub, fit.A.nc.abs, fit.A.nc.abs.pub
    ))

