## 3.3.4 Market Failures

## Which submissions are an Error 1/2.

cb <-  function(x, var) {
    errI <- errII <- rep(NA,9)
    if (!any(is.na(x[var]))) {
        ##
        idxs <- order(-x[var])
        cutoff <- ifelse(x$condition == "Flat", 6, 7)[1]
        ##
        for (i in 1:length(idxs)) {
            idx <- idxs[i]
            if (i <= cutoff) {
                errI[idx] <- 0
                errII[idx] <- ifelse(x[idx,]$published == 0, 1, 0)
            } else {
                errI[idx] <- ifelse(x[idx,]$published == 1, 1, 0)
                errII[idx] <- 0
            }
        }
    }
    return(cbind(round=x$round,
                 player=as.character(x$player),
                 errI=errI,
                 errII=errII
                 ))
}

missplaced <- ddply(p1pr, c("round", "session"), cb, "r.mean.from")
p1pr <- merge(p1pr, missplaced, by=c("session", "player", "round"))

missplaced <- ddply(p1pr, c("round", "session"), cb, "d.pubprev")
colnames(missplaced)[4:5] <- c("errI.inn", "errII.inn")
p1pr <- merge(p1pr, missplaced, by=c("session", "player", "round"))

missplaced <- ddply(p1pr, c("round", "session"), cb, "r.mean.from.clean.asskill")
colnames(missplaced)[4:5] <- c("errI.assfree", "errII.assfree")
p1pr <- merge(p1pr, missplaced, by=c("session", "player", "round"))



p1pr$errI <- as.numeric(p1pr$errI) -1
p1pr$errII <- as.numeric(p1pr$errII) -1
p1pr$errI.inn <- as.numeric(p1pr$errI.inn) -1
p1pr$errII.inn <- as.numeric(p1pr$errII.inn) -1
p1pr$errI.assfree <- as.numeric(p1pr$errI.assfree) -1
p1pr$errII.assfree <- as.numeric(p1pr$errII.assfree) -1
p1pr$round <- as.numeric(p1pr$round)


summ <- summarySE(p1pr, "errI", c("round", "condition"))

ggplot(summ, aes(round, errI, color=condition)) +
    geom_point() + geom_line() +
    geom_errorbar(aes(ymin=errI-ci, ymax=errI+ci))

summ <- summarySE(p1pr, "errI", c("condition"))

summ <- summarySE(p1pr, "errII", c("round", "condition"))

ggplot(summ, aes(round, errII, color=condition)) +
    geom_point() + geom_line(aes(group=condition)) +
    geom_errorbar(aes(ymin=errII-ci, ymax=errII+ci))

p1pr$errII.rel <- p1pr$errII / 9

summ <- summarySE(p1pr, "errII.rel", c("condition"))

ggplot(summ, aes(condition, errII.rel, fill=condition)) +
    geom_bar(stat="identity", position="dodge") +
    geom_errorbar(aes(ymin=errII.rel-ci, ymax=errII.rel+ci))


## Regressions.

## Raw Data.
fit1g <- glmer(errI ~ round + condition +
                   (1|session/player),
               data=p1pr,
               family=binomial)
summary(fit1g)

fit2g <- glmer(errII ~ round + condition +
                   (1|session/player),
               data=p1pr,
               family=binomial)
summary(fit2g)

ttexreg(list(fit1g, fit2g))

## Clean Data.

fit1g <- glmer(errI.assfree ~ round + condition +
                   (1|session/player),
               data=p1pr,
               family=binomial)
summary(fit1g)

fit2g <- glmer(errII.assfree ~ round + condition +
                   (1|session/player),
               data=p1pr,
               family=binomial)
summary(fit2g)

#####################################################
## AMT Scores and Errors (as in Artex PNAS paper). ##
#####################################################

computeErrors <- function(K, data=p1pramt, out='BOTH', add.K=FALSE) {
    data$type1Err <- ifelse(data$overall < K & data$published == 1, 1, 0)
    data$type2Err <- ifelse(data$overall >= K & data$published == 0, 1, 0)

    data$correct <- ifelse((data$overall < K & data$published == 0) |
                           (data$overall >= K & data$published == 1), 1, 0)

    idx <- which(names(data)=="type1Err")
    data.rr <- melt(data, id=names(data)[c(-idx,-(idx+1),-(idx+2))],
                    variable.name="errorType")
    data.rr$errorType <- ifelse(data.rr$errorType == "type1Err", "Type I",
                         ifelse(data.rr$errorType == "type2Err", "Type II",
                                "Correct"))
    data.rr$errorType <- factor(data.rr$errorType,
                                levels=c("Type I", "Type II", "Correct"))

    if (out == 'GLOBAL' || out == 'BOTH') {
        summary.round.global <- summarySE(data.rr, c("value"),
                                       c("errorType","condition","round"),
                                       na.rm=TRUE)
        summary.global <- summarySE(data.rr, c("value"),
                                 c("errorType","condition"),
                                 na.rm=TRUE)

        if (add.K) {
            summary.round.global$K <- K
            summary.global$K <- K
        }

        if (out == 'GLOBAL') {
            return(list(global.round = summary.round.global,
                        global=summary.global))
        }
    }

    summary.round.ex <- summarySE(data.rr, c("value"),
                                  c("errorType","condition", "ex", "round"),
                                  na.rm=TRUE)
    summary.ex <- summarySE(data.rr, c("value"),
                            c("errorType","condition", "ex"),
                            na.rm=TRUE)

    if (add.K) {
        summary.round.ex$K <- K
        summary.ex$K <- K
    }
    if (out == 'BY.EX') {
        return(list(ex.round = summary.round.ex, ex=summary.ex))
    } else {
        return(list(global.round = summary.round.global,
                    global=summary.global,
                    ex.round = summary.round.ex,
                    ex = summary.ex))
    }
}

doErrorsPlots <- function(K, data=p1pramt, idx=NA) {
    if (!is.na(idx)) {
        saveidx <- paste0("idx=", idx)
    } else {
        savedix <- paste0("K=", K)
    }

    ## Prefix for image saving.
    myprefix <- paste0(IMGDIR, "png/", IMGPREFIX)

    errors <- computeErrors(K, data)
                                        ## Type 1 and Type 2 errors
    p <- ggplot(errors$global, aes(condition, value, fill=condition))
    p <- p + geom_bar(stat="identity")
    p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci, width=0.2))
    ##p <- p + facet_grid(~errorType)
    p <- p + facet_grid(~errorType)
    p <- p + xlab("") + ylab('Avg. Error')
    ## p <- p + ylim(0,0.58)
    p <- p + scale_x_discrete(breaks=c(0,1),
                              labels=c("Flat","Stratified"))
    p <- p + myThemeMod + theme(legend.position = "none",
                                plot.margin = unit(c(10,30,10,10),"mm"),
                                axis.text.x = element_text(size=15)
                                ##strip.background = element_blank()
                                )
    p <- p + ggtitle(paste0("K = ", K))


    ggsave(paste0(myprefix, "errtype_", saveidx, ".png"), width=9)

    ## By Exhibition.


    ## Type 1 and Type 2 errors
    p <- ggplot(errors$ex, aes(condition, value, fill=condition))
    p <- p + geom_bar(stat="identity")
    p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci, width=0.2))
    p <- p + facet_grid(ex~errorType)
    ##p <- p + facet_grid(~errorType, labeller = ErrTypeLabeller)
    p <- p + xlab("") + ylab('Avg. Error')
    ## p <- p + ylim(0,0.58)
    p <- p + scale_x_discrete(breaks=c(0,1),
                              labels=c("Flat","Stratified"))
    p <- p + myThemeMod + theme(legend.position = "none",
                                plot.margin = unit(c(10,30,10,10),"mm"),
                                axis.text.x = element_text(size=15),
                                strip.background = element_blank()
                                )
    p <- p + ggtitle(paste0("K = ", K))

    ggsave(paste0(myprefix, "errtype_by_ex_", saveidx, ".png"), width=9)
}


## Does all images.
#Ks <- seq(1,7,0.5)
#idx = 1
#for (K in Ks) {
#    doErrorsPlots(K, idx=idx)
#    idx = idx + 1
#}


## Computes numbers.
idx = 1
## K is a threshold of agreement with AMT reviews. For instance,
## if K = 1, it means that all images with AMT score > 1 (out of 7)
## should have been published in the game.
Ks <- seq(1,7,0.1)
for (K in Ks) {
    errs <- computeErrors(K, add.K=TRUE)
    if (idx == 1) {
        globalErr <- errs$global
        exErr <- errs$ex
    } else {
        globalErr <- rbind(globalErr, errs$global)
        exErr <- rbind(exErr, errs$ex)
    }
    idx = idx + 1
}


aa <- exErr[exErr$condition == "Stratified",]
bb <- summarySE(aa, "value", c("errorType", "ex"))
meanOverall <- mean(p1pramt$overall)
mymeans <- summarySE(p1pramt, "overall", c("condition", "ex"))

## Overall.

ggplot(globalErr, aes(K, value)) +
  geom_vline(aes(xintercept=mean(p1pramt$overall)), alpha=0.5) +
  geom_smooth(aes(color=errorType, linetype=condition))

gggsave("errtypes_curves_smooth-overall.png"))

ggplot(globalErr, aes(K, value, color=errorType, linetype=condition)) +
  geom_vline(aes(xintercept=mean(p1pramt$overall)), alpha=0.5) +
  geom_point() + geom_line() +
  geom_errorbar(aes(ymin=value-ci, ymax=value+ci))

gggsave("errtypes_curves_ci-overall.png")

## By Condition and exhibition.

ggplot(exErr, aes(K, value, color=errorType, group=errorType)) +
    geom_vline(data=mymeans, aes(xintercept=overall), alpha=0.5) +
    geom_point() + geom_line() +
    geom_errorbar(aes(ymin=value-ci, ymax=value+ci)) +
    facet_grid(ex~condition)

gggsave("errtypes_curves_ci.png")


ggplot(exErr, aes(K, value, color=errorType, group=errorType)) +
    geom_vline(data=mymeans, aes(xintercept=overall), alpha=0.5) +
    geom_smooth() +
   facet_grid(ex~condition)

gggsave("errtypes_curves_smooth.png")
