## 3.1.2 Quality Consumption Under Constraints

library(foreign)
library(sandwich)
library(lmtest)
library(MuMIn)

sessionsStratified <- unique(stratified$session)
sessionsFlat <- unique(flat$session)

simulateConsumer <- function(session=NA, condition=NA, iters=100, budget=1) {
    ##
    if (!is.na(session)) {
        cond <- p1pramt[p1pramt$session == session, "condition"][1]
    }
    if (!is.na(condition)) {
        if (exists("cond") && cond != condition)  {
            stop(paste0("session not in condition: ", session, " ", condition))
        }
    }
    for (i in 1:iters) {
        mydf <- simulateConsumerCond(condition=cond,
                                     session=session, budget=budget)

        mydf$idx <- i
        if (i == 1) {
            df <- mydf
        } else {
            df <- rbind(df, mydf)
        }
    }
    df.melted <- melt(df, id=c("condition", "idx", "round",
                               "quality.avg", "quality.clean.avg",
                               "inn.avg", "creativity.avg",
                               "face.avg", "abstract.avg",
                               "quality.max", "quality.clean.max",
                               "inn.max", "creativity.max",
                               "face.max", "abstract.max"))
    return(df.melted)
}


simulateConsumerCond <- function(condition, session, budget) {
    ##
    if (budget > 7 || budget < 1) {
        stop("budget cannot be more than 7 or less than 1")
    }
    ##
    if (is.na(session)) {
        if (condition == "Stratified") {
            session <- sample(sessionsStratified,1)
        } else {
            session <- sample(sessionsFlat,1)
        }
    }
    mydata = p1pramt[p1pramt$session == session,]
    for (r in 1:12) {
        mydatar <- mydata[mydata$round == r & mydata$published == 1,]
        ## Stratified
        if (condition == "Stratified") {
            rowsA <- mydatar[mydatar$ex == "A",]
            rowsB <- mydatar[mydatar$ex == "B",]
            rowsC <- mydatar[mydatar$ex == "C",]
            nA <- nrow(rowsA)
            nB <- nrow(rowsB)
            nC <- nrow(rowsC)
            ##
            ## Different budgets.
            if (budget == 1) {
                if (nA == 1) {
                    rows <- rowsA
                }
                else {
                    if (nB == 1) {
                        rows <- rowsB
                    } else if (nB == 2) {
                        rows <- rowsB[sample(1:2,1),]
                    } else if (nB == 0) {
                        rows <- rowsC[sample(1:nC,1),]
                    }
                }
                ## Budget 2
            } else if (budget == 2) {
                if (nA == 0) {
                    if (nB == 2) {
                        rows <- rowsB
                    } else if (nB == 1) {
                        rows <- rbind(rowsB, rowsC[sample(1:nC,1),])
                    } else if (nB == 0) {
                        rows <- rowsC[sample(1:nC,2),]
                    }
                } else if (nB == 1) {
                    rows <- rbind(rowsA, rowsB)
                } else if (nB == 2) {
                    rows <- rbind(rowsA, rowsB[sample(1:2,1),])
                } else if (nB == 0) {
                    rows <- rbind(rowsA, rowsC[sample(1:nC,1),])
                }
                ## Budget 3
            } else if (budget == 3) {
                nAB <- nA + nB
                if (nAB == 3) {
                    rows <- rbind(rowsA, rowsB)
                } else if (nAB > 0) {
                    needed <- budget-nAB
                    if (needed < nC) {
                        rowsC <- rowsC[sample(1:nC,needed), ]
                    }
                    rows <- rbind(rowsA, rowsB, rowsC)
                } else {
                    rows <- rowsC
                }
            } else if (budget == 4 || budget == 5 || budget == 6) {
                nAB <- nA + nB
                if (nAB > 0) {
                    needed <- budget-nAB
                    if (needed < nC) {
                        rowsC <- rowsC[sample(nC,needed), ]
                    }
                    rows <- rbind(rowsA, rowsB, rowsC)
                } else {
                    rows <- rowsC
                }
            }
            else if (budget == 7) {
                rows <- mydatar
            }

            ## Flat.
        } else {
            ## 6 places in the flat market.
            N = nrow(mydatar)
            if (N > budget) {
                rows = mydatar[sample(N, budget),]
            } else {
                rows <- mydatar
            }
        }
        ## Compute measures.
        ##
        ## Avg.
        avgQuality <- mean(rows$r.mean.from)
        avgQualityClean <- mean(rows$r.mean.from.clean.asskill)
        avgInn <- mean(rows$d.pubprev)
        avgCrea <- mean(rows$creativity)
        avgFace <- mean(rows$face)
        avgAbstract <- mean(rows$abstract)
        ## Max.
        maxQuality <- max(rows$r.mean.from)
        maxQualityClean <- max(rows$r.mean.from.clean.asskill)
        maxInn <- max(rows$d.pubprev)
        maxCrea <- max(rows$creativity)
        maxFace <- max(rows$face)
        maxAbstract <- max(rows$abstract)
        ##
        consumption <- c(r, avgQuality, avgQualityClean, avgInn, avgCrea,
                         avgFace, avgAbstract,
                         maxQuality, maxQualityClean, maxInn, maxCrea, maxFace,
                         maxAbstract)
        if (exists('mydf')) {
            mydf <- rbind(mydf, consumption)
        } else {
            mydf <- consumption
        }
    }
    mydf <- as.data.frame(mydf, row.names = 1:nrow(mydf))
    cols <- c("round", "quality.avg", "quality.clean.avg", "inn.avg",
              "creativity.avg", "face.avg", "abstract.avg",
              "quality.max", "quality.clean.max", "inn.max", "creativity.max",
              "face.max", "abstract.max")
    colnames(mydf) <- cols
    mydf$condition <- condition
    mydf$session <- session
    return(mydf)
}

## SIMULATE

#consumption10 <- consumption

consumption <- NULL
for (b in 1:7) {
    print(b)
    for (s in GOOD.SESSIONS) {
        aa <- simulateConsumer(iters=100, budget=b, session=s)
        aa$budget <- b
        if (!exists("consumption")) {
            consumption <- aa
        } else {
            consumption <- rbind(consumption, aa)
        }
    }
}


## ANALYSIS

mysm <- summarySE(consumption, "quality.clean.avg", c("condition", "budget"))
p <- ggplot(mysm, aes(budget, quality.clean.avg, color=condition))
p <- p + geom_line() + geom_point()
p <- p + geom_errorbar(aes(ymin=quality.clean.avg-ci, ymax=quality.clean.avg+ci))
p

gggsave("quality-clean-avg.png")

mysm <- summarySE(consumption, "quality.avg", c("condition", "budget"))
p <- ggplot(mysm, aes(budget, quality.avg, color=condition))
p <- p + geom_line() + geom_point()
p <- p + geom_errorbar(aes(ymin=quality.avg-ci, ymax=quality.avg+ci))
p

gggsave("quality-avg.png")

mysm <- summarySE(consumption, "inn.avg", c("condition", "budget"), na.rm=TRUE)
p <- ggplot(mysm, aes(budget, inn.avg, color=condition))
p <- p + geom_line() + geom_point()
p <- p + geom_errorbar(aes(ymin=inn.avg-ci, ymax=inn.avg+ci))
p

gggsave("inn-avg.png")


mysm <- summarySE(consumption, "abstract.avg", c("condition", "budget"))
p <- ggplot(mysm, aes(budget, abstract.avg, color=condition))
p <- p + geom_line() + geom_point()
p <- p + geom_errorbar(aes(ymin=abstract.avg-ci, ymax=abstract.avg+ci))
p

gggsave("abstract-avg.png")

mysm <- summarySE(consumption, "creativity.avg", c("condition", "budget"))
p <- ggplot(mysm, aes(budget, creativity.avg, color=condition))
p <- p + geom_line() + geom_point()
p <- p + geom_errorbar(aes(ymin=creativity.avg-ci, ymax=creativity.avg+ci))
p

gggsave("creativity-avg.png")


mysm <- summarySE(consumption, "face.avg", c("condition", "budget"))
p <- ggplot(mysm, aes(budget, face.avg, color=condition))
p <- p + geom_line() + geom_point()
p <- p + geom_errorbar(aes(ymin=face.avg-ci, ymax=face.avg+ci))
p

gggsave("face-avg.png")
