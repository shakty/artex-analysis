## 3.2.4 Inequality in Earnings

mysummary <- summarySE(pr, "payoff", c("condition", "player"), na.rm=TRUE)
summarySE(mysummary, "sum", "condition")


# Payoff Hist.
p <- ggplot(mysummary, aes(round(payoff), color=condition, group=condition, fill=condition))
p <- p + geom_bar(alpha=0.5, position="dodge")
p <- p + ylab("Count") + xlab("Total Payoff")
#p <- p + facet_grid(~condition)
p <- p + myThemeMod + theme(legend.position = c(0.8, 0.85),
                            legend.title = element_text(vjust=3, size=24, face="bold")
                            )
p

x <- mysummary[mysummary$condition == "Stratified",]$payoff
y <- mysummary[mysummary$condition == "Flat",]$payoff

ks.test(x, y)

skewness(x, na.rm=TRUE)
skewness(y, na.rm=TRUE)

## This is excess kurtosis.
kurtosis(stratified$payoff, na.rm=TRUE)
kurtosis(flat$payoff, na.rm=TRUE)


## Average session payoff.

meanPayoff <- function(x) {
    return(mean(x$payoff))
}


mysummary <- summarySE(pr, "payoff", c("condition", "session"),
                       na.rm=TRUE)

payoff.sessions <- doSessionStats(mysummary, meanPayoff, "payoff")


doPlotMean("payoff", P1=FALSE, data=payoff.sessions, format="png",
           PREFIX="aggr_session_", SAVE=SAVEIMG)


mysummary.r <- summarySE(pr, "payoff", c("condition", "session", "round"),
                       na.rm=TRUE)
payoff.sessions.round <- doSessionStats(mysummary.r, meanPayoff, "payoff",
                                        round=TRUE)


doPlotRound("payoff", P1=FALSE, data=payoff.sessions.round, format="png",
            PREFIX="aggr_session_", SAVE=SAVEIMG)

summarySE(payoff.sessions, "payoff", "condition")
t.test(payoff~condition, data=payoff.sessions)
wilcox.test(payoff~condition, data=pr)


## Inequality of all data points by condition.

ineq(mysummary[mysummary$condition == "Stratified", ]$sum, type="Gini")
ineq(mysummary[mysummary$condition == "Flat", ]$sum, type="Gini")


## Average session Inequality.

doGini <- function(x) {
    return(ineq(x$sum, type="Gini"))
}

doGiniRound <- function(x) {
    return(ineq(x$payoff, type="Gini"))
}

doGiniRoundCum <- function(x) {
    return(ineq(x$payoff.cum, type="Gini"))
}


mysummary <- summarySE(pr, "payoff", c("condition", "session", "player"),
                       na.rm=TRUE)

gini.sessions <- doSessionStats(mysummary, doGini, "gini")


doPlotMean("gini", P1=FALSE, data=gini.sessions,
            format="png", SAVE=SAVEIMG, PREFIX="by_sessionround_")

summarySE(gini.sessions, "gini", "condition")
t.test(gini~condition, data=gini.sessions)


## Average session/round inequality.

gini.sessions.round <- doSessionStats(pr, doGiniRound, "gini", round=TRUE)


doPlotRound("gini", P1=FALSE, data=gini.sessions.round,
            format="png", SAVE=SAVEIMG, PREFIX="by_sessionround_")

fit <- lm(gini ~ as.numeric(round) + condition, data=gini.sessions.round)
summary(fit)


## Average session Inequality Cumulative earnings.

gini.sessions.round.cum <- doSessionStats(p1pr, doGiniRoundCum, "gini", round=TRUE)

doPlotRound("gini", P1=FALSE, data=gini.sessions.round.cum,
            format="png", SAVE=SAVEIMG, PREFIX="by_sessionround_cum_")



## Simulation.
##############

## Saved simulated datasets.
## load("files/inequality_FINAL.RData")
## save.image("files/inequality_FINAL.RData")

library(foreign)
library(sandwich)
library(lmtest)
library(MuMIn)

simulateGames <- function(G.stratified=9, G.flat=10, R=12, iters=1000, gini=TRUE,
                          avgRound=TRUE, melt=TRUE, cons=FALSE, ordered=TRUE,
                          pChange=-1) {
    ##
    for (i in 1:iters) {
        if (cons == TRUE) {
            mydf.stratified <- simulateGameCond3("Stratified", G=G.stratified, R=R,
                                            gini=gini, avgRound=avgRound,
                                            ordered=ordered, pChange=pChange)
            mydf.flat <- simulateGameCond3("Flat", G=G.flat, R=R,
                                           gini=gini, avgRound=avgRound,
                                           ordered=ordered, pChange=pChange)
        } else {
            mydf.stratified <- simulateGameCond("Stratified", G=G.stratified, R=R,
                                           gini=gini, avgRound=avgRound,
                                           ordered=ordered)
            mydf.flat <- simulateGameCond("Flat", G=G.flat, R=R,
                                          gini=gini, avgRound=avgRound,
                                          ordered=ordered)
        }
        mydf <- rbind(mydf.stratified, mydf.flat)
        mydf$idx <- i
        if (i == 1) {
            df <- mydf
        } else {
            df <- rbind(df, mydf)
        }
    }
    if (cons==TRUE || !melt) return(df)

    if (gini) {
        df.melted <- melt(df, id=c("condition", "idx", "game", "gini",
                                   "Amis", "Bmis", "Cmis",
                                   "TotMis", "TotMis.adjusted"))
        cols <- c("condition",  "idx", "game", "gini",
                  "Amis", "Bmis", "Cmis", "TotMis", "TotMis.adjusted",
                  "p", "payoff")

        colnames(df.melted) <- cols
    } else {
        df.melted <- melt(df, id=c("condition", "idx", "game",
                                   "Amis", "Bmis", "Cmis",
                                   "TotMis", "TotMis.adjusted"))
        cols <- c("condition",  "idx", "game",
                  "Amis", "Bmis", "Cmis", "TotMis", "TotMis.adjusted",
                  "p", "payoff")
        ## Might not work here.
        ## colnames(df.melted) <- cols
    }
    return(df.melted)
}

simulateGameCond <- function(condition, G=10, R=12, ordered=TRUE, gini=TRUE,
                             avgRound=TRUE) {
    ##
    if (condition == "Stratified") {
        exLimits <- c(1,2,4)
        exPayoffs <- c(500, 250, 125)
        expectedMis <- 2
    } else {
        exLimits <- c(2,2,2)
        exPayoffs <- c(250, 250, 250)
        expectedMis <- 3
    }
    for (g in 1:G) {
        payoffs <- c(0, 0, 0, 0, 0, 0, 0, 0, 0)
        for (r in 1:R) {

            ##  Heterogeneous.
##             exs <- rep(NA, 9)
##             exs[1] <- sample(c(1,2,3), 1, prob=c(0.9, 0.15, 0.05), replace=TRUE)
##             exs[2] <- sample(c(1,2,3), 1, prob=c(0.7, 0.2, 0.1), replace=TRUE)
##             exs[3] <- sample(c(1,2,3), 1, prob=c(0.6, 0.3, 0.1), replace=TRUE)
##             exs[4] <- sample(c(1,2,3), 1, prob=c(0.6, 0.3, 0.1), replace=TRUE)
##             exs[5] <- sample(c(1,2,3), 1, prob=c(0.5, 0.3, 0.2), replace=TRUE)
##             exs[6] <- sample(c(1,2,3), 1, prob=c(0.5, 0.3, 0.2), replace=TRUE)
##             exs[7] <- sample(c(1,2,3), 1, prob=c(0.2, 0.3, 0.3), replace=TRUE)
##             exs[8] <- sample(c(1,2,3), 1, prob=c(0.2, 0.2, 0.4), replace=TRUE)
##             exs[9] <- sample(c(1,2,3), 1, prob=c(0.1, 0.4, 0.5), replace=TRUE)

            exs <- sample(c(1,2,3), 9, replace=TRUE)

            exCounters <- c(0, 0, 0)
            order <- sample(1:9, 9)
            for (p in order) {
                e <- exs[p]
                if (exCounters[e] < exLimits[e]) {
                    payoffs[p] <- payoffs[p] + exPayoffs[e]
                }
                exCounters[e] <- exCounters[e] + 1
            }
            exMiscos <- exCounters - exLimits
            totMis <- sum(exMiscos[exMiscos > 0])
            totMis.adjusted <- totMis - expectedMis
        }
        if (ordered == TRUE) {
            payoffs <- payoffs[order(payoffs)]
        }
        if (avgRound) {
            payoffs <- payoffs / R
        }
        if (gini) {
            gg <- ineq(payoffs, type="Gini")
            payoffs <- c(payoffs, gg)
        }
        payoffs <- c(payoffs, g, exMiscos, totMis, totMis.adjusted)
        if (exists('mydf')) {
            mydf <- rbind(mydf, payoffs)
        } else {
            mydf <- payoffs
        }
    }
    mydf <- as.data.frame(mydf, row.names = 1:nrow(mydf))
    cols <- paste0("p", seq(1,9))
    if (gini == TRUE) {
        cols <- c(cols, "gini")
    }
    cols <- c(cols, "game", "Amis", "Bmis", "Cmis", "TotMis", "TotMis.adjusted")
    colnames(mydf) <- cols
    mydf$condition <- condition
    return(mydf)
}

## THIS COMPUTES MORE DATA, E.G. Number of Consecutive Submissions.
simulateGameCond2 <- function(condition, G=10, R=12, ordered=TRUE, gini=TRUE,
                              pChange = -1, avgRound=TRUE) {
    ##
    if (condition == "Stratified") {
        exLimits <- c(1,2,4)
        exPayoffs <- c(500, 250, 125)
        expectedMis <- 2
    } else {
        exLimits <- c(2,2,2)
        exPayoffs <- c(250, 250, 250)
        expectedMis <- 3
    }
    ##
    for (g in 1:G) {
        payoffs <- rep(0, 9)
        subs <- matrix(rep(0, 27), ncol=9, nrow=3, dimnames=list(c("subA", "subB", "subC")))
        pubs <- matrix(rep(0, 27), ncol=9, nrow=3, dimnames=list(c("pubA", "pubB", "pubC")))
        lastSubs <- rep(0, 9)
        ## All zeros.
        consecutiveSubs <- matrix(rep(0, 9*R), ncol=9, nrow=R)
        ##
        for (r in 1:R) {
            ## Resample outlets according to probs.
            if (pChange == -1 || runif(1) < pChange) {
                exs <- sample(c(1,2,3), 9, replace=TRUE)
            }
            exCounters <- c(0, 0, 0)
            order <- sample(1:9, 9)
            for (p in order) {
                e <- exs[p]
                ## Update submission.
                subs[e,p] <- subs[e,p] + 1
                ## Update publication.
                if (exCounters[e] < exLimits[e]) {
                    payoffs[p] <- payoffs[p] + exPayoffs[e]
                    pubs[e,p] <- pubs[e,p] + 1
                }
                exCounters[e] <- exCounters[e] + 1
                if (r > 1) {
                    if (lastSubs[p] == e) {
                        ## If previous round was not same sub, it is a zero.
                        consecutiveSubs[r, p] <- consecutiveSubs[(r-1), p] + 1
                    }
                }
            }
            ## Store reference to old submissions.
            lastSubs <- exs
            ## Miscordination in the round.
            exMiscos <- exCounters - exLimits
            totMis <- sum(exMiscos[exMiscos > 0])
            totMis.adjusted <- totMis - expectedMis
        }
        if (ordered == TRUE) {
            myorder <- order(payoffs)
            payoffs <- payoffs[myorder]
        }
        if (avgRound) {
            payoffs <- payoffs / R
        }

        payoffs <- matrix(payoffs, nrow=9, dimnames=list(paste0("p", 1:9), "payoff"))

        consecutiveSubs.max <- apply(consecutiveSubs, 2, max)
        ## If somebody ALWAYS submitted to same ex.
        consecutiveSubs.max[consecutiveSubs.max == (R-1)] <- R
        consecutiveSubs.avg <- apply(consecutiveSubs[2:R,], 2, mean)
        plStats <- rbind(subs, pubs, consecutiveSubs.max, consecutiveSubs.avg)
        if (ordered) {
            plStats <- plStats[,myorder]
        }
        plStats <- t(plStats)
        payoffs <- cbind(payoffs, plStats)

        payoffs <- cbind(payoffs, rep(g, 9))

        ## THIS IS WRONG.
        if (gini) {
            g <- ineq(payoffs, type="Gini")
            payoffs <- cbind(payoffs, rep(g))
        }
        payoffs <- cbind(payoffs,
                         matrix(rep(c(exMiscos, totMis, totMis.adjusted), 9),
                                nrow=9,byrow=TRUE))
        payoffs <- cbind(paste0("p", 1:9), payoffs)

        if (exists('mydf')) {
            mydf <- rbind(mydf, payoffs)
        } else {
            mydf <- payoffs
        }
    }
    mydf <- as.data.frame(mydf, row.names = 1:nrow(mydf))
    cols <- c("p", "payoff", "subA", "subB", "subC", "pubA", "pubB", "pubC",
              "cons", "cons.avg", "game")
    if (gini == TRUE) {
        cols <- c(cols, "gini")
    }
    cols <- c(cols, "Amis", "Bmis", "Cmis", "TotMis", "TotMis.adjusted")
    colnames(mydf) <- cols
    mydf$condition <- condition
    return(mydf)
}


## Less data than 2, faster.
simulateGameCond3 <- function(condition, G=10, R=12, ordered=TRUE, gini=TRUE,
                              pChange = -1, avgRound=TRUE) {
    ##
    if (condition == "Stratified") {
        exLimits <- c(1,2,4)
        exPayoffs <- c(500, 250, 125)
        expectedMis <- 2
    } else {
        exLimits <- c(2,2,2)
        exPayoffs <- c(250, 250, 250)
        expectedMis <- 3
    }
    ##
    out <- data.table()
    ##
    for (g in 1:G) {
        payoffs <- rep(0, 9)
        subs <- matrix(rep(0, 27), ncol=9, nrow=3, dimnames=list(c("subA", "subB", "subC")))
        pubs <- matrix(rep(0, 27), ncol=9, nrow=3, dimnames=list(c("pubA", "pubB", "pubC")))
        lastSubs <- rep(0, 9)
        ## All zeros.
        consecutiveSubs <- matrix(rep(0, 9*R), ncol=9, nrow=R)
        ##
        for (r in 1:R) {
            ## Resample outlets according to probs.
            if (pChange == -1 || runif(1) < pChange) {
                exs <- sample(c(1,2,3), 9, replace=TRUE)
            }
            exCounters <- c(0, 0, 0)
            order <- sample(1:9, 9)
            for (p in order) {
                e <- exs[p]
                ## Update submission.
                subs[e,p] <- subs[e,p] + 1
                ## Update publication.
                if (exCounters[e] < exLimits[e]) {
                    payoffs[p] <- payoffs[p] + exPayoffs[e]
                    pubs[e,p] <- pubs[e,p] + 1
                }
                exCounters[e] <- exCounters[e] + 1
                if (r > 1) {
                    if (lastSubs[p] == e) {
                        ## If previous round was not same sub, it is a zero.
                        consecutiveSubs[r, p] <- consecutiveSubs[(r-1), p] + 1
                    }
                }
            }
            ## Store reference to old submissions.
            lastSubs <- exs
            ## Miscordination in the round.
            exMiscos <- exCounters - exLimits
            totMis <- sum(exMiscos[exMiscos > 0])
            totMis.adjusted <- totMis - expectedMis
        }
        ## GINI.
        mygini <- ineq(payoffs, type="Gini")
        ## SORTING.
        consecutiveSubs.avg <- apply(consecutiveSubs[2:R,], 2, mean)
        ##
        row <- data.table(game=g,
                          payoff.avg=mean(payoffs),
                          gini=mygini,
                          cons.avg=mean(consecutiveSubs.avg),
                          totMis=totMis,
                          totMis.adjusted=totMis.adjusted
                          )
        out <- rbindlist(list(out, row))

    }
    out$condition <- condition
    return(out)
}

#############
## ## IN-GAME
#############

## GINI.

cb <- function(x) {
    summ <- summarySE(x, "payoff", "player")
    gini <- ineq(summ$sum, type="Gini")
    summ$session <- x$session[1]
    summ$condition <- x$condition[1]
    return(c(gini=gini,
             session=as.character(x$session[1]),
             condition=as.character(x$condition[1])))
}
gini.ses <- ddply(p1pr, "session", cb)
gini.ses$gini <- as.numeric(gini.ses$gini)
gini.ses$type <- "Game"

doPlotDistr("gini", P1=FALSE, data=gini.ses, PREFIX="simulated_",
            SAVE=SAVEIMG, format="png")

summary(gini.ses[gini.ses$condition == "Flat",]$gini)
summary(gini.ses[gini.ses$condition == "Stratified",]$gini)

## SORTING.
###########

## Compute max.cons as well.
max.cons.r <- ddply(p1pramt, c("session", "condition", "player"), function(x) {
    x <- x[order(x$round),]
    currentMax <- 0
    maxes <- rep(NA, 12)
    for (r in 2:12) {
        mycons <- x[r,]$ex.same.count
        if (mycons > currentMax) {
            currentMax <- mycons
        }
        maxes[r] <- currentMax
    }
    return(cbind(round=x$round,
                 cons.max=maxes))
})

p1pramt <- merge(p1pramt, max.cons.r, by=c("session", "condition", "player", "round"))


cb <- function(x) {
    summ <- summarySE(x, "ex.same.count", "session", na.rm=TRUE)
    cons <- summ$ex.same.count
    cons.sd <- summ$sd
    summ <- summarySE(x, "cons.max", "session", na.rm=TRUE)
    cons.max <- summ$cons.max
    cons.max.sd <- summ$sd
    return(c(cons=cons,
             cons.sd=cons.sd,
             cons.max=cons.max,
             cons.max.sd=cons.max.sd,
             session=as.character(x$session[1]),
             condition=as.character(x$condition[1])))
}
cons.ses <- ddply(p1pramt, "session", cb)
cons.ses$cons <- as.numeric(as.character(cons.ses$cons))
cons.ses$cons.sd <- as.numeric(as.character(cons.ses$cons.sd))
cons.ses$cons.max <- as.numeric(as.character(cons.ses$cons.max))
cons.ses$cons.max.sd <- as.numeric(as.character(cons.ses$cons.max.sd))

cons.ses.full <- cons.ses

cons.ses <- cons.ses[,c("cons", "session", "condition")]
colnames(cons.ses)[1] <- "cons.avg"
cons.ses$type <- "Game"

## By player.

cons.ses.player <- summarySE(p1pramt, "ex.same.count",
                             c("session", "condition", "player"), na.rm=TRUE)
cons.ses.player <- cons.ses.player[,c("condition", "session", "player", "ex.same.count")]
colnames(cons.ses.player)[4] <- "cons.avg"
cons.ses.player$type <- "Game"
summary(cons.ses.player$cons.avg)

## Merging (session level).

ginicons.ses <- merge(gini.ses, cons.ses, by=c("session", "condition", "type"))

## Regressions with Game Data: GINI and SORTING relationship.
#############################################################

mydata=p1pramt[p1pramt$round > 1 &
               p1pramt$age.group2 %in% c("young", "old") &
               p1pramt$svo.group %in% c("Individualistic", "Prosocial")
              ,]


## SORTING REGR.

## Significant. Stratified *
fit.cons <- lmer(ex.same.count ~  round + condition + (1|session/player), data=p1pramt)
summary(fit.cons)

## Significant. Stratified +
fit.cons.controls2 <- lmer(ex.same.count ~ round + condition +
                              payoff.cum.rel.ses.lag.1.sc +
                              svo.group +
                              skill + belief.choice + age.group2 +
                              (1|session/player), mydata)
summary(fit.cons.controls2)

## Significant. Stratified*round
fit.cons.int <- lmer(ex.same.count ~ round*condition + (1|session/player), data=p1pramt)
summary(fit.cons.int)

## Significant. Stratified*round
fit.cons.controls <- lmer(ex.same.count ~ round*condition +
                              payoff.cum.rel.ses.lag.1.sc +
                              svo.group + female +
                              skill + belief.choice + age.group2 +
                              (1|session/player), mydata)
summary(fit.cons.controls)


## Significant
fit.cons.controls.only <- lmer(ex.same.count ~ round +
                              payoff.cum.rel.ses.lag.1.sc +
                              svo.group + female +
                              skill + belief.choice + age.group2 +
                              (1|session/player), mydata)
summary(fit.cons.controls.only)

texreg(list(fit.cons, fit.cons.controls2, fit.cons.int,
            fit.cons.controls),
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = '\\texttt{+}')


## Stratified has significant more consecutive submissions, which lead to higher ineq.





################ COMPARE GAME AND SIMULATIONS CONS

## Simulates batches of games as many as in our datasets (10 Flat, 9 Stratified)
## For a given number of iterations. Computes payoffs and ginis.
## Important! ordered=TRUE, messes up subs and pubsx by player in ex

## start.time <- Sys.time()
## sim <- simulateGames(iters=10000, avgRound=FALSE, ordered=FALSE, cons=TRUE)
## end.time <- Sys.time()
## time.taken <- end.time - start.time
## print(time.taken)

## SIM rows= 10*ITERS + 9*ITERS

## Save file.
## write.csv(sim, "files/sim.csv", row.names=FALSE)

## Load saved file.
sim <- read.csv("files/sim.csv", head=T)

summarySE(sim[, c("gini", "condition")], "gini", c("condition"))
tmp <- summarySE(p1pramt, "ex.same.count", "player", na.rm=TRUE)
summary(tmp$ex.same.count)

## GINI.

gini.sim <- sim[,c("condition", "game", "idx", "gini")]
gini.sim$type <- "Sim"
gini.sim$session <- paste0(gini.sim$condition, "_", gini.sim$idx,
                           "_", gini.sim$game)
gini.sim$game <- NULL
gini.sim$idx <- NULL
gini.sim <- gini.sim[!(duplicated(gini.sim$session)),]

## SORTING.

cons.sim <- sim[,c("condition", "game", "idx", "cons.avg")]
cons.sim$type <- "Sim"
cons.sim$session <- paste0(cons.sim$condition, "_", cons.sim$idx,
                           "_", cons.sim$game)
cons.sim$game <- NULL
cons.sim$idx <- NULL

cons <- rbind(cons.ses, cons.sim)
cons$id <- paste0(cons$condition, "_", cons$session)

## Merging GINICONS.

ginicons.sim <- merge(cons.sim, gini.sim[,c("session", "gini")], by="session")
ginicons.sim <- ginicons.sim[,c("session", "condition", "gini", "cons.avg")]
ginicons.sim$type <- "Sim"

ginicons <- rbind(ginicons.ses, ginicons.sim)
ginicons$type <- factor(ginicons$type, levels=c("Sim","Game"))
ginicons$condition <- as.factor(ginicons$condition)

## Averaging results by ITER.

sim.avg <- summarySE(sim[,c("gini","idx","condition")], "gini", c("condition", "idx"))

sim.avg2 <- summarySE(sim[,c("cons.avg","idx","condition")], "cons.avg", c("condition", "idx"))

sim.avg <- merge(sim.avg[,c("condition", "idx", "gini")],
                 sim.avg2[,c("condition", "idx", "cons.avg")])

sim.avg2 <- summarySE(sim[,c("totMis.adjusted","idx","condition")], "totMis.adjusted",
                      c("condition", "idx"))

sim.avg <- merge(sim.avg,
                 sim.avg2[,c("condition", "idx", "totMis.adjusted")],
                 by=c("condition", "idx"))

ggplot(sim.avg, aes(cons.avg, gini, color=condition)) +
    geom_jitter() + geom_smooth(method="lm")


## GINI.

gini.sim.avg <- sim.avg[,c("condition", "idx", "gini")]
gini.sim.avg$type <- "Sim.Avg"
gini.sim.avg$session <- paste0(gini.sim.avg$condition, "_", gini.sim.avg$idx)
gini.sim.avg$idx <- NULL
gini.sim.avg <- gini.sim.avg[!(duplicated(gini.sim.avg$session)),]

## SORTING.

cons.sim.avg <- sim.avg[,c("condition", "idx", "cons.avg")]
cons.sim.avg$type <- "Sim.Avg"
cons.sim.avg$session <- paste0(cons.sim.avg$condition, "_", cons.sim.avg$idx)
cons.sim.avg$game <- NULL
cons.sim.avg$idx <- NULL

cons.avg <- rbind(cons.ses, cons.sim.avg)
cons.avg$id <- paste0(cons.avg$condition, "_", cons.avg$session)

## cons.sim.avg.player <- sim.avg[,c("condition", "game", "idx", "player", "cons.avg")]
## cons.sim.avg.player$session <- paste0(cons.sim.avg.player$game, "_", cons.sim.avg.player$idx)
## cons.sim.avg.player$game <- NULL
## cons.sim.avg.player$idx <- NULL
## cons.sim.avg.player$type <- "Sim.Avg"


fit <- lm(cons.avg ~ type, data=cons.avg)
summary(fit)
## Significant and Lower.

fit.int <- lm(cons.avg ~ type:condition, data=cons.avg)
summary(fit.int)
## Significant and Lower.

texreg(list(fit, fit.int, fit.game),
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = '\\texttt{+}')


## Merging GINICONS.

ginicons.sim.avg <- merge(cons.sim.avg, gini.sim.avg[,c("session", "gini")], by="session")
ginicons.sim.avg <- ginicons.sim.avg[,c("session", "condition", "gini", "cons.avg")]
ginicons.sim.avg$type <- "Sim.Avg"

ginicons.avg <- rbind(ginicons.ses, ginicons.sim.avg)
ginicons.avg$type <- factor(ginicons.avg$type, levels=c("Sim.Avg","Game"))
ginicons.avg$condition <- as.factor(ginicons.avg$condition)


fit <- lm(gini ~ type, data=ginicons.avg)
summary(fit) # Significant.P < 0.001

fit.cond <- lm(gini ~ type + condition, data=ginicons.avg)
summary(fit.cond) # Significant.P < 0.001

fit.cond.int <- lm(gini ~ type*condition, data=ginicons.avg)
summary(fit.cond.int) # Significant.P < 0.001

fit.stratified <- lm(gini ~ type, data=ginicons.avg[ginicons.avg$condition == "Stratified",])
summary(fit.stratified) # Significant.P < 0.001

fit.flat <- lm(gini ~ type, data=ginicons.avg[ginicons.avg$condition == "Flat",])
summary(fit.flat) # Significant.P < 0.001

fit.stratified2 <- lm(gini ~ type:cons.avg, data=ginicons.avg[ginicons.avg$condition == "Stratified",])
summary(fit.stratified2) # Significant.P < 0.001

fit.flat2 <- lm(gini ~ type:cons.avg, data=ginicons.avg[ginicons.avg$condition == "Flat",])
summary(fit.flat2) # Significant.P < 0.001

fit.game <- lm(gini ~ condition:cons.avg, data=ginicons.avg[ginicons.avg$type=="Game",])
summary(fit.game) # Significant only Stratified.

fit.sim <- lm(gini ~ condition:cons.avg, data=ginicons.avg[ginicons.avg$type=="Sim.Avg",])
summary(fit.sim) # Significant only Stratified.

texreg(list(fit, fit.cond,
            fit.cond.int,
            fit.stratified, fit.flat,
            fit.stratified2, fit.flat2,
            fit.game, fit.sim),
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = '\\texttt{+}')

## Plot Sorting.
################

ggplot(ginicons.avg[ginicons.avg$type == "Game",], aes(cons.avg, gini)) +
    geom_point(aes(color=condition), size=5, alpha=0.7) +
    geom_smooth(method="lm") + xlab('Sorting to Exhibitions') + ylab('Inequality (Gini)')

ggsave(paste0(IMGDIR, 'ineq_gini_sorting_game.png'))

ggplot(ginicons.avg[ginicons.avg$type == "Sim.Avg",],
       aes(cons.avg, gini, color=condition)) +
    geom_point(alpha=0.7) + geom_smooth(method="lm") +
    xlab('Sorting to Exhibitions') + ylab('Inequality (Gini)')

ggsave(paste0(IMGDIR, 'ineq_gini_sorting_simulation.png'))


## Payoffs.
###########

payoff.ses <- summarySE(p1pramt[p1pramt$round == 12,], "payoff.cum",
                        c("condition", "session"))
colnames(payoff.ses)[4] <- "payoff.avg"
payoff.ses$type <- "Game"

payoff.sim <- sim
payoff.sim$session <- paste0(payoff.sim$condition, "_", payoff.sim$idx)
payoff.sim <- summarySE(payoff.sim, "payoff.avg",
                        c("condition", "session"))
payoff.sim$type <- "Sim"

payoffs <- rbind(payoff.ses, payoff.sim)

fit.payoff <- lm(payoff.avg ~ type, data=payoffs)
summary(fit.payoff) # Not sign.

fit.payoff2 <- lm(payoff.avg ~ type*condition, data=payoffs)
summary(fit.payoff2) # Significant only Stratified.

texreg(list(fit.payoff, fit.payoff2),
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = '\\texttt{+}')



