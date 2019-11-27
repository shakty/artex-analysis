source(paste0(THISDIR, 'helper/init.R'))
##
##
################ LOAD P1PR
if (exists("DO.DROPOUTS") && DO.DROPOUTS == TRUE) {
    mysessions="ALL"
} else {
    mysessions="GOOD"
    DO.DROPOUTS <- FALSE
}
##
pr <- loadGame(sessions=mysessions)
pinfo <- loadPlayersInfo()
##
## player + AMT indicators for R0 and R1.
pinfo.amt <- pinfo[, c(1, 589:591,592:601)]
##
p1 <- loadP1()
##
p1 <- merge(p1, pinfo.amt, by="player")
##
p1 <- renameCol(p1, "d.subcurr", prefix="r0.")
p1 <- renameCol(p1, "d.ownprev", prefix="r0.")
p1 <- renameCol(p1, "cf.changes.tot", prefix="r0.")
p1 <- renameCol(p1, "cf.changes.tot.norm", prefix="r0.")
##
## This is quite balanced, now the jump is mostly in middle category.
p1$r0.d.subcurr.group <- cut(p1$r0.d.subcurr, c(0, 0.1638, 0.2139, 0.4))
## This is balanced, 4 in top group, great jump.
p1$r0.d.subcurr.group <- cut(p1$r0.d.subcurr, c(0, 0.2, 0.275, 0.4))
## This is balanced, 5 in top group, great jump, group1 also increases a bit.
p1$r0.d.subcurr.group <- cut(p1$r0.d.subcurr, c(0, 0.2, 0.263, 0.4))
## This is balanced, 5 in top group, up to median, rest. great jump.
p1$r0.d.subcurr.group <- cut(p1$r0.d.subcurr, c(0, 0.1836, 0.263, 0.4))
## This is balanced, 5 in top group, up to first quantile, rest. great jump.
p1$r0.d.subcurr.group <- cut(p1$r0.d.subcurr, c(0, 0.1638, 0.263, 0.4))
##
p1$skill <- p1$r0.d.subcurr
p1$skill.group10 <- p1$r0.d.subcurr.group
p1$skill.group.q <- cut(p1$r0.d.subcurr, c(0, 0.1638, 0.2139, 0.4))
p1$skill.group.2cat <- cut(p1$skill, c(0, mean(p1$skill), 0.4))
##
## This is balanced, 5 in top group, up to first quantile, rest. great jump.
p1$skill.group.dc <- cut(p1$r0.d.subcurr, 10, labels=1:10)
##
p1$skill2 <- p1$cf.changes.tot.norm
##
p1 <- p1[p1$player %in% unique(pr$player),]
##
p1pr <- mergeGameP1()
##
table(p1pr$r0.d.subcurr.group, p1pr$condition)/12
table(p1pr$r0.d.subcurr.group, p1pr$condition)
##
p1pr$exA <- ifelse(p1pr$ex == "A", 1, 0)
p1pr$exB <- ifelse(p1pr$ex == "B", 1, 0)
p1pr$exC <- ifelse(p1pr$ex == "C", 1, 0)
##
p1pr$exAB <- ifelse(p1pr$ex %in% c("A", "B"), 1, 0)
p1pr$exBC <- ifelse(p1pr$ex %in% c("C", "B"), 1, 0)
p1pr$exC <- ifelse(p1pr$ex %in% c("C"), 1, 0)
##
p1pr$r.assed <- ifelse(p1pr$r1.ass.kill.from == 1 |
                       p1pr$r2.ass.kill.from == 1 |
                       p1pr$r3.ass.kill.from == 1, 1, 0)
##
tmp <- ddply(p1pr, c("player", "round"), function(x) {
    r.ass.sum <- sum(x$r1.ass.kill,
                     x$r2.ass.kill,
                     x$r3.ass.kill)
    return(c(r.ass.sum=r.ass.sum,
             r.ass=ifelse(r.ass.sum > 0, 1, 0)))
})
##
p1pr <- merge(p1pr, tmp, by=c("player", "round"))
##
##
#### AMT SCORES
##
scores <- loadAMTScores(mysessions)
scores$image <- scores$id
scores$time.image.sec <- scores$time.image / 1000
##
scores.melted <- meltAMTScores(scores)
##
scores.img.overall <- summarySE(scores, "overall",
                                c("image", "creator",
                                  "condition", "session", "round"))
colnames(scores.img.overall)[8:11] <- paste0("overall.", c("sd", "sum", "se", "ci"))
##
scores.img.creativity <- summarySE(scores, "creativity", c("image"))
colnames(scores.img.creativity)[4:7] <- paste0("creativity.", c("sd", "sum", "se", "ci"))
scores.img.creativity$N <- NULL
##
scores.img.abstract <- summarySE(scores, "abstract", c("image"))
colnames(scores.img.abstract)[4:7] <- paste0("abstract.", c("sd", "sum", "se", "ci"))
scores.img.abstract$N <- NULL
##
scores.img.face <- summarySE(scores, "face", c("image"))
colnames(scores.img.face)[4:7] <- paste0("face.", c("sd", "sum", "se", "ci"))
scores.img.face$N <- NULL
##
scores.img <- merge(scores.img.overall, scores.img.creativity, by="image")
scores.img <- merge(scores.img, scores.img.face, by="image")
scores.img <- merge(scores.img, scores.img.abstract, by="image")
##
scores.img$CAT <- (scores.img$overall + scores.img$creativity + scores.img$face) / 3
##
nrow(scores.img)
##
scores.img.game <- scores.img[scores.img$condition != "Practice",]
##
scores.img.game$player <- scores.img.game$creator
scores.img.game$session <- NULL
scores.img.game$condition <- NULL
scores.img.game$creator <- NULL
##
## MERGE
########
prp1 <- mergeGameP1()
p1pramt <- merge(p1pr, scores.img.game, by=c("player", "round"))
p1pramt$conex <- as.factor(paste0(p1pramt$condition, p1pramt$ex))
#######
##
## Moving Averages.
##
cb <- function(x) {
    if (nrow(x) == 1) {
        inn <- NA
        div <- NA
        crea <- NA
        face <- NA
        own <- NA
        abstract <- NA
        ove <- NA
        rmf <- NA
        rmfc <- NA
    } else {
        x <- x[order(x$round),]
        inn <- sma(x$d.pubprev)$fitted
        div <- sma(x$d.subcurr)$fitted
        own <- sma(x$d.ownprev)$fitted
        crea <- sma(x$creativity)$fitted
        face <- sma(x$face)$fitted
        abstract <- sma(x$abstract)$fitted
        ove <- sma(x$overall)$fitted
        rmf <- sma(x$r.mean.from)$fitted
        rmfc <- sma(x$r.mean.from.clean.asskill)$fitted
    }
    ##
    return(cbind(
        round=x$round,
        ma.inn=inn,
        ma.div=div,
        ma.own=own,
        ma.inn.lag.1=shift(inn),
        ma.div.lag.1=shift(div),
        ma.own.lag.1=shift(own),
        ma.creativity=crea,
        ma.face=face,
        ma.overall=ove,
        ma.abstract=abstract,
        ma.creativity.lag.1=shift(crea),
        ma.face.lag.1=shift(face),
        ma.overall.lag.1=shift(ove),
        ma.abstract.lag.1=shift(abstract),
        ma.r.mean.from=rmf,
        ma.r.mean.from.lag.1=shift(rmf),
        ma.r.mean.from.clean.asskill=rmfc,
        ma.r.mean.from.clean.asskill.lag.1=shift(rmfc)
    ))
}
##
ma <- ddply(p1pramt, c("session", "player"), cb)
head(ma)
##
p1pramt <- merge(p1pramt, ma, by=c("session", "player", "round"))
##
p1pramt$ma.own.diff1 <- p1pramt$ma.own - p1pramt$ma.own.lag.1
p1pramt$ma.inn.diff1 <- p1pramt$ma.inn - p1pramt$ma.inn.lag.1
p1pramt$ma.creativity.diff1 <- p1pramt$ma.creativity - p1pramt$ma.creativity.lag.1
p1pramt$ma.face.diff1 <- p1pramt$ma.face - p1pramt$ma.face.lag.1
p1pramt$ma.overall.diff1 <- p1pramt$ma.overall - p1pramt$ma.overall.lag.1
p1pramt$ma.abstract.diff1 <- p1pramt$ma.abstract - p1pramt$ma.abstract.lag.1
p1pramt$ma.own.diff1.abs <- abs(p1pramt$ma.own.diff1)
p1pramt$ma.r.mean.from.diff1 <- p1pramt$ma.r.mean.from - p1pramt$ma.r.mean.from.lag.1
p1pramt$ma.r.mean.from.clean.asskill.diff1 <- p1pramt$ma.r.mean.from.clean.asskill -
    p1pramt$ma.r.mean.from.clean.asskill.lag.1
##
cb <-  function(x, var) {
    x <- x[order(x$round),]
    crea <- shift(x$creativity)
    face <- shift(x$face)
    abstract <- shift(x$abstract)
    ove <- shift(x$overall)
    return(cbind(
        round=x$round,
        creativity.lag.1=crea,
        face.lag.1=face,
        overall.lag.1=ove,
        abstract.lag.1=abstract
    ))
}
lagged <- ddply(p1pramt, c("session", "player"), cb)
head(lagged)
##
p1pramt <- merge(p1pramt, lagged, by=c("session", "player", "round"))
##
p1pramt$abstract.diff <- p1pramt$abstract - p1pramt$abstract.lag.1
p1pramt$creativity.diff <- p1pramt$creativity - p1pramt$creativity.lag.1
p1pramt$face.diff <- p1pramt$face - p1pramt$face.lag.1
p1pramt$overall.diff <- p1pramt$overall - p1pramt$overall.lag.1
##
## Compute relative payoffs.
##
## We need na.rm=TRUE when using the dropout data.
avgCumPayoffs <- summarySE(p1pr, "payoff.cum", c("condition", "round"), na.rm=TRUE)
avgCumPayoffs.session <- summarySE(p1pr, "payoff.cum", c("session", "condition", "round"), na.rm=TRUE)
##
tmp <- avgCumPayoffs[,c("condition", "round", "payoff.cum")]
colnames(tmp)[3] <- "payoff.cum.avg.cond"
p1pramt <- merge(p1pramt, tmp, by=c("condition", "round"))
##
tmp <- avgCumPayoffs.session[,c("condition", "round", "session", "payoff.cum")]
colnames(tmp)[4] <- "payoff.cum.avg.ses"
p1pramt <- merge(p1pramt, tmp, by=c("condition", "round", "session"))
##
p1pramt$payoff.cum.rel.cond <- p1pramt$payoff.cum - p1pramt$payoff.cum.avg.cond
p1pramt$payoff.cum.rel.ses <- p1pramt$payoff.cum - p1pramt$payoff.cum.avg.ses
##
## Lag relative payoff.
##
cb <- function(x, var) {
    x <- x[order(x$round),]
    p1 <- shift(x$payoff.cum.rel.cond)
    p2 <- shift(x$payoff.cum.rel.ses)
    ##
    return(cbind(
        round=x$round,
        payoff.cum.rel.cond.lag.1=p1,
        payoff.cum.rel.ses.lag.1=p2
    ))
}
lagged <- ddply(p1pramt, c("session", "player"), cb)
head(lagged)
##
p1pramt <- merge(p1pramt, lagged, by=c("session", "player", "round"))
##
## Scale Cumulative Payoff
##
p1pramt$payoff.cum.sc <- scale(p1pramt$payoff.cum)
p1pramt$payoff.cum.lag.1.sc <- scale(p1pramt$payoff.cum.lag.1)
##
p1pramt$payoff.cum.rel.cond.sc <- scale(p1pramt$payoff.cum.rel.cond)
p1pramt$payoff.cum.rel.ses.sc <- scale(p1pramt$payoff.cum.rel.ses)
##
p1pramt$payoff.cum.rel.cond.lag.1.sc <- scale(p1pramt$payoff.cum.rel.cond.lag.1)
p1pramt$payoff.cum.rel.ses.lag.1.sc <- scale(p1pramt$payoff.cum.rel.ses.lag.1)
##
p1pramt$success.rel <- p1pramt$payoff.cum.rel.ses.lag.1.sc
##
## Skill3.
## Skill3 measures how fast they can move the sliders. Some people do not do anything in
## practice so we don't have a measure for them. We use round 1 to impute.
p1pramt$skill3 <- ifelse(p1pramt$r0.cf.changes.tot < 200, NA, p1pramt$r0.cf.changes.tot.norm)
##
## Impute NA using Round 1.
p1pramt$skill3 <- apply(p1pramt, 1, function(x) {
    res <-
    if (is.na(x["skill3"])) {
        res <- p1pramt[p1pramt$round == 1 & p1pramt$player == x["player"],]$cf.changes.tot.norm
    } else {
        res <- x["skill3"]
    }
    return(as.numeric(res))
})
p1pramt$skill3.unscaled <- p1pramt$skill3
##
p1pramt$skill3 <- scale(p1pramt$skill3)
p1pramt$skill3.group <- ifelse(p1pramt$skill3 < 0, "Low Potential", "High Potential")
##
p1pramt$skill.scaled <- scale(p1pramt$skill)
p1pramt$skillskill <- p1pramt$skill.scaled * p1pramt$skill3
##
p1pramt$stratified <- ifelse(p1pramt$condition == "Stratified", 1, 0)
##
## Fix.
p1pramt[is.na(p1pramt$r.mean.from.clean.asskill),]$r.mean.from.clean.asskill <-
    p1pramt[is.na(p1pramt$r.mean.from.clean.asskill),]$r.mean.from.clean5
##
p1pramt$ex.same <- ifelse(p1pramt$ex.same.count > 0, 1, 0)
##
p1pramt$ex2 <- ifelse(p1pramt$condition == "Flat", "Flat", as.character(p1pramt$ex))
p1pramt$ex2 <- factor(p1pramt$ex2, levels=c("Flat", "A", "B", "C"))
##
## Count how many submissions per round per exhibition.
cb <- function(x) {
    tt <- table(x$ex)
    return(cbind(
        nA=tt["A"],
        nB=tt["B"],
        nC=tt["C"],
        miscoA=ifelse(x$stratified[1], tt["A"] - 1, tt["A"] - 2),
        miscoB=ifelse(x$stratified[1], tt["B"] - 2, tt["B"] - 2),
        miscoC=ifelse(x$stratified[1], tt["C"] - 4, tt["C"] - 2)
    ))
}
subs <- ddply(p1pramt, c("round", "session"), cb)
p1pramt <- merge(p1pramt, subs, by=c("round", "session"))
##
p1pramt$nEx <- apply(p1pramt, 1, function(x) {
    return(as.numeric(x[paste0("n",x["ex"])]))
})
##
p1pramt$exNPubs <- ifelse(p1pramt$condition == "Flat", 2, ifelse(p1pramt$ex == "A", 1,
                                                          ifelse(p1pramt$ex == "B", 2, 4)))
##
cb <- function(x) {
    if (as.numeric(x[["round"]]) == 1) return(NA)
    if (is.na(x[["ex.same"]])) return(NA)
    if (is.na(x[["ex"]])) return(NA)
    if (x[["condition"]] == "Flat") return(0) ## or NA??
    if (as.numeric(x[["ex.same"]]) == 1) return(0)
    if (x[["ex"]] == "A") {
        if (x[["ex.lag.1"]] == "B") return(1)
        else return(3) ## or 2?
    } else if (x[["ex"]] == "B") {
        if (x[["ex.lag.1"]] == "A") return(-1)
        else return(2) ## or 1?
    } else if (x[["ex"]] == "C") {
        if (x[["ex.lag.1"]] == "A") return(-3) ## or -2
        else return(-2)
    } else {
        stop("wtf")
    }
}
p1pramt$exChangeDirection <- apply(p1pramt, 1, cb)
##
p1pramt$copy <- p1pramt$copy.x
p1pramt$copy.y <- p1pramt$copy.x <- NULL
##
p1pramt$rank1 <- ifelse(p1pramt$rank.ex == 1, 1, 0)
##
p1pramt$flat <- ifelse(p1pramt$stratified == 1, 0, 1)
##
###############################################
stratified <- p1pramt[p1pramt$condition == "Stratified",]
flat <- p1pramt[p1pramt$condition == "Flat",]
#############################################
##
## REVIEW
reviewsp1pramt <- meltReviewsLast(p1pramt)
##########################################
##
############## LOAD DROPOUTS
##
if (DO.DROPOUTS) {
    ## LOAD DATA in AE_LATEST.
    ##
    p1pramt <- p1pramt[!(p1pramt$session) %in% UNWANTED.SESSIONS,]
    ##
    cb <- function(x) {
        cbind(gini.cum=ineq(x$payoff.cum, type="Gini"))
    }
    ineqrounds <- ddply(p1pramt, c("session", "condition", "round"), cb)
    p1pramt <- merge(p1pramt, ineqrounds, by=c("session", "condition", "round"))
    ineqrounds$session.dropout <- ifelse(ineqrounds$session %in% GOOD.SESSIONS, 0, 1)
    ##
    ## Add nplayers per round.
    bb <- summarySE(pr, "payoff", c("session", "round", "condition"))
    bb2 <- summarySE(pr, "payoff", c("session", "condition"), na.rm=TRUE)
    bb2$nn <- floor(bb2$N / 12)
    bb3 <- summarySE(bb2, "payoff", c("condition", "nn"))
    bb <- bb[,c("session", "round", "N")]
    colnames(bb)[3] <- "nplayers"
    p1pramt <- merge(p1pramt, bb, by=c("session", "round"))
    ##
    dropoutList <- loadDropouts()
    dropoutList$dropout <- 1
    ##
    dropoutList <- dropoutList[!(dropoutList$session %in% UNWANTED.SESSIONS),]
    ##
    ## Who disconnects first in the same session?
    DT <- as.data.table(dropoutList)
    ##
    ## Select all first dropouts (see comments below also).
    first.dropouts <- DT[ , .SD[which.min(round)], by = c("session")]$player
    first.dropouts <- c(as.character(first.dropouts), "ZUOCqClk")
    ##
    dropoutList$first.dropout <- ifelse(dropoutList$player %in% first.dropouts, 1, 0)
    ##
    dropoutList$ps <- paste0(dropoutList$player, dropoutList$session) #
    uniqueDp <- dropoutList[!(duplicated(dropoutList$ps)),]
    sessionDropouts <- unique(dropoutList$session)
    ##
    p1$dropout <- as.factor(ifelse(p1$player %in% unique(dropoutList$player), 1, 0))
    p1$first.dropout <- as.factor(ifelse(p1$player %in% first.dropouts, 1, 0))
    ##
    p1pr$dropout <- as.factor(ifelse(p1pr$player %in% unique(dropoutList$player), 1, 0))
    p1pr$first.dropout <- as.factor(ifelse(p1pr$player %in% first.dropouts, 1, 0))
    ##
    p1pramt$session.dropout <- ifelse(p1pramt$session %in% GOOD.SESSIONS, 0, 1)
    ##
    p1pramt$dropout <- as.factor(ifelse(p1pramt$player %in% unique(dropoutList$player), 1, 0))
    ##
    p1pramt$first.dropout <- as.factor(ifelse(p1pramt$player %in% first.dropouts, 1, 0))
    p1pramt$first.dropout.char <- ifelse(p1pramt$first.dropout == 1, "First Dropout", "Stay")
    p1pramt$first.dropout.char <- factor(p1pramt$first.dropout.char, levels=c("Stay", "First Dropout"))
    ##
    p1pramt$dropout.char <- ifelse(p1pramt$dropout == 1, "Dropout", "Stay")
    p1pramt$dropout.char <- factor(p1pramt$dropout.char, levels=c("Stay", "Dropout"))
    ##
    aa <- DT[ , .SD[which.min(round)], by = c("session")]
    aa$round <- as.numeric(aa$round)
    aa$session <- as.character(aa$session)
    p1pramt$round.num <- as.numeric(p1pramt$round)
    p1pramt$session.char <- as.character(p1pramt$session)
    p1pramt$dropout.num <- as.integer(p1pramt$dropout) -1
    p1pramt$first.dropout.num <- as.integer(p1pramt$first.dropout) - 1
    ##
    p1pramt$r.diff <- p1pramt$r.mean - p1pramt$r.mean.from
    ##
    ##
    p1pramt$dropout.round <- 0
    p1pramt$dropout.thisround <- 0
    p1pramt$dropout.step <- 0
    p1pramt$dropout.happened <- 0
    p1pramt$dropout.first.happened <- 0
    ## Somehow there is a mismatch in uniqueDp about the disconnection round.
    ## We redo all the db and check the time of the step.
    ## for (r in 1:nrow(uniqueDp)) {
    p1pramt <- p1pramt[order(p1pramt$player, p1pramt$round),]
    for (player in uniqueDp$player) {
        dropoutRound <- max(p1pramt[p1pramt$player == player,]$round)
        row <- p1pramt[p1pramt$player == player &
                       p1pramt$round == dropoutRound,]
        p1pramt[p1pramt$player == player &
                p1pramt$round == dropoutRound,]$dropout.step <-
            ifelse(is.na(row$timeSubmission), 2,
            ifelse(is.na(row$timeReview), 3, 4))
        p1pramt[p1pramt$player == player &
                p1pramt$round == dropoutRound,]$dropout.thisround <- 1
        p1pramt[p1pramt$player == player,]$dropout.round <- dropoutRound
        p1pramt[p1pramt$session == row$session &
                p1pramt$round == dropoutRound,]$dropout.happened <- 1
        if (row$first.dropout == 1) {
            p1pramt[p1pramt$session == row$session &
                    p1pramt$round == dropoutRound,]$dropout.first.happened <- 1
        }
    }
    ## Adjust game-process and lagged variables accordingly.
    ## This is the relevant information at the time of dropout.
    p1pramt$dropout.success.rel <- apply(p1pramt, 1, function(x) {
        if (x["dropout.step"] == "4" | x["dropout.thisround"] != "1") {
            return(as.numeric(x["payoff.cum.rel.ses.sc"]))
        } else {
            if (x["round"] == "1") return(NA)
            return(as.numeric(x["payoff.cum.rel.ses.lag.1.sc"]))
        }
    })
    p1pramt$dropout.published <- apply(p1pramt, 1, function(x) {
        if (x["dropout.step"] == "4" | x["dropout.thisround"] != "1") {
            return(as.numeric(x["published"]))
        } else {
            if (x["round"] == "1") return(NA)
            return(as.numeric(x["published.lag.1"]))
        }
    })
    p1pramt$dropout.r.mean <- apply(p1pramt, 1, function(x) {
        if (is.na(x["r.mean"])) {
            if (x["round"] == "1") {
                return(NA)
            } else {
                return(x["r.mean.lag.1"])
            }
        } else {
            return(x["r.mean"])
        }
    })
    p1pramt$dropout.r.mean.from <- apply(p1pramt, 1, function(x) {
        if (x["dropout.step"] == "4" | x["dropout.thisround"] != "1") {
            return(as.numeric(x["r.mean.from"]))
        } else {
            if (x["round"] == "1") return(NA)
            return(as.numeric(x["r.mean.from.lag.1"]))
        }
    })
    p1pramt$dropout.payoff <- apply(p1pramt, 1, function(x) {
        if (x["dropout.step"] == "4" | x["dropout.thisround"] != "1") {
            return(as.numeric(x["payoff"]))
        } else {
            if (x["round"] == "1") return(NA)
            return(as.numeric(x["payoff.lag.1"]))
        }
    })
    p1pramt$dropout.ex <- apply(p1pramt, 1, function(x) {
        if (is.na(x["ex"])) {
            return(x["ex.lag.1"])
        } else {
            return(x["ex"])
        }
    })
    ##
    tmp <- NULL
    uptodropout <- NULL
    for (r in 1:nrow(aa)) {
        row <- aa[r,]
        s <- row$session
        r <- row$round
        tmp <- p1pramt[p1pramt$session.char == s & p1pramt$round.num <= r,]
        if (!exists("uptodropout")) {
            uptodropout <- tmp
        } else {
            uptodropout <- rbind(uptodropout, tmp)
        }
    }
    ##
    uptodropout.full <- rbind(uptodropout, p1pramt[!(p1pramt$session.char %in% aa$session),])
    ##
    ##uptodropout <- merge(uptodropout, scores.r0r1, by=c("player"), all.x=TRUE)
    ##uptodropout.full <- merge(uptodropout.full, scores.r0r1, by=c("player"), all.x=TRUE)
    ##
    (dps <- summarySE(uniqueDp, "dropout", c("condition", "session")))
    dps$session <- as.character(dps$session)
    sessions <- as.character(unique(pr$session))
    ##
    dps.full <- dps
    for (s in sessions) {
        if (!(s %in% dps$session) && !(s %in% UNWANTED.SESSIONS)) {
            c <- as.character(pr[pr$session == s,]$condition[1])
            dps.full <- rbind(dps.full, c(c, s, 0, 0, NA, 0, NA, NA))
        }
    }
    dps.full$dropout.num <- as.integer(dps.full$dropout)
    ##
    ## LOAD DATA in AE_LATEST.
    p1pramt.g <- p1pramt[p1pramt$session %in% GOOD.SESSIONS,]
    ##
    ## Skill
    ##
    summarySE(p1pramt, "skill", c("dropout.char", "condition"), na.rm=TRUE)
    ##
    ## dropout.char condition    N     skill         sd        sum           se
    ## 1         Stay      Flat 1355 0.1942376 0.04261003 263.191957 0.0011575565
    ## 2         Stay     Strat 1787 0.1934882 0.03839753 345.763485 0.0009083246
    ## 3      Dropout      Flat   10 0.2008779 0.02217010   2.008779 0.0070108006
    ## 4      Dropout     Strat   59 0.1718312 0.01995205  10.138043 0.0025975355
    ##            ci
    ## 1 0.002270799
    ## 2 0.001781491
    ## 3 0.015859533
    ## 4 0.005199532
    ##
    ## Upper limit of skill in dropouts in Stratified.
    ulimit <- 0.1718312 + 0.005199532
    p1pramt.g$skill.dropout <- ifelse(p1pramt.g$skill <= ulimit, 1, 0)
    ##################################################################
    ##
    summarySE(p1pramt, "r.mean", c("dropout.char", "condition"), na.rm=TRUE)
    ##   dropout.char condition    N   r.mean       sd       sum         se         ci
    ## 1         Stay      Flat 1353 4.241734 1.977271 5739.0667 0.05375480 0.10545188
    ## 2         Stay     Strat 1786 3.897219 2.079716 6960.4333 0.04921112 0.09651746
    ## 3      Dropout      Flat   10 4.120000 1.038303   41.2000 0.32834038 0.74275755
    ## 4      Dropout     Strat   53 4.494969 1.420792  238.2333 0.19516077 0.39161873
    ##
    ## Upper limit of skill in dropouts in Stratified.
    ulimit <- 4.494969 - 0.39161873
    p1pramt.g$r.mean.dropout <- ifelse(p1pramt.g$r.mean >= ulimit, 1, 0)
    ##################################################################
    ##
    mydata.rmean <- p1pramt.g[p1pramt.g$r.mean.dropout == 1,]
    nrow(mydata.rmean)
    ## [1] 1148
    mydata <- p1pramt.g[p1pramt.g$skill.dropout == 1,]
    nrow(mydata)
    ## [1] 840
##################################################
}
