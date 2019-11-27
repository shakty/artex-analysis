## AMT Scores Robustness Checks.

scores$image <- scores$id
scores$time.image.sec <- scores$time.image / 1000

## QUALITY (almost identical results as with overall only)

## CAT = Consensual Assessment Technique

scores$CAT <- (scores$overall + scores$creativity +
                  scores$face + scores$abstract) / 4

scores$CAT2 <- (scores$overall + scores$creativity + scores$face) / 3


## Testing correlation between in-game and AMT ratings.

doCorr <- function(myscores, no.ex.same=FALSE, latex=FALSE) {
    mydata <- addData2ScoresMelted(myscores)
    mydata <- meltReviews(data=mydata)
    mynewdata <- mydata[!is.na(mydata$r.from),]
    ## mynewdata$ex.same <- ifelse(mynewdata$ex.same == 0, "Another", "Same")
    ## mynewdata$ex.same <- as.factor(mynewdata$ex.same)

    if (no.ex.same == TRUE) {
        mynewdata <- mynewdata[mynewdata$ex.same == 0,]
    }

    allFeatures <- c("face", "creativity", "abstract", "overall",
                     "CAT","CAT2",
                     "r.mean.from", "r.from",
                     "d.ownprev", "d.pubprev", "d.subcurr", "cf.changes.tot",
                     "cf.changes.nfeatures")



    vars <- mynewdata[, allFeatures]
    table <- corstarsl(vars, extremes=FALSE)
    if (latex) {
        print.xtable(xtable(table, caption="Correlation table of different quality measures."))
    } else {
        table
    }

}

doCorrNum <- function(myscores) {
    mydata <- addData2ScoresMelted(myscores)
    mydata <- meltReviews(data=mydata)
    mynewdata <- mydata[!is.na(mydata$r.from),]
    features <- c("CAT", "CAT2", "r.mean.from", "r.from")
    vars <- mynewdata[, features]
    mycorr <- rcorr(as.matrix(vars), type="pearson")
    ##R <- mycorr$R
    ##p <- mycorr$P
    return(mycorr)
}

## Significant correlation between in-game scores and AMT ratings is found
## for early rounds. Then, each session develops its own taste.
doCorr(scores, TRUE)
doCorr(scores[scores$round == 1,], TRUE)


## Testing robustness correlations when sub-setting the datasets with
## faster vs slower raters.

nrow(scores[scores$time.image < 4000,]) ## 1969 image reviews removed.
nrow(scores[scores$time.image > 50000,]) ## 168 image reviews removed.

scores.clean <- scores[scores$time.image > 4000 & scores$time.image < 50000,]
doCorr(scores.clean, TRUE)

nrow(scores[scores$time.image < 6000,])# 4171 removed
scores.clean <- scores[scores$time.image > 6000,]
doCorr(scores.clean, TRUE)

nrow(scores[scores$time.image < 8000,]) ## 7530 images removed.
scores.clean <- scores[scores$time.image > 8000,]
doCorr(scores.clean, TRUE)

nrow(scores[scores$time.image < 10000,]) ## 11138 images removed.
scores.clean <- scores[scores$time.image > 10000,]
doCorr(scores.clean, TRUE)

nrow(scores[scores$time.image < 20000,]) ## 17546 images removed.
scores.clean <- scores[scores$time.image > 20000,]
doCorr(scores.clean, TRUE)

nrow(scores[scores$time.image < 30000,]) ## 18226 images removed.
scores.clean <- scores[scores$time.image > 30000,]
doCorr(scores.clean, TRUE)


## Num Plots.

scores.game <- scores[scores$condition != "Practice",]

corrs <- NULL
for (uplimit in c(40000,50000,60000)) {
    for (downlimit in c(0,seq(1000,30000,1000))) {
        scores.clean <- scores.game[scores.game$time.image > downlimit &
                               scores.game$time.image < uplimit,]
        print(paste(uplimit, downlimit, nrow(scores.clean)))
        m <- doCorrNum(scores.clean)
        v1 <- c("CAT","CAT","CAT",
                "CAT2","CAT2","CAT2",
                "r.from", "r.from", "r.from",
                "r.mean.from", "r.mean.from", "r.mean.from")
        v2 <- c("CAT2","r.from","r.mean.from",
                "CAT","r.from","r.mean.from",
                "CAT", "CAT2", "r.mean.from",
                "CAT", "CAT2", "r.from")
        r <- c(m$r["CAT","CAT2"],
                m$r["CAT","r.from"],
                m$r["CAT","r.mean.from"],
                m$r["CAT2","CAT"],
                m$r["CAT2","r.from"],
                m$r["CAT2","r.mean.from"],
                m$r["r.from","CAT"],
                m$r["r.from","CAT2"],
                m$r["r.from","r.mean.from"],
                m$r["r.mean.from","CAT"],
                m$r["r.mean.from","CAT2"],
                m$r["r.mean.from","r.from"])
        n <- rep(m$n,12)
        P <- c(m$P["CAT","CAT2"],
               m$P["CAT","r.from"],
               m$P["CAT","r.mean.from"],
               m$P["CAT2","CAT"],
               m$P["CAT2","r.from"],
               m$P["CAT2","r.mean.from"],
               m$P["r.from","CAT"],
               m$P["r.from","CAT2"],
               m$P["r.from","r.mean.from"],
               m$P["r.mean.from","CAT"],
               m$P["r.mean.from","CAT2"],
               m$P["r.mean.from","r.from"])
        ##stars <- sapply(aa, function(p) {
        ##    s <- ifelse(p < .001, 3, ifelse(p < .01, 2, ifelse(p < .05, 1, 0)))
        ##    return(s)
        ##})
        ##stars <- stars[,"P"][!is.na(stars[,"P"])]
        d <- data.frame(v1=v1, v2=v2, corr=r,P=P,n=n,
                        lower=downlimit/1000, upper=uplimit/1000)
        if (!exists("corrs")) {
            corrs <- d
        } else {
            corrs <- rbind(corrs,d)
        }
    }
}
corrs$vars <- paste0(corrs$v1, " - ", corrs$v2)
corrs$stars <- ifelse(corrs$P < .001, 3,
               ifelse(corrs$P < .01, 2, ifelse(corrs$P < .05, 1, 0)))
corrs$stars <- as.factor(corrs$stars)


mydata <- corrs[corrs$v1 != "CAT" & corrs$v1 != "CAT2",]
mydata <- mydata[mydata$v2 != "r.from" & mydata$v2 != "r.mean.from",]

p <- ggplot(mydata, aes(lower, corr, color=v1, group=vars))
p <- p + geom_hline(yintercept=0)
p <- p + geom_point(aes(size=stars))
p <- p + geom_line()
p <- p + facet_grid(upper~v2)
p <- p + xlab("Lower Bound for Rating Image in Secs") + ylab("Correlation")
p

gggsave("amt_corr_timelimit.png", width=10)

mydata <- corrs[corrs$v1 == "r.mean.from" & corrs$upper == 60 &
                corrs$v2 == "CAT",]

p <- ggplot(mydata, aes(lower, n/3))
p <- p + geom_point()
p <- p + geom_line()
p <- p + xlab("Number of image reviews kept by lower bound") + ylab("Number of Reviews")
p

gggsave("amt_corr_timelimit_reviews_kept.png")


corrs.round <- NULL
for (uplimit in c(40000,50000,60000)) {
    for (downlimit in c(0,seq(1000,30000,1000))) {
        for (round in 1:12) {
            scores.clean <- scores.game[scores.game$time.image > downlimit &
                                        scores.game$time.image < uplimit &
                                        scores.game$round == round,]
            print(paste(uplimit, downlimit, nrow(scores.clean)), round)
            m <- doCorrNum(scores.clean)
            v1 <- c("CAT","CAT","CAT",
                    "CAT2","CAT2","CAT2",
                    "r.from", "r.from", "r.from",
                    "r.mean.from", "r.mean.from", "r.mean.from")
            v2 <- c("CAT2","r.from","r.mean.from",
                    "CAT","r.from","r.mean.from",
                    "CAT", "CAT2", "r.mean.from",
                    "CAT", "CAT2", "r.from")
            r <- c(m$r["CAT","CAT2"],
                   m$r["CAT","r.from"],
                   m$r["CAT","r.mean.from"],
                   m$r["CAT2","CAT"],
                   m$r["CAT2","r.from"],
                   m$r["CAT2","r.mean.from"],
                   m$r["r.from","CAT"],
                   m$r["r.from","CAT2"],
                   m$r["r.from","r.mean.from"],
                   m$r["r.mean.from","CAT"],
                   m$r["r.mean.from","CAT2"],
                   m$r["r.mean.from","r.from"])
            n <- rep(m$n,12)
            P <- c(m$P["CAT","CAT2"],
                   m$P["CAT","r.from"],
                   m$P["CAT","r.mean.from"],
                   m$P["CAT2","CAT"],
                   m$P["CAT2","r.from"],
                   m$P["CAT2","r.mean.from"],
                   m$P["r.from","CAT"],
                   m$P["r.from","CAT2"],
                   m$P["r.from","r.mean.from"],
                   m$P["r.mean.from","CAT"],
                   m$P["r.mean.from","CAT2"],
                   m$P["r.mean.from","r.from"])
            ## Make dataset.
            d <- data.frame(v1=v1, v2=v2, corr=r,P=P,n=n, round=round,
                            lower=downlimit/1000, upper=uplimit/1000)
            if (!exists("corrs")) {
                corrs.round <- d
            } else {
                corrs.round <- rbind(corrs.round,d)
            }
        }
    }
}
corrs.round$vars <- paste0(corrs.round$v1, " - ", corrs.round$v2)
corrs.round$stars <- ifelse(corrs.round$P < .001, 3,
                     ifelse(corrs.round$P < .01, 2,
                     ifelse(corrs.round$P < .05, 1, 0)))
corrs.round$stars <- as.factor(corrs.round$stars)

mydata <- corrs.round[corrs.round$v1 != "CAT" & corrs.round$v1 != "CAT2",]
mydata <- mydata[mydata$v2 != "r.from" & mydata$v2 != "r.mean.from",]
mydata <- mydata[mydata$v2 != "CAT2",]

p <- ggplot(mydata, aes(lower, corr, color=v1, group=vars))
p <- p + geom_hline(yintercept=0)
p <- p + geom_point(aes(size=stars))
p <- p + geom_line()
p <- p + facet_grid(upper~round)
p <- p + xlab("Lower Bound for Rating Image in Secs") + ylab("Correlation with CAT")
p

gggsave("amt_corr_timelimit_by_round_CAT.png", width=20, height=10)


mydata <- corrs.round[corrs.round$v1 != "CAT" & corrs.round$v1 != "CAT2",]
mydata <- mydata[mydata$v2 != "r.from" & mydata$v2 != "r.mean.from",]
mydata <- mydata[mydata$v2 != "CAT",]

p <- ggplot(mydata, aes(lower, corr, color=v1, group=vars))
p <- p + geom_hline(yintercept=0)
p <- p + geom_point(aes(size=stars))
p <- p + geom_line()
p <- p + facet_grid(upper~round)
p <- p + xlab("Lower Bound for Rating Image in Secs") + ylab("Correlation with CAT2")
p

ggsave("amt_corr_timelimit_by_round_CAT2.png", width=20, height=10)


## Cleaning by Player and Number of Sets.

sets <- loadAMTSetIds()
aa <- as.data.frame(table(sets$player, sets$set.counter))

## Only reviews from persons with 5 sets.
sets5 <- aa[aa$Var2 == 5 & aa$Freq == 1,]$Var1
scores.clean <- scores.game[scores.game$player %in% sets5,]

doCorrNum(scores.clean)

## Only reviews from persons with 1 set.
sets1 <- aa[aa$Var2 == 2 & aa$Freq == 0,]$Var1
scores.clean <- scores.game[scores.game$player %in% sets1,]

doCorrNum(scores.clean)

## Excluding those with only 1 set.
setsMoreThan1 <- aa[aa$Var2 == 2 & aa$Freq != 0,]$Var1
scores.clean <- scores.game[scores.game$player %in% setsMoreThan1,]

doCorrNum(scores.clean)

## Only reviews done in set 5.

scores.clean <- scores.game[scores.game$set.counter == 5,]
doCorrNum(scores.clean)

## Only reviews done in set 2.

scores.clean <- scores.game[scores.game$set.counter == 2,]
doCorrNum(scores.clean)


## Only reviews done in set 1.

scores.clean <- scores.game[scores.game$set.counter == 1,]
doCorrNum(scores.clean)

## Only reviews done in set 1, but from those with more than 1 set.

scores.clean <- scores.game[scores.game$set.counter == 1 &
                           scores.game$player %in% setsMoreThan1,]
doCorrNum(scores.clean)


## There is no clear benefit in subsetting the AMT dataset. All actions
## seems arbitrary, and the potential benefits are small.
