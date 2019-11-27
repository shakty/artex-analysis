## 3.3.3 Miscoordination


## Computes number of submissions left out
## due to miscoordination for a given round.
## It takes a data frame (round-session) and
## returns some stats.
misscoFun <- function(x) {
    #
    condition <- x$condition[1]
    session <- x$session[1]
    round <- as.numeric(x$round[1])
    #
    A <- x[x$ex == "A",]
    B <- x[x$ex == "B",]
    C <- x[x$ex == "C",]
    #
    AmaxInnMis <- NA
    AavgInnMis <- NA
    AtotInnMis <- NA
    #
    BmaxInnMis <- NA
    BavgInnMis <- NA
    BtotInnMis <- NA
    #
    CmaxInnMis <- NA
    CavgInnMis <- NA
    CtotInnMis <- NA
    #
    An <- nrow(A)
    Bn <- nrow(B)
    Cn <- nrow(C)
    #
    if (condition == "Flat") {
        Amis <- An - 2
        Bmis <- Bn - 2
        Cmis <- Cn - 2
    } else {
        Amis <- An - 1
        Bmis <- Bn - 2
        Cmis <- Cn - 4
    }
    #
    totMis <- 0
    if (Amis > 0) {
        totMis <- totMis + Amis
        if (round > 1) {
            AmaxInnMis <- max(A$d.pubprev)
            AavgInnMis <- mean(A$d.pubprev)
            AtotInnMis <- sum(A$d.pubprev)
        }
    }
    if (Bmis > 0) {
        totMis <- totMis + Bmis
        if (round > 1) {
            BmaxInnMis <- max(B$d.pubprev)
            BavgInnMis <- mean(B$d.pubprev)
            BtotInnMis <- sum(B$d.pubprev)
        }
    }
    if (Cmis > 0) {
        totMis <- totMis + Cmis
        if (round > 1) {
            CmaxInnMis <- max(C$d.pubprev)
            CavgInnMis <- mean(C$d.pubprev)
            CtotInnMis <- sum(C$d.pubprev)
        }
    }
    #
    c(
        session = session,
        round = round,
        condition = condition,
        An = An,
        Bn = Bn,
        Cn = Cn,
        Amis = Amis,
        Bmis = Bmis,
        Cmis = Cmis,
        TotMis = totMis,
        AmaxInnMis = AmaxInnMis,
        BmaxInnMis = BmaxInnMis,
        CmaxInnMis = CmaxInnMis,
        AavgInnMis = AavgInnMis,
        BavgInnMis = BavgInnMis,
        CavgInnMis = CavgInnMis,
        AtotInnMis = AtotInnMis,
        BtotInnMis = BtotInnMis,
        CtotInnMis = CtotInnMis
    )
}

## Generates the miscoordination dataset for every round and session.
missco <- ddply(pr, c("session", "round"), misscoFun)
missco$condition <- ifelse(missco$condition == 1, "Flat", "Stratified")

## Adjusted means: given the natural rate of waste in the market, what is
## the excess waste due to miscoordination.
missco$TotMis.adjusted <- ifelse(missco$condition == "Flat",
                                 missco$TotMis - 3, missco$TotMis - 2)

## Regression condition and round.
fit <- lmer(TotMis.adjusted ~ round + condition + (1|session), data=missco)
summary(fit)

ttexreg(list(fit))

## Plot.
mysm <- summarySE(missco, "TotMis.adjusted", c("condition", "round"))
ggplot(mysm, aes(as.factor(round), TotMis.adjusted, color=condition,
                 group=condition)) +
    geom_point(size=4, alpha=0.8) +
    geom_smooth(method="lm", linetype="dashed") +
    ## geom_errorbar(aes(ymin=misco-se, ymax=misco+se)) +
    xlab('Round') + ylab('Miscoordination') +
    theme(legend.position="none")
gggsave("miscoordination_overall.png")


## Statistics at the level of exhibitions.
missco.melt <- melt(missco, measure.vars = c("Amis", "Bmis", "Cmis"),
                    variable.name = "ex", value.name = "misco")
missco.melt$ex <- ifelse(missco.melt$ex == "Amis", "A",
                         ifelse(missco.melt$ex == "Bmis", "B", "C"))


fit.str <- lmer(misco ~ round + ex + (1|session),
                data=missco.melt[missco.melt$condition == "Stratified",])
summary(fit.str)

ttexreg(list(fit, fit.str))

mysm <- summarySE(missco.melt, "misco", c("ex", "condition", "round"))

ggplot(mysm, aes(as.factor(round), misco, group=ex, color=ex)) +
    geom_point(size=4) + geom_line(size=2, alpha=0.5) +
    geom_errorbar(aes(ymin=misco-se, ymax=misco+se)) +
    facet_grid(~condition) +
    xlab('Round') + ylab('Miscoordination') +
    theme(legend.position="none")

gggsave("miscoordination_ex_round.png")

## Flat and Stratified have about the same level of miscoordination.
## However, Stratified has a lot of miscoordination on A and B,
## very little on C. Therefore, the payoffs are much lower.

