## Perceived Fairness

qc <- loadQuestCompulsory()
qc <- qc[qc$session %in% GOOD.SESSIONS,]

qc$exfair.choiceA <- ifelse(qc$exfair.choice == 0, 1, 0)
qc$exfair.choiceB <- ifelse(qc$exfair.choice == 1, 1, 0)
qc$exfair.choiceC <- ifelse(qc$exfair.choice == 2, 1, 0)

adjustTb <- function(tb) {
    cn <- colnames(tb)
    if (!("A" %in% cn)) {
        tb <- cbind(tb, A=c(0,0))
    }
    if (!("B" %in% cn)) {
        tb <- cbind(tb, B=c(0,0))
    }
    if (!("C" %in% cn)) {
        tb <- cbind(tb, C=c(0,0))
    }
    if (!("0" %in% rownames(tb))) {
        tb <- rbind(c(0,0,0), tb)
    }
    print(tb)
    return(tb)
}

cb <- function(x) {
    tb <- table(x$published, x$ex)
    if (dim(tb)[1] != 2 || dim(tb)[2] != 3) {
        tb <- adjustTb(tb)
    }
    return(cbind(
        Arej=tb[1,"A"],
        Apub=tb[2,"A"],
        Brej=tb[1,"B"],
        Bpub=tb[2,"B"],
        Crej=tb[1,"C"],
        Cpub=tb[2,"C"]
    ))
}

tmp <- ddply(p1pramt, "player", cb)

qc <- merge(qc, tmp, by="player")
qc$stratified <- ifelse(qc$condition == "Stratified", 1, 0)

qc <- merge(qc, p1, by="player")

payoffs <- p1pramt[p1pramt$round == 12, c("player", "payoff.cum", "payoff.cum.rel.ses")]

qc <- merge(qc, payoffs, by="player")

mydata <- qc[
    qc$age.group2 %in% c("young", "old") &
    qc$svo.group %in% c("Individualistic", "Prosocial"),
]


fit.controls <- glmer(exfair.choiceA ~
                 age.group2 + svo.group + skill +
                 payoff.cum.rel.ses +
                 female + belief.choice +
                 (1|session),
             data=mydata, family=binomial())
summary(fit.controls)


fit.stratifiedA <- glmer(exfair.choiceA ~ stratified +
                 age.group2 + svo.group + skill +
                 payoff.cum.rel.ses +
                 female + belief.choice +
                 (1|session),
             data=mydata, family=binomial())
summary(fit.stratifiedA)

fit.stratified.intA <- glmer(exfair.choiceA ~ stratified:Apub + stratified:Arej +
                 age.group2 + svo.group + skill +
                 payoff.cum.rel.ses +
                 female + belief.choice +
                 (1|session),
             data=mydata, family=binomial())
summary(fit.stratified.intA)


confint(fit.stratified.intA)


fit.stratifiedB <- glmer(exfair.choiceB ~ stratified +
                 age.group2 + svo.group + skill +
                 payoff.cum.rel.ses +
                 female + belief.choice +
                 (1|session),
             data=mydata, family=binomial())
summary(fit.stratifiedB)

fit.stratified.intB <- glmer(exfair.choiceB ~ stratified:Bpub + stratified:Brej +
                 age.group2 + svo.group + skill +
                 payoff.cum.rel.ses +
                 female + belief.choice +
                 (1|session),
             data=mydata, family=binomial())
summary(fit.stratified.intB)



fit.stratifiedC <- glmer(exfair.choiceC ~ stratified +
                 age.group2 + svo.group + skill +
                 payoff.cum.rel.ses +
                 female + belief.choice +
                 (1|session),
             data=mydata, family=binomial())
summary(fit.stratifiedC)

fit.stratified.intC <- glmer(exfair.choiceC ~ stratified:Cpub + stratified:Crej +
                 age.group2 + svo.group + skill +
                 payoff.cum.rel.ses +
                 female + belief.choice +
                 (1|session),
             data=mydata, family=binomial())
summary(fit.stratified.intC)


ttexreg(list(fit.stratifiedA, fit.stratified.intA,
            fit.stratifiedB, fit.stratified.intB,
            fit.stratifiedC, fit.stratified.intC))



## Is a Review Going to Be Strategic?
#####################################

mydata=reviewsp1pramt[
    reviewsp1pramt$age.group2 %in% c("young", "old") &
    reviewsp1pramt$svo.group %in% c("Individualistic", "Prosocial")
   ,]

mydata$review.ass.f <- as.factor(mydata$review.ass)


fit.across.c <- glmer(review.ass ~ round +
                      stratified +
                      payoff.cum.rel.ses.lag.1.sc +
                      svo.group + age.group2 +
                      skill + belief.choice + female +
                      (1|session/player),
                  control = glmerControl(optimizer = "bobyqa"),
                  data=mydata, family=binomial())
summary(fit.across.c)


fit.vsflat.c <- glmer(review.ass ~ round +
                        ex2 +
                        payoff.cum.rel.ses.lag.1.sc +
                        svo.group + age.group2 +
                        skill + belief.choice + female +
                        (1|session/player),
                    control = glmerControl(optimizer = "bobyqa"),
                    data=mydata, family=binomial())
summary(fit.vsflat.c)



fit.across <- glmer(review.ass ~ round +
                      stratified +
                      (1|session/player),
                  control = glmerControl(optimizer = "bobyqa"),
                  data=mydata, family=binomial())
summary(fit.across)


fit.vsflat <- glmer(review.ass ~ round +
                        ex2 +
                        (1|session/player),
                    control = glmerControl(optimizer = "bobyqa"),
                    data=mydata, family=binomial())
summary(fit.vsflat)


fit.stratified <- glmer(review.ass ~ round +
                        ex +
                        (1|session/player),
                        control = glmerControl(optimizer = "bobyqa"),
                        data=mydata[mydata$stratified == 1,], family=binomial())
summary(fit.stratified)


ttexreg(list(fit.across.c, fit.vsflat.c, fit.across, fit.vsflat, fit.stratified))

