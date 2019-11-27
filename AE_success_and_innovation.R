## 3.2.2 How Previous Success Impacts Innovation

mydata=p1pramt[p1pramt$round > 1 &
            p1pramt$svo.group %in% c("Individualistic", "Prosocial") &
            p1pramt$age.group2 %in% c("young", "old") &
            p1pramt$gender.choice %in% c("Male", "Female") &
            p1pramt$published == 1
           ,]

## Comparing Measures of Cumulative Payoff. and Innovation.
###########################################################


fit.rel.cond <- lmer(d.pubprev ~ round + payoff.cum.rel.cond.lag.1.sc +
                (1|session/player),
            data=p1pramt[p1pramt$round > 1,])
summary(fit.rel.cond)

fit.rel.ses <- lmer(d.pubprev ~ round + payoff.cum.rel.ses.lag.1.sc +
                (1|session/player),
            data=p1pramt[p1pramt$round > 1,])
summary(fit.rel.ses)

fit.sc <- lmer(d.pubprev ~ round + payoff.cum.lag.1.sc +
                   (1|session/player),
               data=p1pramt[p1pramt$round > 1,])
summary(fit.sc)

## Int.

fit.rel.cond.int <- lmer(d.pubprev ~ round + condition*payoff.cum.rel.cond.lag.1.sc +
                (1|session/player),
            data=p1pramt[p1pramt$round > 1,])
summary(fit.rel.cond.int)

fit.rel.ses.int <- lmer(d.pubprev ~ round + condition*payoff.cum.rel.ses.lag.1.sc +
                (1|session/player),
            data=p1pramt[p1pramt$round > 1,])
summary(fit.rel.ses.int)

fit.sc.int <- lmer(d.pubprev ~ round + condition*payoff.cum.lag.1.sc +
                   (1|session/player),
               data=p1pramt[p1pramt$round > 1,])
summary(fit.sc.int)

my.coef.names <- c("Intercept", "Round",
                   "Cum. Payoff - Avg. Condition (CP-C)",
                   "Cum. Payoff - Avg. Session (CP-S)",
                   "Cum. Payoff (CP)",
                   "Stratified",
                   "Stratified:CP-C", "Stratified:CP-S", "Stratified:CP")

texreg(list(fit.rel.cond, fit.rel.ses, fit.sc,
            fit.rel.cond.int, fit.rel.ses.int, fit.sc.int),
       stars = c(0.001, 0.01, 0.05, 0.1),
       symbol = '\\texttt{+}',
       custom.coef.names=my.coef.names)


## Variance test.


var.test(success.rel ~ condition, data=p1pramt[p1pramt$round > 1,])

anova(fit.rel.ses.int)


summ <- summarySE(p1pramt, "success.rel", c("condition", "session"),na.rm=TRUE)

t.test(sd ~ condition, data=summ)

