## 3.2.1 Heterogeneous Responses to Stratification

mydata=p1pramt[p1pramt$round > 1 &
               p1pramt$age.group2 %in% c("young", "old") &
               p1pramt$svo.group %in% c("Individualistic", "Prosocial")
              ,]


## Baseline.

fit.condonly <- lmer(d.pubprev ~ round + condition +
                             (1|session/player),
                         data=mydata)
summary(fit.condonly)


fit.condrols <- lmer(d.pubprev ~ round + condition + payoff.cum.rel.ses.lag.1.sc +
                             svo.group + age.group2 + published.lag.1 +
                             skill + belief.choice + female + copy +
                             (1|session/player),
                         data=mydata)
summary(fit.condrols)

## Interactions.

fit.int.success <- lmer(d.pubprev ~ round + condition:payoff.cum.rel.ses.lag.1.sc +
                            svo.group + age.group2 + published.lag.1 +
                            skill +
                            (1|session/player),
                        data=mydata)
summary(fit.int.success)


fit.int.svo <- lmer(d.pubprev ~ round + payoff.cum.rel.ses.lag.1.sc +
                            condition:svo.group + age.group2 + published.lag.1 +
                            skill +
                            (1|session/player),
                        data=mydata)
summary(fit.int.svo)


fit.int.age <- lmer(d.pubprev ~ round + payoff.cum.rel.ses.lag.1.sc +
                            svo.group + condition:age.group2 + published.lag.1 +
                            skill +
                            (1|session/player),
                        data=mydata)
summary(fit.int.age)

fit.int.female <- lmer(d.pubprev ~ round + payoff.cum.rel.ses.lag.1.sc +
                            svo.group + age.group2 + published.lag.1 +
                            skill + condition:female +
                            (1|session/player),
                        data=mydata)
summary(fit.int.female)

fit.int.skill <- lmer(d.pubprev ~ round + payoff.cum.rel.ses.lag.1.sc +
                            svo.group + age.group2 + published.lag.1 +
                            condition:skill +
                            (1|session/player),
                        data=mydata)
summary(fit.int.skill)


fit.int.belief <- lmer(d.pubprev ~ round + payoff.cum.rel.ses.lag.1.sc +
                            svo.group + age.group2 + published.lag.1 +
                            skill + condition:belief.choice +
                            (1|session/player),
                        data=mydata)
summary(fit.int.belief)


fit.int.round <- lmer(d.pubprev ~ condition:round + payoff.cum.rel.ses.lag.1.sc +
                            svo.group + age.group2 + published.lag.1 +
                            skill +
                            (1|session/player),
                        data=mydata)
summary(fit.int.round)


fit.int.pub <- lmer(d.pubprev ~ round + payoff.cum.rel.ses.lag.1.sc +
                        svo.group +
                        condition:published.lag.1 +
                        skill + age.group2 +
                        (1|session/player),
                    data=mydata)
summary(fit.int.pub)


## All interactions.

##fit.int.all <- lmer(d.pubprev ~ round +
##                        stratified +
##                        stratified:payoff.cum.rel.ses.lag.1.sc +
##                        stratified:svo.group +
##                        stratified:published.lag.1 +
##                        stratified:skill +
##                        stratified:age.group2 +
##                        (1|session/player),
##                    data=mydata)
##summary(fit.int.all)
##
##fit.int.all.smart <- lmer(d.pubprev ~ round +
##                              stratified +
##                              payoff.cum.rel.ses.lag.1.sc +
##                              published.lag.1 +
##                              condition:svo.group +
##                              condition:skill + condition:age.group2 +
##                        (1|session/player),
##                    data=mydata)
##summary(fit.int.all.smart)
##
##fit.int.all.smart2 <- lmer(d.pubprev ~ round +
##                               stratified +
##    payoff.cum.rel.ses.lag.1.sc +
##                              published.lag.1 +
##                              svo.group +
##                              condition:skill + condition:age.group2 +
##                        (1|session/player),
##                    data=mydata)
##summary(fit.int.all.smart2)


fit.int.all.smart2a <- lmer(d.pubprev ~ round +
                                stratified +
                                stratified:published.lag.1 +
                                stratified:payoff.cum.rel.ses.lag.1.sc +
                                stratified:svo.group + stratified:skill + stratified:age.group2 +
                                (1|session/player),
                    data=mydata)
summary(fit.int.all.smart2a)

fit.int.all.smart2b <- lmer(d.pubprev ~ round + stratified*payoff.cum.rel.ses.lag.1.sc +
                                stratified*published.lag.1 + stratified*svo.group +
                                stratified*skill + stratified*age.group2 +
                        (1|session/player),
                    data=mydata)
summary(fit.int.all.smart2b)


fit.int.all.smart2c <- lmer(d.pubprev ~ round + payoff.cum.rel.ses.lag.1.sc +
                                published.lag.1 + svo.group +
                                skill + stratified*age.group2 +
                                (1|session/player),
                    data=mydata)
summary(fit.int.all.smart2c)


my.coef.names <- c("Intercept", "Round",
                   "Stratified",
                   "Relative Success",
                   "Prosocial",
                   "Age $>$ 30",
                   "Published Lag",
                   "Skill", "Self-Efficacy", "Gender Female",
                   "Copy",
                   "Flat:Relative Success",
                   "Stratified:Relative Success",
                   "Flat:Individualistic",
                   "Stratified:Individualistic",
                   "Flat:Prosocial",
                   "Flat:Age $\\le$ 30",
                   "Stratified:Age $\\le$ 30",
                   "Flat:Age $>$ 30",
                   "Flat:Skill",
                   "Stratified:Skill",
                   "Flat:Published Lag",
                   "Stratified:Published Lag",
                   "Flat:Round",
                   "Stratified:Round",
                   "Stratified:Prosocial",
                   "Stratified:Age $>$ 30",
                   "Stratified:Skill",
                   "Stratified:Age $>$ 30"
                   )

ttexreg(list(fit.condonly, fit.condrols, fit.int.success, fit.int.svo,
            fit.int.age, fit.int.skill,
            fit.int.pub, fit.int.round, fit.int.all.smart2a, fit.int.all.smart2b,
             fit.int.all.smart2c))


#########################################

## General statistics Below and Above 30years old participants.

## Take 1 round only to not inflate the p-values.
md <- p1pramt[p1pramt$round == 12,]

## Marginally less confident.
t.test(belief.choice ~ age.group2, md[md$svo.group %in% c("Individualistic", "Prosocial"),])

## Less technically skilled.
t.test(r0.cf.changes.tot.norm ~ age.group2, md[md$age.group2 %in% c("young", "old"),])

## Less originally skilled.
t.test(skill.scaled ~ age.group2, md[md$age.group2 %in% c("young", "old"),])

summarySE(md[md$age.group2 %in% c("young", "old"),], "skill.scaled", "age.group2")


summarySE(p1pramt, "skill3", "age.group2")

summ <- summarySE(p1pramt[p1pramt$age.group2 %in% c("old", "young") & p1pramt$round == 12,], "skill3", "age.group2")
summ$type <- "Change per Sec."
summ2 <- summarySE(p1pramt[p1pramt$age.group2 %in% c("old", "young") & p1pramt$round == 12,], "skill.scaled", "age.group2")
summ2$type <- "Originality"
colnames(summ2)[3] <- "skill3"
summ <- rbind(summ, summ2)

ggplot(summ, aes(type, skill3, group=age.group2, fill=age.group2)) +
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=skill3-ci, ymax=skill3+ci), width=0.3) +
    xlab('') + ylab('Skills (Normalized)') + theme(legend.position="none")

gggsave("skills_age.svg")




## Plots. Innovation by Age Group and Market.

summ <- summarySE(p1pramt[p1pramt$round > 1 &
                          p1pramt$age.group2 %in% c("old", "young"),],
                  "d.pubprev", c("condition", "round", "age.group2"))
summ$label <- ifelse(summ$age.group == "young", "<= 30 Years Old", "> 30 Years Old")



ggplot(summ, aes(as.factor(round), d.pubprev, color=condition, group=condition)) +
    geom_point(size=4) + geom_line(size=2,alpha=0.7) +
    geom_errorbar(aes(ymin=d.pubprev - ci, ymax=d.pubprev+ci), width=0.3) +
    ylab('Innovation') + xlab('Round') +
    theme(legend.position="none") +
    facet_grid(~label)

gggsave("inn_round_age.png")



summ <- summarySE(p1pramt[p1pramt$round > 1 &
                          p1pramt$age.group2 %in% c("old", "young"),],
                  "d.pubprev", c("condition", "age.group2"))
summ$label <- ifelse(summ$age.group == "young", "<= 30 Years Old", "> 30 Years Old")

ggplot(summ, aes(condition, d.pubprev, fill=condition, group=condition)) +
    geom_bar(position="dodge", stat="identity") +
    geom_errorbar(aes(ymin=d.pubprev - ci, ymax=d.pubprev+ci), width=0.3) +
    ylab('Innovation') + xlab('') +
    theme(legend.position="none") +
    facet_grid(~label)

gggsave("inn_bar_age.png")
