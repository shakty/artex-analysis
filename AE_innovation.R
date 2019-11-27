## 3.1.3 Innovation

t.test(d.pubprev ~ condition, p1pramt)


fit.condonly <- lmer(d.pubprev ~ round + condition +
                             (1|session/player),
                         data=p1pramt)
summary(fit.condonly)


mydata=p1pramt[p1pramt$round > 1 &
               p1pramt$age.group2 %in% c("young", "old") &
               p1pramt$svo.group %in% c("Individualistic", "Prosocial")
              ,]

fit.condrols <- lmer(d.pubprev ~ round + condition + payoff.cum.rel.ses.lag.1.sc+
                             svo.group + age.group2 + published.lag.1 +
                             skill + belief.choice + female + copy +
                             (1|session/player),
                         data=mydata)
summary(fit.condrols)


## Plots.

summ <- summarySE(p1pramt[p1pramt$round > 1,], "d.pubprev", c("condition", "round"))

p <- ggplot(summ, aes(as.factor(round), d.pubprev, color=condition, group=condition)) +
    geom_point(size=4) + geom_line(size=2,alpha=0.7) +
    geom_errorbar(aes(ymin=d.pubprev - ci, ymax=d.pubprev+ci), width=0.3) +
    ylab('Innovation') + xlab('Round') +
    theme(legend.position="none")

gggsave("inn_round.svg")
