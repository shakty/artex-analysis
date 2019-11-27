## A.8 Copy Behavior


summarySE(p1pramt, "copy", "condition")

fit.all <- glmer(copy ~ round + condition + (1|session/player),
                 family=binomial(),
                 data=p1pramt)
summary(fit.all)

fit.svo <- glmer(copy ~ round + condition + svo.group + (1|session/player),
                 family=binomial(),
             data=p1pramt[p1pramt$svo.group %in% c("Prosocial", "Individualistic"),])
summary(fit.svo)

fit.young <- glmer(copy ~ round + condition + young + (1|session/player),
                 family=binomial(),
             data=p1pramt)
summary(fit.young)

fit.skill <- glmer(copy ~ round + condition + skill + (1|session/player),
                 family=binomial(),
             data=p1pramt)
summary(fit.skill)

fit.belief <- glmer(copy ~ round + condition + belief.choice + (1|session/player),
                 family=binomial(),
                    data=p1pramt)
summary(fit.belief)

fit.female <- glmer(copy ~ round + condition + female + (1|session/player),
                 family=binomial(),
                    data=p1pramt)
summary(fit.female)

fit.pub <- glmer(copy ~ round + condition + published.lag.1 + (1|session/player),
                 family=binomial(),
                    data=p1pramt[p1pramt$round > 1,])
summary(fit.pub)

fit.success <- glmer(copy ~ round + condition + success.rel + (1|session/player),
                 family=binomial(),
                    data=p1pramt[p1pramt$round > 1,])
summary(fit.success)

fit.complete <- glmer(copy ~ round + condition +
                          female +
                          svo.group +
                          young + skill + belief.choice +
                       (1|session/player),
                      family=binomial(),
                      control=glmerControl(optimizer="bobyqa"),
                 data=p1pramt[p1pramt$round > 1 &
                              p1pramt$svo.group %in% c("Prosocial", "Individualistic"),])
summary(fit.complete)

texreg(list(fit.all, fit.svo, fit.young, fit.skill, fit.belief, fit.female, fit.complete))
