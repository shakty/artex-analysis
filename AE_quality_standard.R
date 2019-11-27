## 3.3.1 Emergence of a Quality Standard

## Max and Average Inn.
pp <- summarySE(p1pramt, "d.pubprev", c("player"), na.rm=TRUE)
pp2 <- ddply(p1pramt, "player", function(x) max(x$d.pubprev, na.rm=TRUE))
pp <- merge(pp, pp2, by="player")
pp <- pp[,c("player", "d.pubprev", "sd", "V1")]
colnames(pp) <- c("player", "inn.avg", "inn.sd", "inn.max")
ppAll <- pp
## Max and Average Exploration.
pp <- summarySE(p1pramt, "d.ownprev", c("player"), na.rm=TRUE)
pp2 <- ddply(p1pramt, "player", function(x) max(x$d.ownprev, na.rm=TRUE))
pp <- merge(pp, pp2, by="player")
pp <- pp[,c("player", "d.ownprev", "sd", "V1")]
colnames(pp) <- c("player", "exp.avg", "exp.sd", "exp.max")
ppAll <- merge(ppAll, pp, by="player")
## Max and Average Creativity
pp <- summarySE(p1pramt, "creativity", c("player"), na.rm=TRUE)
pp2 <- ddply(p1pramt, "player", function(x) max(x$creativity, na.rm=TRUE))
pp <- merge(pp, pp2, by="player")
pp <- pp[,c("player", "creativity", "sd", "V1")]
colnames(pp) <- c("player", "creativity.avg", "creativity.sd", "creativity.max")
ppAll <- merge(ppAll, pp, by="player")
## Max and Average Abstract
pp <- summarySE(p1pramt, "abstract", c("player"), na.rm=TRUE)
pp2 <- ddply(p1pramt, "player", function(x) max(x$abstract, na.rm=TRUE))
pp <- merge(pp, pp2, by="player")
pp <- pp[,c("player", "abstract", "sd", "V1")]
colnames(pp) <- c("player", "abstract.avg", "abstract.sd", "abstract.max")
ppAll <- merge(ppAll, pp, by="player")
## Max and Average Overall
pp <- summarySE(p1pramt, "overall", c("player"), na.rm=TRUE)
pp2 <- ddply(p1pramt, "player", function(x) max(x$overall, na.rm=TRUE))
pp <- merge(pp, pp2, by="player")
pp <- pp[,c("player", "overall", "sd", "V1")]
colnames(pp) <- c("player", "overall.avg", "overall.sd", "overall.max")
ppAll <- merge(ppAll, pp, by="player")
## Max and Average Overall
pp <- summarySE(p1pramt, "face", c("player"), na.rm=TRUE)
pp2 <- ddply(p1pramt, "player", function(x) max(x$face, na.rm=TRUE))
pp <- merge(pp, pp2, by="player")
pp <- pp[,c("player", "face", "sd", "V1")]
colnames(pp) <- c("player", "face.avg", "face.sd", "face.max")
ppAll <- merge(ppAll, pp, by="player")
## Max and Average R.mean.from.
pp <- summarySE(p1pramt, "r.mean.from", c("player"), na.rm=TRUE)
pp2 <- ddply(p1pramt, "player", function(x) max(x$r.mean.from, na.rm=TRUE))
pp <- merge(pp, pp2, by="player")
pp <- pp[,c("player", "r.mean.from", "sd", "V1")]
colnames(pp) <- c("player", "r.mean.from.avg", "r.mean.from.sd", "r.mean.from.max")
ppAll <- merge(ppAll, pp, by="player")

p1pramt <- merge(p1pramt, ppAll, by="player")


p1pramt$inn.diff.from.avg <- p1pramt$d.pubprev - p1pramt$inn.avg
p1pramt$inn.diff.from.max <- abs(p1pramt$d.pubprev - p1pramt$inn.max)
p1pramt$exp.diff.from.avg <- p1pramt$d.ownprev - p1pramt$exp.avg
p1pramt$exp.diff.from.max <- abs(p1pramt$d.ownprev - p1pramt$exp.max)
p1pramt$creativity.diff.from.avg <- p1pramt$creativity - p1pramt$creativity.avg
p1pramt$creativity.diff.from.max <- abs(p1pramt$creativity - p1pramt$creativity.max)
p1pramt$overall.diff.from.avg <- p1pramt$overall - p1pramt$overall.avg
p1pramt$overall.diff.from.max <- abs(p1pramt$overall - p1pramt$overall.max)
p1pramt$face.diff.from.avg <- p1pramt$face - p1pramt$face.avg
p1pramt$face.diff.from.max <- abs(p1pramt$face - p1pramt$face.max)
p1pramt$abstract.diff.from.avg <- p1pramt$abstract - p1pramt$abstract.avg
p1pramt$abstract.diff.from.max <- abs(p1pramt$abstract - p1pramt$abstract.max)
p1pramt$r.mean.from.diff.from.avg <- p1pramt$r.mean.from - p1pramt$r.mean.from.avg
p1pramt$r.mean.from.diff.from.max <- abs(p1pramt$r.mean.from - p1pramt$r.mean.from.max)

mydata=p1pramt[p1pramt$round > 1 &
               p1pramt$age.group2 %in% c("young", "old") &
               p1pramt$svo.group %in% c("Individualistic", "Prosocial")
              ,]

## Review Scores across exhibitions in the Stratified market.

## Raw Review Scores.

fit.basic <- lmer(r.mean.from ~ round +
                      ex +
                      age.group2 + svo.group + skill +
                      (1|session/player),
                  data=mydata[mydata$stratified == 1,])
summary(fit.basic)

fit.basic.p <- lmer(r.mean.from ~ round +
                        ex +
                        age.group2 + svo.group + skill +
                        (1|session/player),
                    data=mydata[mydata$stratified == 1 & mydata$published == 1,])
summary(fit.basic.p)

fit.amt <- lmer(r.mean.from ~ round +
                    ex +
                    age.group2 + svo.group + skill +
                    overall + creativity + abstract +
                    (1|session/player),
                data=mydata[mydata$stratified == 1,])
summary(fit.amt)

fit.amt.p <- lmer(r.mean.from ~ round +
                    ex +
                    age.group2 + svo.group + skill +
                    overall + creativity + abstract +
                    (1|session/player),
                data=mydata[mydata$stratified == 1 & mydata$published == 1,])
summary(fit.amt.p)

## Clean Review Scores.

fit.basic.cl <- lmer(r.mean.from.clean.asskill ~ round +
                      ex +
                      age.group2 + svo.group + skill +
                      (1|session/player),
                  data=mydata[mydata$stratified == 1,])
summary(fit.basic.cl)

fit.basic.p.cl <- lmer(r.mean.from.clean.asskill ~ round +
                        ex +
                        age.group2 + svo.group + skill +
                        (1|session/player),
                    data=mydata[mydata$stratified == 1 & mydata$published == 1,])
summary(fit.basic.p.cl)

fit.amt.cl <- lmer(r.mean.from.clean.asskill ~ round +
                    ex +
                    age.group2 + svo.group + skill +
                    overall + creativity + abstract +
                    (1|session/player),
                data=mydata[mydata$stratified == 1,])
summary(fit.amt.cl)

fit.amt.p.cl <- lmer(r.mean.from.clean.asskill ~ round +
                    ex +
                    age.group2 + svo.group + skill +
                    overall + creativity + abstract +
                    (1|session/player),
                data=mydata[mydata$stratified == 1 & mydata$published == 1,])
summary(fit.amt.p.cl)


ttexreg(list(fit.basic, fit.basic.p, fit.amt, fit.amt.p,
             fit.basic.cl, fit.basic.p.cl, fit.amt.cl, fit.amt.p.cl))


## HP1. Do players submit their best images to the top tier?

## LOGISTIC HAS TROUBLE FOR CONVERGENCE, so we use LINEAR,
## We are not interested in size, rather in casual paths.

## CURRENT

fit.curr.score <- lmer(exA ~ round +
                           r.mean.from.clean.asskill +
                           (1|session/player),
                       data=mydata[mydata$condition == "Stratified",])
summary(fit.curr.score)

fit.curr.score.contr <- lmer(exA ~ round +
                                 r.mean.from.clean.asskill +
                                 success.rel +
                                 age.group2 + female +
                                 svo.group + skill + belief.choice +
                                 (1|session/player),
                             data=mydata[mydata$condition == "Stratified",])
summary(fit.curr.score.contr)


fit.curr.amt <- lmer(exA ~ round +
                         creativity + abstract + overall +
                         (1|session/player),
                     data=mydata[mydata$condition == "Stratified",])
summary(fit.curr.amt)


fit.curr.amt.contr <- lmer(exA ~ round +
                           creativity + abstract + overall +
                           success.rel +
                           age.group2 + female +
                           svo.group + skill + belief.choice +
                           (1|session/player),
                     data=mydata[mydata$condition == "Stratified",])
summary(fit.curr.amt.contr)

fit.curr.exp <- lmer(exA ~ round +
                           d.pubprev + d.ownprev +
                           (1|session/player),
                     data=mydata[mydata$condition == "Stratified",])
summary(fit.curr.exp)

fit.curr.exp.contr <- lmer(exA ~ round +
                           d.pubprev + d.ownprev +
                      success.rel +
                      age.group2 + female +
                      svo.group + skill + belief.choice +
                     (1|session/player),
                     data=mydata[mydata$condition == "Stratified",])
summary(fit.curr.exp.contr)

## DIFF

fit.diff.score <- lmer(exA ~
                round +
                ma.r.mean.from.clean.asskill.diff1 +
                (1|session/player),
                data=mydata[mydata$condition == "Stratified" & mydata$round > 1,])
summary(fit.diff.score)



fit.diff.score.contr <- lmer(exA ~
                                 round +
                                 ma.r.mean.from.clean.asskill.diff1 +
                                 success.rel +
                                 age.group2 + female +
                                 svo.group + skill + belief.choice +
                                 (1|session/player),
                             data=mydata[mydata$condition == "Stratified" & mydata$round > 1,])
summary(fit.diff.score.contr)

fit.diff.amt <- lmer(exA ~
                         round +
                         ma.creativity.diff1 + ma.overall.diff1 +
                         ma.abstract.diff1 +
                         (1|session/player),
                      data=mydata[mydata$condition == "Stratified" & mydata$round > 1,])
summary(fit.diff.amt)


fit.diff.amt.contr <- lmer(exA ~
                         round +
                         ma.creativity.diff1 + ma.overall.diff1 +
                         ma.abstract.diff1 +
                         success.rel +
                         age.group2 + female +
                         svo.group + skill + belief.choice +
                         (1|session/player),
                     data=mydata[mydata$condition == "Stratified" & mydata$round > 1,])
summary(fit.diff.amt.contr)

fit.diff.exp <- lmer(exA ~
                         round +
                         ma.inn.diff1 + ma.own.diff1 +
                         (1|session/player),
                     data=mydata[mydata$condition == "Stratified" & mydata$round > 1,])
summary(fit.diff.exp)

fit.diff.exp.contr <- lmer(exA ~
                               round +
                                ma.inn.diff1 + ma.own.diff1 +
                         success.rel +
                         age.group2 + female +
                         svo.group + skill + belief.choice +
                         (1|session/player),
                     data=mydata[mydata$condition == "Stratified" & mydata$round > 1,])
summary(fit.diff.exp.contr)

## AVG.

fit.avg.score <- lmer(exA ~
                round +
                r.mean.from.diff.from.avg +
                (1|session/player),
                data=mydata[mydata$condition == "Stratified" & mydata$round > 1,])
summary(fit.avg.score)

fit.avg.score.contr <- lmer(exA ~
                round +
                r.mean.from.diff.from.avg +
                success.rel +
                age.group2 + female +
                svo.group + skill + belief.choice +
                (1|session/player),
                data=mydata[mydata$condition == "Stratified" & mydata$round > 1,])
summary(fit.avg.score.contr)


fit.avg.amt <- lmer(exA ~
                         round +
                          creativity.diff.from.avg +
                overall.diff.from.avg +
                abstract.diff.from.avg +
                (1|session/player),
                data=mydata[mydata$condition == "Stratified" & mydata$round > 1,])
summary(fit.avg.amt)

fit.avg.amt.contr <- lmer(exA ~
                               round +
                                creativity.diff.from.avg +
                overall.diff.from.avg +
                abstract.diff.from.avg +
                success.rel +
                age.group2 + female +
                svo.group + skill + belief.choice +
                (1|session/player),
                data=mydata[mydata$condition == "Stratified" & mydata$round > 1,])
summary(fit.avg.amt.contr)


fit.avg.exp <- lmer(exA ~
                         round +
                         inn.diff.from.avg +
                         exp.diff.from.avg +
                (1|session/player),
                data=mydata[mydata$condition == "Stratified" & mydata$round > 1,])
summary(fit.avg.exp)

fit.avg.exp.contr <- lmer(exA ~
                         round +
                   inn.diff.from.avg +
                exp.diff.from.avg +
                success.rel +
                age.group2 + female +
                svo.group + skill + belief.choice +
                (1|session/player),
                data=mydata[mydata$condition == "Stratified" & mydata$round > 1,])
summary(fit.avg.exp.contr)


## MAX

fit.max.score <- lmer(exA ~
                round +
                r.mean.from.diff.from.max +
                (1|session/player),
                data=mydata[mydata$condition == "Stratified" & mydata$round > 1,])
summary(fit.max.score)

fit.max.score.contr <- lmer(exA ~
                round +
                r.mean.from.diff.from.max +
                success.rel +
                age.group2 + female +
                svo.group + skill + belief.choice +
                (1|session/player),
            data=mydata[mydata$condition == "Stratified" & mydata$round > 1,])
summary(fit.max.score.contr)


fit.max.amt <- lmer(exA ~
                        round +
                        creativity.diff.from.max +
                overall.diff.from.max +
                abstract.diff.from.max +
                (1|session/player),
                data=mydata[mydata$condition == "Stratified" & mydata$round > 1,])
summary(fit.max.amt)



fit.max.amt.contr <- lmer(exA ~
                              round +
                        creativity.diff.from.max +
                overall.diff.from.max +
                abstract.diff.from.max +
                success.rel +
                age.group2 + female +
                svo.group + skill + belief.choice +
                (1|session/player),
            data=mydata[mydata$condition == "Stratified" & mydata$round > 1,])
summary(fit.max.amt.contr)

fit.max.exp <- lmer(exA ~
                        round +
                      inn.diff.from.max +
                exp.diff.from.max +
                (1|session/player),
                data=mydata[mydata$condition == "Stratified" & mydata$round > 1,])
summary(fit.max.exp)

fit.max.exp.contr <- lmer(exA ~
                              round +
                      inn.diff.from.max +
                exp.diff.from.max +
                success.rel +
                age.group2 + female +
                svo.group + skill + belief.choice +
                (1|session/player),
            data=mydata[mydata$condition == "Stratified" & mydata$round > 1,])
summary(fit.max.exp.contr)




ttexreg(list(fit.curr.score, fit.curr.amt, fit.curr.exp,
            fit.diff.score, fit.diff.amt, fit.diff.exp,
            fit.avg.score, fit.avg.amt, fit.avg.exp,
            fit.max.score, fit.max.amt, fit.max.exp))


ttexreg(list(fit.curr.score.contr, fit.curr.amt.contr, fit.curr.exp.contr,
            fit.diff.score.contr, fit.diff.amt.contr, fit.diff.exp.contr,
            fit.avg.score.contr, fit.avg.amt.contr, fit.avg.exp.contr,
            fit.max.score.contr, fit.max.amt.contr, fit.max.exp.contr))


## HP2: Do participants as reviewers assume that images submitted to
## the top tier are inherently of better quality (regardless whether
## they actually are so)?



fit.basic.all <- lmer(r.mean.from.clean.asskill ~ round +
                          ex +
                      (1|session/player),
                  data=mydata[mydata$stratified == 1,])
summary(fit.basic.all)


fit.basic.rej <- lmer(r.mean.from.clean.asskill ~ round +
                      ex +
                      (1|session/player),
                  data=mydata[mydata$stratified == 1 & mydata$published == 0,])
summary(fit.basic.rej)

fit.basic <- lmer(r.mean.from.clean.asskill ~ round +
                      ex +
                      (1|session/player),
                  data=mydata[mydata$stratified == 1 & mydata$published == 1,])
summary(fit.basic)


fit.controls2.all <- lmer(r.mean.from.clean.asskill ~ round +
                         ex +
                         overall + creativity + abstract +
                         female +
                         age.group2 + svo.group + skill + success.rel +
                         (1|session/player),
                     data=mydata[mydata$published == 0,])
summary(fit.controls2.all)

fit.controls2.rej <- lmer(r.mean.from.clean.asskill ~ round +
                         ex +
                         overall + creativity + abstract +
                         female +
                         age.group2 + svo.group + skill + success.rel +
                         (1|session/player),
                     data=mydata[mydata$published == 0,])
summary(fit.controls2.rej)

fit.controls2 <- lmer(r.mean.from.clean.asskill ~ round +
                         ex +
                         overall + creativity + abstract +
                         female +
                         age.group2 + svo.group + skill + success.rel +
                         (1|session/player),
                     data=mydata[mydata$stratified == 1 & mydata$published == 1,])
summary(fit.controls2)


ttexreg(list(fit.basic.all, fit.basic.rej, fit.basic,
            fit.controls2.all,
            fit.controls2.rej,
            fit.controls2))

##  HP 3: If More participants are submitting to the top tier, does it
## increase the likelihood of having at least one very good image ?

## Now controlling for the number of submissions in exhibition.


fit.basic.all <- lmer(r.mean.from.clean.asskill ~ round +
                          ex + nEx +
                      (1|session/player),
                  data=mydata[mydata$stratified == 1,])
summary(fit.basic.all)


fit.basic.rej <- lmer(r.mean.from.clean.asskill ~ round +
                      ex + nEx +
                      (1|session/player),
                  data=mydata[mydata$stratified == 1 & mydata$published == 0,])
summary(fit.basic.rej)

fit.basic <- lmer(r.mean.from.clean.asskill ~ round +
                      ex + nEx +
                      (1|session/player),
                  data=mydata[mydata$stratified == 1 & mydata$published == 1,])
summary(fit.basic)


fit.controls2.all <- lmer(r.mean.from.clean.asskill ~ round +
                         ex +
                         overall + creativity + abstract +
                         female +
                         age.group2 + svo.group + skill + success.rel + nEx +
                         (1|session/player),
                     data=mydata[mydata$published == 0,])
summary(fit.controls2.all)



fit.controls2.rej <- lmer(r.mean.from.clean.asskill ~ round +
                         ex + nEx
                         overall + creativity + abstract +
                         female +
                         age.group2 + svo.group + skill + success.rel + nEx +
                         (1|session/player),
                     data=mydata[mydata$published == 0,])
summary(fit.controls2.rej)

fit.controls2 <- lmer(r.mean.from.clean.asskill ~ round +
                         ex + nEx
                         overall + creativity + abstract +
                         female +
                         age.group2 + svo.group + skill + success.rel + nEx +
                         (1|session/player),
                     data=mydata[mydata$stratified == 1 & mydata$published == 1,])
summary(fit.controls2)



ttexreg(list(fit.basic.all, fit.basic.rej, fit.basic,
            fit.controls2.all,
            fit.controls2.rej,
            fit.controls2))

## Hp 4: Is the top-tier institution, by filtering the number of
## published images, artificially increasing the quality of its
## output, all the rest being equal?


fit.basic.r1 <- lmer(r.mean.from.clean.asskill ~ round +
                      ex +
                      (1|session/player),
                  data=mydata[mydata$stratified == 1 & mydata$rank1 == 1 ,])
summary(fit.basic.r1)



fit.basic.nx.r1 <- lmer(r.mean.from.clean.asskill ~ round +
                      ex + nEx +
                      (1|session/player),
                  data=mydata[mydata$stratified == 1 & mydata$rank1 == 1,])
summary(fit.basic.nx.r1)


fit.controls2.r1 <- lmer(r.mean.from.clean.asskill ~ round +
                         ex +
                         overall + creativity + abstract +
                         female +
                         age.group2 + svo.group + skill + success.rel +
                         (1|session/player),
                     data=mydata[mydata$stratified == 1 & mydata$rank1 == 1,])
summary(fit.controls2.r1)


fit.controls2.nx.r1 <- lmer(r.mean.from.clean.asskill ~ round +
                         ex + nEx +
                         overall + creativity + abstract +
                         female +
                         age.group2 + svo.group + skill + success.rel +
                         (1|session/player),
                     data=mydata[mydata$stratified == 1 & mydata$rank1 == 1,])
summary(fit.controls2.nx.r1)



ttexreg(list(fit.basic.r1, fit.basic.nx.r1,
             fit.controls2.r1, fit.controls2.nx.r1))



## Exhibitions in Stratified vs Flat market as a whole.

fit.basic.cl <- lmer(r.mean.from.clean.asskill ~ round +
                      ex2 +
                      (1|session/player),
                  data=mydata)
summary(fit.basic.cl)


fit.basic.r.cl <- lmer(r.mean.from.clean.asskill ~ round +
                        ex2 +
                        (1|session/player),
                    data=mydata[mydata$published == 0,])
summary(fit.basic.r.cl)

fit.basic.p.cl <- lmer(r.mean.from.clean.asskill ~ round +
                        ex2 +
                        (1|session/player),
                    data=mydata[mydata$published == 1,])
summary(fit.basic.p.cl)


fit.amt.cl <- lmer(r.mean.from.clean.asskill ~ round +
                    ex2 +
                    age.group2 + svo.group + skill +
                    overall + creativity + abstract +
                    (1|session/player),
                data=mydata)
summary(fit.amt.cl)

fit.amt.r.cl <- lmer(r.mean.from.clean.asskill ~ round +
                        ex2 +
                        age.group2 + svo.group + skill +
                        overall + creativity + abstract +
                        (1|session/player),
                    data=mydata[mydata$published == 0,])
summary(fit.amt.r.cl)

fit.amt.p.cl <- lmer(r.mean.from.clean.asskill ~ round +
                    ex2 +
                    age.group2 + svo.group + skill +
                    overall + creativity + abstract +
                    (1|session/player),
                data=mydata[mydata$published == 1,])
summary(fit.amt.p.cl)


ttexreg(list(fit.basic.cl, fit.basic.r.cl, fit.basic.p.cl,
             fit.amt.cl, fit.amt.r.cl, fit.amt.p.cl))
