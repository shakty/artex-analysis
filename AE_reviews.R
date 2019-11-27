## 3.1.1 Quality Across Markets.

## r.mean.from = average review per image (3 reviewers).
## r.mean = individual review.

## All.
t.test(r.mean.from ~ condition, data=p1pramt)

## Published.
t.test(r.mean.from ~ condition, data=p1pramt[p1pramt$published == 1,])

myAnova <- lmer(r.mean.from ~ ex + (1|session/player),
                data=p1pramt[p1pramt$condition == "Stratified" & p1pramt$published == 1,])
anova(myAnova)

summary(glht(myAnova, linfct = mcp(ex = "Tukey")),
        test = adjusted(type = "bonferroni"))
## Still significant.


## Regressions.
###############

mydata=p1pramt[p1pramt$age.group2 %in% c("young", "old") &
               p1pramt$svo.group %in% c("Individualistic", "Prosocial")
              ,]

fit.vsflat <- lmer(r.mean.from.clean.asskill ~ round + ex2 + (1|session/player),
            mydata)
summary(fit.vsflat)

fit.vsflat.r <- lmer(r.mean.from.clean.asskill ~ round + ex2 + (1|session/player),
            mydata[mydata$published == 0,])
summary(fit.vsflat.r)

fit.vsflat.p <- lmer(r.mean.from.clean.asskill ~ round + ex2 + (1|session/player),
            mydata[mydata$published == 1,])
summary(fit.vsflat.p)


fit.vsflat.contr <- lmer(r.mean.from.clean.asskill ~ round + ex2 +
                       overall + creativity + abstract +
                       female + age.group2 + svo.group2 + skill + success.rel +
    (1|session/player),
            mydata)
summary(fit.vsflat.contr)

fit.vsflat.r.contr <- lmer(r.mean.from.clean.asskill ~ round + ex2 +
                       overall + creativity + abstract +
                       female + age.group2 + svo.group2 + skill + success.rel +
                       (1|session/player),
            mydata[mydata$published == 0,])
summary(fit.vsflat.r.contr)

fit.vsflat.p.contr <- lmer(r.mean.from.clean.asskill ~ round + ex2 +
                         overall + creativity + abstract +
                         female + age.group2 + svo.group2 + skill + success.rel +
                       (1|session/player),
            mydata[mydata$published == 1,])
summary(fit.vsflat.p.contr)

ttexreg(list(fit.vsflat, fit.vsflat.r, fit.vsflat.p,
             fit.vsflat.contr, fit.vsflat.r.contr, fit.vsflat.p.contr))



## Unit of analysis single review.
##################################

mydata <- meltReviews(data=p1pramt)

summarySE(mydata, "r.mean", "condition", na.rm=TRUE)


doPlotMean("r.from", P1=FALSE, data=mydata, format="png",
           na.rm=TRUE, SAVE=SAVEIMG)

doPlotRound("r.from", P1=FALSE, data=mydata, format="png",
            na.rm=TRUE, SAVE=SAVEIMG)

doPlotDistr("r.from", P1=FALSE, data=mydata, format="png",
            SAVE=SAVEIMG)


## Plots.
#########

## Mean.

doPlotMean("r.from", data=mydata, P1=FALSE, facet="ex.review", format="png",
           SAVE=SAVEIMG, width=10, na.rm=TRUE)

## Round.

doPlotRound("r.from", data=mydata, P1=FALSE, facet="ex.review", format="png",
            SAVE=SAVEIMG, width=10, na.rm=TRUE)

customTheme <- theme(
    legend.position = c(0.37, 0.85),
    legend.title = element_text(vjust=3, size=20, face="bold")
) + myThemeMod

doPlotRound("r.from", data=mydata, P1=FALSE, ex=TRUE, format="png",
            SAVE=SAVEIMG, width=10, ex.name="ex.review", na.rm=TRUE,
            customize = customTheme)

## Distr.

doPlotDistr("r.from", data=mydata, P1=FALSE, facet="ex.review", format="png",
            SAVE=SAVEIMG, width=10, na.rm=TRUE)

doPlotDistr("r.from", data=mydata, P1=FALSE, ex=TRUE, format="png",
            ex.name="ex.review", na.rm=TRUE,
            SAVE=SAVEIMG, width=10, doLegend=TRUE)
