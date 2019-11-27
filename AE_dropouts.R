## 3.2.5 Participation

## Note! You need to select DO.DROPOUTS <- TRUE in file AE_INDEX.R,
## and then load the dataset.

## Specific info about single dropouts in file AE_dropouts_info.R.


## T-Test overall.
##################

tt <- table(dps.full$condition, dps.full$dropout)
t.test(dropout.num ~ condition, dps.full)
## t = -2.1654, df = 28.986, p-value = 0.03873

fit <- lm(dropout ~ condition, data=dps.full)
summary(fit)
## conditionStratified   0.3462     0.1682   2.058   0.0487 *

doPlotMean("dropout.num", P1=FALSE, data=dps.full, SAVE=SAVEIMG,
           format="svg",
           customize=(ylab('Rate of Dropouts')))

## Among all the dropouts, how are they distributed?
####################################################

chisq.test(table(uniqueDp$condition))
## X-squared = 4.7647, df = 1, p-value = 0.0290

## Among all the first dropouts, how are they distributed?
chisq.test(table(uniqueDp[uniqueDp$first.dropout == 1,]$condition))
## X-squared = 4.7647, df = 1, p-value = 0.0290


## Actual dropout histories.
############################

mydata <- uptodropout[uptodropout$dropout == 1,]
mydata$published <- as.factor(mydata$published)
mydata <- mydata[order(mydata$condition),]

p <- ggplot(mydata, aes(round, r.mean.from, color=condition, group=player))
p <- p + geom_point(aes(shape=ex, size=published))
p <- p + geom_line()
##p <- p + scale_shape_manual(values=c("A", "B", "C"))
p <- p + scale_size_manual(values=c(3,5))
##p <- p + geom_errorbar(aes(ymin=session.dropout-ci,
##                           ymax=session.dropout+ci), width=0.5)
p <- p + xlab('Round') + ylab('Received Review Score')
p <- p + facet_wrap(~player)
p

gggsave("dropouts_individual_histories.png", width=12, height=16)

## Differences dropouts non-dropouts.
#####################################

doViolin <- function(varname, individual.feature=FALSE, width=7, height=7,
                     PREFIX='', SAVE=FALSE, y.lab="", all.dropouts=FALSE) {
    ##
    dr <- ifelse(all.dropouts == TRUE, "dropout.char", "first.dropout.char")
    ##
    if (individual.feature == FALSE) {
        mydata <- uptodropout
    } else {
        mydata <- uptodropout[uptodropout$round == 1,]
    }
    p <- ggplot(mydata, aes_string(dr, varname)) +
        geom_jitter(alpha=0.15) +
        geom_violin(alpha=0.7, size=1, aes(color=condition, fill=condition),
                    draw_quantiles = c(0.25, 0.5, 0.75)) +
        facet_grid(~condition) +
        xlab("") + ylab(y.lab) + theme(legend.position = "none")

    return(saveOrDisplayPlot(p, varname, SAVE=SAVEIMG, type="violin",
                             format="svg", width=width, height=height,
                             PREFIX=PREFIX, facet="condition", ex=FALSE))
}

## GAME FACTS.

doViolin("r.mean", FALSE, SAVE=SAVEIMG, y.lab="Review Score Given")

doViolin("r.mean.from", FALSE, SAVE=SAVEIMG, y.lab="Review Score Received")

doViolin("r.diff", FALSE, SAVE=SAVEIMG, y.lab="Difference Review Score Given - Received")

doViolin("dropout.success.rel", FALSE, SAVE=SAVEIMG, y.lab="Relative Success")

doViolin("creativity", FALSE, SAVE=SAVEIMG, y.lab="AMT Creativity")

doViolin("overall", FALSE, SAVE=SAVEIMG, y.lab="AMT Overall")

doViolin("d.pubprev", FALSE, SAVE=SAVEIMG, y.lab="Innovation")


## PERSON FACTS.


doViolin("belief.choice", TRUE, SAVE=SAVEIMG, y.lab="Self Efficacy")

doViolin("skill", TRUE, SAVE=SAVEIMG, y.lab="Skill")

doViolin("skill", TRUE, SAVE=SAVEIMG, y.lab="Skill", all.dropouts=TRUE)

doViolin("skill3.unscaled", TRUE, SAVE=SAVEIMG, y.lab="Changes per Sec.")

doViolin("svo", TRUE, SAVE=FALSE)

doViolin("skill3.rescaled", TRUE, SAVE=SAVEIMG, y.lab="Changes per Sec.")

## Age
summ <- summarySE(uptodropout, "dropout.num", c("condition", "age.group2"), na.rm=TRUE)
summ

summ <- summarySE(uptodropout, "first.dropout", c("condition", "age.group2"), na.rm=TRUE)
summ

mydata <- uptodropout[uptodropout$dropout.first.happened == 1,]

tt <- table(mydata$first.dropout, mydata$condition, mydata$age.group2)
tt <- as.data.frame(tt)
colnames(tt) <- c("Dropout", "Market", "Age", "Freq")
tt <- tt[tt$Age %in% c("young", "old"),]
tt$Age <- ifelse(tt$Age == "young", "Age \u2264 30", ifelse(tt$Age == "old", "Age > 30", "Other"))

ggplot(tt,
       aes(Age, Freq, group=Dropout, fill=Market)) +
    geom_bar(stat="identity", position="dodge", color="white") +
    facet_grid(~Market) + theme(legend.position = "none") +
    xlab("") + ylab("Count")

gggsave("dropout_age.png")


tt <- table(mydata$first.dropout, mydata$condition, mydata$svo.group)
tt <- as.data.frame(tt)
colnames(tt) <- c("Dropout", "Market", "SVO", "Freq")

ggplot(tt[tt$SVO %in% c("Individualistic", "Prosocial"),],
       aes(SVO, Freq, group=interaction(SVO,Dropout), fill=Market)) +
    geom_bar(stat="identity", position="dodge", color="white") +
    facet_grid(~Market) + theme(legend.position = "none") +
    xlab("") + ylab("Count")

gggsave("dropout_svo.png")

tt <- table(mydata$first.dropout, mydata$condition, mydata$gender.choice)
tt <- as.data.frame(tt)
colnames(tt) <- c("Dropout", "Market", "Gender", "Freq")

ggplot(tt[tt$Gender %in% c("Male", "Female"),],
       aes(Gender, Freq, group=Dropout, fill=Market)) +
    geom_bar(stat="identity", position="dodge", color="white") +
    facet_grid(~Market) + theme(legend.position = "none") +
    xlab("") + ylab("Count")

gggsave("dropout_gender.png")


tt <- table(mydata$first.dropout, mydata$condition, mydata$belief.choice)
tt <- as.data.frame(tt)
colnames(tt) <- c("Dropout", "Market", "Self.Eff", "Freq")

ggplot(tt,
       aes(Self.Eff, Freq, group=Dropout, fill=Market)) +
    geom_bar(stat="identity", position="dodge", color="white") +
    facet_grid(Dropout~Market) + theme(legend.position = "none") +
    xlab("") + ylab("Count")

gggsave("dropout_gender.png")


######################################################################

### TESTS

uptodropout.full.r1 <- uptodropout.full[uptodropout.full$round == 1,]
uptodropout.r1 <- uptodropout[uptodropout$round == 1,]
mystratified <- uptodropout[uptodropout$stratified == 1,]
mystratified.r1 <- mystratified[mystratified$round == 1,]


## All p-values
ps <- c(0.0351,
        0.4714,
        0.1271,
        0.5818,
        0.7303,
        0.9831,
        7.946e-11,
        0.05696,
        0.1861,
        0.4565,
        0.05907,
        0.1713
        )

round(p.adjust(ps, method="bonferroni"), 3)
round(p.adjust(ps, method="holm"), 3)

## SKILL.

t.test(skill ~ first.dropout, data=mystratified.r1)
## t = 1.5579, df = 12.712, p-value = 0.1438

t.test(skill ~ dropout, data=mystratified.r1)
## t = 2.2524, df = 21.031, p-value = 0.0351

t.test(skill ~ dropout, data=uptodropout.r1)
## t = 1.976, df = 28.709, p-value = 0.05784

t.test(skill ~ dropout, data=uptodropout.full.r1)
## t = 2.2169, df = 21.334, p-value = 0.03762

t.test(skill ~ dropout, data=uptodropout.full.r1[uptodropout.full.r1$stratified == 1,])
## t = 2.3953, df = 16.938, p-value = 0.02844

t.test(skill ~ first.dropout, data=uptodropout.full.r1)
## t = 2.0526, df = 13.292, p-value = 0.06035

t.test(skill ~ first.dropout, data=uptodropout.full.r1[uptodropout.full.r1$stratified == 1,])
## t = 1.677, df = 11.102, p-value = 0.1214


## Resistent to correction
p.adjust(0.0351, n=12) ## , method="holm"

p.adjust(0.0351, n=12, method="holm")


## SVO

mydata <- mystratified.r1[mystratified.r1$svo.group %in% c("Individualistic", "Prosocial"),]

prop.test(table(mydata$first.dropout, mydata$svo.group), correct=FALSE)
## X-squared = 0.51872, df = 1, p-value = 0.4714

prop.test(table(mydata$dropout, mydata$svo.group), correct=FALSE)
## X-squared = 1.6673, df = 1, p-value = 0.1966


## GENDER

mydata <- mystratified.r1[mystratified.r1$gender.choice %in% c("Male", "Female"),]

prop.test(table(mydata$first.dropout, mydata$female), correct=FALSE)
## X-squared = 2.3274, df = 1, p-value = 0.1271

prop.test(table(mydata$dropout, mydata$female), correct=FALSE)
## X-squared = 1.7389, df = 1, p-value = 0.1873


## AGE

mydata <- mystratified.r1[mystratified.r1$age.group2 %in% c("old", "young"),]

prop.test(table(mydata$first.dropout, mydata$young), correct=FALSE)
## X-squared = 0.30339, df = 1, p-value = 0.5818

prop.test(table(mydata$dropout, mydata$young), correct=FALSE)
## X-squared = 0.58199, df = 1, p-value = 0.4455


## SELF-EFFICACY.

t.test(belief.choice ~ first.dropout, data=mystratified.r1)
## t = -0.35405, df = 10.584, p-value = 0.7303

t.test(belief.choice ~ dropout, data=mystratified.r1)
## t = 0.42671, df = 15.131, p-value = 0.6756


p.adjust(0.0351, n=2, method="holm")


## SPEED.

t.test(skill3 ~ first.dropout, data=mystratified.r1)
## t = 0.021633, df = 12.749, p-value = 0.9831

t.test(belief.choice ~ dropout, data=mystratified.r1)
## t = 0.42671, df = 15.131, p-value = 0.6756



## ## MOOD NEG.
##
## t.test(mood.neg ~ first.dropout, data=mystratified.r1)
## ## t = 0.13958, df = 10.316, p-value = 0.8917
##
## t.test(mood.neg ~ dropout, data=mystratified.r1)
## ## t = 0.5747, df = 14.937, p-value = 0.5741
##
##
## t.test(mood.neg ~ first.dropout, data=uptodropout.r1)
## ## t = 0.69079, df = 12.798, p-value = 0.502
##
## t.test(mood.neg ~ dropout, data=uptodropout.r1)
## ## t = 0.99339, df = 20.146, p-value = 0.3323
##
##
## ## MOOD POS.
##
## t.test(mood.pos ~ first.dropout, data=mystratified.r1)
## ## t = -0.28318, df = 15.481, p-value = 0.7808
##
## t.test(mood.pos ~ dropout, data=mystratified.r1)
## ## t = -0.26244, df = 22.475, p-value = 0.7954


## GAME BEHAVIOR
################

## SUCCESS REL.

t.test(success.rel ~ first.dropout, data=mystratified)
## t = 7.6702, df = 69.104, p-value = 7.946e-11

t.test(success.rel ~ dropout, data=mystratified)
## t = 6.396, df = 84.245, p-value = 8.512e-09

## Resistent to correction
p.adjust(7.946e-11, n=12) ## , method="holm"

fit <- lmer(success.rel ~ first.dropout + (1|session/player), data=mystratified)
summary(fit)

anova(fit)

summary(glht(fit, linfct = mcp(first.dropout = "Tukey")), test = adjusted(type = "bonferroni"))


fit <- lmer(success.rel ~ first.dropout + (1|session/player), data=mystratified)
summary(fit)




## Significant negative as expected

fit <- lmer(success.rel ~ dropout + (1|session/player), data=mystratified)
summary(fit)

## Close to significance.

fit <- lmer(payoff ~ first.dropout + (1|session/player), data=mystratified)
summary(fit)
## Close to significance.

fit <- lmer(payoff ~ dropout + (1|session/player), data=mystratified)
summary(fit)
## Significant.

fit <- lmer(payoff ~ dropout+stratified + (1|session/player), data=uptodropout)
summary(fit)
## Significant.

fit <- lmer(payoff ~ success.rel+stratified +round+ (1|session/player), data=uptodropout)
summary(fit)
## Close to significance.

fit <- lmer(payoff ~ success.rel+stratified + round +(1|session/player), data=uptodropout)
summary(fit)
## Close to significance.


## REVIEW SCORE GIVEN


t.test(r.mean ~ first.dropout, data=mystratified)
## t = -1.9524, df = 46.311, p-value = 0.05696

t.test(r.mean ~ dropout, data=mystratified)
## t = -3.3644, df = 68.031, p-value = 0.001264

## Still passes correction.
p.adjust(0.001264, n=12)

## Still passes correction.
p.adjust(0.001264, n=12)

## Not significant.
fit <- lmer(r.mean ~ first.dropout  +(1|session/player), data=mystratified)
summary(fit)

## Not significant.
fit <- lmer(r.mean ~ dropout  +(1|session/player), data=mystratified)
summary(fit)

## Not significant.
fit <- lmer(r.mean ~ first.dropout +stratified  +(1|session/player), data=uptodropout)
summary(fit)

## Not significant.
fit <- lmer(r.mean ~ dropout  + stratified + (1|session/player), data=uptodropout)
summary(fit)


## REVIEW SCORE RECEIVED

t.test(r.mean.from ~ first.dropout, data=mystratified)
## t = 1.3419, df = 47.096, p-value = 0.1861

t.test(r.mean.from ~ dropout, data=mystratified)
## t = 0.22122, df = 63.869, p-value = 0.8256

## Not significant.
fit <- lmer(r.mean.from ~ first.dropout  +(1|session/player), data=mystratified)
summary(fit)

## Not significant.
fit <- lmer(r.mean.from ~ dropout  +(1|session/player), data=mystratified)
summary(fit)

## Not significant.
fit <- lmer(r.mean.from ~ first.dropout +stratified  +(1|session/player), data=uptodropout)
summary(fit)

## Not significant.
fit <- lmer(r.mean.from ~ dropout  + stratified + (1|session/player), data=uptodropout)
summary(fit)


## INNOVATION

t.test(d.pubprev ~ first.dropout, data=mystratified)
## t = -0.75366, df = 32.554, p-value = 0.4565

t.test(r.mean.from ~ dropout, data=mystratified)
## t = 0.22122, df = 63.869, p-value = 0.8256

## Not significant.
fit <- lmer(d.pubprev ~ first.dropout  +(1|session/player), data=mystratified)
summary(fit)

## Not significant.
fit <- lmer(d.pubprev ~ dropout  +(1|session/player), data=mystratified)
summary(fit)

## Not significant.
fit <- lmer(d.pubprev ~ first.dropout +stratified  +(1|session/player), data=uptodropout)
summary(fit)

## Not significant.
fit <- lmer(d.pubprev ~ dropout  + stratified + (1|session/player), data=uptodropout)
summary(fit)


## DIVERSITY

t.test(d.subcurr ~ first.dropout, data=mystratified)
## t = -1.0376, df = 42.599, p-value = 0.3053

t.test(d.subcurr ~ dropout, data=mystratified)
## t = 0.1638, df = 57.751, p-value = 0.8705

## Not significant.
fit <- lmer(d.subcurr ~ first.dropout  +(1|session/player), data=mystratified)
summary(fit)

## Not significant.
fit <- lmer(d.subcurr ~ dropout  +(1|session/player), data=mystratified)
summary(fit)

## Not significant.
fit <- lmer(d.subcurr ~ first.dropout +stratified  +(1|session/player), data=uptodropout)
summary(fit)

## Not significant.
fit <- lmer(d.subcurr ~ dropout  + stratified + (1|session/player), data=uptodropout)
summary(fit)



## CREATIVITY

t.test(creativity ~ first.dropout, data=mystratified)
## t = -1.9406, df = 41.838, p-value = 0.05907

t.test(creativity ~ dropout, data=mystratified)
## t = -0.88198, df = 55.206, p-value = 0.3816

## Not significant.
fit <- lmer(creativity ~ first.dropout  +(1|session/player), data=mystratified)
summary(fit)

## Not significant.
fit <- lmer(creativity ~ dropout  +(1|session/player), data=mystratified)
summary(fit)

## Not significant.
fit <- lmer(creativity ~ first.dropout +stratified  +(1|session/player), data=uptodropout)
summary(fit)

## Not significant.
fit <- lmer(creativity ~ dropout  + stratified + (1|session/player), data=uptodropout)
summary(fit)


## ABSTRACT

t.test(abstract ~ first.dropout, data=mystratified)
## t = -1.3918, df = 42.295, p-value = 0.1713

t.test(abstract ~ dropout, data=mystratified)
## t = -1.1417, df = 58.047, p-value = 0.2583

## Not significant.
fit <- lmer(abstract ~ first.dropout  +(1|session/player), data=mystratified)
summary(fit)

## Not significant.
fit <- lmer(abstract ~ dropout  +(1|session/player), data=mystratified)
summary(fit)

## Not significant.
fit <- lmer(abstract ~ first.dropout +stratified  +(1|session/player), data=uptodropout)
summary(fit)

## Not significant.
fit <- lmer(abstract ~ dropout  + stratified + (1|session/player), data=uptodropout)
summary(fit)


## OVERALL

t.test(overall ~ first.dropout, data=mystratified)
## t = -1.3918, df = 42.295, p-value = 0.1713

t.test(overall ~ dropout, data=mystratified)
## t = -1.1417, df = 58.047, p-value = 0.2583

## Not significant.
fit <- lmer(overall ~ first.dropout  +(1|session/player), data=mystratified)
summary(fit)

## Not significant.
fit <- lmer(overall ~ dropout  +(1|session/player), data=mystratified)
summary(fit)

## Not significant.
fit <- lmer(overall ~ first.dropout +stratified  +(1|session/player), data=uptodropout)
summary(fit)

## Not significant.
fit <- lmer(overall ~ dropout  + stratified + (1|session/player), data=uptodropout)
summary(fit)



## Consider only the first dropout.
###################################


## Payoff
doPlotMean("payoff", P1=FALSE, data=uptodropout, SAVE=SAVEIMG,
           group="first.dropout.char", facet="condition", color="condition",
           format="svg", na.rm=TRUE, PREFIX="first-dropout_")



doPlotMean("payoff", P1=FALSE, data=uptodropout.full, SAVE=SAVEIMG,
           group="first.dropout.char", facet="condition", color="condition",
           format="png", na.rm=TRUE, PREFIX="full_first-dropout_")


## R.diff avg score given - received
doPlotMean("r.diff", P1=FALSE, data=uptodropout, SAVE=SAVEIMG,
           group="first.dropout.char", facet="condition", color="condition",
           format="svg", na.rm=TRUE, PREFIX="first-dropout_",
           customize=(ylab('Avg. Diff. Review Score Given - Received')))



doPlotMean("r.diff", P1=FALSE, data=uptodropout.full, SAVE=SAVEIMG,
           group="first.dropout.char", facet="condition", color="condition",
           format="png", na.rm=TRUE, PREFIX="full_first-dropout_",
           customize=(ylab('Avg. Diff. Review Score Given - Received')))


## Score given
doPlotMean("r.mean", P1=FALSE, data=uptodropout, SAVE=SAVEIMG,
           group="first.dropout.char", facet="condition", color="condition",
           format="svg", na.rm=TRUE, PREFIX="first-dropout_",
           customize=(ylab('Avg. Review Score Given')))



doPlotMean("r.mean", P1=FALSE, data=uptodropout.full, SAVE=SAVEIMG,
           group="first.dropout.char", facet="condition", color="condition",
           format="png", na.rm=TRUE, PREFIX="full_first-dropout_",
           customize=(ylab('Avg. Review Score Given')))



## Score received
doPlotMean("r.mean.from", P1=FALSE, data=uptodropout, SAVE=SAVEIMG,
           group="first.dropout.char", facet="condition", color="condition",
           format="svg", na.rm=TRUE, PREFIX="first-dropout_",
           customize=(ylab('Avg. Review Score Received')))



doPlotMean("r.mean.from", P1=FALSE, data=uptodropout.full, SAVE=SAVEIMG,
           group="first.dropout.char", facet="condition", color="condition",
           format="svg", na.rm=TRUE, PREFIX="full_first-dropout_",
           customize=(ylab('Avg. Review Score Received')))



## Divinn
doPlotMean("d.subcurr", P1=FALSE, data=uptodropout, SAVE=SAVEIMG,
           group="first.dropout.char", facet="condition", color="condition",
           format="svg", na.rm=TRUE, PREFIX="first-dropout_",
           customize=(ylab('Avg. Diversity')))

doPlotMean("d.pubprev", P1=FALSE, data=uptodropout, SAVE=SAVEIMG,
           group="first.dropout.char", facet="condition", color="condition",
           format="svg", na.rm=TRUE, PREFIX="first-dropout_",
           customize=(ylab('Avg. Innovation')))

doPlotMean("d.ownprev", P1=FALSE, data=uptodropout, SAVE=SAVEIMG,
           group="first.dropout.char", facet="condition", color="condition",
           format="svg", na.rm=TRUE, PREFIX="first-dropout_",
           customize=(ylab('Avg. Personal Change')))


## AMT Scores

## No significant differences with AMT scores.


doPlotMean("creativity", P1=FALSE, data=uptodropout, SAVE=SAVEIMG,
           group="first.dropout.char", facet="condition", color="condition",
           format="svg", na.rm=TRUE, PREFIX="first-dropout_",
           customize=(ylab('Avg. AMT Creativity')))


doPlotMean("abstract", P1=FALSE, data=uptodropout, SAVE=SAVEIMG,
           group="first.dropout.char", facet="condition", color="condition",
           format="svg", na.rm=TRUE, PREFIX="first-dropout_",
           customize=(ylab('Avg. AMT Abstract')))


doPlotMean("face", P1=FALSE, data=uptodropout, SAVE=SAVEIMG,
           group="first.dropout.char", facet="condition", color="condition",
           format="svg", na.rm=TRUE, PREFIX="first-dropout_",
           customize=(ylab('Avg. AMT Face')))

doPlotMean("overall", P1=FALSE, data=uptodropout, SAVE=SAVEIMG,
           group="first.dropout.char", facet="condition", color="condition",
           format="svg", na.rm=TRUE, PREFIX="first-dropout_",
           customize=(ylab('Avg. AMT Overall')))



