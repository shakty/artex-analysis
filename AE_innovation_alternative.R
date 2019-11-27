## A.4.3 Innovation: Alternative Measures

distances <- loadGame(file='ae_all_distances.csv')


# Diversity, Innovation and Personal Change in one plot.
########################################################

pAll <- ggplot(distances, aes(x=round, group=1))
pAll <- pAll + geom_smooth(aes(y = d.subcurr, linetype="Diversity"),
                           color="black", size=2)
pAll <- pAll + geom_smooth(aes(y = d.pubprev, linetype="Innovation"),
                           color="black", size=2)
pAll <- pAll + geom_smooth(aes(y = d.ownprev, linetype="Personal Change"),
                           color="black",size=2)
pAll <- pAll + ylab("Avg. Difference") + xlab("Round")
pAll <- pAll + scale_linetype_manual(breaks=c("Diversity","Innovation","Personal Change"),
                                     values=c(1,2,3))
#pAll <- pAll + scale_y_continuous(limits=c(0.1,0.35))
pAll <- pAll + myThemeMod + theme(legend.position = c(0.7, 0.45))
#pAll <- pAll + facet_grid(~condition)
pAll


pAll <- ggplot(distances, aes(x=round, group=1))
pAll <- pAll + geom_smooth(aes(y = d.subcurr, color="Diversity"))
pAll <- pAll + geom_smooth(aes(y = d.subcurr.rm, color="Diversity Rm"))
pAll <- pAll + geom_smooth(aes(y = dgeom.subcurr.rm, color="Diversity Geom Rm"))
pAll <- pAll + geom_smooth(aes(y = d.pubprev, linetype="Innovation"),
                           color="black")
## pAll <- pAll + geom_smooth(aes(y = dgeom.pubprev, linetype="Innovation Geom"),
##                            color="black")
pAll <- pAll + ylab("Avg. Difference") + xlab("Round")
#pAll <- pAll + scale_y_continuous(limits=c(0.1,0.35))
pAll <- pAll + myThemeMod + theme(legend.position = c(0.3, 0.8))
#pAll <- pAll + facet_grid(~condition)
pAll

##ggsave(filename=paste0(IMGDIR, 'divinnown.svg'))
gggsave('dist_measures_smooth.png')


## Plus DFA.
pAll <- ggplot(distances, aes(x=round, group=1))
pAll <- pAll + geom_smooth(aes(y = d.subcurr, color="Diversity"))
pAll <- pAll + geom_smooth(aes(y = d.subcurr.rm, color="Diversity Rm"))
pAll <- pAll + geom_smooth(aes(y = dgeom.subcurr.rm, color="Diversity Geom Rm"))
pAll <- pAll + geom_smooth(aes(y = dfa.subcurr, color="Dist Avg. Face"))
pAll <- pAll + geom_smooth(aes(y = dfa.subcurr.rm, color="Dist Avg. Face Rm"))
pAll <- pAll + geom_smooth(aes(y = d.pubprev, linetype="Innovation"),
                           color="black")
##pAll <- pAll + geom_smooth(aes(y = dgeom.pubprev, linetype="Innovation Geom"),
##                           color="black")
pAll <- pAll + ylab("Avg. Difference") + xlab("Round")
#pAll <- pAll + scale_y_continuous(limits=c(0.1,0.35))
pAll <- pAll + myThemeMod + theme(legend.position = c(0.3, 0.8))
#pAll <- pAll + facet_grid(~condition)
pAll

##ggsave(filename=paste0(IMGDIR, 'divinnown.svg'))
gggsave('dist_measures_with_dfa_smooth.png')


### DFA.


pAll <- ggplot(distances, aes(x=round, group=1))
pAll <- pAll + geom_smooth(aes(y = dfa.subcurr, linetype="Diversity"),
                           color="black", size=2)
pAll <- pAll + geom_smooth(aes(y = dfa.subcurr.rm, linetype="Diversity Rm"),
                           color="black", size=2)
pAll <- pAll + geom_smooth(aes(y = dfa.pubprev, linetype="Innovation"),
                           color="black", size=2)
pAll <- pAll + geom_smooth(aes(y = dfa.ownprev, linetype="Personal Change"),
                           color="black",size=2)
pAll <- pAll + ylab("Avg. Difference") + xlab("Round")
#pAll <- pAll + scale_y_continuous(limits=c(0.1,0.35))
pAll <- pAll + myThemeMod + theme(legend.position = c(0.7, 0.45))
#pAll <- pAll + facet_grid(~condition)
pAll


## By condition.

mydata <- melt(distances, measure.vars = c("d.subcurr", "d.subcurr.rm", "dgeom.subcurr.rm",
                                           "dfa.subcurr", "dfa.subcurr.rm"),
               variable.name="distance")
mysummary <- summarySE(mydata, "value", c("round", "distance", "condition"),
                       na.rm = TRUE)

p <- ggplot(mysummary, aes(round, value, color=condition, group=distance))
p <- p + geom_point(size=2)
p <- p + geom_line(alpha=.4, size=1, aes(group=interaction(condition, distance)))
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci), width=0.2)
p <- p + ylab("Avg. Difference") + xlab("Round")
##p <- p + scale_x_continuous(breaks=c(2,4,6,8,10,12))
p <- p + myThemeMod + theme(legend.position = c(0.24, 0.838))
p <- p + facet_grid(~distance)
p

gggsave('dist_measures_by_cond.png', width=16)


## COSINE.

pAll <- ggplot(distances, aes(x=round, group=1))
pAll <- pAll + geom_smooth(aes(y = d.cos.subcurr, color="Cosine Diversity"))
pAll <- pAll + geom_smooth(aes(y = d.cos.subcurr.rm, color="Cosine Diversity Rm"))
pAll <- pAll + geom_smooth(aes(y = d.cos.pubprev, linetype="Cosine Innovation"),
                           color="black")
pAll <- pAll + geom_smooth(aes(y = d.cos.ownprev, linetype="Cosine Own Change"),
                           color="black")
pAll <- pAll + ylab("Avg. Difference") + xlab("Round")
#pAll <- pAll + scale_y_continuous(limits=c(0.1,0.35))
pAll <- pAll + myThemeMod + theme(legend.position = c(0.3, 0.2))
#pAll <- pAll + facet_grid(~condition)
pAll

##ggsave(filename=paste0(IMGDIR, 'divinnown.svg'))
gggsave('dist_measures_cosine_smooth.png'))


## By condition.
mydata <- melt(distances, measure.vars = c("d.cos.ownprev", "d.cos.pubprev",
                                    "d.cos.subcurr", "d.cos.subcurr.rm"),
               variable.name="distance")
mysummary <- summarySE(mydata, "value", c("round", "distance", "condition"),
                       na.rm = TRUE)

p <- ggplot(mysummary, aes(round, value, color=condition, group=distance))
p <- p + geom_point(size=2)
p <- p + geom_line(alpha=.4, size=1, aes(group=interaction(condition, distance)))
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci), width=0.2)
p <- p + ylab("Cosine - Avg. Difference") + xlab("Round")
p <- p + scale_linetype_manual(labels=c("Personal Change",
                                        "Innovation",
                                        "Diversity",
                                        "Diversity Rm"
                                        ), values=c(3,1,2))
##p <- p + scale_x_continuous(breaks=c(2,4,6,8,10,12))
p <- p + myThemeMod + theme(legend.position = c(0.24, 0.838))
p <- p + facet_grid(~distance)
p


gggsave('dist_measures_cosine_bycond.png', width=16)



## Euclidean SQRT (No Norm).

pAll <- ggplot(distances, aes(x=round, group=1))
pAll <- pAll + geom_smooth(aes(y = dsqrt.subcurr, color="Sqrt Diversity"))
pAll <- pAll + geom_smooth(aes(y = dsqrt.subcurr.rm, color="Sqrt Diversity Rm"))
pAll <- pAll + geom_smooth(aes(y = dsqrt.pubprev, linetype="Sqrt Innovation"),
                           color="black")
pAll <- pAll + geom_smooth(aes(y = dsqrt.ownprev, linetype="Sqrt Own Change"),
                           color="black")
pAll <- pAll + ylab("Avg. Difference") + xlab("Round")
#pAll <- pAll + scale_y_continuous(limits=c(0.1,0.35))
pAll <- pAll + myThemeMod + theme(legend.position = c(0.3, 0.2))
#pAll <- pAll + facet_grid(~condition)
pAll


gggsave('dist_measures_euclideansqrt_smooth.png')


## By condition.
mydata <- melt(distances, measure.vars = c("dsqrt.ownprev", "dsqrt.pubprev",
                                    "dsqrt.subcurr", "dsqrt.subcurr.rm"),
               variable.name="distance")
mysummary <- summarySE(mydata, "value", c("round", "distance", "condition"),
                       na.rm = TRUE)

p <- ggplot(mysummary, aes(round, value, color=condition, group=distance))
p <- p + geom_point(size=2)
p <- p + geom_line(alpha=.4, size=1, aes(group=interaction(condition, distance)))
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci), width=0.2)
p <- p + ylab("Sqrt NoNorm - Avg. Difference") + xlab("Round")
p <- p + scale_linetype_manual(labels=c("Personal Change",
                                        "Innovation",
                                        "Diversity",
                                        "Diversity Rm"
                                        ), values=c(3,1,2))
##p <- p + scale_x_continuous(breaks=c(2,4,6,8,10,12))
p <- p + myThemeMod + theme(legend.position = c(0.1, 0.838))
p <- p + facet_grid(~distance)
p


gggsave('euclideasqrt-nonorm_bycond.png', width=16)



## Euclidean SQRT NORM.

pAll <- ggplot(distances, aes(x=round, group=1))
pAll <- pAll + geom_smooth(aes(y = dsqrt.norm.subcurr, color="Sqrt-Norm Diversity"))
pAll <- pAll + geom_smooth(aes(y = dsqrt.norm.subcurr.rm, color="Sqrt-Norm Diversity Rm"))
pAll <- pAll + geom_smooth(aes(y = dsqrt.norm.pubprev, linetype="Sqrt-Norm Innovation"),
                           color="black")
pAll <- pAll + geom_smooth(aes(y = dsqrt.norm.ownprev, linetype="Sqrt-Norm Own Change"),
                           color="black")
pAll <- pAll + ylab("Avg. Difference") + xlab("Round")
#pAll <- pAll + scale_y_continuous(limits=c(0.1,0.35))
pAll <- pAll + myThemeMod + theme(legend.position = c(0.3, 0.9))
#pAll <- pAll + facet_grid(~condition)
pAll

##ggsave(filename=paste0(IMGDIR, 'divinnown.svg'))
gggsave('euclideansqrt-norm_smooth.png')


## By condition.
mydata <- melt(distances, measure.vars = c("dsqrt.norm.ownprev", "dsqrt.norm.pubprev",
                                    "dsqrt.norm.subcurr", "dsqrt.norm.subcurr.rm"),
               variable.name="distance")
mysummary <- summarySE(mydata, "value", c("round", "distance", "condition"),
                       na.rm = TRUE)

p <- ggplot(mysummary, aes(round, value, color=condition, group=distance))
p <- p + geom_point(size=2)
p <- p + geom_line(alpha=.4, size=1, aes(group=interaction(condition, distance)))
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci), width=0.2)
p <- p + ylab("Sqrt-Norm - Avg. Difference") + xlab("Round")
##p <- p + scale_x_continuous(breaks=c(2,4,6,8,10,12))
p <- p + myThemeMod + theme(legend.position = c(0.1, 0.838))
p <- p + facet_grid(~distance)
p


gggsave('euclideansqrt-norm_bycond.png', width=16)


## Euclidean SQRT NORM Geom

pAll <- ggplot(distances, aes(x=round, group=1))
pAll <- pAll + geom_smooth(aes(y = dgeom.sqrt.norm.subcurr.rm, color="Sqrt-Norm-Geom Diversity Rm"))
pAll <- pAll + geom_smooth(aes(y = dgeom.sqrt.norm.pubprev, linetype="Sqrt-Norm-Geom Innovation"),
                           color="black")
pAll <- pAll + geom_smooth(aes(y = dgeom.sqrt.norm.ownprev, linetype="Sqrt-Norm-Geom Own Change"),
                           color="black")
pAll <- pAll + ylab("Avg. Difference") + xlab("Round")
#pAll <- pAll + scale_y_continuous(limits=c(0.1,0.35))
pAll <- pAll + myThemeMod + theme(legend.position = c(0.4, 0.9))
#pAll <- pAll + facet_grid(~condition)
pAll

##ggsave(filename=paste0(IMGDIR, 'divinnown.svg'))
ggsave('euclideansqrt-norm-geom_smooth.png')


## By condition.
mydata <- melt(distances, measure.vars = c("dgeom.sqrt.norm.ownprev", "dgeom.sqrt.norm.pubprev",
                                    "dgeom.sqrt.norm.subcurr.rm"),
               variable.name="distance")
mysummary <- summarySE(mydata, "value", c("round", "distance", "condition"),
                       na.rm = TRUE)

p <- ggplot(mysummary, aes(round, value, color=condition, group=distance))
p <- p + geom_point(size=2)
p <- p + geom_line(alpha=.4, size=1, aes(group=interaction(condition, distance)))
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci), width=0.2)
p <- p + ylab("Sqrt-Norm-Geom - Avg. Difference") + xlab("Round")
##p <- p + scale_x_continuous(breaks=c(2,4,6,8,10,12))
p <- p + myThemeMod + theme(legend.position = c(0.1, 0.838))
p <- p + facet_grid(~distance)
p



gggsave('euclideansqrt-norm-geom_bycond.png', width=16)



## Euclidean SQRT NO-NORM Geom

pAll <- ggplot(distances, aes(x=round, group=1))
pAll <- pAll + geom_smooth(aes(y = dgeom.sqrt.subcurr.rm, color="Sqrt-No-Norm-Geom Diversity Rm"))
pAll <- pAll + geom_smooth(aes(y = dgeom.sqrt.pubprev, linetype="Sqrt-No-Norm-Geom Innovation"),
                           color="black")
pAll <- pAll + geom_smooth(aes(y = dgeom.sqrt.ownprev, linetype="Sqrt-No-Norm-Geom Own Change"),
                           color="black")
pAll <- pAll + ylab("Avg. Difference") + xlab("Round")
#pAll <- pAll + scale_y_continuous(limits=c(0.1,0.35))
pAll <- pAll + myThemeMod + theme(legend.position = c(0.5, 0.35))
#pAll <- pAll + facet_grid(~condition)
pAll


gggsave('euclideansqrt-no-norm-geom_smooth.png')


## By condition.
mydata <- melt(distances, measure.vars = c("dgeom.sqrt.ownprev", "dgeom.sqrt.pubprev",
                                    "dgeom.sqrt.subcurr.rm"),
               variable.name="distance")
mysummary <- summarySE(mydata, "value", c("round", "distance", "condition"),
                       na.rm = TRUE)

p <- ggplot(mysummary, aes(round, value, color=condition, group=distance))
p <- p + geom_point(size=2)
p <- p + geom_line(alpha=.4, size=1, aes(group=interaction(condition, distance)))
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci), width=0.2)
p <- p + ylab("Sqrt-Norm-Geom - Avg. Difference") + xlab("Round")
##p <- p + scale_x_continuous(breaks=c(2,4,6,8,10,12))
p <- p + myThemeMod + theme(legend.position = c(0.1, 0.838))
p <- p + facet_grid(~distance)
p

gggsave('euclideansqrt-no-norm-geom_bycond.png', width=16)



## Euclidean NO NORM.

pAll <- ggplot(distances, aes(x=round, group=1))
pAll <- pAll + geom_smooth(aes(y = d.nonorm.subcurr, color="NoNorm Diversity"))
pAll <- pAll + geom_smooth(aes(y = d.nonorm.subcurr.rm, color="NoNorm Diversity Rm"))
pAll <- pAll + geom_smooth(aes(y = d.nonorm.pubprev, linetype="NoNorm Innovation"),
                           color="black")
pAll <- pAll + geom_smooth(aes(y = d.nonorm.ownprev, linetype="NoNorm Own Change"),
                           color="black")
pAll <- pAll + ylab("Avg. Difference") + xlab("Round")
#pAll <- pAll + scale_y_continuous(limits=c(0.1,0.35))
pAll <- pAll + myThemeMod + theme(legend.position = c(0.5, 0.3))
#pAll <- pAll + facet_grid(~condition)
pAll

##ggsave(filename=paste0(IMGDIR, 'divinnown.svg'))
gggsave('euclidean-abs-no-norm_smooth.png')


## By condition.
mydata <- melt(distances, measure.vars = c("d.nonorm.ownprev", "d.nonorm.pubprev",
                                    "d.nonorm.subcurr", "d.nonorm.subcurr.rm"),
               variable.name="distance")
mysummary <- summarySE(mydata, "value", c("round", "distance", "condition"),
                       na.rm = TRUE)

p <- ggplot(mysummary, aes(round, value, color=condition, group=distance))
p <- p + geom_point(size=2)
p <- p + geom_line(alpha=.4, size=1, aes(group=interaction(condition, distance)))
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci), width=0.2)
p <- p + ylab("NoNorm - Avg. Difference") + xlab("Round")
##p <- p + scale_x_continuous(breaks=c(2,4,6,8,10,12))
p <- p + myThemeMod + theme(legend.position = c(0.1, 0.838))
p <- p + facet_grid(~distance)
p


##ggsave(filename=paste0(IMGDIR, 'divinnown.svg'))
ggsave('euclidean-abs-no-norm_bycond.png', width=16)



## Euclidean NO NORM Geom.

pAll <- ggplot(distances, aes(x=round, group=1))
pAll <- pAll + geom_smooth(aes(y = dgeom.nonorm.subcurr.rm, color="NoNorm Geom Diversity Rm"))
pAll <- pAll + geom_smooth(aes(y = dgeom.nonorm.pubprev, linetype="NoNorm Geom Innovation"),
                           color="black")
pAll <- pAll + geom_smooth(aes(y = dgeom.nonorm.ownprev, linetype="NoNorm Geom Own Change"),
                           color="black")
pAll <- pAll + ylab("Avg. Difference") + xlab("Round")
#pAll <- pAll + scale_y_continuous(limits=c(0.1,0.35))
pAll <- pAll + myThemeMod + theme(legend.position = c(0.5, 0.3))
#pAll <- pAll + facet_grid(~condition)
pAll

##ggsave(filename=paste0(IMGDIR, 'divinnown.svg'))
ggsave('euclideansqrt-no-norm-geom_smooth.png')


## By condition.
mydata <- melt(distances, measure.vars = c("dgeom.nonorm.ownprev", "dgeom.nonorm.pubprev",
                                    "dgeom.nonorm.subcurr.rm"),
               variable.name="distance")
mysummary <- summarySE(mydata, "value", c("round", "distance", "condition"),
                       na.rm = TRUE)

p <- ggplot(mysummary, aes(round, value, color=condition, group=distance))
p <- p + geom_point(size=2)
p <- p + geom_line(alpha=.4, size=1, aes(group=interaction(condition, distance)))
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci), width=0.2)
p <- p + ylab("NoNorm - Avg. Difference") + xlab("Round")
##p <- p + scale_x_continuous(breaks=c(2,4,6,8,10,12))
p <- p + myThemeMod + theme(legend.position = c(0.1, 0.838))
p <- p + facet_grid(~distance)
p


##ggsave(filename=paste0(IMGDIR, 'divinnown.svg'))
ggsave('euclidean-abs-no-norm_bycond.png', width=16)



### DPOINTS.


pAll <- ggplot(distances, aes(x=round, group=1))
pAll <- pAll + geom_smooth(aes(y = dpoints.subcurr, linetype="Diversity"),
                           color="black", size=2)
pAll <- pAll + geom_smooth(aes(y = dpoints.subcurr.rm, linetype="Diversity Rm"),
                           color="black", size=2)
pAll <- pAll + geom_smooth(aes(y = dpoints.pubprev, linetype="Innovation"),
                           color="black", size=2)
pAll <- pAll + geom_smooth(aes(y = dpoints.ownprev, linetype="Personal Change"),
                           color="black",size=2)
pAll <- pAll + ylab("Avg. Difference") + xlab("Round")
#pAll <- pAll + scale_y_continuous(limits=c(0.1,0.35))
pAll <- pAll + myThemeMod + theme(legend.position = c(0.7, 0.45))
#pAll <- pAll + facet_grid(~condition)
pAll



## By condition.

mydata <- melt(distances, measure.vars = c("dpoints.subcurr.rm", "dpoints.geom.subcurr.rm",
                                    "d.subcurr", "d.subcurr.rm"),
               variable.name="distance")
mysummary <- summarySE(mydata, "value", c("round", "distance", "condition"),
                       na.rm = TRUE)

p <- ggplot(mysummary, aes(round, value, color=condition, group=distance))
p <- p + geom_point(size=2)
p <- p + geom_line(alpha=.4, size=1, aes(group=interaction(condition, distance)))
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci), width=0.2)
p <- p + ylab("Avg. Difference") + xlab("Round")
##p <- p + scale_x_continuous(breaks=c(2,4,6,8,10,12))
p <- p + myThemeMod + theme(legend.position = c(0.24, 0.838))
p <- p + facet_grid(~distance)
p




## DIV, INN, OWN AVERAGES ALL ROUNDS.
#####################################

#"dsqrt.ownprev", "dsqrt.pubprev", "dsqrt.subcurr.rm",
#"dgeom.sqrt.ownprev", "dgeom.sqrt.pubprev", "dgeom.sqrt.subcurr.rm",
#"d.nonorm.ownprev", "d.nonorm.pubprev", "d.nonorm.subcurr.rm",
#"dgeom.nonorm.ownprev", "dgeom.nonorm.pubprev", "dgeom.nonorm.subcurr.rm"



all.diversity.norm = c("d.subcurr.rm",
                       "dgeom.subcurr.rm",
                       "dsqrt.norm.subcurr.rm",
                       "dgeom.sqrt.norm.subcurr.rm")


all.diversity.nonorm = c("d.nonorm.subcurr.rm",
                         "dgeom.nonorm.subcurr.rm",
                         "dsqrt.subcurr.rm",
                         "dgeom.sqrt.subcurr.rm")

all.inn.norm = c("d.pubprev",
                 "dgeom.pubprev",
                 "dsqrt.norm.pubprev",
                 "dgeom.sqrt.norm.pubprev")

all.inn.nonorm = c("d.nonorm.pubprev",
                   "dgeom.nonorm.pubprev"
                   "dsqrt.pubprev",
                   "dgeom.sqrt.pubprev")


mydata <- melt(distances, measure.vars = c("d.subcurr.rm",
                                    "dgeom.subcurr.rm"), variable.name="distance")

mysummary <- summarySE(mydata, "value", c("distance", "condition"), na.rm = TRUE)
p <- ggplot(mysummary, aes(distance, value, color=condition))
p <- p + geom_point(position=position_dodge(width=0.9), size=4)
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci),
                       position="dodge")
p <- p + ylab("Avg. Distance") + xlab("")
p <- p + scale_x_discrete(labels=c("Div", "Geom"))
p <- p + myThemeMod + theme(legend.position = "none")
p
p1 <- p


mydata <- melt(distances, measure.vars = c("dsqrt.norm.subcurr.rm",
                                    "dgeom.sqrt.norm.subcurr.rm"), variable.name="distance")
mysummary <- summarySE(mydata, "value", c("distance", "condition"), na.rm = TRUE)
p <- ggplot(mysummary, aes(distance, value, color=condition))
p <- p + geom_point(position=position_dodge(width=0.9), size=4)
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci),
                       position="dodge")
p <- p + ylab("Avg. Distance") + xlab("")
p <- p + scale_x_discrete(labels=c("Sqrt", "Sqrt Geom"))
p <- p + myThemeMod + theme(legend.position = "none")
p
p2 <- p


mydata <- melt(distances, measure.vars = c("d.nonorm.subcurr.rm",
                                    "dgeom.nonorm.subcurr.rm"), variable.name="distance")

mysummary <- summarySE(mydata, "value", c("distance", "condition"), na.rm = TRUE)
p <- ggplot(mysummary, aes(distance, value, color=condition))
p <- p + geom_point(position=position_dodge(width=0.9), size=4)
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci),
                       position="dodge")
p <- p + ylab("Avg. Distance") + xlab("")
p <- p + scale_x_discrete(labels=c("No-Norm", "No-Norm Geom"))
p <- p + myThemeMod + theme(legend.position = "none")
p
p3 <- p

mydata <- melt(distances, measure.vars = c("dsqrt.subcurr.rm",
                                    "dgeom.sqrt.subcurr.rm"), variable.name="distance")
mysummary <- summarySE(mydata, "value", c("distance", "condition"), na.rm = TRUE)
p <- ggplot(mysummary, aes(distance, value, color=condition))
p <- p + geom_point(position=position_dodge(width=0.9), size=4)
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci),
                       position="dodge")
p <- p + ylab("Avg. Distance") + xlab("")
p <- p + scale_x_discrete(labels=c("No-Norm Sqrt", "No-Norm Sqrt Geom"))
p <- p + myThemeMod + theme(legend.position = "none")
p
p4 <- p


if (SAVEIMG) {
    png(filename=paste0(IMGDIR, 'dist_measures_means-diversity.png'), height=600, width=800)
}
multiplot(p1, p2, p3, p4, cols=2)
if (SAVEIMG) {
    dev.off()
}


mydata <- melt(distances, measure.vars = c("d.pubprev.rm",
                                    "dgeom.pubprev.rm"), variable.name="distance")

mysummary <- summarySE(mydata, "value", c("distance", "condition"), na.rm = TRUE)
p <- ggplot(mysummary, aes(distance, value, color=condition))
p <- p + geom_point(position=position_dodge(width=0.9), size=4)
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci),
                       position="dodge")
p <- p + ylab("Avg. Distance") + xlab("")
p <- p + scale_x_discrete(labels=c("Div", "Geom"))
p <- p + myThemeMod + theme(legend.position = "none")
p
p1 <- p


mydata <- melt(distances, measure.vars = c("dsqrt.norm.pubprev.rm",
                                    "dgeom.sqrt.norm.pubprev.rm"), variable.name="distance")
mysummary <- summarySE(mydata, "value", c("distance", "condition"), na.rm = TRUE)
p <- ggplot(mysummary, aes(distance, value, color=condition))
p <- p + geom_point(position=position_dodge(width=0.9), size=4)
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci),
                       position="dodge")
p <- p + ylab("Avg. Distance") + xlab("")
p <- p + scale_x_discrete(labels=c("Sqrt", "Sqrt Geom"))
p <- p + myThemeMod + theme(legend.position = "none")
p
p2 <- p


mydata <- melt(distances, measure.vars = c("d.nonorm.pubprev.rm",
                                    "dgeom.nonorm.pubprev.rm"), variable.name="distance")

mysummary <- summarySE(mydata, "value", c("distance", "condition"), na.rm = TRUE)
p <- ggplot(mysummary, aes(distance, value, color=condition))
p <- p + geom_point(position=position_dodge(width=0.9), size=4)
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci),
                       position="dodge")
p <- p + ylab("Avg. Distance") + xlab("")
p <- p + scale_x_discrete(labels=c("No-Norm", "No-Norm Geom"))
p <- p + myThemeMod + theme(legend.position = "none")
p
p3 <- p

mydata <- melt(distances, measure.vars = c("dsqrt.pubprev.rm",
                                    "dgeom.sqrt.pubprev.rm"), variable.name="distance")
mysummary <- summarySE(mydata, "value", c("distance", "condition"), na.rm = TRUE)
p <- ggplot(mysummary, aes(distance, value, color=condition))
p <- p + geom_point(position=position_dodge(width=0.9), size=4)
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci),
                       position="dodge")
p <- p + ylab("Avg. Distance") + xlab("")
p <- p + scale_x_discrete(labels=c("No-Norm Sqrt", "No-Norm Sqrt Geom"))
p <- p + myThemeMod + theme(legend.position = "none")
p
p4 <- p

if (SAVEIMG) {
    png(filename=paste0(IMGDIR, 'dist_measures_means-innovation.png'), height=600, width=800)
}
multiplot(p1, p2, p3, p4, cols=2)
if (SAVEIMG) {
    dev.off()
}


## Do we have significant differences?
######################################

mydata <- melt(distances, measure.vars = c("d.ownprev", "d.pubprev", "d.subcurr"),
               variable.name="distance")
mysummary <- summarySE(mydata, "value", c("distance", "condition"),
                       na.rm = TRUE)
p <- ggplot(mysummary, aes(distance, value, group=condition, ))
p <- p + geom_bar(aes(fill=condition), stat="identity", position="dodge")
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci),
                       position="dodge")
p <- p + ylab("Avg. Distance") + xlab("")
p <- p + scale_x_discrete(labels=c("Personal\nChange",
                                   "Innovation", "Diversity"))
p <- p + myThemeMod + theme(legend.position = "none")
p


## DGeom Significant
mydata <- melt(distances, measure.vars = c("dgeom.ownprev", "dgeom.pubprev", "dgeom.subcurr.rm"),
               variable.name="distance")
mysummary <- summarySE(mydata, "value", c("distance", "condition"),
                       na.rm = TRUE)
p <- ggplot(mysummary, aes(distance, value, group=condition, ))
p <- p + geom_bar(aes(fill=condition), stat="identity", position="dodge")
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci),
                       position="dodge")
p <- p + ylab("Avg. Distance") + xlab("")
p <- p + scale_x_discrete(labels=c("Personal\nChange",
                                   "Innovation", "Diversity"))
p <- p + myThemeMod + theme(legend.position = "none")
p


mydata <- melt(distances, measure.vars = c("d.ownprev", "d.pubprev",
                                    "d.subcurr", "d.subcurr.rm"),
               variable.name="distance")
mysummary <- summarySE(mydata, "value", c("distance", "condition"),
                       na.rm = TRUE)
p1 <- ggplot(mysummary, aes(distance, value, color=condition))
p1 <- p1 + geom_point(position=position_dodge(width=0.9), size=4)
p1 <- p1 + geom_errorbar(aes(ymax = value + ci, ymin = value - ci),
                       position="dodge")
str <- "t=-4.3399\np=1.506e-05"
p1 <- p1 + annotate("text", x=2, y=0.195, label=str, size=5)
str <- "t = -3.5155\np=0.0004"
p1 <- p1 + annotate("text", x=3, y=0.19, label=str, size=5)
str <- "t = -3.5155\np = 0.0004"
p1 <- p1 + annotate("text", x=4, y=0.21, label=str, size=5)
p1 <- p1 + ylab("Avg. Distance") + xlab("")
p1 <- p1 + scale_x_discrete(labels=c("Personal\nChange",
                                   "Innovation", "Diversity", "Diversity Rm"))
p1 <- p1 + myThemeMod + theme(legend.position = "none")
p1


## DGeom Significant
mydata <- melt(distances, measure.vars = c("dgeom.ownprev", "dgeom.pubprev", "dgeom.subcurr.rm"),
               variable.name="distance")
mysummary <- summarySE(mydata, "value", c("distance", "condition"),
                       na.rm = TRUE)
p2 <- ggplot(mysummary, aes(distance, value, color=condition))
p2 <- p2 + geom_point(position=position_dodge(width=0.9), size=4)
p2 <- p2 + geom_errorbar(aes(ymax = value + ci, ymin = value - ci),
                       position="dodge")
p2 <- p2 + ylab("Avg. Distance Geom") + xlab("")
p2 <- p2 + scale_x_discrete(labels=c("Personal\nChange",
                                   "Innovation", "Diversity Rm"))
p2 <- p2 + myThemeMod + theme(legend.position = "none")
str <- "t = -4.6113\np = 4.279e-06"
p2 <- p2 + annotate("text", x=2, y=0.17, label=str, size=5)
str <- "t = -3.1215\np = 0.0018"
p2 <- p2 + annotate("text", x=3, y=0.205, label=str, size=5)
p2


## Cosine not Significant.
mydata <- melt(distances, measure.vars = c("d.cos.ownprev", "d.cos.pubprev", "d.cos.subcurr"),
               variable.name="distance")
mysummary <- summarySE(mydata, "value", c("distance", "condition"),
                       na.rm = TRUE)

p3 <- ggplot(mysummary, aes(distance, value, group=condition, color=condition))
p3 <- p3 + geom_point(position=position_dodge(width=0.9), size=4)
p3 <- p3 + geom_errorbar(aes(ymax = value + ci, ymin = value - ci),
                       position="dodge")
p3 <- p3 + ylab("Avg. Cosine Distance") + xlab("")
p3 <- p3 + scale_x_discrete(labels=c("Personal\nChange",
                                   "Innovation", "Diversity"))
p3 <- p3 + myThemeMod + theme(legend.position = "none")
p3

if (SAVEIMG) {
    png(filename=paste0(IMGDIR, 'dist_measures_means.png'), height=600, width=1400)
}
multiplot(p1, p2, p3, cols=3)
if (SAVEIMG) {
    dev.off()
}

t.test(dgeom.pubprev ~ condition, data=pr)
t.test(dgeom.subcurr.rm ~ condition, data=pr)

t.test(d.pubprev ~ condition, data=pr)
t.test(d.subcurr ~ condition, data=pr)
t.test(d.subcurr.rm ~ condition, data=pr)


## Test
t.test(d.pubprev ~ condition, data=pr)
wilcox.test(d.pubprev ~ condition, data=pr)
## T test.
## t = -4.3399, df = 1779.7, p-value = 1.506e-05
## W = 392570, p-value = 3.772e-05

## Test
t.test(d.subcurr ~ condition, data=pr)
wilcox.test(d.subcurr ~ condition, data=pr)
## T test.
## t = -3.5155, df = 1945.8, p-value = 0.0004491
## W = 479980, p-value = 0.0008064

### PUBCUM

mydata <- melt(distances, measure.vars = c("d.pubcum"), variable.name="distance")
mysummary <- summarySE(mydata, "value", c("round", "distance", "condition"),
                       na.rm = TRUE)

p <- ggplot(mysummary, aes(round, value, color=condition, group=distance))
p <- p + geom_point(size=2)
p <- p + geom_line(alpha=.4, size=1, aes(group=interaction(condition, distance)))
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci), width=0.2)
p <- p + ylab("Avg. Difference") + xlab("Round")
##p <- p + scale_x_continuous(breaks=c(2,4,6,8,10,12))
p <- p + myThemeMod + theme(legend.position = c(0.24, 0.838))
p <- p + facet_grid(~distance)
p

mydata <- melt(distances, measure.vars = c("d.pubcum"), variable.name="distance")

mysummary <- summarySE(mydata, "value", c("distance", "condition"), na.rm = TRUE)
p <- ggplot(mysummary, aes(distance, value, color=condition))
p <- p + geom_point(position=position_dodge(width=0.9), size=4)
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci),
                       position="dodge")
p <- p + ylab("Avg. Distance") + xlab("")
p <- p + scale_x_discrete(labels=c("Div", "Geom"))
p <- p + myThemeMod + theme(legend.position = "none")
p


### INTRACLASS CORRELATION COEF ICC

library('psych')
library('irr')


pr <- ddply(distances, c("session", "round"), function(x){ x$pid = 1:nrow(x); x})
pr$pround <- paste0(pr$pid, "_", pr$round)


pr$pround <- paste0(pr$pid, "_", pr$round)


## Get negative values, do not make sense
## mydata.orig <- pr[, c("session", "pid", "d.subcurr")]
## mydata <- dcast(mydata.orig, pid ~ session, value.var="d.subcurr",
##                 fun.aggregate=mean)
## mydata[is.nan(mydata)] <- NA
## mydata <- mydata[,-1]
## ICC(mydata, missing=F)



mydata.orig <- pr[, c("session", "pround", "d.subcurr")]
mydata <- dcast(mydata.orig, pround ~ session, value.var="d.subcurr")

mydata[is.nan(mydata)] <- NA
mydata <- mydata[,-1]
ICC(mydata, missing=F)

## Intraclass correlation coefficients
##                          type   ICC   F df1  df2 p lower bound upper bound
## Single_raters_absolute   ICC1 0.085 2.8 107 1944 0       0.056        0.12
## Single_random_raters     ICC2 0.101 4.4 107 1926 0       0.067        0.15
## Single_fixed_raters      ICC3 0.151 4.4 107 1926 0       0.111        0.20
## Average_raters_absolute ICC1k 0.638 2.8 107 1944 0       0.531        0.73
## Average_random_raters   ICC2k 0.681 4.4 107 1926 0       0.578        0.77
## Average_fixed_raters    ICC3k 0.772 4.4 107 1926 0       0.704        0.83


mydata2 <- dcast(mydata.orig, session ~ pround, value.var="d.subcurr")
mydata2[is.nan(mydata2)] <- NA
mydata2 <- mydata2[,-1]
icc(mydata)

##  Single Score Intraclass Correlation
##
##    Model: oneway
##    Type : consistency
##
##    Subjects = 108
##      Raters = 19
##      ICC(1) = 0.085
##
##  F-Test, H0: r0 = 0 ; H1: r0 > 0
## F(107,1944) = 2.77 , p = 8.45e-18
##
##  95%-Confidence Interval for ICC Population Values:
##   0.056 < ICC < 0.125



mydata.orig <- pr[, c("session", "pround", "d.pubprev")]
mydata <- dcast(mydata.orig, pround ~ session, value.var="d.pubprev")

mydata[is.nan(mydata)] <- NA
mydata <- mydata[,-1]
ICC(mydata, missing=F)

## Intraclass correlation coefficients
##                          type   ICC   F df1  df2       p lower upper
## Single_raters_absolute   ICC1 0.050 2.0 107 1944 1.5e-08 0.028 0.082
## Single_random_raters     ICC2 0.066 2.9 107 1926 0.0e+00 0.042 0.100
## Single_fixed_raters      ICC3 0.091 2.9 107 1926 0.0e+00 0.061 0.132
## Average_raters_absolute ICC1k 0.501 2.0 107 1944 1.5e-08 0.353 0.628
## Average_random_raters   ICC2k 0.574 2.9 107 1926 0.0e+00 0.454 0.678
## Average_fixed_raters    ICC3k 0.654 2.9 107 1926 0.0e+00 0.552 0.742

mydata2 <- dcast(mydata.orig, session ~ pround, value.var="d.pubprev")
mydata2[is.nan(mydata2)] <- NA
mydata2 <- mydata2[,-1]
icc(mydata)

##  Single Score Intraclass Correlation
##
##    Model: oneway
##    Type : consistency
##
##    Subjects = 99
##      Raters = 19
##      ICC(1) = 0.0502
##
##  F-Test, H0: r0 = 0 ; H1: r0 > 0
##  F(98,1782) = 2 , p = 5.44e-08
##
##  95%-Confidence Interval for ICC Population Values:
##   0.027 < ICC < 0.083


mydata.orig <- pr[, c("player", "round", "d.subcurr")]
mydata <- dcast(mydata.orig, round ~ player, value.var="d.subcurr")
mydata[is.nan(mydata)] <- NA
mydata <- mydata[,-1]
ICC(mydata, missing=F)

## Intraclass correlation coefficients
##                          type  ICC  F df1  df2 p lower bound upper bound
## Single_raters_absolute   ICC1 0.12 25  11 2040 0       0.062        0.29
## Single_random_raters     ICC2 0.12 59  11 1870 0       0.065        0.29
## Single_fixed_raters      ICC3 0.25 59  11 1870 0       0.143        0.50
## Average_raters_absolute ICC1k 0.96 25  11 2040 0       0.919        0.99
## Average_random_raters   ICC2k 0.96 59  11 1870 0       0.922        0.99
## Average_fixed_raters    ICC3k 0.98 59  11 1870 0       0.966        0.99

mydata.orig <- pr[, c("player", "round", "d.pubprev")]
mydata <- dcast(mydata.orig, round ~ player, value.var="d.pubprev")
mydata[is.nan(mydata)] <- NA
mydata <- mydata[,-1]
ICC(mydata, missing=F)

## Intraclass correlation coefficients
##                          type   ICC  F df1  df2 p lower bound upper bound
## Single_raters_absolute   ICC1 0.082 16  11 2040 0       0.040        0.21
## Single_random_raters     ICC2 0.088 35  11 1870 0       0.044        0.22
## Single_fixed_raters      ICC3 0.167 35  11 1870 0       0.089        0.37
## Average_raters_absolute ICC1k 0.938 16  11 2040 0       0.877        0.98
## Average_random_raters   ICC2k 0.943 35  11 1870 0       0.888        0.98
## Average_fixed_raters    ICC3k 0.972 35  11 1870 0       0.943        0.99
