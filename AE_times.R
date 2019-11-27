## TIMES
########

mydata <- melt(p1pramt[p1pramt$step != "creation0",],
               measure.vars = c("time", "timeSubmission", "timeReview", "timeResults"),
               variable.name = "time")
mydata$value <- mydata$value / 1000

mydata$round <- as.factor(mydata$round)

mysummary <- summarySE(mydata, "value", c("condition", "round", "time"),
                       na.rm=TRUE)

p <- ggplot(mysummary, aes(round, value, color=condition, group=condition))
p <- p + geom_point(size=2)
p <- p + geom_line()
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci), width=0.2)
p <- p + ylab("Avg. Time") + xlab("Round") # + ylim(0,0.6)
p <- p + myThemeMod + theme(legend.position = c(0.1, -0.13))
p <- p + facet_wrap(~time, labeller=time_labeller, scale="free")
p <- p + guides(colour = guide_legend(nrow = 1))
p

gggsave(times_game.svg)

## There is no difference across treatments.
## Time generally decreases.


mysummary <- summarySE(mydata, "value", c("condition", "round", "time", "ex"), na.rm=TRUE)

p <- ggplot(mysummary, aes(round, value, shape=time, color=condition, group=condition))
p <- p + geom_point(size=2)
p <- p + geom_line()
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci), width=0.2)
p <- p + ylab("Avg. Time") + xlab("Round") # + ylim(0,0.6)
p <- p + myThemeMod #+ theme(legend.position = c(0.2, 0.95),
                    #        legend.title = element_text(vjust=3, size=24, face="bold"))
p <- p + facet_grid(time~ex)
p

mysummary <- summarySE(mydata, "value", c("condition", "round", "time", "ex"), na.rm=TRUE)

p <- ggplot(mysummary, aes(round, value, shape=ex, color=ex, group=ex))
p <- p + geom_point(size=2)
p <- p + geom_line()
p <- p + geom_errorbar(aes(ymax = value + ci, ymin = value - ci), width=0.2)
p <- p + ylab("Avg. Time") + xlab("Round") # + ylim(0,0.6)
p <- p + myThemeMod #+ theme(legend.position = c(0.2, 0.95),
                    #        legend.title = element_text(vjust=3, size=24, face="bold"))
p <- p + facet_grid(condition~time)
p

# There is no difference between conditions even when we look at the single exhibitions.
