
source(paste0(THISDIR, 'quest/init_quest.R'))

## Load data.

data.full <- loadComments()
data.full <- data.full[data.full$duplicated == 0,]
## data.notfull <- loadComments('NOTFULL')
## data.all <- rbind(data.full, data.notfull)

data.full$session <- paste0('lgc', data.full$session)
data.good <- data.full[data.full$session %in% GOOD.SESSIONS,]


## Make summaries.
##################

# Good.
mysummary.good.all <- getMeltedDb(data.good)
mydata <- data.good[data.good$question %in% freeQuestions.optional,]
mysummary.good.opt <- getMeltedDb(mydata)

## Plots.
#########

p <- makeSentimentPlot(mysummary.good.all)
p

p <- makeSentimentPlot(mysummary.good.opt)
p <- p + coord_cartesian(ylim=c(-0.02,0.85))
p

## T Test.
###########

## not significant.
t.test(sent.pos ~ condition, data.good)

## not significant.
t.test(sent.neg ~ condition, data.good)

## not significant.
t.test(sent.neu ~ condition, data.good)

# not significant.
t.test(sent.com ~ condition, data.good)

## Only optional questions.
mydata <- data.good[data.good$question %in% freeQuestions.optional,]

# significant
t.test(sent.pos ~ condition, mydata)

## not significant.
t.test(sent.neg ~ condition, mydata)

# not significant.
t.test(sent.neu ~ condition, mydata)

# not significant.
t.test(sent.com ~ condition, mydata)

## Only compulsory questions.
mydata <- data.good[data.good$question %in% freeQuestions.compulsory,]

## not significant.
t.test(sent.pos ~ condition, mydata)

## not significant.
t.test(sent.neg ~ condition, mydata)

## not significant.
t.test(sent.neu ~ condition, mydata)

# not significant.
t.test(sent.com ~ condition, mydata)
