
source(paste0(THISDIR, 'quest/init_quest.R'))

## Must be ordered.
## alllabels <- c("Liked\nmost",
##             "High\ninnovative",
##             "Low own\nexhibit",
##             "Lower\nto A",
##             "Lower\nto B",
##             "Fit style\nexhibit",
##             "Lower\nto C",
##             "Stricter \nA>B>C",
##
##             "Reward",
##             "Beautiful\nto A,B",
##             "Innovative\nto A,B",
##             "Fit style \nexhibit",
##             "Random",
##             "Innovative\nto B,C",
##             "Popular\nexhibit",
##             "Beautiful\nto B,C",
##
##             "Wished to have\nmore sliders",
##             "Immediately\neasy",
##             "Learnt quickly\n how to use",
##             "Wished to have\nless sliders",
##
##             "No difference",
##             "C was different",
##             "A was different",
##             "B was different",
##
##             "Seldom\n or never",
##             "Liked and\nimprove style",
##             "Old image\ncome back",
##             "Save time",
##             "Get similar\nto win",
##             "Needed\nnew ideas",
##
##             "Dissimilar\nprevious rounds",
##             "Dissimilar\nin exhibit",
##             "Random",
##             "Similar\nin exhibit",
##             "Similar\nprevious rounds",


## TIMES

qo$question.unique <- paste0(qo$group, '.', qo$question)

mydata <- qo[qo$question != "freecomment",]
mydata$time.question <- mydata$time.question / 1000
mydata$time.group <- mydata$time.group / 1000
summary.time <- summarySE(mydata, "time.question", c("condition", "question.unique"))
p <- makeAllQuestionsTimePlot(summary.time, unique=TRUE)
saveOrDisplayIt(p, " quest_times_all_opt_questions", SAVEIMG, width=12)


qo$time.group.sec <- qo$time.group / 1000
summary.time <- summarySE(qo, "time.group.sec", c("condition", "group"))
summary.time$time.group <- summary.time$time.group.sec
p <- makeGroupQuestionsTimePlot(summary.time)
saveOrDisplayIt(p, "quest_times_group.svg", SAVEIMG, width=10)


summary.time <- summarySE(qo, "time.group.sec", c("condition"))
summary.time.all <- summary.time
summary.time.all$part <- "Optional"
summary.time$time.group <- summary.time$time.group.sec
makeQuestionsTimePlot(summary.time)

## Excludes freecomments (same as with freecomments).

summary.time <- summarySE(mydata, "time.group", c("condition"))
p <- makeQuestionsTimePlot(summary.time)
saveOrDisplayIt(p, "quest_times_opt_overall.svg", SAVEIMG)

# Stratified much faster, significant.
t.test(time.group ~ condition, data=qo)


## Compulsory.

qc <- loadQuestCompulsory(MELTED=TRUE)
qc <- qc[qc$session %in% FULL.SESSIONS,]
mydata <- qc[qc$question != "freetext",]
mydata$time.group <- mydata$time.group / 1000
mydata$time.question <- mydata$time.question / 1000

summary.time <- summarySE(mydata, "time.question", c("condition", "question"))
p <- makeAllQuestionsTimePlot(summary.time, angle=FALSE)
p <- p + scale_x_discrete(labels=c("Fair\nReviews", "Innovative\nExhibit", "Beautiful\nExhibit",
                            "Competitive", "Enjoy"))
saveOrDisplayIt(p, "quest_times_comp.svg", SAVEIMG)

summary.time <- summarySE(qc, "time.group", c("condition"))
summary.time$part <- "Compulsory"

p <- makeQuestionsTimePlot(summary.time)
saveOrDisplayIt(p, "quest_times_comp_overall.svg", SAVEIMG)

# Stratified much faster, significant.
t.test(time.group ~ condition, data=qc)

# Both in one plot.
summary.time.all <- rbind(summary.time.all, summary.time)
makeQuestionsTimePlot(summary.time.all, both=TRUE)
