
source(paste0(THISDIR, 'quest/init_quest.R'))

## Compulsory Questions.
########################

qc <- loadQuestCompulsory(MELTED=TRUE)
mydata <- qc[qc$session %in% GOOD.SESSIONS,]

labels <- c("Flat", "Stratified")

## How competitive it was.
makeQuestPlot("competitive", data=mydata, treatment=TRUE, labels=labels)
gggsave("quest_comp_competitive_full_sessions.png")

md <- mydata[mydata$question == "competitive",]
fit <- lmer(answer.num ~ condition + (1|session), data=md)
summary(fit)

## How much did you enjoy it.
makeQuestPlot("enjoy", data=mydata, treatment=TRUE, labels=labels)
gggsave("quest_comp_enjoy_full_sessions.png")

md <- mydata[mydata$question == "enjoy",]
fit <- lmer(answer.num ~ condition + (1|session), data=md)
summary(fit)

md <- mydata[mydata$question == "exbeau",]
table(md$answer, md$condition)

fit <- glmer(as.factor(answer) ~ condition + (1|session), data=md,
             family=binomial)
summary(fit)

md <- mydata[mydata$question == "exinn",]
table(md$answer, md$condition)

fit <- glmer(as.factor(answer) ~ condition + (1|session), data=md,
             family=binomial)
summary(fit)

md <- mydata[mydata$question == "exfair",]
table(md$answer, md$condition)

fit <- glmer(as.factor(answer) ~ condition + (1|session), data=md,
             family=binomial)
summary(fit)

## Where are the most beautiful images displayed.

## Absolute Counts.
title <- 'Most Beautiful Images'
p <- ggplot(mydata[mydata$question == 'exbeau',], aes(answer, fill=treatment))
p <- p + geom_bar(position="dodge", color="black",)
p <- p + scale_x_discrete(breaks=c("0","1","2","Other"),
                          labels=c("A","B","C","Don't know"))
p <- p + xlab('') + ylab('Counts') + ggtitle(title)
p <- p + theme(legend.position="none")
p <- p + coord_cartesian(ylim=c(-2,42))
p

gggsave("quest_comp_inn-beau_full_sessions.png")

## By session averages.
## mydata <- mydata[mydata$question == 'exbeau',]
## mytable <- table(mydata$session, mydata$answer)
## mytable <- as.data.frame.matrix(mytable)
## mytable$session <- row.names(mytable)
## mytable <- mytable[mytable$session %in% GOOD.SESSIONS,]
## mytable2 <- melt(mytable, id.vars="session",
##                  variable.name="ex", value.name="count")
## mysummary <- summarySE(mytable2, "count", c("ex"))

## Where are the most innovative images displayed.
title <- 'Most Innovative Images'
p <- ggplot(mydata[mydata$question == 'exinn',], aes(answer, fill=treatment))
p <- p + geom_bar(position="dodge", color="black",)
p <- p + scale_x_discrete(breaks=c("0","1","2","Other"),
                          labels=c("A","B","C","Don't know"))
p <- p + xlab('') + ylab('Counts') + ggtitle(title)
p <- p + theme(legend.position="none")
p <- p + coord_cartesian(ylim=c(-2,42))
p

gggsave("quest_comp_inn-inn_full_sessions.png")

## Where are the most fair reviews done.
aa <- mydata[mydata$question == 'exfair' & mydata$answer != "Other",]
## aa$answer <- factor(aa$answer, levels=c("2", "1", "0"))

title <- ''
p <- ggplot(aa, aes(answer, fill=condition))
p <- p + geom_bar(position="dodge", color="black")
p <- p + scale_x_discrete(breaks=c("0","1","2","Other"),
                          labels=c("A","B","C","Don't know"))
p <- p + xlab('') + ylab('Self-Reported Fairness of Reviews (counts)') + ggtitle(title)
## p <- p + coord_flip() ## coord_cartesian(ylim=c(-2,42))
p <- p + theme(axis.text=element_text(size=24))
p <- p + facet_grid(~condition)
p

gggsave("quest_comp_inn-fair_full_sessions.png")


# Optional Questions.
#####################

qo.all <- loadQuest(OPTIONAL=TRUE, MELTED=TRUE)
qo <- qo.all[qo.all$session %in% GOOD.SESSIONS,]

## Stats.

answersByPlayer <- summarySE(qo, "answer.num", c("player", "session", "condition"))
colnames(answersByPlayer) <-  c("player", "session", "condition", "Nq", "answer.num",
                                "sd", "sum", "se", "ci")
t.test(Nq ~ condition, answersByPlayer)

tmp <- summarySE(answersByPlayer, "Nq", c("condition"))

p <- ggplot(tmp, aes(condition, Nq, fill=condition))
p <- p + geom_bar(position="dodge", stat="identity", color="black", width=0.5)
p <- p + geom_errorbar(aes(ymin=Nq-ci, ymax=Nq+ci), position="dodge", width=0.5)
p <- p + xlab('') + ylab('Avg. Number of Optional Questions per Participant')
p <- p + theme(legend.position="none")
p

gggsave("quest_opt_nquestions.png")

## How many players on average take the optional questions?
aa <- answersByPlayer
aa$session <- as.character(aa$session)
aa <- table(aa$condition, aa$session)
aa <- as.data.frame(aa)
## At least one player per session took it (remove wrong session-rows).
aa <- aa[aa$Freq != 0,]
chisq.test(table(aa$Var1, aa$Freq))
summarySE(aa, "Freq", "Var1")

## How Do I Review?

## Must be ordered.
labels <- c("Liked\nmost",
            "High\ninnovative",
            "Low own\nexhibit",
            "Lower\nto A",
            "Lower\nto B",
            "Fit style\nexhibit",
            "Lower\nto C",
            "Stricter \nA>B>C")
p <- makeQuestPlot("review", labels=labels, treatment=TRUE, countsInTitle=TRUE)
p <- p + coord_cartesian(ylim=c(-0.2,9.5))
p


gggsave("quest_review.svg", width=12)

## How Do I Submit?

labels <- c("Reward",
            "Beautiful\nto A,B",
            "Innovative\nto A,B",
            "Fit style \nexhibit",
            "Random",
            "Innovative\nto B,C",
            "Popular\nexhibit",
            "Beautiful\nto B,C"
            )
## TODO: Check labels, they do not get applied in the correct order.
p <- makeQuestPlot("submission", labels=labels, treatment=TRUE,
                   countsInTitle=TRUE)
p <- p + coord_cartesian(ylim=c(-0.2,9.5))
p

gggsave("quest_submission.svg", width=12)

## What do I think of UI?

labels <- c("Wished to have\nmore sliders",
            "Immediately\neasy",
            "Learnt quickly\n how to use",
            "Wished to have\nless sliders"
            )
p <- makeQuestPlot("ui", labels=labels, treatment=TRUE,
                   title="UI", countsInTitle=TRUE)
p <- p + coord_cartesian(ylim=c(-0.2,9.5))
p

gggsave("quest_ui.svg", width=10)

## Were exhibition specialized?

labels <- c("No difference",
            "C was different",
            "A was different",
            "B was different"
            )
## TODO: Check labels, they do not get applied in the correct order.
p <- makeQuestPlot("specialization", labels=labels, treatment=TRUE,
                   countsInTitle=TRUE)
p <- p + coord_cartesian(ylim=c(-0.2,9.5))
p

gggsave("quest_exhibit.svg", width=10)

## How did I copy?

labels <- c("Seldom\n or never",
            "Liked and\nimprove style",
            "Old image\ncome back",
            "Save time",
            "Get similar\nto win",
            "Needed\nnew ideas"
            )
## TODO: Check labels, they do not get applied in the correct order.
p <- makeQuestPlot("copy", labels=labels, treatment=TRUE, countsInTitle=TRUE)
p <- p + coord_cartesian(ylim=c(-0.2,9.5))
p

gggsave("quest_copy.svg", width=10)

## How did I create?

labels <- c("Dissimilar\nprevious rounds",
            "Dissimilar\nin exhibit",
            "Random",
            "Similar\nin exhibit",
            "Similar\nprevious rounds"
            )
p <- makeQuestPlot("creation", treatment=TRUE,countsInTitle=TRUE)
p <- p + coord_cartesian(ylim=c(-0.2,9.5))
p

gggsave("quest_create.svg", width=10)


## Looking into the distribution of answers

## Submission/reward

## Stratified has many more 10s.
ggplot(qo[qo$question=="reward",], aes(answer.num, fill=condition)) +
  geom_density(alpha=0.5)

t.test(answer.num ~ condition, data=qo[qo$question=="reward",])

x <- qo[qo$question=="reward" & qo$condition == "Flat",]$answer.num
y <- qo[qo$question=="reward" & qo$condition == "Stratified",]$answer.num
ks.test(x,y)

# Not significantly different.

## Submission/reward

## It is very bi-modal 0/10, and it is even more for Flat.
ggplot(qo[qo$question=="byex",], aes(answer.num, fill=condition)) +
  geom_density(alpha=0.5)

t.test(answer.num ~ condition, data=qo[qo$question=="byex",])

x <- qo[qo$question=="byex" & qo$condition == "Flat",]$answer.num
y <- qo[qo$question=="byex" & qo$condition == "Stratified",]$answer.num
ks.test(x,y)


t.test(answer.num ~ condition, data=qo[qo$question=="qualityup",])

## Free comments of both compulsory and optional questions.

## Full sessions.
freetexts <- qc[qc$question == "freetext" & qc$answer != "",]
freetexts$len <- nchar(freetexts$answer)
freecomments <- qo[qo$question == "freecomment" & qo$answer != "",]
freecomments$len <- nchar(freecomments$answer)
comments <- rbind(freetexts, freecomments)
comments$nclicks <- NULL
comments$time.question <- NULL
comments$answer.num <- NULL
comments$question <- NULL

## Stats.

mydata <- freetexts[freetexts$len > 4 & freetexts$session %in% GOOD.SESSIONS,]
freetsummary <- summarySE(mydata, "len", "treatment")
freetsummary$group <- "Optional"
mydata <- freecomments[freecomments$len > 4 & freecomments$session %in% GOOD.SESSIONS,]
freecsummary <- summarySE(mydata, "len", "treatment")
freecsummary$group <- "Compulsory"
mydata <- comments[comments$len > 4 & comments$session %in% GOOD.SESSIONS,]
allsummary <- summarySE(mydata, "len", "treatment")
allsummary$group <- "All"
allsummary <- rbind(freecsummary, freetsummary, allsummary)

p <- ggplot(allsummary, aes(treatment, len, fill=treatment))
p <- p + geom_bar(position="dodge", stat="identity", color="black")
p <- p + geom_errorbar(aes(ymin=len-ci, ymax=len+ci), position="dodge")
p <- p + facet_grid(~group)
p <- p + scale_x_discrete(labels=c("Flat", "Stratified"))
p <- p + xlab('') + ylab('Avg. Length of Free Comments')
p <- p + theme(legend.position="none", strip.background = element_blank())
p

gggsave("quest_comp_inn-fair_full_sessions.png")


## Save.
mydata <- comments[comments$session %in% GOOD.SESSIONS,]
mydata$session <- splitSessionName(mydata)
mydata$treatment <- as.character(mydata$treatment)
mydata$treatment <- ifelse(mydata$treatment == "rank_same", "E", "M")
mydata$group <- as.character(mydata$group)
mydata$group <- ifelse(mydata$group == "competitive", "comp.", mydata$group)
mydata$group <- ifelse(mydata$group == "freecomment", "free", mydata$group)
mydata$group <- ifelse(mydata$group == "specialization", "exhibit", mydata$group)
mydata$group <- ifelse(mydata$group == "submission", "sub", mydata$group)
colnames(mydata) <- c("Player", "Q", "T", "Group", "Time", "Comment")

## write.csv(mydata, paste0(DIR, 'comments_full_sessions.csv'), row.names=FALSE)


## Not full sessions.
mydata <- comments[!(comments$session %in% GOOD.SESSIONS),]

## Save.
mydata$session <- splitSessionName(mydata)
mydata$group <- as.character(mydata$group)
mydata$treatment <- as.character(mydata$treatment)
mydata$treatment <- ifelse(mydata$treatment == "rank_same", "E", "M")
mydata$group <- ifelse(mydata$group == "competitive", "comp.", mydata$group)
mydata$group <- ifelse(mydata$group == "freecomment", "free", mydata$group)
mydata$group <- ifelse(mydata$group == "specialization", "exhibit", mydata$group)
mydata$group <- ifelse(mydata$group == "submission", "sub", mydata$group)
colnames(mydata) <- c("Player", "Q", "T", "Group", "Time", "Comment")

## write.csv(mydata, paste0(DIR, 'comments_not_full_sessions.csv'), row.names=FALSE)
