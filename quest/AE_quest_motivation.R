
source(paste0(THISDIR, 'quest/init_quest.R'))

library(reshape)


qo$question.unique <- paste0(qo$group, '.', qo$question)

comments <- loadComments('ALL')
comments <- comments[comments$duplicated == 0,]
comments$session <- paste0('lgc', comments$session)
comments <- comments[comments$session %in% GOOD.SESSIONS,]

## How many optional questions answered and how fast.

summary.opt.pl <- summarySE(qo, "time.question", c("player"))

motivation <- summary.opt.pl[,c("player", "N", "sum")]
colnames(motivation) <- c("player", "n.opt.quest", "time.opt.quest")

## How fast in the compulsory questionnaire.

mydata <- qc[qc$question != "freetext",]
summary.comp.pl <- summarySE(mydata, "time.question", c("player"))
time.comp <- summary.comp.pl[,c("player", "sum")]
colnames(time.comp) <- c("player", "time.comp.quest")

motivation <- merge(motivation, time.comp, by=c("player"), all.y = TRUE)

## How much text wrote in comments.

summary.comm.pl <- summarySE(comments, "len", c("player", "opt"))

summary.comm.pl <- summarySE(comments, "len", c("condition"))

summary.comm.pl$ci <- NULL
summary.comm.pl$sd <- NULL
summary.comm.pl$len <- NULL
summary.comm.pl$se <- NULL
summary.comm.pl$N <- NULL

texts <- cast(summary.comm.pl, player ~ opt, mean)
colnames(texts) <- c("player", "nchar.comp", "nchar.opt")

texts[is.nan(texts)] <- NA

motivation <- merge(motivation, texts, by=c("player"), all.x=TRUE)

## What is the average sentiment in comments.

makeSentimentMotivation <- function(sentiment) {
  summary.comm.pl <- summarySE(comments, sentiment, c("player", "opt"))
  sent <- summary.comm.pl[,c("player", sentiment, "opt")]
  sent <- cast(sent, player ~ opt, mean, value=sentiment)
  colnames(sent) <- c("player", paste0(sentiment, ".avg.comp"),
                      paste0(sentiment, ".avg.opt"))
  sent[is.nan(sent)] <- NA
  motivation <- merge(motivation, sent, by=c("player"), all.x=TRUE)
  return(motivation)
}

motivation <- makeSentimentMotivation("sent.neg")
motivation <- makeSentimentMotivation("sent.pos")
motivation <- makeSentimentMotivation("sent.neu")
motivation <- makeSentimentMotivation("sent.com")


## Clean all NA. (keep them)
## motivation[is.na(motivation)] <- 0

## Save it!
write.csv(motivation, paste0(DIR, "quest_motivation_not_full.csv"), row.names=FALSE)


