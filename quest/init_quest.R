# init_quest

## QUEST

freeQuestions.compulsory <- c("comp.", "competitive", "enjoy", "exbeau",
                              "freetext", "exfair", "exinn")

freeQuestions.optional <- c("freecomment", "free", "ui", "creation", "review",
                            "submission", "sub", "copy", "exhibit",
                            "specialization")

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}

makeQuestPlot <- function(group=NA, treatment=FALSE, title=NA, labels=NA,
                          breaks=NA, countsInTitle=FALSE, data=qo) {

  ## Clear free comments.
  mydata <- data[data$question != "freecomment" &
                 data$question != "freetext",]
  ## Group.
  if (!is.na(group)) mydata <- mydata[mydata$group == group,]
  ## Treatment.
  if (treatment == TRUE) {
    mydata <- summarySE(mydata, "answer.num", c("question", "treatment"))
    mybars <- geom_bar(stat="identity", aes(fill=treatment), color="black",
                       position="dodge", size=1)
    myerrbars <- geom_errorbar(aes(ymin=answer.num-ci,ymax=answer.num+ci,
                                   fill=treatment),
                               position="dodge")
    treatments <- unique(mydata$treatment)
  } else {
    mydata <- summarySE(mydata, "answer.num", c("question"))
    mybars <- geom_bar(stat="identity", fill="lightblue", color="black", size=1)
    myerrbars <- geom_errorbar(aes(ymin=answer.num-ci,ymax=answer.num+ci),
                               width = 0.4)
  }
  ## Title.
  if (is.na(title)) mytitle <- simpleCap(group)
  else mytitle <- title
  if (countsInTitle == TRUE) {
    counts <- summarySE(mydata, "N", "treatment")
    mytitle <- paste0(mytitle, " (", counts$sum[1], ";", counts$sum[2], ")")
  }
  ## Plot.
  if (length(unique(mydata$question)) > 1) {
    treatmentX <- FALSE
    p <- ggplot(mydata, aes(reorder(question, -answer.num), y=answer.num))
  } else {
    treatmentX <- TRUE
    p <- ggplot(mydata, aes(treatment, answer.num))
    p <- p + theme(legend.position="none")
  }
  p <- p + mybars
  p <- p + myerrbars
  p <- p + xlab('') + ylab('Avg. Agreement 0-10') + ggtitle(mytitle)
  if (!any(is.na(labels))) {
    if (any(is.na(breaks))) {
      if (treatment == TRUE) {
        if (treatmentX == TRUE) {
          breaks <- unique(mydata$treatment)
        } else {
          mydata <- mydata[mydata$treatment == treatments[1],]
          breaks <- mydata[order(-mydata$answer.num),]$question
        }
      } else {
        breaks <- mydata[order(-mydata$answer.num),]$question
      }
    }
    p <- p + scale_x_discrete(breaks = breaks, labels=labels)
  }
  p <- p + scale_fill_discrete(labels=c("Flat", "Stratified"), name="")
  p
}

makeSentimentPlot <- function(data, var=NA) {
  if (is.na(var)) {
    p <- ggplot(data, aes(condition, value, fill=condition))
    p <- p + geom_bar(stat="identity", position="dodge", color="black")
    p <- p + geom_errorbar(aes(ymin=value-ci, ymax=value+ci),width=0.5)
    p <- p + facet_grid(~var)
    p <- p + ylab(paste0('Avg. sentiment'))

  } else {
    p <- ggplot(data, aes_string(x = "condition", y = var,fill="condition"))
    p <- p + geom_bar(stat="identity", position="dodge", color="black")
    p <- p + geom_errorbar(aes_string(ymin=paste0(var, "-ci"),
                                      ymax=paste0(var, "+ci")), width=0.5)
    p <- p + ylab(paste0('Avg. ', var, ' sentiment'))
  }
  p <- p + xlab('')
  p <- p + theme(legend.position="none", strip.background = element_blank())
  p
}

getMeltedDb <- function(data) {
  ##
  mysummary.pos <- summarySE(data, 'sent.pos', 'condition')
  mysummary.pos$value <- mysummary.pos$sent.pos
  mysummary.pos$var <- "Positive"
  mysummary.pos$sent.pos <- NULL
  ##
  mysummary.neg <- summarySE(data, 'sent.neg', 'condition')
  mysummary.neg$value <- mysummary.neg$sent.neg
  mysummary.neg$var <- "Negative"
  mysummary.neg$sent.neg <- NULL
  ##
  mysummary.neu <- summarySE(data, 'sent.neu', 'condition')
  mysummary.neu$value <- mysummary.neu$sent.neu
  mysummary.neu$var <- "Neutral"
  mysummary.neu$sent.neu <- NULL
  ##
  mysummary.compound <- summarySE(data, 'sent.com', 'condition')
  mysummary.compound$value <- mysummary.compound$sent.com
  mysummary.compound$var <- "Compound"
  mysummary.compound$sent.com <- NULL
  ## MERGE.

  mysummary.all <- rbind(mysummary.pos,mysummary.neg,
                         mysummary.neu, mysummary.compound)
  return(mysummary.all)
}


baseBarPlot <- function(p, var, angle) {
  p <- p + geom_bar(stat="identity", position="dodge", color="black")
  p <- p + geom_errorbar(aes_string(ymin=paste0(var, "-ci"),
                                    ymax=paste0(var, "+ci")),
                         position="dodge")
  p <- p + ylab(paste0('Avg. Time'))
  p <- p + xlab('')
  p <- p + theme(legend.position="none")
  if (angle == TRUE) {
    p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  }
  p
}

makeAllQuestionsClicksPlot <- function(data, unique=FALSE, angle=TRUE) {
  if (unique) {
    p <- ggplot(data, aes(reorder(question.unique, -nclicks),
                          y=nclicks, fill=condition))
  } else {
    p <- ggplot(data, aes(reorder(question, -nclicks),
                          y=nclicks, fill=condition))
  }
  baseBarPlot(p, "nclicks", angle)
}


makeAllQuestionsTimePlot <- function(data, unique=FALSE, angle=TRUE) {
  if (unique) {
    p <- ggplot(data, aes(reorder(question.unique, -time.question),
                          y=time.question, fill=condition))
  } else {
    p <- ggplot(data, aes(reorder(question, -time.question),
                          y=time.question, fill=condition))
  }
  baseBarPlot(p, "time.question", angle)
}


makeGroupQuestionsTimePlot <- function(data) {
  p <- ggplot(data, aes(reorder(group, -time.group),
                                y=time.group, fill=condition))
  p <- p + geom_bar(stat="identity", position="dodge", color="black")
  p <- p + geom_errorbar(aes(ymin=time.group-ci, ymax=time.group+ci),
                         position="dodge") ## width=0.5,
  p <- p + ylab(paste0('Avg. Time'))
  p <- p + xlab('')
  p <- p + theme(legend.position="none")
  p <- p + scale_x_discrete(labels=c("Free comment", "Creation", "UI", "Copy",
                              "Exhibit", "Submission", "Review"))
  p
}

makeQuestionsTimePlot <- function(data, both=FALSE) {
  if (both == TRUE) {
    p <- ggplot(data, aes(condition, y=time.group, fill=condition, group=part))
  } else {
    p <- ggplot(data, aes(condition, y=time.group, fill=condition))
  }
  p <- p + geom_bar(stat="identity", position="dodge", color="black")
  p <- p + geom_errorbar(aes(ymin=time.group-ci, ymax=time.group+ci),
                         position="dodge") ## width=0.5,
  p <- p + ylab(paste0('Avg. Time'))
  p <- p + xlab('')
  p <- p + theme(legend.position="none")
  p
}
