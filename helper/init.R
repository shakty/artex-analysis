# INIT.
library(ggplot2)
library(scales)
library(reshape2)
library(grid)
library(plyr)
library(data.table)
library(ineq)
library(texreg)
library(nnet)
library(Hmisc)
library(MASS)
library(stargazer)
library(corrplot)
library(Matching)
library(lme4)
library(nlme)
library(multcomp)
library(coin)
library(tseries)
library(e1071)
require(smooth)
require(Mcomp)

source(paste0(THISDIR, 'helper/init_plots.R'))
source(paste0(THISDIR, 'helper/corstarsl.R'))
source(paste0(THISDIR, 'helper/getVcovCL.R'))

## Save/display images in the right dir.
gggsave <- function(filename, ...) {
    ff <- paste0(IMGDIR, filename)
    if (SAVEIMG) {
        ggsave(filename=ff, ...)
    }
    print(ff)
}

ttexreg <- function(...) {
    stars = c(0.001, 0.01, 0.05, 0.1)
    symbol = '\\texttt{+}'
    texreg(stars=stars, symbol=symbol, ...)
}


## COLORS.

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

colors <- gg_color_hue(3)
red <- colors[1]
green <- colors[2]
blue <- colors[3]

gamePlusPracticeColors <- c(gg_color_hue(2), "grey60")


## SESSIONS

# 102 skipped.

INCOMPLETE.SESSIONS <- c('lgc100', 'lgc101', 'lgc104', 'lgc106', 'lgc114',
                         'lgc117', 'lgc123', 'lgc129', 'lgc132', 'lgc133',
                         'lgc135', 'lgc136', 'lgc139')

ALMOST.FULL.SESSIONS <-  c('lgc113', 'lgc115', 'lgc127', 'lgc128')
ALMOST.FULL.NOT.SMALL.SESSIONS <- ALMOST.FULL.SESSIONS

INCOMPLETE.NOT.SMALL.SESSIONS <- c('lgc100', 'lgc101', 'lgc104', 'lgc106',
                                   'lgc114','lgc117', 'lgc129', 'lgc132',
                                   'lgc133', 'lgc136')

FULL.SESSIONS <- c('lgc103', 'lgc105', 'lgc107', 'lgc108', 'lgc109',
                   'lgc110', 'lgc111', 'lgc112', 'lgc116', 'lgc118', 'lgc119',
                   'lgc120', 'lgc121', 'lgc122', 'lgc124', 'lgc125',
                   'lgc126', 'lgc131', 'lgc134', 'lgc137', 'lgc138')

FULL.NOT.SMALL.SESSIONS <- c('lgc103', 'lgc105', 'lgc107', 'lgc108', 'lgc109',
                             'lgc110', 'lgc112', 'lgc118', 'lgc119',
                             'lgc120', 'lgc121', 'lgc122', 'lgc124', 'lgc125',
                             'lgc126', 'lgc131', 'lgc134', 'lgc137', 'lgc138')
## Some aliases.
REGULAR.SESSIONS <- GOOD.SESSIONS <- FULL.NOT.SMALL.SESSIONS

WEIRD.SESSIONS <- c('lgc100', 'lgc101')

SMALL.SESSIONS <- c('lgc111', 'lgc116', 'lgc123', 'lgc130', 'lgc135', 'lgc140')

SMALL.COMPLETE.SESSIONS <- c('lgc111', 'lgc116', 'lgc130', 'lgc140')

# only rank_skew
SAME.TREATMENT.SESSIONS <- c('lgc131', 'lgc132', 'lgc133', 'lgc134','lgc135')

UNWANTED.SESSIONS <- c(WEIRD.SESSIONS, SMALL.SESSIONS)

ALL.SESSIONS <- c(INCOMPLETE.SESSIONS, ALMOST.FULL.SESSIONS, FULL.SESSIONS)

USABLE.SESSIONS <- ALL.SESSIONS[!(ALL.SESSIONS %in% UNWANTED.SESSIONS)]

## FONT for plots
theme_set(theme_bw(base_size = 20))

myThemeMod <- theme(axis.title.x = element_text(vjust=-1, size=24),
                    axis.title.y = element_text(vjust=-0.1, size=24),
                    plot.margin=unit(c(10,10,10,10),"mm"),
                    plot.title = element_text(vjust=3, size=24,face="bold"),
                    legend.background = element_rect(fill = "white", color="grey"),
                    legend.title = element_blank(),
                                        #legend.title = element_text(vjust=3, size=16, face="bold"),
                                        #legend.direction = "horizontal",
                    legend.text = element_text(size=16),
                    legend.key.width = unit(1.5, "cm"),
                    legend.key = element_rect(fill = "white", colour = "white")
                    )


niceCondition <- function(data, var.name="condition",
                          factor=TRUE, part1="Part1") {

    out <- ifelse(data[,var.name] == "rank_skew", "Stratified",
           ifelse(data[,var.name] == "rank_same", "Flat", part1))
  if (factor == TRUE) {
    out <- as.factor(out)
  }
  return(out)
}

is.nan.data.frame <- function(x) {
  do.call(cbind, lapply(x, is.nan))
}

## Mode element function.
getMode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This is does the summary; it's not easy to understand...
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun= function(xx, col, na.rm) {
                           c( N    = length2(xx[,col], na.rm=na.rm),
                              mean = mean   (xx[,col], na.rm=na.rm),
                              sd   = sd     (xx[,col], na.rm=na.rm),
                              sum  = sum    (xx[,col], na.rm=na.rm)
                              )
                          },
                    measurevar,
                    na.rm
             )

    # Rename the "mean" column
    datac <- plyr::rename(datac, c("mean"=measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval:
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}

loadDropouts <- function() {
  dropoutList <- read.csv(paste0(DIR, 'ae_all_dropouts_b.csv'))
  dropoutList$condition <- niceCondition(dropoutList, "treatment", part1="Practice")
  dropoutList$dropout <- 1
  first.dropouts <- getFirstDropouts(dropoutList)
  dropoutList$first.dropout <- ifelse(dropoutList$player %in% first.dropouts, 1, 0)

  return(dropoutList)
}

getFirstDropouts <- function(dropoutList) {

  ## Who disconnects first in the same session?
  DT <- as.data.table(dropoutList)

  ## Select all first dropouts (see comments below also).
  first.dropouts <- DT[ , .SD[which.min(round)], by = c("session")]$player
  first.dropouts <- c(as.character(first.dropouts), "ZUOCqClk")

  ##  DT[ , .SD[which.min(round)], by = c("player","session")]
  ##       player session round treatment       date condition dropout
  ##  1: 6z2J3c1Z  lgc100     4 rank_same 08_29_2016      Flat       1
  ##  2: BWpVHri2  lgc101     4 rank_skew 08_29_2016     Strat       1
  ##  3: AqPnJafL  lgc104     2 rank_skew 09_08_2016     Strat       1
  ##  4: bHwEcEWZ  lgc106     2 rank_skew 09_08_2016     Strat       1
  ##  5: ZUOCqClk  lgc106     2 rank_skew 09_08_2016     Strat       1
  ##  6: EpviMK1Y  lgc113    11 rank_skew 09_30_2016     Strat       1
  ##  7: b6Nz2Y9l  lgc114     4 rank_same 09_30_2016      Flat       1
  ##  8: XivoKPs9  lgc115    12 rank_skew 09_29_2016     Strat       1
  ##  9: 0iuHYqac  lgc117     2 rank_same 10_11_2016      Flat       1
  ## 10: jrCxuT8z  lgc117     3 rank_same 10_11_2016      Flat       1
  ## 11: vFt0yzUh  lgc117     5 rank_same 10_11_2016      Flat       1
  ## 12: 4bFFZmWn  lgc123     4 rank_same 10_11_2016      Flat       1
  ## 13: Gj3ihHJU  lgc127     8 rank_skew 10_19_2016     Strat       1
  ## 14: x3DGa4FG  lgc129     2 rank_skew 10_19_2016     Strat       1
  ## 15: U1duMgf2  lgc132     2 rank_skew 10_24_2016     Strat       1
  ## 16: a1l6syCM  lgc132     4 rank_skew 10_24_2016     Strat       1
  ## 17: fOkTIZwV  lgc133     2 rank_skew 10_24_2016     Strat       1
  ## 18: L2uHkc2O  lgc139     4 rank_skew 10_25_2016     Strat       1
  ## 19: d9nKW6bn  lgc139     9 rank_skew 10_25_2016     Strat       1
  ## 20: TDK1511i  lgc139    12 rank_skew 10_25_2016     Strat       1
  ## > DT[ , .SD[which.min(round)], by = c("session")]
  ##     session round   player treatment       date condition dropout
  ##  1:  lgc100     4 6z2J3c1Z rank_same 08_29_2016      Flat       1
  ##  2:  lgc101     4 BWpVHri2 rank_skew 08_29_2016     Strat       1
  ##  3:  lgc104     2 AqPnJafL rank_skew 09_08_2016     Strat       1
  ##  4:  lgc106     2 bHwEcEWZ rank_skew 09_08_2016     Strat       1
  ##  5:  lgc113    11 EpviMK1Y rank_skew 09_30_2016     Strat       1
  ##  6:  lgc114     4 b6Nz2Y9l rank_same 09_30_2016      Flat       1
  ##  7:  lgc115    12 XivoKPs9 rank_skew 09_29_2016     Strat       1
  ##  8:  lgc117     2 0iuHYqac rank_same 10_11_2016      Flat       1
  ##  9:  lgc123     4 4bFFZmWn rank_same 10_11_2016      Flat       1
  ## 10:  lgc127     8 Gj3ihHJU rank_skew 10_19_2016     Strat       1
  ## 11:  lgc129     2 x3DGa4FG rank_skew 10_19_2016     Strat       1
  ## 12:  lgc132     2 U1duMgf2 rank_skew 10_24_2016     Strat       1
  ## 13:  lgc133     2 fOkTIZwV rank_skew 10_24_2016     Strat       1
  ## 14:  lgc139     4 L2uHkc2O rank_skew 10_25_2016     Strat       1

  ## Only in session 106 two people disconnects at the same time, i.e.
  ## after round 1.

  ## 4: bHwEcEWZ  lgc106     2 rank_skew 09_08_2016     Strat       1
  ## 5: ZUOCqClk  lgc106     2 rank_skew 09_08_2016     Strat       1
    return(first.dropouts)
}

loadP1 <- function() {
  p1 <- read.csv(paste0(DIR, 'part1_15Nov.csv'))

  ## Remove published. Why is it there anyway?
  p1$published <- NULL

  ## Remove autoplay player (early session with problems).
  autoplay <- c(2310286974534,179404729278758,550490482244640,627798256231471)
  p1 <- p1[!(p1$player %in% autoplay),]

  # Remove redundant dfa.ownprev (same as d.ownprev).
  p1$dfa.ownprev <- NULL

  ## Quiz attempts.
  p1$quiz.attempts <- splitQuizAttempts(p1)

  ## Adjust belief prediction.
  ## (saved as 0-8, and it maps to positions 9-1; position 1 = max belief).
  p1$belief.choice <- p1$belief.choice + 1
  p1$willwin <- ifelse(p1$belief.choice == 9, 1, 0)
  p1$willlose <- ifelse(p1$belief.choice == 1, 1, 0)

  ## Order factor
  p1$job.choice <- factor(p1$job.choice,
                          levels = c("No", "1-2y", "1y",
                                     "3-5y", "5y+", "Do-not-say"))



  ## Compute SVO.
  p1$svo.own.mean <- apply(p1[,c("svo.1.choice.own", "svo.2.choice.own",
                                 "svo.3.choice.own", "svo.4.choice.own",
                                 "svo.5.choice.own", "svo.6.choice.own")],
                           1, mean)

  p1$svo.other.mean <- apply(p1[,c("svo.1.choice.other", "svo.2.choice.other",
                                   "svo.3.choice.other", "svo.4.choice.other",
                                   "svo.5.choice.other", "svo.6.choice.other")],
                             1, mean)
  p1$svo <- atan((p1$svo.other.mean - 50) / (p1$svo.own.mean - 50))
  ## atan returns radians, we want degree
  p1$svo.degree <- p1$svo * 57.2958
  p1$svo.group <- ifelse(p1$svo.degree < -12.04, "Competitive",
                  ifelse(p1$svo.degree < 22.45, "Individualistic",
                  ifelse(p1$svo.degree < 57.15, "Prosocial", "Altruist")))
  p1$svo.group2 <- ifelse(p1$svo.degree > 22.45, "Prosocial&Altruist", "Individualistic&Competitive")
  ##




  ## Compute Positive and Negative Mood.
  p1$mood.neg <- apply(p1[,c("Ashamed.choice", "Upset.choice",
                             "Hostile.choice", "Nervous.choice",
                             "Afraid.choice")] + 1
                       , 1, mean)
  p1$mood.pos <- apply(p1[,c("Active.choice", "Attentive.choice",
                             "Determined.choice", "Inspired.choice",
                             "Alert.choice")] + 1
                       , 1, mean)
  ##

  ## Age.
  p1$age.oldest <- ifelse(p1$age.choice == "61-70",1, 0)
  ## age.group
  g1 <- c("18-20", "21-30")
  g2 <- c("31-40")
  g3 <- c("41-50", "51-60", "61-70")
  p1$age.group <- ifelse(p1$age.choice %in% g1, "young",
                  ifelse(p1$age.choice %in% g2, "middle",
                  ifelse(p1$age.choice %in% g3, "old", "do-not-say")))
  p1$age.group <- factor(p1$age.group, levels=c("young", "middle", "old", "do-not-say"))


  p1$age.label <- ifelse(p1$age.group == "young", "Young\n(18-30)",
                  ifelse(p1$age.group == "middle", "Middle\n(31-40)",
                  ifelse(p1$age.group == "old", "Old\n(41-60)", "Do Not Say")))

  p1$age.label <- factor(p1$age.label, levels=c("Young\n(18-30)",
                                                "Middle\n(31-40)",
                                                "Old\n(41-60)",
                                                "Do Not Say"))

  p1$age.group2 <- ifelse(p1$age.choice %in% g1, "young",
                   ifelse(p1$age.choice %in% c(g2, g3), "old","do-not-say"))
  p1$age.group2 <- factor(p1$age.group2, levels=c("young", "old", "do-not-say"))

  p1$young <- ifelse(p1$age.group2 == "young", 1, 0)
  p1$old <- ifelse(p1$age.group2 == "old", 1, 0)

  p1$female <- ifelse(p1$gender.choice == "Female", 1, 0)
  p1$male <- ifelse(p1$gender.choice == "Male", 1, 0)

  p1$prosocial <- ifelse(p1$svo.group == "Prosocial", 1,
                  ifelse(p1$svo.group == "Individualistic", 0, NA))

  p1$individualistic <- ifelse(p1$svo.group == "Prosocial", 0,
                        ifelse(p1$svo.group == "Individualistic", 1, NA))

  p1$gender.choice <- factor(p1$gender.choice,
                             levels=c("Male", "Female", "Other", "Do-not-say"))

  p1$location.choice <- factor(p1$location.choice,
                               levels=c("US", "India", "Other", "Do-not-say"))

  # job.creativity.y
  p1$job.creativity.y <- lapply(p1$job.choice, function(v) {
      if (v == "No") return(0)
      if (v == "1y") return(1)
      if (v == "1-2y") return(1.5)
      if (v == "3-5y") return (4)
      if (v == "5y+") return(7)
      ## Do not say.
      return(2)
  })

  p1$job.creativity.y <- unlist(p1$job.creativity.y)

  ##Job creative or not.
  p1$job.creative <- ifelse(p1$job.choice %in% c("No", "Do-not-say"), 0, 1)

  p1$condition <- "Practice"

  ## Times.
  p1$time.training.sec <- p1$time.training / 1000

  ## Effort.
  ## Notice: Practice had 60 seconds, normal game 50.
  ## Normalize for time differences in round.

  ## Normalize by the actual time spent.
  mydata <- p1[,c(
      "cf.changes.nfeatures",
      "cf.changes.tot",
      "time.training.sec"
  )]
  p1$cf.changes.nfeatures.norm <- apply(mydata, 1, function(row) {
      return(row["cf.changes.nfeatures"]/row["time.training.sec"])
  });
  p1$cf.changes.tot.norm <- apply(mydata, 1, function(row) {
    return(row["cf.changes.tot"]/row["time.training.sec"])
  });

  ## Normalize by average time.
  avgTimePractice <- mean(p1$time.training.sec)
  p1$cf.changes.nfeatures.norm.avg <- p1$cf.changes.nfeatures / avgTimePractice
  p1$cf.changes.tot.norm.avg <- p1$cf.changes.tot  / avgTimePractice
  ## Normalize by relative nominal time.
  p1$cf.changes.nfeatures.norm.fix <- p1$cf.changes.nfeatures * (5/6)
  p1$cf.changes.tot.norm.fix <- p1$cf.changes.tot * (5/6)
  #
  return(p1)
}

getAMTScoresByPlayer <- function(data, up.to.round=NA, P1=TRUE) {
  if (P1 == FALSE) {
    data <- data[data$round != 0,]
  }
  if (!is.na(up.to.round)) {
    data <- data[data$round <= up.to.round,]
  }
  s <- summarySE(data, "score", c("creator", "dimension"), na.rm=TRUE)
  return(s)
}

loadAMTSetIds <- function(FILEPREFIX = '') {
    sets <- read.csv(paste0(DIR, FILEPREFIX, 'mturk_sets.csv'))
    return(sets)
}

loadAMTScores <- function(sessions="GOOD", R0R1=FALSE, P1 = TRUE,
                          FILEPREFIX = '', MELTED=FALSE) {

  ## If just R0R1, just do it!
  if (R0R1) {
    scores <- read.csv(paste0(DIR, FILEPREFIX, 'scores-r0r1.csv'))
    return(scores)
  }

  ## db2 contains session information.
  scores <- read.csv(paste0(DIR, FILEPREFIX, 'mturk_db2_mod-fixsession.csv'))

  ## Adjust times.

  ## NOTICE: between the last dimension scored in one image, and the first
  ## dimension in the next image there is an interval (pressing the submit
  ## button and the loading next image). That time is not captured currently.

  times <- c("overall.time", "abstract.time", "face.time", "creativity.time")

  ## Cumulative time **after** current image in current set.
  scores$time.set.cum <- apply(scores[, times], 1, max)

  DT <- as.data.table(scores)
  DT[, time.set.cum.lag := shift(time.set.cum, 1, 0, "lag"),
     by = list(player, set.id)]
  DT[, nsets := max(set.counter), by=player]

  scores <- as.data.frame(DT)

  scores$abstract.time.r <- ifelse(scores$reconnect == 1, scores$abstract.time,
                                 scores$abstract.time - scores$time.set.cum.lag)
  scores$face.time.r <- ifelse(scores$reconnect == 1, scores$face.time,
                                 scores$face.time - scores$time.set.cum.lag)
  scores$overall.time.r <- ifelse(scores$reconnect == 1, scores$overall.time,
                                 scores$overall.time - scores$time.set.cum.lag)
  scores$creativity.time.r <- ifelse(scores$reconnect == 1, scores$creativity.time,
                                 scores$creativity.time - scores$time.set.cum.lag)

  scores$time.set.cum.lag <- ifelse(scores$reconnect == 1, 0,
                                    scores$time.set.cum.lag)

  scores$time.image <- apply(scores[, paste0(times, ".r")], 1, max)

  if (sessions == "ALL" && !P1) {
    scores <- scores[scores$session != 'part1',]
  } else if (sessions != "ALL" && !P1) {
    scores <- scores[scores$session %in% get(paste0(sessions, ".SESSIONS")),]
  } else if (sessions != "ALL") {
    scores <- scores[scores$session %in% get(paste0(sessions, ".SESSIONS")) |
                     scores$session == 'part1',]
  }

  ## Make unique id.
  scores$iid <- paste0(scores$player, '_', scores$id, "_", scores$set.counter)

  scores$part1 <- ifelse(scores$treatment == "part1", 1, 0)
  scores$part1 <- factor(scores$part1, levels=c(1, 0))

  ## This is no longer needed with file mturk_db2_mod-fixsession.csv.
  ## Make the scores 1-7 instead of 0-6.
  ## scores$overall <- scores$overall + 1
  ## scores$face <- scores$face + 1
  ## scores$abstract <- scores$abstract + 1
  ## scores$creativity <- scores$creativity + 1

  ## For some unspecified reason player 1ztlCyU4 in group 04_11_2017
  ## rated the same set 3 times. It is the only case of the same person
  ## rating the same image (even across sets).

  ## Also player ThY9XAuU in group 05_02_2017 has duplicates. This might be due
  ## to the early restart of the server due the incorrect sets.
  scores <- scores[!duplicated(scores$iid),]

  if (length(unique(scores$iid)) != nrow(scores)) {
    stop(paste0('Duplicates!!! ', (nrow(scores) - length(unique(scores$iid)))))
  }
  if (MELTED) scores <- meltAMTScores(scores)
  ##
  scores$condition <- niceCondition(scores, "treatment", part1="Practice")
  ##
  scores$iid <- NULL
  ##
  scores$practice <- ifelse(scores$condition == "Practice", 1, 0)
  ##
  return(scores)
}

meltAMTScores <- function(scores) {
  iidExists <- "iid" %in% colnames(scores)
  if (!iidExists) {
    scores$iid <- paste0(scores$player, '_', scores$id, "_", scores$set.counter)
  }
  basenames <- c("group", "player", "date", "id", "iid", "treatment",
                 "creator", "round", "img.counter")

  scores.melted.choice <- reshape2::melt(scores,
                               variable.name = "dimension",
                               value.name = "score",
                               measure.vars=c("overall",
                                 "face",
                                 "creativity",
                                 "abstract"))

  ## Stupid melt does not work as it should...SOMETIMES.
  if ("value" %in% colnames(scores.melted.choice)) {
    scores.melted.choice$score <- scores.melted.choice$value
    scores.melted.choice$dimension <- scores.melted.choice$variable
    scores.melted.choice$variable <- NULL
    scores.melted.choice$value <- NULL
  }

  scores.melted.choice$overall.order <- NULL
  scores.melted.choice$overall.time <- NULL
  scores.melted.choice$overall.clicks <- NULL

  scores.melted.choice$face.order <- NULL
  scores.melted.choice$face.time <- NULL
  scores.melted.choice$face.clicks <- NULL

  scores.melted.choice$abstract.order <- NULL
  scores.melted.choice$abstract.time <- NULL
  scores.melted.choice$abstract.clicks <- NULL

  scores.melted.choice$creativity.order <- NULL
  scores.melted.choice$creativity.time <- NULL
  scores.melted.choice$creativity.clicks <- NULL

  ## time
  scores.melted.time <- reshape2::melt(scores,
                             variable.name = "dimension",
                             value.name = "time",
                             measure.vars=c("overall.time",
                               "face.time",
                               "creativity.time",
                               "abstract.time"))

  ## Stupid melt does not work as it should...SOMETIMES.
  if ("value" %in% colnames(scores.melted.time)) {
    scores.melted.time$time <- scores.melted.time$value
    scores.melted.time$dimension <- scores.melted.time$variable
    scores.melted.time$variable <- NULL
    scores.melted.time$value <- NULL
  }

  aa <- subset(scores.melted.time, select=c("iid", "dimension", "time"))
  aa$dimension <- gsub(".time", "", aa$dimension)
  scores.melted.choice <- merge(scores.melted.choice, aa, by=c("iid", "dimension"))

  ## order
  scores.melted.order <- reshape2::melt(scores,
                              variable.name = "dimension",
                              value.name = "order",
                              measure.vars=c("overall.order",
                                "face.order",
                                "creativity.order",
                                "abstract.order"))

  ## Stupid melt does not work as it should...SOMETIMES.
  if ("value" %in% colnames(scores.melted.order)) {
    scores.melted.order$order <- scores.melted.order$value
    scores.melted.order$dimension <- scores.melted.order$variable
    scores.melted.order$variable <- NULL
    scores.melted.order$value <- NULL
  }

  aa <- subset(scores.melted.order, select=c("iid", "dimension", "order"))
  aa$dimension <- gsub(".order", "", aa$dimension)
  scores.melted.choice <- merge(scores.melted.choice, aa, by=c("iid", "dimension"))

  ## clicks
  scores.melted.clicks <- reshape2::melt(scores,
                               variable.name = "dimension",
                               value.name = "clicks",
                               measure.vars=c("overall.clicks",
                                 "face.clicks",
                                 "creativity.clicks",
                                 "abstract.clicks"))

  ## Stupid melt does not work as it should...SOMETIMES.
  if ("value" %in% colnames(scores.melted.clicks)) {
    scores.melted.clicks$clicks <- scores.melted.clicks$value
    scores.melted.clicks$dimension <- scores.melted.clicks$variable
    scores.melted.clicks$variable <- NULL
    scores.melted.clicks$value <- NULL
  }

  aa <- subset(scores.melted.clicks, select=c("iid", "dimension", "clicks"))
  aa$dimension <- gsub(".clicks", "", aa$dimension)
  scores.melted.choice <- merge(scores.melted.choice, aa, by=c("iid", "dimension"))

  scores.melted <- scores.melted.choice

  # It was added in here.
  if (!iidExists) {
    scores.melted$iid <- NULL
  }

  return(scores.melted)
}

loadGame <- function(sessions="GOOD", file='ae_all_latest.csv') {
  ## Session 109, round 11, there are some NAs for reviews. Not
  ## sure why. Might be able to recover them somehow.
  ##> aa$player
  ##[1] XEJgwZKh ii4C7rrT XIC0Ij6l
  ##> aa$session
  ##[1] lgc109 lgc109 lgc109
  ##> aa$round
  ##[1] 11 11 11
  ##> aa$r3.id
  ##[1] t75CU5za XEJgwZKh XEJgwZKh

  pr <- read.csv(paste0(DIR, file))
  if (sessions != "ALL") {
      pr <- pr[pr$session %in% get(paste0(sessions, ".SESSIONS")),]
  }
  ## Nice condition.
  pr$condition <- niceCondition(pr, "treatment")

  ## Times.
  pr$time.sec <- pr$time / 1000

  ## Normalize Effort.
  mydata <- pr[,c("cf.changes.nfeatures", "cf.changes.tot", "time.sec")]
  pr$cf.changes.nfeatures.norm <- apply(mydata, 1, function(row) {
      return(row["cf.changes.nfeatures"]/row["time.sec"])
  });
  pr$cf.changes.tot.norm <- apply(mydata, 1, function(row) {
      return(row["cf.changes.tot"]/row["time.sec"])
  });
  avgTimeGame <- mean(pr$time.sec)

  ## It is the same in pr.
  pr$cf.changes.nfeatures.norm.fix <- pr$cf.changes.nfeatures
  pr$cf.changes.tot.norm.fix <- pr$cf.changes.tot

  pr$cf.changes.nfeatures.norm.avg <- pr$cf.changes.nfeatures / avgTimeGame
  pr$cf.changes.tot.norm.avg <- pr$cf.changes.tot  / avgTimeGame

  ## Fix same.ex (it is incorrectly saved)
  pr$r1.same.ex <- ifelse(pr$ex == pr$r1.ex, 1, 0)
  pr$r2.same.ex <- ifelse(pr$ex == pr$r2.ex, 1, 0)
  pr$r3.same.ex <- ifelse(pr$ex == pr$r3.ex, 1, 0)

  ## Copy Flag.
  pr$copy <- ifelse(is.na(pr$copy.ex), 0, 1)

##   ## Compute MAX diversity and innovation.
##   pr <- ddply(pr, c("session", "round"), function(x) {
##       mymax = max(x$d.subcurr)
##       x$is.max.div = ifelse(x$d.subcurr == mymax, 1, 0)
##       mymax = max(x$dgeom.subcurr.rm)
##       x$is.max.div.geom = ifelse(x$dgeom.subcurr.rm == mymax, 1, 0)
##       mymax = max(x$d.pubprev)
##       x$is.max.inn = ifelse(x$d.pubprev == mymax, 1, 0)
##       mymax = max(x$dgeom.pubprev)
##       x$is.max.inn.geom = ifelse(x$dgeom.pubprev == mymax, 1, 0)
##       x
##   })

##   ## Rank contributions by diversity and innovation.
##   pr <- ddply(pr, c("session", "round"), function(x) {
##       a <- x[order(x$d.subcurr),]
##       x$rank.d.subcurr <- match(x$player, a$player)
##       a <- x[order(x$dgeom.subcurr.rm),]
##       x$rank.dgeom.subcurr.rm <- match(x$player, a$player)
##       if (x$round[1] == 1) {
##           x$rank.d.pubprev <- NA
##           x$rank.dgeom.pubprev <- NA
##       } else {
##           a <- x[order(x$d.pubprev),]
##           x$rank.d.pubprev <- match(x$player, a$player)
##           a <- x[order(x$dgeom.pubprev),]
##           x$rank.dgeom.pubprev <- match(x$player, a$player)
##       }
##       x
##   })

  return(pr)
}

meltReviews <- function(data=pr) {
    mydata <- melt(data, measure.vars=c("r1","r2","r3"), variable.name="review",
                   variable_name="review")
    mydata$r.from <- mydata$value
    mydata$value <- NULL

    mydata$ex.review <- factor(ifelse(mydata$review == "r1", mydata$r1.ex,
                               ifelse(mydata$review == "r2", mydata$r2.ex,
                                      mydata$r3.ex)))
    levels(mydata$ex.review) <- list("A" = "1", "B" = "2", "C" = "3")
    mydata$r1.ex <- NULL
    mydata$r2.ex <- NULL
    mydata$r3.ex <- NULL


    mydata$ex.same <- ifelse(mydata$review == "r1", mydata$r1.same.ex,
                      ifelse(mydata$review == "r2", mydata$r2.same.ex,
                             mydata$r3.same.ex))
    mydata$r1.same.ex <- NULL
    mydata$r2.same.ex <- NULL
    mydata$r3.same.ex <- NULL

    mydata$reviewed.id <- ifelse(mydata$review == "r1", as.character(mydata$r1.id.from),
                          ifelse(mydata$review == "r2", as.character(mydata$r2.id.from),
                                 as.character(mydata$r3.id.from)))
    mydata$reviewed.id <- as.factor(mydata$reviewed.id)
    mydata$r1.id.from <- NULL
    mydata$r2.id.from <- NULL
    mydata$r3.id.from <- NULL

    mydata$review.changed <- ifelse(mydata$review == "r1", mydata$r1.changed,
                             ifelse(mydata$review == "r2", mydata$r2.changed,
                                    mydata$r3.changed))
    mydata$r1.changed <- NULL
    mydata$r2.changed <- NULL
    mydata$r3.changed <- NULL

    return(mydata)
}

meltReviews2 <- function(data=pr) {
    mydata <- melt(data, measure.vars=c("r1.from","r2.from","r3.from"), variable.name="review")
    mydata$r.from <- mydata$value
    mydata$value <- NULL

    mydata$ex.review <- factor(ifelse(mydata$review == "r1.from", mydata$r1.ex,
                               ifelse(mydata$review == "r2.from", mydata$r2.ex,
                                      mydata$r3.ex)))
    levels(mydata$ex.review) <- list("A" = "1", "B" = "2", "C" = "3")
    mydata$r1.ex <- NULL
    mydata$r2.ex <- NULL
    mydata$r3.ex <- NULL


    mydata$ex.same <- ifelse(mydata$review == "r1.from", mydata$r1.same.ex,
                      ifelse(mydata$review == "r2.from", mydata$r2.same.ex,
                             mydata$r3.same.ex))
    mydata$r1.same.ex <- NULL
    mydata$r2.same.ex <- NULL
    mydata$r3.same.ex <- NULL

    mydata$reviewer.id <- ifelse(mydata$review == "r1.from", as.character(mydata$r1.id.from),
                          ifelse(mydata$review == "r2.from", as.character(mydata$r2.id.from),
                                 as.character(mydata$r3.id.from)))
    mydata$reviewer.id <- as.factor(mydata$reviewer.id)
    mydata$r1.id.from <- NULL
    mydata$r2.id.from <- NULL
    mydata$r3.id.from <- NULL

    mydata$review.changed <- ifelse(mydata$review == "r1.from", mydata$r1.changed.from,
                             ifelse(mydata$review == "r2.from", mydata$r2.changed.from,
                                    mydata$r3.changed.from))
    mydata$r1.from.changed <- NULL
    mydata$r2.from.changed <- NULL
    mydata$r3.from.changed <- NULL

    return(mydata)
}


meltReviewsLast <- function(data=pr) {
    mydata <- melt(data, measure.vars=c("r1","r2","r3"), variable.name="review")
    mydata$review.value <- mydata$value
    mydata$value <- NULL

    mydata$ex.review <- factor(ifelse(mydata$review == "r1", mydata$r1.ex,
                               ifelse(mydata$review == "r2", mydata$r2.ex,
                                      mydata$r3.ex)))
    levels(mydata$ex.review) <- list("A" = "1", "B" = "2", "C" = "3")
    mydata$r1.ex <- NULL
    mydata$r2.ex <- NULL
    mydata$r3.ex <- NULL


    mydata$ex.same <- ifelse(mydata$review == "r1", mydata$r1.same.ex,
                      ifelse(mydata$review == "r2", mydata$r2.same.ex,
                             mydata$r3.same.ex))
    mydata$r1.same.ex <- NULL
    mydata$r2.same.ex <- NULL
    mydata$r3.same.ex <- NULL

    mydata$reviewed.id <- ifelse(mydata$review == "r1", as.character(mydata$r1.id.from),
                          ifelse(mydata$review == "r2", as.character(mydata$r2.id.from),
                                 as.character(mydata$r3.id.from)))
    mydata$reviewed.id <- as.factor(mydata$reviewed.id)
    mydata$r1.id.from <- NULL
    mydata$r2.id.from <- NULL
    mydata$r3.id.from <- NULL

    mydata$review.changed <- ifelse(mydata$review == "r1", mydata$r1.changed,
                             ifelse(mydata$review == "r2", mydata$r2.changed,
                                    mydata$r3.changed))
    mydata$r1.changed <- NULL
    mydata$r2.changed <- NULL
    mydata$r3.changed <- NULL

    mydata$review.changed <- ifelse(mydata$review == "r1", mydata$r1.changed,
                             ifelse(mydata$review == "r2", mydata$r2.changed,
                                    mydata$r3.changed))
    mydata$r1.changed <- NULL
    mydata$r2.changed <- NULL
    mydata$r3.changed <- NULL

    mydata$review.ass <- ifelse(mydata$review == "r1", mydata$r1.ass.kill,
                         ifelse(mydata$review == "r2", mydata$r2.ass.kill,
                                mydata$r3.ass.kill))
    mydata$r1.ass.kill <- NULL
    mydata$r2.ass.kill <- NULL
    mydata$r3.ass.kill <- NULL

    return(mydata)
}

splitQuizAttempts <- function(data) {
  tmp <- strsplit(as.character(data$quiz.rewards.attempts), "\\\\,")
  tmp2 <- lapply(tmp, function(x) {
    return(length(x))
  })
  tmp2 <- unlist(tmp2)
  return(tmp2)
}

splitSessionName <- function(data) {
  tmp <- strsplit(as.character(data$session), "lgc")
  tmp2 <- lapply(tmp, function(x) {
    return(x[2])
  })
  tmp2 <- unlist(tmp2)
  return(tmp2)
}

splitQuestionName <- function(data, KEEP.TEXT = TRUE) {
  tmp <- strsplit(data$question, "[.]")
  tmp2 <- lapply(tmp, function(x) {
    if (KEEP.TEXT & (x[2] == "freecomment" | x[2] == "freetext")) return(x[2])
    return(x[1])
  })
  tmp2 <- unlist(tmp2)
  return(tmp2)
}

loadQuest <- function(OPTIONAL=FALSE, MELTED=FALSE) {
  if (OPTIONAL) {
    q <- loadQuestOptional(MELTED)
  } else {
    q <- loadQuestCompulsory(MELTED)
  }
  return(q)
}

## Check duplicates problem as in Compulsory.
loadQuestOptional <- function(MELTED=FALSE) {
  q <- read.csv(paste0(DIR, 'ae_quest_o_all.csv'))
  ## Somehow there are some dublicates in optional. They are exactly the same,
  ## but some have do not have timestamp. Here we lose some timestamps,
  ## but in case we need them it is in the data.
  q$iid <- paste0(q$player, q$group)
  q <- q[!duplicated(q$iid),]
  q$round <- NULL
  q$step <- NULL
  q$iid <- NULL
  ## Reorder columns.
  vars <- c("time", "timeup", "timestamp",
            "group", "player", "treatment", "session")
  q2 <- q[,vars]
  q <- merge(q2, q, by=vars)
  if (MELTED) {
    q2 <- q
    q2$timeup <- NULL
    q2$timestamp <- NULL
    qo.melted <- melt(q2, id.vars=c("player", "treatment",
                            "session", "group", "time"))
    rm(q2)
    qo.melted <- qo.melted[complete.cases(qo.melted),]
    colnames(qo.melted) <- c("player", "treatment", "session", "group",
                             "time", "question", "answer")

    qo.melted$question <- as.character(qo.melted$question)

    choices.names <- unique(grep("choice", x = qo.melted$question,
                                 value=TRUE))
    comment.names <- unique(grep("freecomment", x = qo.melted$question,
                                 value=TRUE))
    choices.names <- c(choices.names, comment.names)

    choices <- qo.melted[qo.melted$question %in% choices.names,]
    choices$question <- splitQuestionName(choices)
    colnames(choices) <- c("player", "treatment", "session", "group",
                           "time.group", "question", "answer")

    nclicks <- qo.melted[grep("nClicks", x = qo.melted$question),]
    nclicks$treatment <- NULL
    nclicks$time <- NULL
    nclicks$session <- NULL
    colnames(nclicks) <- c("player", "group", "question", "nclicks")
    nclicks$question <- splitQuestionName(nclicks)

    times <- qo.melted[grep("time", x = qo.melted$question),]
    ## We need to drop savetime.choice.
    times <- times[times$question != "savetime.nClicks" &
                   times$question != "savetime.choice",]
    times$treatment <- NULL
    times$time <- NULL
    times$session <- NULL
    colnames(times) <- c("player", "group", "question", "time.question")
    times$question <- splitQuestionName(times)

    ## Order seems always NA. Need to check the data if we need it.

    choices.merged <- merge(choices, nclicks,
                            by=c("player", "group", "question"),
                            all.x = TRUE)
    choices.merged <- merge(choices.merged, times,
                            by=c("player", "group", "question"),
                            all.x = TRUE)

    q <- choices.merged[choices.merged$answer != "",]
    q$answer.num <- as.numeric(q$answer)

    ## Correct numeric time.
    q$time.question <- as.numeric(q$time.question)
    q$time.group <- as.numeric(q$time.group)
    ## Correct freecomment time.question.
    q$time.question <- ifelse(q$question == "freecomment",
                              q$time.group, q$time.question)
    ## Integer nclicks.
    q$nclicks <- as.integer(q$nclicks)
  }
  ## Nice condition.
  q$condition <- niceCondition(q, "treatment")
  return(q)
}

loadQuestCompulsory <- function(MELTED=FALSE) {
  q <- read.csv(paste0(DIR, 'ae_quest_c_all.csv'))
  ## We have duplicates.
  q <- q[!duplicated(q),]
  ## Player JLLhsGl2 said he/she wanted to select another option,
  ## but for technical reasons could not. Update here.
  q[q$player == "JLLhsGl2",]$competitive.choice = 10
  q[q$player == "JLLhsGl2",]$enjoy.choice = 10
  q[q$player == "JLLhsGl2",]$exbeau.choice = 0
  q[q$player == "JLLhsGl2",]$exfair.choice = 2
  ## Player kEgbMNbu put the values in the freetext.. Update here.
  q[q$player == "kEgbMNbu",]$competitive.choice = 5
  q[q$player == "kEgbMNbu",]$enjoy.choice = 7
  q[q$player == "kEgbMNbu",]$exbeau.choice = 1
  q[q$player == "kEgbMNbu",]$exfair.choice = 1
  q[q$player == "kEgbMNbu",]$exinn.choice = 2
  ##
  q$round <- NULL
  q$step <- NULL
  q$timeup <- NULL
  q$timestamp <- NULL
  ## Reorder columns.
  vars <- c("time", "player", "treatment", "session")
  q2 <- q[,vars]
  q <- merge(q2, q, by=vars)
  if (MELTED) {
    q2 <- q
    q.melted <- melt(q2, id.vars=c("player", "treatment", "session", "time"))
    rm(q2)
    q.melted <- q.melted[complete.cases(q.melted),]
    colnames(q.melted) <- c("player", "treatment", "session",
                               "time.group", "question", "answer")
    ## Add group.
    q.melted$question <- as.character(q.melted$question)
    q.melted$group <- splitQuestionName(q.melted, KEEP.TEXT = FALSE)

    choices.names <- unique(grep("choice", x = q.melted$question,
                                 value=TRUE))
    comment.names <- unique(grep("freetext", x = q.melted$question,
                                 value=TRUE))
    choices.names <- c(choices.names, comment.names)

    choices <- q.melted[q.melted$question %in% choices.names,]
    choices$question <- splitQuestionName(choices)

    nclicks <- q.melted[grep("nClicks", x = q.melted$question),]
    nclicks$treatment <- NULL
    nclicks$time.group <- NULL
    nclicks$session <- NULL
    colnames(nclicks) <- c("player", "question", "nclicks", "group")
    nclicks$question <- splitQuestionName(nclicks)

    times <- q.melted[grep("time", x = q.melted$question),]
    times$treatment <- NULL
    times$time.group <- NULL
    times$session <- NULL
    colnames(times) <- c("player", "question", "time.question", "group")
    times$question <- splitQuestionName(times)

    ## Order seems not to exist. Need to check the data if we need it.

    choices.merged <- merge(choices, nclicks,
                            by=c("player", "group", "question"),
                            all.x = TRUE)
    choices.merged <- merge(choices.merged, times,
                            by=c("player", "group", "question"),
                            all.x = TRUE)

    q <- choices.merged[choices.merged$answer != "",]
    q$answer.num <- as.numeric(q$answer)

    ## Duplicates are created here.
    q$iid <- paste0(q$player, q$group, q$question)
    q <- q[!duplicated(q$iid),]
    q$iid <- NULL
    ## Correct numeric time.
    q$time.question <- as.numeric(q$time.question)
    q$time.group <- as.numeric(q$time.group)
    ## Integer nclicks.
    q$nclicks <- as.integer(q$nclicks)
  }
  ## Nice condition.
  q$condition <- niceCondition(q, "treatment")
  return(q)
}

loadComments <- function(DATA='FULL') {
  if (DATA == "FULL") {
    data <- read.csv(paste0(DIR, 'comments_full_sessions_plus_vs.csv'))
  } else if (DATA == "NOTFULL") {
    data <- read.csv(paste0(DIR, 'comments_not_full_sessions_plus_vs.csv'))
  } else if (DATA == "ALL") {
    data <- rbind(read.csv(paste0(DIR, 'comments_full_sessions_plus_vs.csv')),
                  read.csv(paste0(DIR, 'comments_not_full_sessions_plus_vs.csv')))
  } else {
    stop(paste0('unknown value: ', DATA))
  }
  colnames(data) <- c("player", "question", "condition", "session", "time",
                      "sent.neg", "sent.pos", "sent.neu", "sent.com", "comment")
  data$condition <- ifelse(data$condition == "M", "Stratified", "Flat")
  data$comment <- as.character(data$comment)
  data$len <- nchar(data$comment)
  data$opt <- ifelse(data$question %in% freeQuestions.optional, 1, 0)
  data$opt <- as.factor(data$opt)
  ## dupl <- comments[duplicated(comments$comment),]$comment
  data$duplicated <- ifelse(duplicated(data$comment), 1, 0)
  return(data)
}

loadQuestMotivation <- function(DATA='FULL') {
  if (DATA == "FULL") {
    data <- read.csv(paste0(DIR, 'quest_motivation_full.csv'))
  } else if (DATA == "NOTFULL") {
    data <- read.csv(paste0(DIR, 'quest_motivation_not_full.csv'))
  } else if (DATA == "ALL") {
    data <- rbind(read.csv(paste0(DIR, 'quest_motivation_full.csv')),
                  read.csv(paste0(DIR, 'quest_motivation_not_full.csv')))
  } else {
    stop(paste0('unknown value: ', DATA))
  }
  return(data)
}

## quest.opt does not work
loadPlayersInfo <- function(motivation=TRUE, quest=FALSE, quest.opt=FALSE, all.rounds=FALSE, ex=FALSE, payoff=FALSE, sessions="ALL") {
  ## Part 1.
  p1 <- loadP1()
  ## R1 of game.
  pr <- loadGame(sessions=sessions)
  if (all.rounds == TRUE) {
    pr.r1 <- pr
  } else {
    pr.r1 <- pr[pr$round == 1,]
  }
  ## Payoff.
  if (payoff == TRUE) {
      payoffs <- summarySE(pr, "payoff", c("player"), na.rm=TRUE)
      colnames(payoffs) <- c("player", "rounds", "payoff.mean",
                             "payoff.sd", "payoff.sum", "payoff.se",
                             "payoff.ci")
      pr.r1 <- merge(pr.r1, payoffs, by=c("player"), all.ex=TRUE)
  }
  ## Exhibition counts.
  if (ex == TRUE) {
    exhibits <- table(pr$player, pr$ex)
    exhibits <- as.data.frame(exhibits)
    exhibits <- dcast(exhibits, Var1 ~ Var2)
    colnames(exhibits) <- c("player", "A", "B", "C")
    pr.r1 <- merge(pr.r1, exhibits, by=c("player"), all.x=TRUE)
  }
  ## Rename some columns.
  idx <- which(names(p1) == "cf.changes.tot")
  colnames(p1)[idx] <- "r0.cf.changes.tot"
  idx <- which(names(p1) == "cf.changes.nfeatures")
  colnames(p1)[idx] <- "r0.cf.changes.nfeatures"
  ## Remove condition in p1.
  p1$condition <- NULL
  ## We lose some players who did not start r1.
  p1pr.r1 <- merge(p1, pr.r1, by="player")
  ## Scores for practice and R1.
  scores.r0r1 <- loadAMTScores(sessions="ALL", R0R1=TRUE)
  p1pr.r1 <- merge(p1pr.r1, scores.r0r1, by="player")
  colnames(p1pr.r1)[names(p1pr.r1) == "condition.y"] <- "condition"
  p1pr.r1$condition.x <- NULL
  if (motivation == TRUE) {
    motivation <- loadQuestMotivation('ALL')
    p1pr.r1 <- merge(p1pr.r1, motivation, by=c("player"), all.x=TRUE)
  }
  if (quest == TRUE) {
    quest <- loadQuestCompulsory()
    quest$condition <- NULL
    quest$session <- NULL
    p1pr.r1 <- merge(p1pr.r1, quest, by=c("player"), all.x=TRUE)
  }
  ## Does not work!!
  if (quest.opt == TRUE) {
    quest <- loadQuestOptional()
    quest$condition <- NULL
    quest$session <- NULL
    p1pr.r1 <- merge(p1pr.r1, quest, by=c("player"), all.x=TRUE)
  }
  return(p1pr.r1)
}

mergeGameP1AMT <- function() {
    library(reshape)
    prp1 <- mergeGameP1()
    prp1$pround <- paste0(pr$player, pr$round)
    scores.melted$pround <- paste0(scores.melted$creator, scores.melted$round)
    scores.avg <- summarySE(scores.melted, "score", c("pround", "dimension"))
    scores.avg.casted <- cast(scores.avg, pround ~dimension, value="score")
    prp1scores <- merge(prp1, scores.avg.casted, by=c("pround"), all.x=TRUE)
    return(prp1scores)
}


addData2ScoresMelted <- function(myscores.melted) {
    library(reshape)
    prp1 <- mergeGameP1()
    prp1$pround <- paste0(pr$player, pr$round)
    myscores.melted <- myscores.melted[myscores.melted$condition != "Practice",]
    myscores.melted$pround <- paste0(myscores.melted$creator,
                                     myscores.melted$round)
    myscores.melted$reviewer <- myscores.melted$player
    myscores.melted$player <- NULL
    prp1scores <- merge(myscores.melted, prp1, by=c("pround"),
                        all.x=TRUE)
    return(prp1scores)
}

## Lagging variables.

renameAfterMergeP1 <- function(prp1, varname, deleteP1=FALSE) {
    idx <- which(names(prp1)==paste0(varname, ".x"))
    colnames(prp1)[idx] <- varname
    if (deleteP1 == FALSE) {
        idx <- which(names(prp1)==paste0(varname, ".y"))
        colnames(prp1)[idx] <- paste0("p1.", varname)
    } else {
        prp1[,paste0(varname, ".y")] <- NULL
    }
    return(prp1)
}

renameCol <- function(data, colname, newname=NA, prefix="", suffix="") {
    idx <- which(names(data) == colname)
    if (missing(newname) && missing(prefix) && missing(suffix)) {
        stop("Missing parameters")
    }
    if (missing(newname)) {
        newname <- colname
    }
    newname <- paste0(prefix, newname, suffix)
    colnames(data)[idx] <- newname
    data[,colname] <- NULL
    return(data)
}

mergeGameP1 <- function() {

    ## Data.Table mode.
    DT <- as.data.table(pr)

    ## Adding cumulative stuff.
    DT[, payoff.cum := cumsum(payoff), by = list(session, player)]
    DT[, published.cum := cumsum(published), by = list(session, player)]

    ## Add lags.
    cols <- paste0("d.pubprev.lag.", seq(1:12))
    DT[, (cols) := shift(d.pubprev, 1:12, NA, "lag"),
       by = list(session, player)]
    #
    cols <- paste0("d.ownprev.lag.", seq(1:12))
    DT[, (cols) := shift(d.ownprev, 1:12, NA, "lag"),
       by = list(session, player)]
    #
    cols <- paste0("d.subcurr.lag.", seq(1:12))
    DT[, (cols) := shift(d.subcurr, 1:12, NA, "lag"),
       by = list(session, player)]
    #
    cols <- paste0("published.lag.", seq(1:12))
    DT[, (cols) := shift(published, 1:12, NA, "lag"),
       by = list(session, player)]
    #
    cols <- paste0("payoff.lag.", seq(1:12))
    DT[, (cols) := shift(payoff, 1:12, NA, "lag"),
       by = list(session, player)]
    #
    cols <- paste0("ex.lag.", seq(1:12))
    DT[, (cols) := shift(ex, 1:12, NA, "lag"),
       by = list(session, player)]
    #
    cols <- paste0("r.mean.from.lag.", seq(1:12))
    DT[, (cols) := shift(r.mean.from, 1:12, NA, "lag"),
       by = list(session, player)]
    #
    cols <- paste0("r.mean.lag.", seq(1:12))
    DT[, (cols) := shift(r.mean, 1:12, NA, "lag"),
       by = list(session, player)]
    #
    cols <- paste0("rank.ex.lag.", seq(1:12))
    DT[, (cols) := shift(rank.ex, 1:12, NA, "lag"),
       by = list(session, player)]
    #
    cols <- paste0("rank.global.lag.", seq(1:12))
    DT[, (cols) := shift(rank.global, 1:12, NA, "lag"),
       by = list(session, player)]
    #
    cols <- paste0("cf.changes.tot.lag.", seq(1:12))
    DT[, (cols) := shift(cf.changes.tot, 1:12, NA, "lag"),
       by = list(session, player)]
    #
    cols <- paste0("cf.changes.tot.norm.lag.", seq(1:12))
    DT[, (cols) := shift(cf.changes.tot.norm, 1:12, NA, "lag"),
       by = list(session, player)]
    #
    cols <- paste0("cf.changes.nfeatures.lag.", seq(1:12))
    DT[, (cols) := shift(cf.changes.nfeatures, 1:12, NA, "lag"),
       by = list(session, player)]
    #
    cols <- paste0("cf.changes.nfeatures.norm.lag.", seq(1:12))
    DT[, (cols) := shift(cf.changes.nfeatures.norm, 1:12, NA, "lag"),
       by = list(session, player)]
    #
    # Lags of cum variables.
    DT[, published.cum.lag.1 := shift(published.cum, 1, 0, "lag"),
       by = list(session, player)]
    #
    DT[, payoff.cum.lag.1 := shift(payoff.cum, 1, 0, "lag"),
       by = list(session, player)]
    #
    pr <- as.data.frame(DT)

    ## PDATA mode.
    ## library(plm)
    ## ppr <- pdata.frame(pr, index=c('player','round'))
    ## ppr$r.mean.lag <- lag(ppr$r.mean, 1)
    ## ppr$published.lag <- lag(ppr$published, 1)
    ## ppr$ex.lag <- lag(ppr$ex, 1)
    ## ppr$ex.stay <- (ppr$ex == ppr$ex.lag) * 1
    ## ppr$payoff.lag <- lag(ppr$payoff)
    ## ppr$d.subcurr.lag <- lag(ppr$d.subcurr)
    ## ppr$d.ownprev.lag <- lag(ppr$d.ownprev)
    ## ppr$d.pubprev.lag <- lag(ppr$d.pubprev)
    ## pr <- as.data.frame(ppr)

    prp1 <- merge(pr, p1, by="player", all.x=TRUE)

    prp1 <- renameAfterMergeP1(prp1, "dfa.subcurr")
    prp1 <- renameAfterMergeP1(prp1, "d.subcurr.rm")

    prp1 <- renameAfterMergeP1(prp1, "d.subcurr")
    prp1 <- renameAfterMergeP1(prp1, "d.pubprev")
    prp1 <- renameAfterMergeP1(prp1, "d.ownprev")
    prp1 <- renameAfterMergeP1(prp1, "condition", deleteP1=TRUE)

    prp1 <- renameAfterMergeP1(prp1, "cf.changes.tot")
    prp1 <- renameAfterMergeP1(prp1, "cf.changes.tot.norm")
    prp1 <- renameAfterMergeP1(prp1, "cf.changes.tot.norm.fix")
    prp1 <- renameAfterMergeP1(prp1, "cf.changes.tot.norm.avg")
    prp1 <- renameAfterMergeP1(prp1, "cf.changes.nfeatures")
    prp1 <- renameAfterMergeP1(prp1, "cf.changes.nfeatures.norm")
    prp1 <- renameAfterMergeP1(prp1, "cf.changes.nfeatures.norm.avg")
    prp1 <- renameAfterMergeP1(prp1, "cf.changes.nfeatures.norm.fix")

    prp1 <- renameAfterMergeP1(prp1, "timestamp")

    prp1 <- renameAfterMergeP1(prp1, "cf.head_radius")
    prp1 <- renameAfterMergeP1(prp1, "cf.head_scale_x")
    prp1 <- renameAfterMergeP1(prp1, "cf.head_scale_y")
    prp1 <- renameAfterMergeP1(prp1, "cf.eye_height")
    prp1 <- renameAfterMergeP1(prp1, "cf.eye_spacing")
    prp1 <- renameAfterMergeP1(prp1, "cf.eye_scale_x")
    prp1 <- renameAfterMergeP1(prp1, "cf.eye_scale_y")
    prp1 <- renameAfterMergeP1(prp1, "cf.eyebrow_length")
    prp1 <- renameAfterMergeP1(prp1, "cf.eyebrow_angle")
    prp1 <- renameAfterMergeP1(prp1, "cf.eyebrow_eyedistance")
    prp1 <- renameAfterMergeP1(prp1, "cf.eyebrow_spacing")
    prp1 <- renameAfterMergeP1(prp1, "cf.mouth_top_y")
    prp1 <- renameAfterMergeP1(prp1, "cf.mouth_bottom_y")


    prp1 <- renameCol(prp1, "cf0.head_radius", prefix="p1.")
    prp1 <- renameCol(prp1, "cf0.head_scale_x", prefix="p1.")
    prp1 <- renameCol(prp1, "cf0.head_scale_y", prefix="p1.")
    prp1 <- renameCol(prp1, "cf0.eye_height", prefix="p1.")
    prp1 <- renameCol(prp1, "cf0.eye_spacing", prefix="p1.")
    prp1 <- renameCol(prp1, "cf0.eye_scale_x", prefix="p1.")
    prp1 <- renameCol(prp1, "cf0.eye_scale_y", prefix="p1.")
    prp1 <- renameCol(prp1, "cf0.eyebrow_length", prefix="p1.")
    prp1 <- renameCol(prp1, "cf0.eyebrow_angle", prefix="p1.")
    prp1 <- renameCol(prp1, "cf0.eyebrow_eyedistance", prefix="p1.")
    prp1 <- renameCol(prp1, "cf0.eyebrow_spacing", prefix="p1.")
    prp1 <- renameCol(prp1, "cf0.mouth_top_y", prefix="p1.")
    prp1 <- renameCol(prp1, "cf0.mouth_bottom_y", prefix="p1.")

    ## c(10,11,3,5,9,12,2,1,7,8,4)
    ## prp1$round <- factor(prp1$round,  1:12)

    return(prp1)
}

## Names.

names.cf0s <- c("cf0.head_radius", "cf0.head_scale_x", "cf0.head_scale_y",
                "cf0.eye_height", "cf0.eye_spacing", "cf0.eye_scale_x",
                "cf0.eye_scale_y", "cf0.eyebrow_length", "cf0.eyebrow_angle",
                "cf0.eyebrow_eyedistance", "cf0.eyebrow_spacing",
                "cf0.mouth_top_y", "cf0.mouth_bottom_y")

names.demo <- c("age.choice", "job.choice",
                "gender.choice", "location.choice", "job.creative")

names.demo.times <- c("age.time", "job.time",
                     "gender.time", "location.time")

names.demo.clicks <- c("age.nClicks", "job.nClicks",
                       "gender.nClicks", "location.nClicks")

names.instr.times <- c("time.instr", "time.instr.step2")

names.mood <- c("mood.neg", "mood.pos")

names.mood.choices <- c("Upset.choice", "Hostile.choice", "Alert.choice",
                        "Ashamed.choice", "Inspired.choice", "Nervous.choice",
                        "Determined.choice", "Attentive.choice",
                        "Afraid.choice", "Active.choice")

names.mood.times <- c("Upset.time", "Hostile.time", "Alert.time",
                        "Ashamed.time", "Inspired.time", "Nervous.time",
                        "Determined.time", "Attentive.time",
                        "Afraid.time", "Active.time")

names.mood.nclicks <- c("Upset.nclicks", "Hostile.nclicks", "Alert.nclicks",
                        "Ashamed.nclicks", "Inspired.nclicks",
                        "Nervous.nclicks", "Determined.nclicks",
                        "Attentive.nclicks", "Afraid.nclicks", "Active.nclicks")

names.quiz <- c("quiz.attempts")

names.p1.times.only <- c("time.intro", "time.mood",
                         "time.svo" , "time.demo" , "time.instr" ,
                         "time.instr.step2" , "time.quiz" ,
                         "time.training_intro" , "time.training" ,
                         "time.belief")

names.belief <- c("belief.choice", "belief.nClicks")

names.svo <- c("svo.own.mean", "svo.other.mean", "svo")

names.motivation <- c("n.opt.quest", "time.opt.quest", "time.comp.quest",
                      "nchar.comp", "nchar.opt")

names.quest.sent <- c("sent.neg.avg.comp", "sent.neg.avg.opt",
                      "sent.pos.avg.comp", "sent.pos.avg.opt",
                      "sent.neu.avg.comp", "sent.neu.avg.opt",
                      "sent.com.avg.comp", "sent.com.avg.opt")

names.scores.r0 <- c("r0.overall", "r0.overall.sd", "r0.creativity",
                     "r0.creativity.sd", "r0.abstract", "r0.abstract.sd",
                     "r0.face", "r0.face.sd")

names.scores.r1 <- c("r1.overall", "r1.overall.sd", "r1.creativity",
                     "r1.creativity.sd", "r1.abstract", "r1.abstract.sd",
                     "r1.face", "r1.face.sd")

names.scores.all <- c(names.scores.r0, names.scores.r1)

names.scores.all.nosd <- c("r0.overall", "r0.creativity",
                           "r0.abstract", "r0.face",
                           "r1.overall", "r1.creativity",
                           "r1.abstract", "r1.face")

names.cf.changes <- c("r0.cf.changes.tot", "r0.cf.changes.nfeatures",
                      "cf.changes.tot", "cf.changes.nfeatures")

names.p1 <- c(names.mood, "belief.choice", names.svo, names.demo, names.quiz)

names.p1.times <- c(names.p1, names.p1.times.only)

names.p1.times.cfc <- c(names.p1.times, names.cf.changes)

names.p1.times.cfc.scores <- c(names.p1.times.cfc, names.scores.all)

names.p1.times.cfc.scores.mot <- c(names.p1.times.cfc.scores, names.motivation)

names.p1.times.cfc.scores.mot.sent <-
    c(names.p1.times.cfc.scores.mot, names.quest.sent)


makeUpToDropout <- function(df, SESSIONS=USABLE.SESSIONS, DROPOUT.ONLY=FALSE) {
    if (!exists("dropoutList")) {
        dropoutList <- loadDropouts()
    }
    if (!exists("first.dropouts")) {
        first.dropouts <- getFirstDropouts(dropoutList)
    }
    dropoutList <- dropoutList[dropoutList$session %in% SESSIONS,]

    p1$dropout <- as.factor(ifelse(p1$player %in% unique(dropoutList$player), 1, 0))
    p1$first.dropout <- as.factor(ifelse(p1$player %in% first.dropouts, 1, 0))

    df$dropout <- as.factor(ifelse(df$player %in% unique(dropoutList$player), 1, 0))

    df$first.dropout <- as.factor(ifelse(df$player %in% first.dropouts, 1, 0))
    df$first.dropout.char <- ifelse(df$first.dropout == 1, "First Dropout", "Stay")
    df$first.dropout.char <- factor(df$first.dropout.char, levels=c("Stay", "First Dropout"))

    df$dropout.char <- ifelse(df$dropout == 1, "Dropout", "Stay")
    df$dropout.char <- factor(df$dropout.char, levels=c("Stay", "Dropout"))

    DT <- as.data.table(dropoutList)
    aa <- DT[ , .SD[which.min(round)], by = c("session")]
    aa$round <- as.numeric(aa$round)
    aa$session <- as.character(aa$session)

    df$round.num <- as.numeric(df$round)
    df$session.char <- as.character(df$session)
    df$dropout.num <- as.integer(df$dropout)

    df$r.diff <- df$r.mean - df$r.mean.from

    tmp <- NULL
    uptodropout <- NULL
    for (r in 1:nrow(aa)) {
        row <- aa[r,]
        s <- row$session
        r <- row$round
        tmp <- df[df$session.char == s & df$round.num <= r,]
        if (!exists("uptodropout")) {
            uptodropout <- tmp
        } else {
            uptodropout <- rbind(uptodropout, tmp)
        }
    }

    if (DROPOUT.ONLY == FALSE) {
        uptodropout <- rbind(uptodropout, df[!(df$session.char %in% aa$session),])
    }
    ## If not already merged with r0r1, do it!
    if (!("r0.overall.sd" %in% names(uptodropout))) {
        if (!exists('scores.r0r1')) {
            scores.r0r1 <- loadAMTScores(sessions=SESSIONS, R0R1=TRUE)
        }
        uptodropout <- merge(uptodropout, scores.r0r1, by=c("player",
                                                            "condition"),
                             all.x=TRUE)
    }

    return(uptodropout)
}


getSessionDataDropout <- function() {
    if (!exists("dropoutList")) {
        dropoutList <- loadDropouts()
    }

    dropoutList$ps <- paste0(dropoutList$player, dropoutList$session)
    uniqueDp <- dropoutList[!(duplicated(dropoutList$ps)),]
    sessionDropouts <- unique(dropoutList$session)

    (dps <- summarySE(uniqueDp, "dropout", c("condition", "session")))
    dps$session <- as.character(dps$session)
    sessions <- as.character(unique(pr$session))
    dps.full <- dps
    for (s in sessions) {
        if (!(s %in% dps$session) && !(s %in% UNWANTED.SESSIONS)) {
            c <- as.character(pr[pr$session == s,]$condition[1])
            dps.full <- rbind(dps.full, c(c, s, 0, 0, NA, 0, NA, NA))
        }
    }
    dps.full$dropout.num <- as.integer(dps.full$dropout)
    return(dps.full)
}

addThatExtraInfo <- function(df) {
    ##
    pinfo <- loadPlayersInfo(payoff=TRUE, ex=TRUE)
    pinfo.useful <- pinfo[,c(1, 309:350)]
    pinfo.useful$condition <- NULL
    ##
    df <- merge(df, pinfo.useful, by=c("player"), all.x=TRUE)
    df$conex <- as.factor(paste0(df$condition, df$ex))
    ##
    scores.r0r1 <- loadAMTScores(sessions="ALL", R0R1=TRUE)
    bb <- pr[,c("session","player")]
    bb <- bb[!(duplicated(bb)),]
    scores.r0r1 <- merge(scores.r0r1, bb, by="player", all.x=TRUE)
    scores.r0r1 <- scores.r0r1[!(scores.r0r1$session %in% UNWANTED.SESSIONS),]
    scores.r0r1$id <- NULL
    scores.r0r1$condition <- NULL
    scores.r0r1$session <- NULL
    ##
    df <- merge(df, scores.r0r1, by=c("player"), all.x=TRUE)
    return(df)
}


## Ginis.


doSessionRoundGinis <- function(measures=c(), P1=TRUE, data=pr) {
    len <- length(measures)
    if (len == 0) stop('no measure given')
    v1 <- measures[1]
    if (len > 1) {
        v2 <- measures[2]
        if (len > 2) {
            v3 <- measures[3]
            if (len > 3) {
                v4 <- measures[4]
            }
        }
    }

    ## Continue here.
    ginis <- data.frame()
    for (s in unique(data$session)) {
        for (r in unique(data$round)) {
            d <- data[data$session == s & data$round == r,]
            if (len == 1) {
                ## data.frame
                ginis <- rbind(ginis, data.frame(
                                          session = d$session[[1]],
                                          condition = d$condition[[1]],
                                          v1 = ineq(d[,v1], type="Gini"),
                                          round = r
                                      ))
            } else if (len == 2) {
                ## data.frame
                ginis <- rbind(ginis, data.frame(
                                          session = d$session[[1]],
                                          condition = d$condition[[1]],
                                          v1 = ineq(d[,v1], type="Gini"),
                                          v2 = ineq(d[,v2], type="Gini"),
                                          round = r
                                      ))
            } else if (len == 3) {
                ## data.frame
                ginis <- rbind(ginis, data.frame(
                                          session = d$session[[1]],
                                          condition = d$condition[[1]],
                                          v1 = ineq(d[,v1], type="Gini"),
                                          v2 = ineq(d[,v2], type="Gini"),
                                          v3 = ineq(d[,v3], type="Gini"),
                                          round = r
                                      ))
            } else  {
                ## data.frame
                ginis <- rbind(ginis, data.frame(
                                          session = d$session[[1]],
                                          condition = d$condition[[1]],
                                          v1 = ineq(d[,v1], type="Gini"),
                                          v2 = ineq(d[,v2], type="Gini"),
                                          v3 = ineq(d[,v3], type="Gini"),
                                          v4 = ineq(d[,v4], type="Gini"),
                                          round = r
                                      ))
            }
        }
    }
    ginis[[v1]] <- ginis$v1
    ginis$v1 <- NULL
    if (len > 1) {
        ginis[[v2]] <- ginis$v2
        ginis$v2 <- NULL
    }
    if (len > 2) {
        ginis[[v3]] <- ginis$v3
        ginis$v3 <- NULL
    }
    if (len > 3) {
        ginis[[v4]] <- ginis$v4
        ginis$v4 <- NULL
    }
    if (P1) {
        if (len == 1) {
            g <- ifelse(v1 %in% names(p1), ineq(p1[,v1], type="Gini"), NA)
            ginis.p1 <- data.frame(v1=g1)
            ginis.p1[[v1]] <- ginis.p1$v1
            ginis.p1$v1 <- NULL
        }
        else if (len == 2) {
            g1 <- ifelse(v1 %in% names(p1), ineq(p1[,v1], type="Gini"), NA)
            g2 <- ifelse(v2 %in% names(p1), ineq(p1[,v2], type="Gini"), NA)
            ginis.p1 <- data.frame(v1=g1, v2=g2)
            ginis.p1[[v1]] <- ginis.p1$v1
            ginis.p1$v1 <- NULL
            ginis.p1[[v2]] <- ginis.p1$v2
            ginis.p1$v2 <- NULL
        }
        else if (len == 3) {
            g1 <- ifelse(v1 %in% names(p1), ineq(p1[,v1], type="Gini"), NA)
            g2 <- ifelse(v2 %in% names(p1), ineq(p1[,v2], type="Gini"), NA)
            g3 <- ifelse(v3 %in% names(p1), ineq(p1[,v3], type="Gini"), NA)
            ginis.p1 <- data.frame(v1=g1, v2=g2, v3=g3)
            ginis.p1[[v1]] <- ginis.p1$v1
            ginis.p1$v1 <- NULL
            ginis.p1[[v2]] <- ginis.p1$v2
            ginis.p1$v2 <- NULL
            ginis.p1[[v3]] <- ginis.p1$v3
            ginis.p1$v3 <- NULL
        }
        else if (len == 4) {
            g1 <- ifelse(v1 %in% names(p1), ineq(p1[,v1], type="Gini"), NA)
            g2 <- ifelse(v2 %in% names(p1), ineq(p1[,v2], type="Gini"), NA)
            g3 <- ifelse(v3 %in% names(p1), ineq(p1[,v3], type="Gini"), NA)
            g4 <- ifelse(v4 %in% names(p1), ineq(p1[,v4], type="Gini"), NA)
            ginis.p1 <- data.frame(v1=g1, v2=g2, v3=g3, v4=g4)
            ginis.p1[[v1]] <- ginis.p1$v1
            ginis.p1$v1 <- NULL
            ginis.p1[[v2]] <- ginis.p1$v2
            ginis.p1$v2 <- NULL
            ginis.p1[[v3]] <- ginis.p1$v3
            ginis.p1$v3 <- NULL
            ginis.p1[[v4]] <- ginis.p1$v4
            ginis.p1$v4 <- NULL
        }
        return(list(ginis=ginis, ginis.p1=ginis.p1))
    } else {
        return(ginis)
    }
}

namesDistances <- list(
    'd.subcurr'="Diversity",
    'd.pubprev'="Innovation",
    'd.ownprev'="Personal Change"
)
namesDistancesArray <- c("Diversity","Innovation", "Personal Change")

doSessionStats <- function(data, func, name=NA, round=FALSE) {
    if (exists("tmp.sessions")) {
        rm(tmp.sessions)
    }
    for (s in unique(data$session)) {
        d <- data[data$session == s,]
        c <- d[1,]$condition
        if (round == TRUE) {
            for (r in unique(d$round)) {
                dr <- d[d$round == r,]
                g <- func(dr)
                dr <- data.frame(stat=g, condition=c, round=r, session=s)
                if (!exists("tmp.sessions")) {
                    tmp.sessions <- dr
                } else {
                    tmp.sessions <- rbind(tmp.sessions, dr)
                }
            }
        } else {
            g <- func(d)
            d <- data.frame(stat=g, condition=c, session=s)
            if (!exists("tmp.sessions")) {
                tmp.sessions <- d
            } else {
                tmp.sessions <- rbind(tmp.sessions, d)
            }
        }

    }
    if (!is.na(name)) {
        tmp.sessions[,name] <- tmp.sessions$stat
        tmp.sessions$stat <- NULL
    }
    if (round == TRUE) {
        tmp.sessions$round <- factor(tmp.sessions$round, 1:12)
    }
    tmp.sessions
}


f.diagno <- function(mod, type.res=NULL) {
  ## Purpose: produce residuals diagnostics for linear mixed models
  ## ----------------------------------------------------------------------
  ## Arguments: mod= the fitted model,
  ##            type.res= the residuals used for plots
  ## ----------------------------------------------------------------------
  ## Author: Matteo Tanadini, Date: 16 Dec 2013, 11:18

  ## 0) Preparations:
    res <- residuals(mod, type=type.res)
    max.res <- max(abs(res))

  ## 1) QQ-plot:
    par(mfrow=c(2,2), oma=c(0,0,2,0), mar=rep(1.8, 4))
    title <- paste(paste(formula(mod)[c(2,1,3:length(formula(mod)))],
                   sep="", collapse=""),  "\n" , type.res)
    qqnorm(res, main="")
    qqline(res)
    title(title, cex=0.5, outer=T)
  ##---

  ## 2) Tukey-Anscombe plot:
    plot(fitted(mod), res,
         ylim=(c(-max.res, max.res)))
    abline(h=0)
    lines(lowess(fitted(mod), res), col="red")
  ##---

  ## 3) Scale location plot:
    plot(fitted(mod), sqrt(abs(res)))
    abline(h=0)
    lines(lowess(fitted(mod),sqrt(abs(res))), col="red")
    rug(fitted(mod))
  ##---

  ## 4) QQ-plot of the a posteriori random intercepts:
    qqnorm(ranef(mod)[[1]][[1]], main="")
    qqline(ranef(mod)[[1]][[1]])
  ##---
}

## Fix session in AMT.
## prall <- loadGame("ALL")
## scores <- loadAMTScores("ALL")
## getRightSession <- function(row) {
##     if (row[["session"]] == "part1" & row[["treatment"]] != "part1") {
##         session <- prall[prall[["player"]] == row[["creator"]],]$session[1]
##     } else {
##         session <- row[["session"]]
##     }
##     return(as.character(session))
## }
## sessions.fixed <- apply(scores, 1, getRightSession)
## scores$session.fixed <- unlist(sessions.fixed)
## # Only lgc115 and lgc116 were erroneously assigned to part1.
## bb <- scores[scores$session != scores$session.fixed,]
## scores$session <- scores$session.fixed
## scores$session.fixed <- NULL
## write.csv(scores, paste0(DIR, 'mturk_db2_mod-fixsession.csv'), row.names=FALSE)
## nn <- scores <- read.csv(paste0(DIR, 'mturk_db2_mod-fixsession.csv'))
## nrow(nn)
## ncol(nn)
## nrow(scores)
## ncol(scores)
## scores$creativity - nn$creativity

cf.features <- c(
    "cf.head_radius",
    "cf.head_scale_x",
    "cf.head_scale_y",
    "cf.eye_height",
    "cf.eye_spacing",
    "cf.eye_scale_x",
    "cf.eye_scale_y",
    "cf.eyebrow_length",
    "cf.eyebrow_angle",
    "cf.eyebrow_eyedistance",
    "cf.eyebrow_spacing",
    "cf.mouth_top_y",
    "cf.mouth_bottom_y"
)


loadUniqueDropouts <- function(clean=FALSE) {
    ## Dropout.

    dropoutList <- loadDropouts()
    dropoutList$dropout <- 1

    ## Who disconnects first in the same session?
    DT <- as.data.table(dropoutList)

    ## Select all first dropouts (see comments below also).
    first.dropouts <- DT[ , .SD[which.min(round)], by = c("session")]$player
    first.dropouts <- c(as.character(first.dropouts), "ZUOCqClk")


    ##  DT[ , .SD[which.min(round)], by = c("player","session")]
    ##       player session round treatment       date condition dropout
    ##  1: 6z2J3c1Z  lgc100     4 rank_same 08_29_2016      Flat       1
    ##  2: BWpVHri2  lgc101     4 rank_skew 08_29_2016     Strat       1
    ##  3: AqPnJafL  lgc104     2 rank_skew 09_08_2016     Strat       1
    ##  4: bHwEcEWZ  lgc106     2 rank_skew 09_08_2016     Strat       1
    ##  5: ZUOCqClk  lgc106     2 rank_skew 09_08_2016     Strat       1
    ##  6: EpviMK1Y  lgc113    11 rank_skew 09_30_2016     Strat       1
    ##  7: b6Nz2Y9l  lgc114     4 rank_same 09_30_2016      Flat       1
    ##  8: XivoKPs9  lgc115    12 rank_skew 09_29_2016     Strat       1
    ##  9: 0iuHYqac  lgc117     2 rank_same 10_11_2016      Flat       1
    ## 10: jrCxuT8z  lgc117     3 rank_same 10_11_2016      Flat       1
    ## 11: vFt0yzUh  lgc117     5 rank_same 10_11_2016      Flat       1
    ## 12: 4bFFZmWn  lgc123     4 rank_same 10_11_2016      Flat       1
    ## 13: Gj3ihHJU  lgc127     8 rank_skew 10_19_2016     Strat       1
    ## 14: x3DGa4FG  lgc129     2 rank_skew 10_19_2016     Strat       1
    ## 15: U1duMgf2  lgc132     2 rank_skew 10_24_2016     Strat       1
    ## 16: a1l6syCM  lgc132     4 rank_skew 10_24_2016     Strat       1
    ## 17: fOkTIZwV  lgc133     2 rank_skew 10_24_2016     Strat       1
    ## 18: L2uHkc2O  lgc139     4 rank_skew 10_25_2016     Strat       1
    ## 19: d9nKW6bn  lgc139     9 rank_skew 10_25_2016     Strat       1
    ## 20: TDK1511i  lgc139    12 rank_skew 10_25_2016     Strat       1
    ## > DT[ , .SD[which.min(round)], by = c("session")]
    ##     session round   player treatment       date condition dropout
    ##  1:  lgc100     4 6z2J3c1Z rank_same 08_29_2016      Flat       1
    ##  2:  lgc101     4 BWpVHri2 rank_skew 08_29_2016     Strat       1
    ##  3:  lgc104     2 AqPnJafL rank_skew 09_08_2016     Strat       1
    ##  4:  lgc106     2 bHwEcEWZ rank_skew 09_08_2016     Strat       1
    ##  5:  lgc113    11 EpviMK1Y rank_skew 09_30_2016     Strat       1
    ##  6:  lgc114     4 b6Nz2Y9l rank_same 09_30_2016      Flat       1
    ##  7:  lgc115    12 XivoKPs9 rank_skew 09_29_2016     Strat       1
    ##  8:  lgc117     2 0iuHYqac rank_same 10_11_2016      Flat       1
    ##  9:  lgc123     4 4bFFZmWn rank_same 10_11_2016      Flat       1
    ## 10:  lgc127     8 Gj3ihHJU rank_skew 10_19_2016     Strat       1
    ## 11:  lgc129     2 x3DGa4FG rank_skew 10_19_2016     Strat       1
    ## 12:  lgc132     2 U1duMgf2 rank_skew 10_24_2016     Strat       1
    ## 13:  lgc133     2 fOkTIZwV rank_skew 10_24_2016     Strat       1
    ## 14:  lgc139     4 L2uHkc2O rank_skew 10_25_2016     Strat       1

    ## Only in session 106 two people disconnects at the same time, i.e.
    ## after round 1.

    ## 4: bHwEcEWZ  lgc106     2 rank_skew 09_08_2016     Strat       1
    ## 5: ZUOCqClk  lgc106     2 rank_skew 09_08_2016     Strat       1


    dropoutList$first.dropout <- ifelse(dropoutList$player %in% first.dropouts, 1, 0)


    ## Who disconnects first in the same session?
    DT <- as.data.table(dropoutList)

    ## Select all first dropouts (see comments below also).
    first.dropouts <- DT[ , .SD[which.min(round)], by = c("session")]$player
    first.dropouts <- c(as.character(first.dropouts), "ZUOCqClk")

    if (clean) {
        dropoutList$condition <- NULL
        dropoutList$treatment <- NULL
        dropoutList$session <- NULL
        dropoutList$date <- NULL
    }

    dropoutList$player <- as.character(dropoutList$player)
    colnames(dropoutList)[1] <- "dropout.round"

    uniqueDp <- dropoutList[!(duplicated(dropoutList$player)),]
    return(uniqueDp)
}
