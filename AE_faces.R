## Explore the faces created.
#############################

library(stringr)
library(gridExtra)
library(png)

FACESDIR <- paste0(THISDIR, "faces/")

files <- list.files(pattern = "\\.png$", path = FACESDIR)

pinfo <- unique(p1pr[,c("player", "session", "condition")])

cb <- function() {
    ## Init.
    mylist <- list()
    for (i in 1:12) {
        mylist[[paste0("stratfaces", i)]] <- matrix(rep(0,500*500), nrow=500, ncol=500)
        mylist[[paste0("flatfaces", i)]] <- matrix(rep(0,500*500), nrow=500, ncol=500)
    }
    ##
    for (f in files) {
        tokens <- str_split(f, "_")[[1]]
        round <- tokens[length(tokens)]
        round <- gsub(".png", "", round)
        player <- tokens[length(tokens)-1]
        pData <- pinfo[pinfo$player == player,]
        if (nrow(pData) == 1) {
            filename <- paste0(FACESDIR, f)
            pngMatrix <- readPNG(filename)
            ## Just rounding to 0,1 and taking the flat part of the jpg image.
            img.flattened <- round(pngMatrix[,,3])
            ## Reversing the 0s and 1s.
            img.flattened <- abs(img.flattened-1)
            ##
            if (pData$condition == "Stratified") {
                mylist[[paste0("stratfaces", round)]] <-
                    mylist[[paste0("stratfaces", round)]] + img.flattened
            } else {
                mylist[[paste0("flatfaces", round)]] <-
                    mylist[[paste0("flatfaces", round)]] + img.flattened
            }
        }
    }
    return(mylist)
}

allfaces <- cb()

LIMIT <- -1

for (i in 1:12) {
    ## Stratified Market faces.
    tmp <- melt(allfaces[[paste0("stratfaces", i)]])
     tmp <- tmp[tmp$value > LIMIT,]
    tmp$Value <- tmp$value
    p <- ggplot(tmp, aes(Var2, Var1, color=Value)) + geom_point() +
        scale_y_reverse() + xlab("") + ylab("") + ggtitle("Stratified")
    assign(paste0("pstrat", i), p)
    filename <- paste0("images_overlapping_stratified_round", i, ".png")
    ggsave(paste0(IMGDIR, filename))
    ##
    ## Flat Market faces.
    tmp <- melt(allfaces[[paste0("flatfaces", i)]])
    tmp <- tmp[tmp$value > LIMIT,]
    tmp$Value <- tmp$value
    p <- ggplot(tmp, aes(Var2, Var1, color=Value)) + geom_point() +
        scale_y_reverse() + xlab("") + ylab("") + ggtitle("Flat")
    assign(paste0("pflat", i), p)
    filename <- paste0("images_overlapping_flat_round", i, ".png")
    ggsave(paste0(IMGDIR, filename))
}

gStrat <- arrangeGrob(pstrat1, pstrat2, pstrat3,
                      pstrat4, pstrat5, pstrat6,
                      pstrat7, pstrat8, pstrat9,
                      pstrat10, pstrat11, pstrat12,
                      nrow = 3)
ggsave(paste0(IMGDIR, "images_overlapping_strat_rounds.png"), gStrat, width=20, height=15)
##
gFlat <- arrangeGrob(pflat1, pflat2, pflat3,
                      pflat4, pflat5, pflat6,
                      pflat7, pflat8, pflat9,
                      pflat10, pflat11, pflat12,
                      nrow = 3)
ggsave(paste0(IMGDIR, "images_overlapping_flat_rounds.png"), gFlat, width=20, height=15)


## TOP IMAGES.
##############

LIMIT <- 20

## Requires permission to create sym links
## (not enabled by default in Windows)

## CREATIVITY
OUTDIR <- paste0(IMGDIR, "faces_most_creative/")
aa <- p1pramt[order(-p1pramt$creativity),]
for (i in 1:LIMIT) {
    row <- aa[i,]
    filepath.in <- paste0(FACESDIR, row$image)
    filepath.out <- paste0(OUTDIR, "Rank", i, "-",
                           row$condition, "_",
                           row$player, "_", row$round, ".png")
    ##
    file.symlink(filepath.in, filepath.out)
}


## OVERALL
OUTDIR <- paste0(IMGDIR, "faces_most_overall/")
aa <- p1pramt[order(-p1pramt$overall),]
for (i in 1:LIMIT) {
    row <- aa[i,]
    filepath.in <- paste0(FACESDIR, row$image)
    filepath.out <- paste0(OUTDIR, "Rank", i, "-",
                           row$condition, "_",
                           row$player, "_", row$round, ".png")
    ##
    file.symlink(filepath.in, filepath.out)
}

## ABSTRACT
OUTDIR <- paste0(IMGDIR, "faces_most_abstract/")
aa <- p1pramt[order(-p1pramt$abstract),]
for (i in 1:LIMIT) {
    row <- aa[i,]
    filepath.in <- paste0(FACESDIR, row$image)
    filepath.out <- paste0(OUTDIR, "Rank", i, "-",
                           row$condition, "_",
                           row$player, "_", row$round, ".png")
    ##
    file.symlink(filepath.in, filepath.out)
}

## FACE
OUTDIR <- paste0(IMGDIR, "faces_most_face/")
aa <- p1pramt[order(-p1pramt$face),]
for (i in 1:LIMIT) {
    row <- aa[i,]
    filepath.in <- paste0(FACESDIR, row$image)
    filepath.out <- paste0(OUTDIR, "Rank", i, "-",
                           row$condition, "_",
                           row$player, "_", row$round, ".png")
    ##
    file.symlink(filepath.in, filepath.out)
}

## INGAMERATINGS
OUTDIR <- paste0(IMGDIR, "faces_most_ingameratings/")
aa <- p1pramt[order(-p1pramt$r.mean.from.clean.asskill),]

aa2 <- aa[aa$stratified == 1,]
for (i in 1:LIMIT) {
    row <- aa2[i,]
    filepath.in <- paste0(FACESDIR, row$image)
    filepath.out <- paste0(OUTDIR, "Rank", i, "-",
                           row$condition, "_",
                           row$player, "_", row$round, ".png")
    ##
    file.symlink(filepath.in, filepath.out)
}

aa2 <- aa[aa$stratified == 0,]
for (i in 1:LIMIT) {
    row <- aa2[i,]
    filepath.in <- paste0(FACESDIR, row$image)
    filepath.out <- paste0(OUTDIR, "Rank", i, "-",
                           row$condition, "_",
                           row$player, "_", row$round, ".png")
    ##
    file.symlink(filepath.in, filepath.out)
}
