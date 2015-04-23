#install libraries

library(XML)
library(seqinr)
library(stringr)
library(xtable)
library(directlabels)
library(grid)
library(quadprog)
library(ggplot2)

#enter the week
week <- "1"

#grab the data
cbs_nz_url <- paste("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/RB/", week, "/nathan_zegura/ppr?&print_rows=9999", sep="")
cbs_je_url <- paste("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/RB/", week, "/jamey_eisenberg/ppr?&print_rows=9999", sep="")
cbs_dr_url <- paste("http://fantasynews.cbssports.com/fantasyfootball/stats/weeklyprojections/RB/", week, "/dave_richard/ppr?&print_rows=9999", sep="")
url1_espn <- paste("http://games.espn.go.com/ffl/tools/projections?slotCategoryId=2&scoringPeriodId=", week, "&seasonId=2014", sep="")
url2_espn <- paste("http://games.espn.go.com/ffl/tools/projections?slotCategoryId=2&scoringPeriodId=", week, "&seasonId=2014&startIndex=40", sep="")
url3_espn <- paste("http://games.espn.go.com/ffl/tools/projections?slotCategoryId=2&scoringPeriodId=", week, "&seasonId=2014&startIndex=80", sep="")
url4_espn <- paste("http://games.espn.go.com/ffl/tools/projections?slotCategoryId=2&scoringPeriodId=", week, "&seasonId=2014&startIndex=120", sep="")
url_fftoday1 <- paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=2014&GameWeek=", week, "&PosID=20", sep="")
url_fftoday2 <- paste("http://www.fftoday.com/rankings/playerwkproj.php?Season=2014&GameWeek=", week, "&PosID=20&LeagueID=1&order_by=FFPts&sort_order=DESC&cur_page=1", sep="")
url_bsports <- "http://www.bsports.com/decisionmakerrb"
url_ffs <- "http://www.fantasysharks.com/apps/Projections/WeeklyRBProjections.php"

partial_espn_rbproj1 <- readHTMLTable(url1_espn, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0
partial_espn_rbproj2 <- readHTMLTable(url2_espn, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0
partial_espn_rbproj3 <- readHTMLTable(url3_espn, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0
partial_espn_rbproj4 <- readHTMLTable(url4_espn, as.data.frame=TRUE, stringsAsFactors=FALSE)$playertable_0
nz_rbproj <- readHTMLTable(cbs_nz_url, as.data.frame=TRUE, stringsAsFactors=FALSE)[7]$`NULL`
je_rbproj <- readHTMLTable(cbs_je_url, as.data.frame=TRUE, stringsAsFactors=FALSE)[7]$`NULL`
dr_rbproj <- readHTMLTable(cbs_dr_url, as.data.frame=TRUE, stringsAsFactors=FALSE)[7]$`NULL`
import_fftoday_rbproj1 <- readHTMLTable(url_fftoday1, as.data.frame=TRUE, stringsAsFactors=FALSE)[11]$`NULL`
import_fftoday_rbproj2 <- readHTMLTable(url_fftoday2, as.data.frame=TRUE, stringsAsFactors=FALSE)[11]$`NULL`
import_bsports_rbproj <- readHTMLTable(url_bsports, as.data.frame=TRUE, stringsAsFactors=FALSE)$`tablepress-173`
import_ffs <- readHTMLTable(url_ffs, as.data.frame=TRUE, stringsAsFactors=FALSE)[6]$`NULL`

#trim the data
nz_rbproj <- nz_rbproj[4:(dim(nz_rbproj)[1]-1),]
je_rbproj <- je_rbproj[4:(dim(je_rbproj)[1]-1),]
dr_rbproj <- dr_rbproj[4:(dim(dr_rbproj)[1]-1),]
partial_espn_rbproj1 <- partial_espn_rbproj1[2:(dim(partial_espn_rbproj1)[1]),]
partial_espn_rbproj2 <- partial_espn_rbproj2[2:(dim(partial_espn_rbproj2)[1]),]
partial_espn_rbproj3 <- partial_espn_rbproj3[2:(dim(partial_espn_rbproj3)[1]),]
partial_espn_rbproj4 <- partial_espn_rbproj4[2:(dim(partial_espn_rbproj4)[1]),]
import_fftoday_rbproj1 <- import_fftoday_rbproj1[3:(dim(import_fftoday_rbproj1)[1]),]
import_fftoday_rbproj2 <- import_fftoday_rbproj2[3:(dim(import_fftoday_rbproj2)[1]),]
import_ffs <- subset(import_ffs, select = c("V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13"))
import_ffs <- import_ffs[3:(dim(import_ffs)[1]-1),]
import_ffs <- na.omit(import_ffs)

#rbind the necessary data
espn_rbproj <- rbind(partial_espn_rbproj1, partial_espn_rbproj2, partial_espn_rbproj3, partial_espn_rbproj4)
fftoday_rbproj <- rbind(import_fftoday_rbproj1, import_fftoday_rbproj2)

#convert into my naming convention
bsp_rbproj <- import_bsports_rbproj
ffs_rbproj <- import_ffs

#trim the top 100 wr projections
nz_rbproj <- nz_rbproj[1:100,]
je_rbproj <- je_rbproj[1:100,]
dr_rbproj <- dr_rbproj[1:100,]
espn_rbproj <- espn_rbproj[1:100,]
#less than 100, no need to trim ## fftoday_rbproj <- fftoday_rbproj[1:100,]
bsp_rbproj <- bsp_rbproj[1:100,]
ffs_rbproj <- ffs_rbproj[1:100,]

#remove an NAs
espn_rbproj <- na.omit(espn_rbproj)
ffs_rbproj <- na.omit(ffs_rbproj)

#get rid of unwanted columns
espn_rbproj <- subset(espn_rbproj, select = c(1,8,9,10,11,12,13,14))
fftoday_rbproj <- subset(fftoday_rbproj, select = c(2,5,6,7,8,9,10,11))
bsp_rbproj <- subset(bsp_rbproj, select = c(2,5,6,7,8,9,10,11,12))
ffs_rbproj <- subset(ffs_rbproj, select = c(1,4,8,9,10,11,12))

#name the columns
names(nz_rbproj) <- c("player","nz_rushes","nz_yd","nz_avg","nz_rush_td","nz_recpts","nz_rec_yd","nz_rec_avg","nz_rec_td","nz_fl","nz_fpts")
names(je_rbproj) <- c("player","je_rushes","je_yd","je_avg","je_rush_td","je_recpts","je_rec_yd","je_rec_avg","je_rec_td","je_fl","je_fpts")
names(dr_rbproj) <- c("player","dr_rushes","dr_yd","dr_avg","dr_rush_td","dr_recpts","dr_rec_yd","dr_rec_avg","dr_rec_td","dr_fl","dr_fpts")
names(espn_rbproj) <- c("player","espn_rushes","espn_yd","espn_td","espn_recpts","espn_rec_yd","espn_rec_td","espn_fpts")
names(fftoday_rbproj) <- c("player","fft_rushes","fft_yd","fft_td","fft_recpts","fft_rec_yd","fft_rec_td","fft_fpts")
names(bsp_rbproj) <- c("player","bsp_rushes","bsp_yd","bsp_td","bsp_recpts","bsp_rec_yd","bsp_rec_td","bsp_fpts","bsp_fpts_standard")
names(ffs_rbproj) <- c("player","ffs_yd","ffs_td","ffs_recpts","ffs_rec_yd","ffs_rec_td","ffs_fpts")

#convert to numeric
nz_rbproj$nz_rushes <- as.numeric(nz_rbproj$nz_rushes)
nz_rbproj$nz_yd <- as.numeric(nz_rbproj$nz_yd)
nz_rbproj$nz_avg <- as.numeric(nz_rbproj$nz_avg)
nz_rbproj$nz_rush_td <- as.numeric(nz_rbproj$nz_rush_td)
nz_rbproj$nz_recpts <- as.numeric(nz_rbproj$nz_recpts)
nz_rbproj$nz_rec_yd <- as.numeric(nz_rbproj$nz_rec_yd)
nz_rbproj$nz_rec_avg <- as.numeric(nz_rbproj$nz_rec_avg)
nz_rbproj$nz_rec_td <- as.numeric(nz_rbproj$nz_rec_td)
nz_rbproj$nz_fl <- as.numeric(nz_rbproj$nz_fl)
nz_rbproj$nz_fpts <- as.numeric(nz_rbproj$nz_fpts)

je_rbproj$je_rushes <- as.numeric(je_rbproj$je_rushes)
je_rbproj$je_yd <- as.numeric(je_rbproj$je_yd)
je_rbproj$je_avg <- as.numeric(je_rbproj$je_avg)
je_rbproj$je_rush_td <- as.numeric(je_rbproj$je_rush_td)
je_rbproj$je_recpts <- as.numeric(je_rbproj$je_recpts)
je_rbproj$je_rec_yd <- as.numeric(je_rbproj$je_rec_yd)
je_rbproj$je_rec_avg <- as.numeric(je_rbproj$je_rec_avg)
je_rbproj$je_rec_td <- as.numeric(je_rbproj$je_rec_td)
je_rbproj$je_fl <- as.numeric(je_rbproj$je_fl)
je_rbproj$je_fpts <- as.numeric(je_rbproj$je_fpts)

dr_rbproj$dr_rushes <- as.numeric(dr_rbproj$dr_rushes)
dr_rbproj$dr_yd <- as.numeric(dr_rbproj$dr_yd)
dr_rbproj$dr_avg <- as.numeric(dr_rbproj$dr_avg)
dr_rbproj$dr_rush_td <- as.numeric(dr_rbproj$dr_rush_td)
dr_rbproj$dr_recpts <- as.numeric(dr_rbproj$dr_recpts)
dr_rbproj$dr_rec_yd <- as.numeric(dr_rbproj$dr_rec_yd)
dr_rbproj$dr_rec_avg <- as.numeric(dr_rbproj$dr_rec_avg)
dr_rbproj$dr_rec_td <- as.numeric(dr_rbproj$dr_rec_td)
dr_rbproj$dr_fl <- as.numeric(dr_rbproj$dr_fl)
dr_rbproj$dr_fpts <- as.numeric(dr_rbproj$dr_fpts)

espn_rbproj$espn_rushes <- as.numeric(espn_rbproj$espn_rushes)
espn_rbproj$espn_yd <- as.numeric(espn_rbproj$espn_yd)
espn_rbproj$espn_td <- as.numeric(espn_rbproj$espn_td)
espn_rbproj$espn_recpts <- as.numeric(espn_rbproj$espn_recpts)
espn_rbproj$espn_rec_yd <- as.numeric(espn_rbproj$espn_rec_yd)
espn_rbproj$espn_rec_td <- as.numeric(espn_rbproj$espn_rec_td)
espn_rbproj$espn_fpts <- as.numeric(espn_rbproj$espn_fpts)

fftoday_rbproj$fft_rushes <- as.numeric(fftoday_rbproj$fft_rushes)
fftoday_rbproj$fft_yd <- as.numeric(fftoday_rbproj$fft_yd)
fftoday_rbproj$fft_td <- as.numeric(fftoday_rbproj$fft_td)
fftoday_rbproj$fft_recpts <- as.numeric(fftoday_rbproj$fft_recpts)
fftoday_rbproj$fft_rec_yd <- as.numeric(fftoday_rbproj$fft_rec_yd)
fftoday_rbproj$fft_rec_td <- as.numeric(fftoday_rbproj$fft_rec_td)
fftoday_rbproj$fft_fpts <- as.numeric(fftoday_rbproj$fft_fpts)

bsp_rbproj$bsp_rushes <- as.numeric(bsp_rbproj$bsp_rushes)
bsp_rbproj$bsp_yd <- as.numeric(bsp_rbproj$bsp_yd)
bsp_rbproj$bsp_td <- as.numeric(bsp_rbproj$bsp_td)
bsp_rbproj$bsp_recpts <- as.numeric(bsp_rbproj$bsp_recpts)
bsp_rbproj$bsp_rec_yd <- as.numeric(bsp_rbproj$bsp_rec_yd)
bsp_rbproj$bsp_rec_td <- as.numeric(bsp_rbproj$bsp_rec_td)
bsp_rbproj$bsp_fpts <- as.numeric(bsp_rbproj$bsp_fpts)

ffs_rbproj$ffs_yd <- as.numeric(ffs_rbproj$ffs_yd)
ffs_rbproj$ffs_td <- as.numeric(ffs_rbproj$ffs_td)
ffs_rbproj$ffs_recpts <- as.numeric(ffs_rbproj$ffs_recpts)
ffs_rbproj$ffs_rec_yd <- as.numeric(ffs_rbproj$ffs_rec_yd)
ffs_rbproj$ffs_rec_td <- as.numeric(ffs_rbproj$ffs_rec_td)
ffs_rbproj$ffs_fpts <- as.numeric(ffs_rbproj$ffs_fpts)

#round bsp projections
bsp_rbproj$bsp_recpts <- round(bsp_rbproj$bsp_recpts, digits = 0)
bsp_rbproj$bsp_yd <- round(bsp_rbproj$bsp_yd, digits = 0)
bsp_rbproj$bsp_td <- round(bsp_rbproj$bsp_td, digits = 0)
bsp_rbproj$bsp_fpts <- round(bsp_rbproj$bsp_fpts, digits = 0)

#convert espn, fftoday to ppr
espn_rbproj$espn_fpts <- espn_rbproj$espn_recpts + espn_rbproj$espn_fpts
fftoday_rbproj$fft_fpts <- fftoday_rbproj$fft_recpts + fftoday_rbproj$fft_fpts
ffs_rbproj$ffs_fpts <- ffs_rbproj$ffs_recpts + ffs_rbproj$ffs_fpts

#remove team from player names & create name column
nz_rbproj$name <- str_sub(nz_rbproj$player, end=str_locate(string=nz_rbproj$player, ',')[,1]-1)
je_rbproj$name <- str_sub(je_rbproj$player, end=str_locate(string=je_rbproj$player, ',')[,1]-1)
dr_rbproj$name <- str_sub(dr_rbproj$player, end=str_locate(string=dr_rbproj$player, ',')[,1]-1)
espn_rbproj$name <- str_sub(espn_rbproj$player, end=str_locate(string=espn_rbproj$player, ',')[,1]-1)
espn_rbproj$name <- str_replace_all(espn_rbproj$name, "\\*", "")
bsp_rbproj$name <- bsp_rbproj$player
ffs_rbproj$name <- ffs_rbproj$player

#remove symbol from fftoday
fftoday_rbproj$name <- str_replace_all(fftoday_rbproj$player, "Â", "")
fftoday_rbproj$name <- str_replace_all(fftoday_rbproj$name, "^\\s+", "")

#remove formating from ffs
ffs_rbproj$name <- str_replace_all(ffs_rbproj$name, " R", "")
ffs_rbproj$name <- strsplit(ffs_rbproj$name, ",")
ffs_rbproj$name <- unlist(lapply(ffs_rbproj$name, 
                                 function(x) paste(x[1:length(x) %% 2 == 0], 
                                                   x[1:length(x) %% 2 != 0])))

#add team column
nz_rbproj$team <- str_trim(str_sub(nz_rbproj$player, start= -3))
je_rbproj$team <- str_trim(str_sub(je_rbproj$player, start= -3))
dr_rbproj$team <- str_trim(str_sub(dr_rbproj$player, start= -3))

espn_rbproj$team_espn <- str_sub(espn_rbproj$player, start=str_locate(string=espn_rbproj$player, ',')[,1]+2, end = str_locate(string=espn_rbproj$player, ',')[,1]+4)
espn_rbproj$team_espn <- str_trim(espn_rbproj$team_espn, side="right")
espn_rbproj$team_espn <- toupper(espn_rbproj$team_espn)
espn_rbproj$team_espn[espn_rbproj$team_espn=="WSH"] <- "WAS"

espn_rbproj[espn_rbproj$name=="Gio Bernard", "name"] <- "Giovani Bernard"
bsp_rbproj[bsp_rbproj$name=="LeVeon Bell","name"] <- "Le'Veon Bell"

#fix ffs' names
#names_ffs <- ffs_rbproj$player

#resort data
nz_rbproj <- subset(nz_rbproj, select = c("name","team","nz_yd","nz_rush_td","nz_recpts","nz_rec_yd","nz_rec_td","nz_fpts"))
je_rbproj <- subset(je_rbproj, select = c("name","team","je_yd","je_rush_td","je_recpts","je_rec_yd","je_rec_td","je_fpts"))
dr_rbproj <- subset(dr_rbproj, select = c("name","team","dr_yd","dr_rush_td","dr_recpts","dr_rec_yd","dr_rec_td","dr_fpts"))
espn_rbproj <- subset(espn_rbproj, select = c("name","espn_yd","espn_td","espn_recpts","espn_rec_yd","espn_rec_td","espn_fpts"))
fftoday_rbproj <- subset(fftoday_rbproj, select = c("name","fft_yd","fft_td","fft_recpts","fft_rec_yd","fft_rec_td","fft_fpts"))
bsp_rbproj <- subset(bsp_rbproj, select = c("name","bsp_yd","bsp_td","bsp_recpts","bsp_rec_yd","bsp_rec_td","bsp_fpts"))
ffs_rbproj <- subset(ffs_rbproj, select = c("name","ffs_yd","ffs_td","ffs_recpts","ffs_rec_yd","ffs_rec_td","ffs_fpts"))

#merge data (probably a better way)
projections <- merge(nz_rbproj, je_rbproj, by="name")
projections <- merge(projections, dr_rbproj, by="name")
projections <- merge(projections, espn_rbproj, by="name")
projections <- merge(projections, fftoday_rbproj, by="name")
projections <- merge(projections, bsp_rbproj, by="name")
projections <- merge(projections, ffs_rbproj, by="name")

#remove duplicate team columns (probably a better way - maybe not send them in the first place?)
projections$team.y <- NULL
projections$team <- NULL
projections$team_espn <- NULL

#subset name and each predicted fpts
fantasyproj <- subset(projections, select = c("name","nz_fpts","espn_fpts","bsp_fpts", "je_fpts", "fft_fpts", "dr_fpts", "ffs_fpts"))

#ncol count
columns_fantasyproj <- ncol(fantasyproj)

#find standard deviation of projections
fantasyproj_stdev <- apply(fantasyproj[2:columns_fantasyproj], 1, sd, na.rm = TRUE)

#find mean of projections
fantasyproj_mean <- apply(fantasyproj[2:columns_fantasyproj], 1, mean)

#round mean/stdev
fantasyproj_mean <- round(fantasyproj_mean, digits = 1)
fantasyproj_stdev <- round(fantasyproj_stdev, digits = 1)

#add mean/stdev
fantasyproj$mean <- fantasyproj_mean
fantasyproj$sd <- fantasyproj_stdev

#remove players without projections
#fantasyproj <- fantasyproj[!(fantasyproj$SD==0),]

#sort players by stdev
fantasyproj <- fantasyproj[order(-fantasyproj$sd), , drop = FALSE]

#just mean & sd
#fantasyproj <- subset(fantasyproj, select = c("name","mean","sd"))

#training data
training <- subset(fantasyproj, select = c("sd"))

#clustering
# cluster <- Mclust(training$sd, G=8)
# fantasyproj$Tier <- cluster$classification

#add ceiling / floor columns
fantasyproj$Ceiling <- fantasyproj$mean + fantasyproj$sd
fantasyproj$Floor <- fantasyproj$mean - fantasyproj$sd

#sort by floor
fantasyproj <- fantasyproj[order(-fantasyproj$Floor), , drop = FALSE]

#id system
fantasyproj <- data.frame(fantasyproj, id=1:nrow(fantasyproj))

#GRAPHING
#top 20
top20 <- fantasyproj[1:20,]

q <- ggplot(top20, aes(y=id, x=mean, colour=sd))

q + 
  geom_point(size = I(4)) + 
  geom_errorbarh(aes(xmax = mean + sd, xmin = mean - sd, height = .2), alpha = I(0.5)) + 
  geom_text(aes(x = mean - sd, label=name, hjust=(1.2), vjust=(0), angle=(0), size=1)) +
  scale_colour_gradient(low = "blue", high = "red") +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  coord_cartesian(xlim = c(top20$mean-top20$sd-3, top20$mean+top20$sd)) +
  scale_y_reverse() +
  xlab("Average FPTS Projection") + ylab("Rank by Floor") + labs(title = paste("Week ", week, " RBs Uncertainty", sep=""))

#find width
graph.width <- min(fantasyproj$mean - fantasyproj$sd - 5)

p <- ggplot(fantasyproj, aes(y=id, x=mean, colour=sd))

p + 
  geom_point(size = I(4)) + 
  geom_errorbarh(aes(xmax = mean + sd, xmin = mean - sd, height = .4), alpha = I(0.5)) + 
  geom_text(aes(x = mean - sd, label=name, hjust=(1.2), vjust=(0), angle=(0), size=1)) +
  scale_colour_gradient(low = "blue", high = "red") +
  theme(
    plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,panel.border = element_blank()
    ,panel.background = element_blank()
  ) +
  coord_cartesian(xlim = c(-3, fantasyproj$mean+fantasyproj$sd+2)) +
  scale_y_reverse() +
  xlab("Average FPTS Projection") + ylab("Rank by Floor") + labs(title = paste("Week ", week, " RBs Uncertainty", sep=""))

#reddit friendly
#summary table
reddit.summary <- subset(fantasyproj, select = c("name","mean","sd"))
reddit.summary <- reddit.summary[order(-reddit.summary$sd), , drop = FALSE]
names(reddit.summary) <- c("Name","FPTS","+/- FPTS")
reddit.summary <- xtable(reddit.summary)
reddit.summary.filename <- paste("week", week, "rb_uncertainty_reddit_summary.html", sep="")

#detailed table
reddit.detailed <- fantasyproj
reddit.detailed.fpts <- subset(reddit.detailed, select = c("nz_fpts","espn_fpts","bsp_fpts", "je_fpts", "fft_fpts", "dr_fpts", "ffs_fpts"))
reddit.detailed$Min <- apply(reddit.detailed.fpts,1,min)
reddit.detailed$Max <- apply(reddit.detailed.fpts,1,max)
reddit.detailed <- subset(reddit.detailed, select = c("name","mean","Ceiling","Floor","Min","Max"))
names(reddit.detailed) <- c("Name","Average","Ceiling","Floor","Min","Max")
reddit.detailed <- subset(reddit.detailed, select = c("Name","Min","Floor","Average","Ceiling","Max"))
reddit.detailed <- xtable(reddit.detailed)
reddit.detailed.filename <- paste("week", week, "rb_uncertainty_reddit_detailed.html", sep="")

#put table into html
htmltable_fantasyproj <- xtable(fantasyproj)
htmltable_projections <- xtable(projections)

#save the html file
print.xtable(reddit.summary, type="html", file=reddit.summary.filename)
print.xtable(reddit.detailed, type="html", file=reddit.detailed.filename)
#print.xtable(htmltable_fantasyproj, type="html", file="week11wrrisk.html", include.rownames=FALSE)
#print.xtable(htmltable_projections, type="html", file="week11wrprojections.html", include.rownames=FALSE)