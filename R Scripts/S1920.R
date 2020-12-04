library(rvest)
library(xml2)


LoadandFill <- function(){


url <-'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=Score&agg=Accumulated'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
PointsTable <- html_table(table)
TPtsNotSorted <- as.data.frame(PointsTable)
TPtsdf <- TPtsNotSorted[order(TPtsNotSorted$Team),]
Points <- TPtsdf$PointsPTS
Adv <- data.frame("Teams" = c(TPtsdf$Team))
Adv$Pts <- c(Points)

url <-'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=FieldGoalsAttempted2&agg=Accumulated'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
Fga2Table <- html_table(table)
FGA2NotSorted <- as.data.frame(Fga2Table)
FGA2df <- FGA2NotSorted[order(FGA2NotSorted$Team),]
FGA2 <- FGA2df$AttemptedAtt.
FGM2 <- FGA2df$Made
Adv$FGM2 <- c(FGM2)
Adv$FGA2 <- c(FGA2)

url <-'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=FieldGoalsAttempted3&agg=Accumulated'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
Fga3Table <- html_table(table)
FGA3NotSorted <- as.data.frame(Fga3Table)
FGA3df <- FGA3NotSorted[order(FGA3NotSorted$Team),]
FGA3 <- FGA3df$AttemptedAtt.
FGM3 <- FGA3df$Made
Adv$FGM3 <- c(FGM3)
Adv$FGA3 <- c(FGA3)
               
url <-'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=FreeThrowsAttempted&agg=Accumulated'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
FtaTable <- html_table(table)
FTANotSorted <- as.data.frame(FtaTable)
FTAdf <- FTANotSorted[order(FTANotSorted$Team),]
FTA <- FTAdf$AttemptedAtt.
FTM <- FTAdf$Made
Adv$FTM <- c(FTM)
Adv$FTA <- c(FTA)


url <-'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=TimePlayed&agg=Accumulated'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
MpTable <- html_table(table)
TMPNotSorted <- as.data.frame(MpTable)
TMPdf <- TMPNotSorted[order(TMPNotSorted$Team),]
MP <- TMPdf$`Minutes playedMP`
Adv$Tmp <- c(MP)

url <-'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=Turnovers&agg=Accumulated'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
TOTable <- html_table(table)
TONotSorted <- as.data.frame(TOTable)
TOdf <- TONotSorted[order(TONotSorted$Team),]
TO <- TOdf$TurnoversTO
Adv$TO <- c(TO)

url <-'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=OffensiveRebounds&agg=Accumulated'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
TorTable <- html_table(table)
TORNotSorted <- as.data.frame(TorTable)
TORdf <- TORNotSorted[order(TORNotSorted$Team),]
OR <- TORdf$`Offensive ReboundsOR`
Adv$OR <- c(OR)

url <- 'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=Score&agg=AccumulatedReverse'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
OppPtsTable <- html_table(table)
OppPtsNotSorted <- as.data.frame(OppPtsTable)
OppPtsdf <- OppPtsNotSorted[order(OppPtsNotSorted$Team),]
OPTS <- OppPtsdf$PointsPTS
Adv$OppPts<- c(OPTS)

url <- 'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=DefensiveRebounds&agg=AccumulatedReverse'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
OppDefTable <- html_table(table)
OppDefRebNotSorted <- as.data.frame(OppDefTable)
OppDefRebdf <- OppDefRebNotSorted[order(OppDefRebNotSorted$Team),]
OPDefReb <- OppDefRebdf$`Defensive ReboundsDR`
Adv$OppDefReb <- c(OPDefReb)

url <-'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=Assistances&agg=Accumulated'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
AstTable <- html_table(table)
ASTNotSorted <- as.data.frame(AstTable)
ASTdf <- ASTNotSorted[order(ASTNotSorted$Team),]
AST <- ASTdf$Assists
Adv$AST <- c(AST)

url <- 'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=OffensiveRebounds&agg=AccumulatedReverse'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
OppOffTable <- html_table(table)
OppOffRebNotSorted <- as.data.frame(OppOffTable)
OppOffRebdf <- OppOffRebNotSorted[order(OppOffRebNotSorted$Team),]
OPOffReb <- OppOffRebdf$`Offensive ReboundsOR`
Adv$OppOffReb <- c(OPOffReb)

url <- 'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=FieldGoalsMade2&agg=AccumulatedReverse'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
OppFGM2Table <- html_table(table)
OppFGM2NotSorted <- as.data.frame(OppFGM2Table)
OppFGM2df <- OppFGM2NotSorted[order(OppFGM2NotSorted$Team),]
OPFGM2 <- OppFGM2df$Made
OPFGA2 <- OppFGM2df$AttemptedAtt.
Adv$OppFGM2 <- c(OPFGM2)
Adv$OppFGA2 <- c(OPFGA2)

url <- 'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=FieldGoalsMade3&agg=AccumulatedReverse'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
OppFGM3Table <- html_table(table)
OppFGM3NotSorted <- as.data.frame(OppFGM3Table)
OppFGM3df <- OppFGM3NotSorted[order(OppFGM3NotSorted$Team),]
OPFGM3 <- OppFGM3df$Made
OPFGA3 <- OppFGM3df$AttemptedAtt.
Adv$OppFGM3 <- c(OPFGM3)
Adv$OppFGA3 <- c(OPFGA3)


url <- 'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=FreeThrowsMade&agg=AccumulatedReverse'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
OppFTMTable <- html_table(table)
OppFTMNotSorted <- as.data.frame(OppFTMTable)
OppFTMdf <- OppFTMNotSorted[order(OppFTMNotSorted$Team),]
OPFTM <- OppFTMdf$Made
OPFTA <- OppFTMdf$AttemptedAtt.
Adv$OppFTM <- c(OPFTM)
Adv$OppFTA <- c(OPFTA)

url <- 'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=TimePlayed&agg=AccumulatedReverse'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
OppMPTable <- html_table(table)
OppMPNotSorted <- as.data.frame(OppMPTable)
OppMPdf <- OppMPNotSorted[order(OppMPNotSorted$Team),]
OPMP <- OppMPdf$`Minutes playedMP`
Adv$OppMP <- c(OPMP)

url <- 'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=Turnovers&agg=AccumulatedReverse'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
OppTOTable <- html_table(table)
OppTONotSorted <- as.data.frame(OppTOTable)
OppTOdf <- OppTONotSorted[order(OppTONotSorted$Team),]
OPTO <- OppTOdf$TurnoversTO
Adv$OppTO <- c(OPTO)

url <- 'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=BlocksFavour&agg=Accumulated'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
BLKTable <- html_table(table)
BLKNotSorted <- as.data.frame(BLKTable)
BLKdf <- BLKNotSorted[order(BLKNotSorted$Team),]
TBLK <- BLKdf$`BlocksBL+`
Adv$BLK <- c(TBLK)

url <- 'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=DefensiveRebounds&agg=Accumulated'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
DefTable <- html_table(table)
DefRebNotSorted <- as.data.frame(DefTable)
DefRebdf <- DefRebNotSorted[order(DefRebNotSorted$Team),]
DefReb <- DefRebdf$`Defensive ReboundsDR`
Adv$DefReb <- c(DefReb)

url <- 'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=FoulsCommited&agg=Accumulated'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
FLCTable <- html_table(table)
FLCNotSorted <- as.data.frame(FLCTable)
FLCdf <- FLCNotSorted[order(FLCNotSorted$Team),]
FLC <- FLCdf$'Fouls CommitedFL-'
Adv$FLC <- c(FLC)

url <- 'https://www.euroleague.net/main/statistics?mode=Leaders&entity=Clubs&seasonmode=Single&seasoncode=E2019&cat=Steals&agg=Accumulated'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="ctl00_ctl00_ctl00_ctl00_maincontainer_maincontent_contentpane_ctl01_ctl01_gvResults"]')
STLTable <- html_table(table)
STLNotSorted <- as.data.frame(STLTable)
STLdf <- STLNotSorted[order(STLNotSorted$Team),]
STL <- STLdf$StealsST
Adv$STL <- c(STL)


write.csv(Adv, file ="AP1920.csv",row.names = FALSE)
fileName <- "AP1920.csv"

CreateAdvDF(fileName)
}
LoadandFill()






