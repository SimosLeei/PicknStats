library(scales)
library(ggplot2)



convertToMin <- function(MinPlayedDirty){
  
  MinutesDir2 <- strsplit(MinPlayedDirty,":")
  MinutesDir3 <- matrix(unlist(MinutesDir2), ncol=2, byrow=TRUE)
  MinutesDir4 <- as.double(MinutesDir3[1]) 
  MinutesDir5 <- as.double(MinutesDir3[2])
  
  
  Seconds100  <- ((100*MinutesDir5)/60)/100
  MP <- MinutesDir4+ Seconds100
}

Possessions <- function(FGA,FTA,TO,OREB){
  possessions <- FGA + ( 0.44 * FTA ) + TO - OREB
}

T2P <- function(FGM2,FGA2){
  Twopoints <- FGM2/FGA2
}

T3P <- function(FGM3,FGA3){
  Threepoints <- FGM3/FGA3
}

TrueShooting <- function(TPTS,FGA,FTA){
  trueshootin <- (TPTS/2)/ (FGA +0.44*FTA)
}
PointsPerPoss <- function(TPTS,TeamPoss){
  pPp <- TPTS/TeamPoss
}
Pace <- function(TeamPoss, TMPS){
  pacee <- 5*(TeamPoss/TMPS)*40
}
DRTG <- function(OPPTS,TeamPoss){
  drtg <- (OPPTS/TeamPoss)*100
}
ORTG <- function(TPTS,TeamPoss){
  ortg <- (TPTS/TeamPoss)*100
}
NRTG <- function(DefRating, OffRating){
  nrtg <- OffRating - DefRating
}


CreateAdvDF <- function(fileName){
  
  DFName <<- fileName
  dfStats <<-  as.data.frame(read.csv(DFName,header = TRUE,sep = ",",stringsAsFactors = FALSE))
  AdvStats <- data.frame(Teams = character(1), Possessions = double(1), Pace = c(1), TS = c(1), 
                         PPP = c(1), DRTG = c(1), ORTG = c(1),NRTG = c(1),eFG = c(1),FGM2P= c(1),FGM3P = c(1),FGA = c(1), stringsAsFactors = FALSE)
  x <- AdvStats$Teams

  NumOTeams <- c(1:length(row.names(dfStats)))
  for (i in NumOTeams){
    teamStats <- dfStats[i, ]
    TPTS <- teamStats$Pts
    FGM <- teamStats$FGM2+ teamStats$FGM3
    FGM2 <- teamStats$FGM2
    FGA2 <- teamStats$FGA2
    FGM3 <- teamStats$FGM3
    FGA3 <- teamStats$FGA3
    FGA <- teamStats$FGA2+ teamStats$FGA3
    FTA <- teamStats$FTA
    MinPlayedDirty <- teamStats$Tmp
    TMPS <- convertToMin(MinPlayedDirty)
 
   
    TO <- teamStats$TO
    OREB <- teamStats$OR
    OPPTS <- teamStats$OppPts
  
    TeamName <- teamStats$Teams
    FGM2Per <- label_percent(accuracy = 0.01)(T2P(FGM2,FGA2))
    FGM3Per <- label_percent(accuracy = 0.01)(T3P(FGM3,FGA3))
    TeamPoss <- round(Possessions(FGA, FTA, TO, OREB),0)
    TrueShooting <- label_percent(accuracy = 0.01)(TrueShooting(TPTS,FGA,FTA))
    eFGPer <- label_percent(accuracy = 0.01)((FGM + 0.5 * FGM3) / FGA)
    PPP <- round(PointsPerPoss(TPTS,TeamPoss),2)
    TeamPace <- round(Pace(TeamPoss,TMPS),1)
    DefRating <- round(DRTG(OPPTS,TeamPoss),1)
    OffRating <- round(ORTG(TPTS,TeamPoss),1)
    NetRating <- round(NRTG(DefRating,OffRating),1)
  
    AdvStats[i,] <- data.frame( Teams = c(TeamName), Possessions = c(TeamPoss),
                                Pace = c(TeamPace), TS = c(TrueShooting),
                                PPP = c(PPP),DRTG = c(DefRating),
                                ORTG = c(OffRating),NRTG = c(NetRating),eFG = c(eFGPer),FGM2P= c(FGM2Per),FGM3P = c(FGM3Per), FGA = c(FGA),
                                stringsAsFactors = FALSE)
  
  }
  nameAppend <- "Adv"
  NewName <- paste(nameAppend,DFName,sep = "")
  write.csv(AdvStats, file = NewName,row.names = FALSE)

}


