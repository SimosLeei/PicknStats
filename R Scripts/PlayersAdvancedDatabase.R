library(tidyr)

Playersdb <- as.data.frame(read.csv("PlayersDatabase.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
PlayersSorted <- Playersdb[order(Playersdb$Season,Playersdb$Season),]

S1920 <- as.data.frame(read.csv("AP1920.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
S1920$Team <- substr(S1920$Team,1,nchar(S1920$Team)-3)
NumOfTeams19 <- c(1:length(row.names(S1920)))

S1819 <- as.data.frame(read.csv("AP1819.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
S1819$Teams <- substr(S1819$Teams,1,nchar(S1819$Teams)-3)
NumOfTeams18 <- c(1:length(row.names(S1819)))

S1718 <- as.data.frame(read.csv("AP1718.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
S1718$Teams <- substr(S1718$Teams,1,nchar(S1718$Teams)-3)
NumOfTeams17 <- c(1:length(row.names(S1718)))


S1617 <- as.data.frame(read.csv("AP1617.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
S1617$Teams <- substr(S1617$Teams,1,nchar(S1617$Teams)-3)
NumOfTeams16 <- c(1:length(row.names(S1617)))


convertToMin <- function(MinutesDir1){
  
  MinutesDir2 <- strsplit(MinutesDir1,":")
  MinutesDir3 <- matrix(unlist(MinutesDir2), ncol=2, byrow=TRUE)
  MinutesDir4 <- as.double(MinutesDir3[1]) 
  MinutesDir5 <- as.double(MinutesDir3[2])

  
  Seconds100  <- ((100*MinutesDir5)/60)/100
  MP <- MinutesDir4+ Seconds100
}

IndivOrtg <- function(PointsProd  , TotalPlayerPoss  ){
IndivORTG  <- 100 * (PointsProd / TotalPlayerPoss)
}

PointsProduction <- function (PointsProd_FG_Part  , PointsProd_AST_Part  , PointsProd_ORB_Part  , FTM  , Team_ORB  , Team_Scoring_Poss  , Team_ORB_weight  , Team_Playp  ){
PointsProduction  <- (PointsProd_FG_Part + PointsProd_AST_Part + FTM) * (1 - (Team_ORB / Team_Scoring_Poss) * Team_ORB_weight * Team_Playp) + PointsProd_ORB_Part
}

PointsProd_ORB_part <- function (ORB, Team_ORB_weight, Team_Playp, Team_PTS, Team_FGM, Team_FTM, Team_FTA){
PointsProd_ORB_Part  <- ORB * Team_ORB_weight * Team_Playp * (Team_PTS / (Team_FGM + (1 - (1 - (Team_FTM / Team_FTA)) ^ 2) * 0.44 * Team_FTA))
}  

PointsProd_AST_part <- function (Team_FGM, Team_FGM3  , Team_PTS  , Team_FTM , Team_FGA,FGM, FGM3,PTS, FTM, FGA, AST  ){
PointsProd_AST_Part  <- 2 * ((Team_FGM - FGM + 0.5 * (Team_FGM3 - FGM3)) / (Team_FGM - FGM)) * 0.5 * (((Team_PTS - Team_FTM) - (PTS - FTM)) / (2 * (Team_FGA - FGA))) * AST
}

PointsProd_FG_part <- function(FGM  , FGM3  , PTS  , FTM  , FGA  , Qst  ){
PointsProd_FG_Part  <- 2 * (FGM + 0.5 * FGM3) * (1 - 0.5 * ((PTS - FTM) / (2 * FGA)) * Qst)
}
TotalPlayerPossesion <- function (ScPoss  , MissedFGPoss  , MissedFTPoss  , TOV  ){
TotalPlayerPoss  <- ScPoss + MissedFGPoss + MissedFTPoss + TOV
}

MissedFTPart <- function (FTM  , FTA  ){
MissedFTPart  <- ((1 - (FTM / FTA)) ^ 2) * 0.44 * FTA
}

MissedFGPart <- function (FGA, FGM, Team_ORBp){
MissedFGPart  <- (FGA - FGM) * (1 - 1.07 * Team_ORBp)
}
ScPossesion <- function (FGPart, ASTPart, FTPart, Team_ORB, Team_Scoring_Poss, Team_ORB_weight, Team_Playp, ORB_Part){
ScPoss  <- (FGPart + ASTPart + FTPart) * (1 - (Team_ORB / Team_Scoring_Poss) * Team_ORB_weight * Team_Playp) + ORB_Part
}
OR_Part<- function (ORB  , Team_ORB_weight  , Team_Playp  ){
ORPart <- ORB * Team_ORB_weight * Team_Playp
}


Team_ORB_Weight <- function (Team_ORBp  , Team_Playp  ){
Team_ORB_weight  <- ((1 - Team_ORBp) * Team_Playp) / ((1 - Team_ORBp) * Team_Playp + Team_ORBp * (1 - Team_Playp))
}
TeamORBpercentage <- function (Team_ORB  , Opponent_DRB ){
TeamORBpercentage  <- Team_ORB / (Team_ORB + Opponent_DRB)
}

TeamPlay <- function (Team_Scoring_Poss  , Team_FGA  , Team_FTA  , Team_TO  ){
TeamPlay  <- Team_Scoring_Poss / (Team_FGA + Team_FTA * 0.44 + Team_TO)
}
TeamScoringPoss <- function (Team_FGM  , Team_FTM  , Team_FTA  ){
TeamScoringPoss  <- Team_FGM + 0.44 * (1 - (1 - (Team_FTM / Team_FTA)) ^ 2) * Team_FTA
}


FT_Part<- function (FTM, FTA){
FTPart <- (1 - (1 - (FTM / FTA)) ^ 2) * 0.44 * FTA
}
AST_Part<- function (Team_PTS, Team_FTM, Team_FGA, PTS, FTM, FGA, AST){
ASTPart <- 0.5 * (((Team_PTS - Team_FTM) - (PTS - FTM)) / (2 * (Team_FGA - FGA))) * AST
}

QAST <- function (MP,Team_MP, Team_AST, AST, Team_FGM, FGM){
Qast  <- ((MP / (Team_MP / 5)) * (1.14 * ((Team_AST - AST) / Team_FGM))) + (((1 - (MP / (Team_MP / 5))) * (((Team_AST / Team_MP) * MP * 5 - AST) / ((Team_FGM / Team_MP) * MP * 5 - FGM))))
}

FG_Part <- function (FGM  , FTM  , FGA  , PTS  , QAST  ){
FGPart  <- FGM * (1 - 0.5 * ((PTS - FTM) / (2 * FGA)) * QAST)
}

#End of Rating Stats Functions

#Start for other function
Ast_To <- function(AST,TO){
  if(TO == 0){
    A_TO <- 0
  }else{
  A_TO <- AST/TO
  }
}

Usg <- function (FGA,FTA,TO,Team_MP,MP,Team_FGA,Team_FTA,Team_TO){
  usgpercent <-  100 * ((FGA + 0.44 * FTA + TO) * (Team_MP / 5)) / (MP * (Team_FGA + 0.44 * Team_FTA + Team_TO))
}

PointsPerShot <- function(PTS,FGA){
  PointPershot <- PTS/FGA
}

TrueShootin <- function(PTS,FGA,FTA){
  trueshoot <- (PTS/2)/ (FGA +0.44*FTA)
}
ORBper <- function(ORB,Team_MP,MP,Team_ORB,Opponent_DRB){
  Orb <- 100 * (ORB * (Team_MP / 5)) / (MP * (Team_ORB + Opponent_DRB)) 
}

FmWtFun <- function(DFGper,DORper){
  fm <- (DFGper * (1 - DORper)) / (DFGper * (1 - DORper) + (1 - DFGper) * DORper)
}


AdvPlayerStats <- data.frame(Player = c(1),Games = c(1), Min = c(1), "Ast/To" = c(1), 
                       PPS = c(1), DRTG = c(1), ORTG = c(1),NRTG = c(1),"TS%"= c(1),"USG%" = c(1),"ORB%"= c(1),
                       "DRB%" = c(1), "TRB%" = c(1),FGA = c(1),
                       FGA2= c(1),FGA3= c(1),FGM2= c(1),FGM3= c(1),
                       StlPer =c(1), AstPer = c(1), eFGPer = c(1), FloorPer = c(1),
                       Season = c(1),Team = c(1), stringsAsFactors = FALSE)


NumOfPlayers <- c(1:length(row.names(PlayersSorted)))

for (player in NumOfPlayers){

  PlayerStats <- PlayersSorted[player, ]
  MinutesDir1 <- gsub(":00","",PlayerStats$Min)
  MP <- convertToMin(MinutesDir1)
 if(PlayerStats$G >5 & MP > 100){
  if(PlayerStats$Season == "2019-2020"){
    for(team in NumOfTeams19){
      TeamStats <- S1920[team,]
      
 
      if(PlayerStats$Team == TeamStats$Team){
       
        #Player Stats
          FGM <- PlayerStats$FGM2 + PlayerStats$FGM3
          FGA <- PlayerStats$FGA2 + PlayerStats$FGA3
          FTM <- PlayerStats$FTM
         
          PlayerStats$Team[is.na(PlayerStats$Team)] <- 0
          Team <- PlayerStats$Team
          FTA <- PlayerStats$FTA
          FGM3 <-PlayerStats$FGM3
          MinutesDir1 <- gsub(":00","",PlayerStats$Min)
          MP <- convertToMin(MinutesDir1)
          AST <- PlayerStats$As
          PTS <- PlayerStats$Pts
          TO <- PlayerStats$To
          ORB <- PlayerStats$O
          STL <- PlayerStats$St
          BLK <- PlayerStats$Fv
          DRB <- PlayerStats$D
          PF <-  PlayerStats$Cm
          
        #Team Stats
          Team_ORB <- TeamStats$OR
          Opponent_DRB <- TeamStats$OppDefReb
          Team_FGM <- TeamStats$FGM2+TeamStats$FGM3
          Team_FGA <- TeamStats$FGA2 + TeamStats$FGA3
          Team_FTM <- TeamStats$FTM
          Team_FTA <- TeamStats$FTA
          Team_FGM3 <- TeamStats$FGM3
          Team_TO <- TeamStats$TO
          MinPlayedDirty <- TeamStats$Tmp
          Team_MP <- as.double(gsub(":00","",MinPlayedDirty))
          Team_AST <- TeamStats$AST
          Team_PTS <- TeamStats$Pts
          #Function Variables
          
          Qst <- QAST(MP,Team_MP,Team_AST,AST,Team_FGM,FGM)
         
          
          
          Team_ORBp <- TeamORBpercentage (Team_ORB,Opponent_DRB)
          Team_Scoring_Poss <- TeamScoringPoss(Team_FGM,Team_FTM,Team_FTA)
          Team_Playp <- TeamPlay(Team_Scoring_Poss,Team_FGA,Team_FTA,Team_TO)
          Team_ORB_weight <- Team_ORB_Weight(Team_ORBp,Team_Playp)
          
          FGPart <- FG_Part(FGM,FTM,FGA,PTS,Qst)
          ASTPart <- AST_Part(Team_PTS,Team_FTM,Team_FGA,PTS,FTM,FGA,AST)
          FTPart <- FT_Part(FTM,FTA)
          ORBPart <- ORPart(ORB,Team_ORB_weight,Team_Playp)
          ScPoss <- ScPossesion(FGPart,ASTPart,FTPart,Team_ORB,Team_Scoring_Poss,Team_ORB_weight,Team_Playp,ORBPart)
          MissedFGPoss <- MissedFGPart(FGA,FGM,Team_ORBp)
          MissedFTPoss <- MissedFTPart(FTM,FTA)
          TotalPlayerPoss <- TotalPlayerPossesion(ScPoss,MissedFGPoss,MissedFTPoss,TO)
          PointsProd_FG_Part <- PointsProd_FG_part(FGM,FGM3,PTS,FTM,FGA,Qst)
          PointsProd_AST_Part <- PointsProd_AST_part(Team_FGM,Team_FGM3,Team_PTS,Team_FTM,Team_FGA,FGM,FGM3,PTS,FTM,FGA,AST)
          PointsProd_ORB_Part <- PointsProd_ORB_part(ORB,Team_ORB_weight,Team_Playp,Team_PTS,Team_FGM,Team_FTM,Team_FTA)
          PointsProd <- PointsProduction(PointsProd_FG_Part, PointsProd_AST_Part, PointsProd_ORB_Part,FTM,Team_ORB,Team_Scoring_Poss,Team_ORB_weight,Team_Playp)
          IndivORTG <- round(IndivOrtg(PointsProd,TotalPlayerPoss),2)
          
          #Drtg
          Opponent_FTA <- TeamStats$FTA
          Opponent_FTM <- TeamStats$FTM
          OppPlayedDirty <- TeamStats$OppMP
          Opponent_MP <- as.double(gsub(":00","",OppPlayedDirty))
          Opponent_TO <- TeamStats$OppTO
          Opponent_FGA <- TeamStats$OppFGA2+TeamStats$OppFGA3
          Opponent_FGM <- TeamStats$OppFGM2 + TeamStats$OppFGM3
          Opponent_ORB <- TeamStats$OppOffReb
          Opponent_PTS <- TeamStats$OppPts
          Opponent_Poss <- Opponent_FGA + ( 0.44 * Opponent_FTA ) + Opponent_TO - Opponent_ORB
          
          Team_DRB <- TeamStats$DefReb
          Team_BLK <- TeamStats$BLK
          Team_STL <- TeamStats$STL
          Team_PF <- TeamStats$FLC
          Team_Possessions <- Team_FGA + ( 0.44 * Team_FTA ) + Team_TO - Team_ORB
          
          DORper <- Opponent_ORB/(Opponent_ORB+Team_DRB) 
          DFGper <- Opponent_FGM/Opponent_FGA
          FMwt <- FmWtFun(DFGper,DORper)
          Stop1 <- STL + BLK * FMwt * (1 - 1.07 * DORper) + DRB * (1 - FMwt)
          Stop2 <- (((Opponent_FGA - Opponent_FGM - Team_BLK) / Team_MP) * FMwt * (1 - 1.07 * DORper) +
                      ((Opponent_TO - Team_STL) / Team_MP)) * MP + (PF / Team_PF) * 0.4 * Opponent_FTA * (1 - (Opponent_FTM / Opponent_FTA))^2
          Stops = Stop1+Stop2
          StopPer <- (Stops * Opponent_MP) / (Team_Possessions * MP)
          
          Team_Defensive_Rating <- 100 * (Opponent_PTS / Team_Possessions)
          D_Pts_per_ScPoss <- Opponent_PTS / (Opponent_FGM + (1 - (1 - (Opponent_FTM / Opponent_FTA))^2) * Opponent_FTA*0.4)
          IndivDRTG  <- round(Team_Defensive_Rating + 0.2 * (100 * D_Pts_per_ScPoss * (1 - StopPer) - Team_Defensive_Rating),2)
          
          
          
          AstTo <- round(Ast_To(AST,TO),4)
          UsgPer <- round(Usg(FGA,FTA,TO,Team_MP,MP,Team_FGA,Team_FTA,Team_TO),4)
          PPS <- round(PointsPerShot(PTS,FGA),2)
          TS <- round(TrueShootin(PTS,FGA,FTA),4)
          ORBPercentage <-round(ORBper(ORB,Team_MP,MP,Team_ORB,Opponent_DRB),2)
          DRBPercentage <- round(100 * (DRB * (Team_MP / 5)) / (MP * (Team_DRB + Opponent_ORB)),2)
          TotalRebPer <- round(100 * ((ORB+ DRB) * (Team_MP / 5)) / (MP * (((Team_ORB+Team_DRB) + (Opponent_DRB+Opponent_ORB)))),2)
          IndivNRTG <- round((IndivORTG -IndivDRTG),2)
          FGA3 <- if(PlayerStats$FGA3==0){1}else{PlayerStats$FGA3}
          FGA2 <- if(PlayerStats$FGA2==0){1}else{PlayerStats$FGA2} 
          TovPer <- round(100 * (TO/(FGA + 0.44*FTA*TO)),2)
          AstPer <- round(100 * (AST / (((MP / (Team_MP / 5)) * Team_FGM) - FGM)),2)
          eFGPer <- round(100 * ((FGM + 0.5 * FGM3) / FGA),2)
          FloorPer <- round(100 * (ScPoss / TotalPlayerPoss),2)
          StlPer <- round(100 * (STL * ((Team_MP /5)/(MP * Opponent_Poss))),2)
          
          AdvPlayerStats[player,] <- data.frame(Player = c(PlayerStats$Player),Games = c(PlayerStats$G), Min = c(MinutesDir1), "Ast/To" = c(AstTo), 
                                       PPS = c(PPS), DRTG = c(IndivDRTG), ORTG = c(IndivORTG),NRTG = c(IndivNRTG) ,"TS%"= c(TS),"USG%" = c(UsgPer),"ORB%"= c(ORBPercentage),
                                       "DRB%" = c(DRBPercentage), "TRB%" = c(TotalRebPer),FGA = c(FGA),
                                       FGA2= c(FGA2),FGA3= c(FGA3),FGM2= c(PlayerStats$FGM2),FGM3= c(PlayerStats$FGM3),
                                       StlPer =c(StlPer), AstPer = c(AstPer), eFGPer = c(eFGPer), FloorPer = c(FloorPer),
                                       Season = c(PlayerStats$Season),Team = c(Team), stringsAsFactors = FALSE)
       }
    }
    
  }else if(PlayerStats$Season == "2018-2019"){
    for(team in NumOfTeams18){
      TeamStats <- S1819[team,]
      
      
      if(PlayerStats$Team == TeamStats$Teams){
       
        
        #Player Stats
        FGM <- PlayerStats$FGM2 + PlayerStats$FGM3
        FGA <- PlayerStats$FGA2 + PlayerStats$FGA3
        FTM <- PlayerStats$FTM
        
        PlayerStats$Team[is.na(PlayerStats$Team)] <- 0
        Team <- PlayerStats$Team
        FTA <- PlayerStats$FTA
        FGM3 <-PlayerStats$FGM3
        MinutesDir1 <- gsub(":00","",PlayerStats$Min)
        MP <- convertToMin(MinutesDir1)
        AST <- PlayerStats$As
        PTS <- PlayerStats$Pts
        TO <- PlayerStats$To
        ORB <- PlayerStats$O
        STL <- PlayerStats$St
        BLK <- PlayerStats$Fv
        DRB <- PlayerStats$D
        PF <-  PlayerStats$Cm
        
        #Team Stats
        Team_ORB <- TeamStats$OR
        Opponent_DRB <- TeamStats$OppDefReb
        Team_FGM <- TeamStats$FGM2+TeamStats$FGM3
        Team_FGA <- TeamStats$FGA2 + TeamStats$FGA3
        Team_FTM <- TeamStats$FTM
        Team_FTA <- TeamStats$FTA
        Team_FGM3 <- TeamStats$FGM3
        Team_TO <- TeamStats$TO
        MinPlayedDirty <- TeamStats$Tmp
        Team_MP <- as.double(gsub(":00","",MinPlayedDirty))
        Team_AST <- TeamStats$AST
        Team_PTS <- TeamStats$Pts
        #Function Variables
        
        Qst <- QAST(MP,Team_MP,Team_AST,AST,Team_FGM,FGM)
        
        
        
        Team_ORBp <- TeamORBpercentage (Team_ORB,Opponent_DRB)
        Team_Scoring_Poss <- TeamScoringPoss(Team_FGM,Team_FTM,Team_FTA)
        Team_Playp <- TeamPlay(Team_Scoring_Poss,Team_FGA,Team_FTA,Team_TO)
        Team_ORB_weight <- Team_ORB_Weight(Team_ORBp,Team_Playp)
        
        FGPart <- FG_Part(FGM,FTM,FGA,PTS,Qst)
        ASTPart <- AST_Part(Team_PTS,Team_FTM,Team_FGA,PTS,FTM,FGA,AST)
        FTPart <- FT_Part(FTM,FTA)
        ORBPart <- ORPart(ORB,Team_ORB_weight,Team_Playp)
        ScPoss <- ScPossesion(FGPart,ASTPart,FTPart,Team_ORB,Team_Scoring_Poss,Team_ORB_weight,Team_Playp,ORBPart)
        MissedFGPoss <- MissedFGPart(FGA,FGM,Team_ORBp)
        MissedFTPoss <- MissedFTPart(FTM,FTA)
        TotalPlayerPoss <- TotalPlayerPossesion(ScPoss,MissedFGPoss,MissedFTPoss,TO)
        PointsProd_FG_Part <- PointsProd_FG_part(FGM,FGM3,PTS,FTM,FGA,Qst)
        PointsProd_AST_Part <- PointsProd_AST_part(Team_FGM,Team_FGM3,Team_PTS,Team_FTM,Team_FGA,FGM,FGM3,PTS,FTM,FGA,AST)
        PointsProd_ORB_Part <- PointsProd_ORB_part(ORB,Team_ORB_weight,Team_Playp,Team_PTS,Team_FGM,Team_FTM,Team_FTA)
        PointsProd <- PointsProduction(PointsProd_FG_Part, PointsProd_AST_Part, PointsProd_ORB_Part,FTM,Team_ORB,Team_Scoring_Poss,Team_ORB_weight,Team_Playp)
        IndivORTG <- round(IndivOrtg(PointsProd,TotalPlayerPoss),2)
        
        #Drtg
        Opponent_FTA <- TeamStats$FTA
        Opponent_FTM <- TeamStats$FTM
        OppPlayedDirty <- TeamStats$OppMP
        Opponent_MP <- as.double(gsub(":00","",OppPlayedDirty))
        Opponent_TO <- TeamStats$OppTO
        Opponent_FGA <- TeamStats$OppFGA2+TeamStats$OppFGA3
        Opponent_FGM <- TeamStats$OppFGM2 + TeamStats$OppFGM3
        Opponent_ORB <- TeamStats$OppOffReb
        Opponent_PTS <- TeamStats$OppPts
        Opponent_Poss <- Opponent_FGA + ( 0.44 * Opponent_FTA ) + Opponent_TO - Opponent_ORB
        
        Team_DRB <- TeamStats$DefReb
        Team_BLK <- TeamStats$BLK
        Team_STL <- TeamStats$STL
        Team_PF <- TeamStats$FLC
        Team_Possessions <- Team_FGA + ( 0.44 * Team_FTA ) + Team_TO - Team_ORB
        
        DORper <- Opponent_ORB/(Opponent_ORB+Team_DRB) 
        DFGper <- Opponent_FGM/Opponent_FGA
        FMwt <- FmWtFun(DFGper,DORper)
        Stop1 <- STL + BLK * FMwt * (1 - 1.07 * DORper) + DRB * (1 - FMwt)
        Stop2 <- (((Opponent_FGA - Opponent_FGM - Team_BLK) / Team_MP) * FMwt * (1 - 1.07 * DORper) +
                    ((Opponent_TO - Team_STL) / Team_MP)) * MP + (PF / Team_PF) * 0.4 * Opponent_FTA * (1 - (Opponent_FTM / Opponent_FTA))^2
        Stops = Stop1+Stop2
        StopPer <- (Stops * Opponent_MP) / (Team_Possessions * MP)
        
        Team_Defensive_Rating <- 100 * (Opponent_PTS / Team_Possessions)
        D_Pts_per_ScPoss <- Opponent_PTS / (Opponent_FGM + (1 - (1 - (Opponent_FTM / Opponent_FTA))^2) * Opponent_FTA*0.4)
        IndivDRTG  <- round(Team_Defensive_Rating + 0.2 * (100 * D_Pts_per_ScPoss * (1 - StopPer) - Team_Defensive_Rating),2)
        
        
        
        AstTo <- round(Ast_To(AST,TO),2)
        UsgPer <- round(Usg(FGA,FTA,TO,Team_MP,MP,Team_FGA,Team_FTA,Team_TO),2)
        PPS <- round(PointsPerShot(PTS,FGA),2)
        TS <- round(TrueShootin(PTS,FGA,FTA),2)
        ORBPercentage <-round(ORBper(ORB,Team_MP,MP,Team_ORB,Opponent_DRB),2)
        DRBPercentage <- round(100 * (DRB * (Team_MP / 5)) / (MP * (Team_DRB + Opponent_ORB)),2)
        TotalRebPer <- round(100 * ((ORB+ DRB) * (Team_MP / 5)) / (MP * (((Team_ORB+Team_DRB) + (Opponent_DRB+Opponent_ORB)))),2)
        IndivNRTG <- round((IndivORTG -IndivDRTG),2)
        FGA3 <- if(PlayerStats$FGA3==0){1}else{PlayerStats$FGA3}
        FGA2 <- if(PlayerStats$FGA2==0){1}else{PlayerStats$FGA2} 
        TovPer <- round(100 * (TO/(FGA + 0.44*FTA*TO)),2)
        AstPer <- round(100 * (AST / (((MP / (Team_MP / 5)) * Team_FGM) - FGM)),2)
        eFGPer <- round(100 * ((FGM + 0.5 * FGM3) / FGA),2)
        FloorPer <- round(100 * (ScPoss / TotalPlayerPoss),2)
        StlPer <- round(100 * (STL * ((Team_MP /5)/(MP * Opponent_Poss))),2)
        
        
        AdvPlayerStats[player,] <- data.frame(Player = c(PlayerStats$Player),Games = c(PlayerStats$G), Min = c(MinutesDir1), "Ast/To" = c(AstTo), 
                                              PPS = c(PPS), DRTG = c(IndivDRTG), ORTG = c(IndivORTG),NRTG = c(IndivNRTG) ,"TS%"= c(TS),"USG%" = c(UsgPer),"ORB%"= c(ORBPercentage),
                                              "DRB%" = c(DRBPercentage), "TRB%" = c(TotalRebPer),FGA = c(FGA),
                                              FGA2= c(FGA2),FGA3= c(FGA3),FGM2= c(PlayerStats$FGM2),FGM3= c(PlayerStats$FGM3),
                                              StlPer =c(StlPer), AstPer = c(AstPer), eFGPer = c(eFGPer), FloorPer = c(FloorPer),
                                              Season = c(PlayerStats$Season),Team = c(Team), stringsAsFactors = FALSE)
      }
    }
    
    
    
  }else if(PlayerStats$Season == "2017-2018"){
    for(team in NumOfTeams17){
      TeamStats <- S1718[team,]
      
      
      if(PlayerStats$Team == TeamStats$Teams){
        
        #Player Stats
        FGM <- PlayerStats$FGM2 + PlayerStats$FGM3
        FGA <- PlayerStats$FGA2 + PlayerStats$FGA3
        FTM <- PlayerStats$FTM
        
        PlayerStats$Team[is.na(PlayerStats$Team)] <- 0
        Team <- PlayerStats$Team
        FTA <- PlayerStats$FTA
        FGM3 <-PlayerStats$FGM3
        MinutesDir1 <- gsub(":00","",PlayerStats$Min)
        MP <- convertToMin(MinutesDir1)
        AST <- PlayerStats$As
        PTS <- PlayerStats$Pts
        TO <- PlayerStats$To
        ORB <- PlayerStats$O
        STL <- PlayerStats$St
        BLK <- PlayerStats$Fv
        DRB <- PlayerStats$D
        PF <-  PlayerStats$Cm
        
        #Team Stats
        Team_ORB <- TeamStats$OR
        Opponent_DRB <- TeamStats$OppDefReb
        Team_FGM <- TeamStats$FGM2+TeamStats$FGM3
        Team_FGA <- TeamStats$FGA2 + TeamStats$FGA3
        Team_FTM <- TeamStats$FTM
        Team_FTA <- TeamStats$FTA
        Team_FGM3 <- TeamStats$FGM3
        Team_TO <- TeamStats$TO
        MinPlayedDirty <- TeamStats$Tmp
        Team_MP <- as.double(gsub(":00","",MinPlayedDirty))
        Team_AST <- TeamStats$AST
        Team_PTS <- TeamStats$Pts
        #Function Variables
        
        Qst <- QAST(MP,Team_MP,Team_AST,AST,Team_FGM,FGM)
        
        
        
        Team_ORBp <- TeamORBpercentage (Team_ORB,Opponent_DRB)
        Team_Scoring_Poss <- TeamScoringPoss(Team_FGM,Team_FTM,Team_FTA)
        Team_Playp <- TeamPlay(Team_Scoring_Poss,Team_FGA,Team_FTA,Team_TO)
        Team_ORB_weight <- Team_ORB_Weight(Team_ORBp,Team_Playp)
        
        FGPart <- FG_Part(FGM,FTM,FGA,PTS,Qst)
        ASTPart <- AST_Part(Team_PTS,Team_FTM,Team_FGA,PTS,FTM,FGA,AST)
        FTPart <- FT_Part(FTM,FTA)
        ORBPart <- ORPart(ORB,Team_ORB_weight,Team_Playp)
        ScPoss <- ScPossesion(FGPart,ASTPart,FTPart,Team_ORB,Team_Scoring_Poss,Team_ORB_weight,Team_Playp,ORBPart)
        MissedFGPoss <- MissedFGPart(FGA,FGM,Team_ORBp)
        MissedFTPoss <- MissedFTPart(FTM,FTA)
        TotalPlayerPoss <- TotalPlayerPossesion(ScPoss,MissedFGPoss,MissedFTPoss,TO)
        PointsProd_FG_Part <- PointsProd_FG_part(FGM,FGM3,PTS,FTM,FGA,Qst)
        PointsProd_AST_Part <- PointsProd_AST_part(Team_FGM,Team_FGM3,Team_PTS,Team_FTM,Team_FGA,FGM,FGM3,PTS,FTM,FGA,AST)
        PointsProd_ORB_Part <- PointsProd_ORB_part(ORB,Team_ORB_weight,Team_Playp,Team_PTS,Team_FGM,Team_FTM,Team_FTA)
        PointsProd <- PointsProduction(PointsProd_FG_Part, PointsProd_AST_Part, PointsProd_ORB_Part,FTM,Team_ORB,Team_Scoring_Poss,Team_ORB_weight,Team_Playp)
        IndivORTG <- round(IndivOrtg(PointsProd,TotalPlayerPoss),2)
        
        #Drtg
        Opponent_FTA <- TeamStats$FTA
        Opponent_FTM <- TeamStats$FTM
        OppPlayedDirty <- TeamStats$OppMP
        Opponent_MP <- as.double(gsub(":00","",OppPlayedDirty))
        Opponent_TO <- TeamStats$OppTO
        Opponent_FGA <- TeamStats$OppFGA2+TeamStats$OppFGA3
        Opponent_FGM <- TeamStats$OppFGM2 + TeamStats$OppFGM3
        Opponent_ORB <- TeamStats$OppOffReb
        Opponent_PTS <- TeamStats$OppPts
        Opponent_Poss <- Opponent_FGA + ( 0.44 * Opponent_FTA ) + Opponent_TO - Opponent_ORB
        
        Team_DRB <- TeamStats$DefReb
        Team_BLK <- TeamStats$BLK
        Team_STL <- TeamStats$STL
        Team_PF <- TeamStats$FLC
        Team_Possessions <- Team_FGA + ( 0.44 * Team_FTA ) + Team_TO - Team_ORB
        
        DORper <- Opponent_ORB/(Opponent_ORB+Team_DRB) 
        DFGper <- Opponent_FGM/Opponent_FGA
        FMwt <- FmWtFun(DFGper,DORper)
        Stop1 <- STL + BLK * FMwt * (1 - 1.07 * DORper) + DRB * (1 - FMwt)
        Stop2 <- (((Opponent_FGA - Opponent_FGM - Team_BLK) / Team_MP) * FMwt * (1 - 1.07 * DORper) +
                    ((Opponent_TO - Team_STL) / Team_MP)) * MP + (PF / Team_PF) * 0.4 * Opponent_FTA * (1 - (Opponent_FTM / Opponent_FTA))^2
        Stops = Stop1+Stop2
        StopPer <- (Stops * Opponent_MP) / (Team_Possessions * MP)
        
        Team_Defensive_Rating <- 100 * (Opponent_PTS / Team_Possessions)
        D_Pts_per_ScPoss <- Opponent_PTS / (Opponent_FGM + (1 - (1 - (Opponent_FTM / Opponent_FTA))^2) * Opponent_FTA*0.4)
        IndivDRTG  <- round(Team_Defensive_Rating + 0.2 * (100 * D_Pts_per_ScPoss * (1 - StopPer) - Team_Defensive_Rating),2)
        
        
        
        AstTo <- round(Ast_To(AST,TO),2)
        UsgPer <- round(Usg(FGA,FTA,TO,Team_MP,MP,Team_FGA,Team_FTA,Team_TO),2)
        PPS <- round(PointsPerShot(PTS,FGA),2)
        TS <- round(TrueShootin(PTS,FGA,FTA),2)
        ORBPercentage <-round(ORBper(ORB,Team_MP,MP,Team_ORB,Opponent_DRB),2)
        DRBPercentage <- round(100 * (DRB * (Team_MP / 5)) / (MP * (Team_DRB + Opponent_ORB)),2)
        TotalRebPer <- round(100 * ((ORB+ DRB) * (Team_MP / 5)) / (MP * (((Team_ORB+Team_DRB) + (Opponent_DRB+Opponent_ORB)))),2)
        IndivNRTG <- round((IndivORTG -IndivDRTG),2)
        FGA3 <- if(PlayerStats$FGA3==0){1}else{PlayerStats$FGA3}
        FGA2 <- if(PlayerStats$FGA2==0){1}else{PlayerStats$FGA2} 
        TovPer <- round(100 * (TO/(FGA + 0.44*FTA*TO)),2)
        AstPer <- round(100 * (AST / (((MP / (Team_MP / 5)) * Team_FGM) - FGM)),2)
        eFGPer <- round(100 * ((FGM + 0.5 * FGM3) / FGA),2)
        FloorPer <- round(100 * (ScPoss / TotalPlayerPoss),2)
        StlPer <- round(100 * (STL * ((Team_MP /5)/(MP * Opponent_Poss))),2)
        
        AdvPlayerStats[player,] <- data.frame(Player = c(PlayerStats$Player),Games = c(PlayerStats$G), Min = c(MinutesDir1), "Ast/To" = c(AstTo), 
                                              PPS = c(PPS), DRTG = c(IndivDRTG), ORTG = c(IndivORTG),NRTG = c(IndivNRTG) ,"TS%"= c(TS),"USG%" = c(UsgPer),"ORB%"= c(ORBPercentage),
                                              "DRB%" = c(DRBPercentage), "TRB%" = c(TotalRebPer),FGA = c(FGA),
                                              FGA2= c(FGA2),FGA3= c(FGA3),FGM2= c(PlayerStats$FGM2),FGM3= c(PlayerStats$FGM3),
                                              StlPer =c(StlPer), AstPer = c(AstPer), eFGPer = c(eFGPer), FloorPer = c(FloorPer),
                                              Season = c(PlayerStats$Season),Team = c(Team), stringsAsFactors = FALSE)
      }
    }
    
    
   
  }else if(PlayerStats$Season == "2016-2017"){
    
    for(team in NumOfTeams16){
      TeamStats <- S1617[team,]
      Team <- TeamStats$Teams
    
      if(PlayerStats$Team == TeamStats$Teams){
        
        
        #Player Stats
        FGM <- PlayerStats$FGM2 + PlayerStats$FGM3
        FGA <- PlayerStats$FGA2 + PlayerStats$FGA3
        FTM <- PlayerStats$FTM
        
        PlayerStats$Team[is.na(PlayerStats$Team)] <- 0
        Team <- PlayerStats$Team
        FTA <- PlayerStats$FTA
        FGM3 <-PlayerStats$FGM3
        MinutesDir1 <- gsub(":00","",PlayerStats$Min)
        MP <- convertToMin(MinutesDir1)
        AST <- PlayerStats$As
        PTS <- PlayerStats$Pts
        TO <- PlayerStats$To
        ORB <- PlayerStats$O
        STL <- PlayerStats$St
        BLK <- PlayerStats$Fv
        DRB <- PlayerStats$D
        PF <-  PlayerStats$Cm
        
        #Team Stats
        Team_ORB <- TeamStats$OR
        Opponent_DRB <- TeamStats$OppDefReb
        Team_FGM <- TeamStats$FGM2+TeamStats$FGM3
        Team_FGA <- TeamStats$FGA2 + TeamStats$FGA3
        Team_FTM <- TeamStats$FTM
        Team_FTA <- TeamStats$FTA
        Team_FGM3 <- TeamStats$FGM3
        Team_TO <- TeamStats$TO
        MinutesDir <- gsub(":00","",TeamStats$Tmp)
        Team_MP <- convertToMin(MinutesDir1 = MinutesDir)
        Team_AST <- TeamStats$AST
        Team_PTS <- TeamStats$Pts
        #Function Variables
        
        Qst <- QAST(MP,Team_MP,Team_AST,AST,Team_FGM,FGM)
        
        
        
        Team_ORBp <- TeamORBpercentage (Team_ORB,Opponent_DRB)
        Team_Scoring_Poss <- TeamScoringPoss(Team_FGM,Team_FTM,Team_FTA)
        Team_Playp <- TeamPlay(Team_Scoring_Poss,Team_FGA,Team_FTA,Team_TO)
        Team_ORB_weight <- Team_ORB_Weight(Team_ORBp,Team_Playp)
        
        FGPart <- FG_Part(FGM,FTM,FGA,PTS,Qst)
        ASTPart <- AST_Part(Team_PTS,Team_FTM,Team_FGA,PTS,FTM,FGA,AST)
        FTPart <- FT_Part(FTM,FTA)
        ORBPart <- ORPart(ORB,Team_ORB_weight,Team_Playp)
        ScPoss <- ScPossesion(FGPart,ASTPart,FTPart,Team_ORB,Team_Scoring_Poss,Team_ORB_weight,Team_Playp,ORBPart)
        MissedFGPoss <- MissedFGPart(FGA,FGM,Team_ORBp)
        MissedFTPoss <- MissedFTPart(FTM,FTA)
        TotalPlayerPoss <- TotalPlayerPossesion(ScPoss,MissedFGPoss,MissedFTPoss,TO)
        PointsProd_FG_Part <- PointsProd_FG_part(FGM,FGM3,PTS,FTM,FGA,Qst)
        PointsProd_AST_Part <- PointsProd_AST_part(Team_FGM,Team_FGM3,Team_PTS,Team_FTM,Team_FGA,FGM,FGM3,PTS,FTM,FGA,AST)
        PointsProd_ORB_Part <- PointsProd_ORB_part(ORB,Team_ORB_weight,Team_Playp,Team_PTS,Team_FGM,Team_FTM,Team_FTA)
        PointsProd <- PointsProduction(PointsProd_FG_Part, PointsProd_AST_Part, PointsProd_ORB_Part,FTM,Team_ORB,Team_Scoring_Poss,Team_ORB_weight,Team_Playp)
        IndivORTG <- round(IndivOrtg(PointsProd,TotalPlayerPoss),2)
        
        #Drtg
        Opponent_FTA <- TeamStats$FTA
        Opponent_FTM <- TeamStats$FTM
        OppPlayedDirty <- TeamStats$OppMP
        if(str_sub(OppPlayedDirty, start= -3) ==":00"){
        Opponent_MP <- as.double(gsub(":00","",OppPlayedDirty))
        }else{
        Opponent_MP <- as.double(gsub(":16","",OppPlayedDirty))}
        Opponent_TO <- TeamStats$OppTO
        Opponent_FGA <- TeamStats$OppFGA2+TeamStats$OppFGA3
        Opponent_FGM <- TeamStats$OppFGM2 + TeamStats$OppFGM3
        Opponent_ORB <- TeamStats$OppOffReb
        Opponent_PTS <- TeamStats$OppPts
        Opponent_Poss <- Opponent_FGA + ( 0.44 * Opponent_FTA ) + Opponent_TO - Opponent_ORB
        
        Team_DRB <- TeamStats$DefReb
        Team_BLK <- TeamStats$BLK
        Team_STL <- TeamStats$STL
        Team_PF <- TeamStats$FLC
        Team_Possessions <- Team_FGA + ( 0.44 * Team_FTA ) + Team_TO - Team_ORB
        
        DORper <- Opponent_ORB/(Opponent_ORB+Team_DRB) 
        DFGper <- Opponent_FGM/Opponent_FGA
        FMwt <- FmWtFun(DFGper,DORper)
        Stop1 <- STL + BLK * FMwt * (1 - 1.07 * DORper) + DRB * (1 - FMwt)
        Stop2 <- (((Opponent_FGA - Opponent_FGM - Team_BLK) / Team_MP) * FMwt * (1 - 1.07 * DORper) +
                    ((Opponent_TO - Team_STL) / Team_MP)) * MP + (PF / Team_PF) * 0.4 * Opponent_FTA * (1 - (Opponent_FTM / Opponent_FTA))^2
        Stops = Stop1+Stop2
        StopPer <- (Stops * Opponent_MP) / (Team_Possessions * MP)
        
        Team_Defensive_Rating <- 100 * (Opponent_PTS / Team_Possessions)
        D_Pts_per_ScPoss <- Opponent_PTS / (Opponent_FGM + (1 - (1 - (Opponent_FTM / Opponent_FTA))^2) * Opponent_FTA*0.4)
        IndivDRTG  <- round(Team_Defensive_Rating + 0.2 * (100 * D_Pts_per_ScPoss * (1 - StopPer) - Team_Defensive_Rating),2)
        
        
        
        AstTo <- round(Ast_To(AST,TO),2)
        UsgPer <- round(Usg(FGA,FTA,TO,Team_MP,MP,Team_FGA,Team_FTA,Team_TO),2)
        PPS <- round(PointsPerShot(PTS,FGA),2)
        TS <- round(TrueShootin(PTS,FGA,FTA),2)
        ORBPercentage <-round(ORBper(ORB,Team_MP,MP,Team_ORB,Opponent_DRB),2)
        DRBPercentage <- round(100 * (DRB * (Team_MP / 5)) / (MP * (Team_DRB + Opponent_ORB)),2)
        TotalRebPer <- round(100 * ((ORB+ DRB) * (Team_MP / 5)) / (MP * (((Team_ORB+Team_DRB) + (Opponent_DRB+Opponent_ORB)))),2)
        IndivNRTG <- round((IndivORTG -IndivDRTG),2)
        FGA3 <- if(PlayerStats$FGA3==0){1}else{PlayerStats$FGA3}
        FGA2 <- if(PlayerStats$FGA2==0){1}else{PlayerStats$FGA2} 
        TovPer <-  round(100 * (TO/(FGA + 0.44*FTA*TO)),2)
        AstPer <- round(100 * (AST / (((MP / (Team_MP / 5)) * Team_FGM) - FGM)),2)
        eFGPer <- round(100 * ((FGM + 0.5 * FGM3) / FGA),2)
        FloorPer <- round(100 * (ScPoss / TotalPlayerPoss),2)
        StlPer <- round(100 * (STL * ((Team_MP /5)/(MP * Opponent_Poss))),2)
        
        
        
        AdvPlayerStats[player,] <- data.frame(Player = c(PlayerStats$Player),Games = c(PlayerStats$G), Min = c(MinutesDir1), "Ast/To" = c(AstTo), 
                                              PPS = c(PPS), DRTG = c(IndivDRTG), ORTG = c(IndivORTG),NRTG = c(IndivNRTG) ,"TS%"= c(TS),"USG%" = c(UsgPer),"ORB%"= c(ORBPercentage),
                                              "DRB%" = c(DRBPercentage), "TRB%" = c(TotalRebPer),FGA = c(FGA),
                                              FGA2= c(FGA2),FGA3= c(FGA3),FGM2= c(PlayerStats$FGM2),FGM3= c(PlayerStats$FGM3),
                                              StlPer =c(StlPer), AstPer = c(AstPer), eFGPer = c(eFGPer), FloorPer = c(FloorPer),
                                              Season = c(PlayerStats$Season),Team = c(Team), stringsAsFactors = FALSE)
      }
    }
    
    
   }else if(PlayerStats$Season == "2015-2016"){
    #AddData
  }else if(PlayerStats$Season == "2014-2015"){
    #AddDate
  }
 

  AdvPlayerStats[is.na(AdvPlayerStats)] <- 0
  write.csv(AdvPlayerStats, file = "AdvPlayerStats.csv",row.names = FALSE)
 }else{
   
 } 
}