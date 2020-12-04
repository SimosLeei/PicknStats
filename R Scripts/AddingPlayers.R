
library(dplyr)
library(rvest)
library(xml2)
library(tidyr)

Alba <- function (){
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=BER&seasoncode=E2019#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  Alba1920 <- as.data.frame(html_table(table))
  names(Alba1920) <- Alba1920[1,]
  Alba1920 <- Alba1920[-1,]
  Alba1920["Season"] <- "2019-2020"
  Alba1920["Team"] <- "ALBA BerlinBER"
  PlayersAlba <- rbind(Alba1920)
  
}

AnadoluEfes <- function(){
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=IST&seasoncode=E2019#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  Efes1920 <- as.data.frame(html_table(table))
  names(Efes1920) <- Efes1920[1,]
  Efes1920 <- Efes1920[-1,]
  Efes1920["Season"] <- "2019-2020"
  Efes1920["Team"] <- "Anadolu Efes IstanbulIST"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=IST&seasoncode=E2018#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  Efes1819 <- as.data.frame(html_table(table))
  names(Efes1819) <- Efes1819[1,]
  Efes1819 <- Efes1819[-1,]
  Efes1819["Season"] <- "2018-2019"
  Efes1819["Team"] <- "Anadolu Efes IstanbulIST"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=IST&seasoncode=E2017#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  Efes1718 <- as.data.frame(html_table(table))
  names(Efes1718) <- Efes1718[1,]
  Efes1718 <- Efes1718[-1,]
  Efes1718["Season"] <- "2017-2018"
  Efes1718["Team"] <- "Anadolu Efes IstanbulIST"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=IST&seasoncode=E2016#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  Efes1617 <- as.data.frame(html_table(table))
  names(Efes1617) <- Efes1617[1,]
  Efes1617 <- Efes1617[-1,]
  Efes1617["Season"] <- "2016-2017"
  Efes1617["Team"] <- "Anadolu Efes IstanbulIST"
  
  PlayersEfes <- rbind( Efes1920, Efes1819,Efes1718,Efes1617)
  
}

Armani <- function(){
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=MIL&seasoncode=E2019#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  Arm1920 <- as.data.frame(html_table(table))
  names(Arm1920) <- Arm1920[1,]
  Arm1920 <- Arm1920[-1,]
  Arm1920["Season"] <- "2019-2020"
  Arm1920["Team"] <- "AX Armani Exchange MilanMIL"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=MIL&seasoncode=E2018#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  Arm1819 <- as.data.frame(html_table(table))
  names(Arm1819) <- Arm1819[1,]
  Arm1819 <- Arm1819[-1,]
  Arm1819["Season"] <- "2018-2019"
  Arm1819["Team"] <- "AX Armani Exchange Olimpia MilanMIL"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=MIL&seasoncode=E2017#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  Arm1718 <- as.data.frame(html_table(table))
  names(Arm1718) <- Arm1718[1,]
  Arm1718 <- Arm1718[-1,]
  Arm1718["Season"] <- "2017-2018"
  Arm1718["Team"] <- "AX Armani Exchange Olimpia MilanMIL"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=MIL&seasoncode=E2016#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  Arm1617 <- as.data.frame(html_table(table))
  names(Arm1617) <- Arm1617[1,]
  Arm1617 <- Arm1617[-1,]
  Arm1617["Season"] <- "2016-2017"
  Arm1617["Team"] <- "EA7 Emporio Armani MilanMIL"
  PlayersArmani <- rbind(Arm1920, Arm1819,Arm1718,Arm1617)
}

RedStar <- function(){
  
url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=RED&seasoncode=E2019#!stats'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
Czr1920 <- as.data.frame(html_table(table))
names(Czr1920) <- Czr1920[1,]
Czr1920 <- Czr1920[-1,]
Czr1920["Season"] <- "2019-2020"
Czr1920["Team"] <- "Crvena Zvezda mts BelgradeRED"


url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=RED&seasoncode=E2017#!stats'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
Czr1718 <- as.data.frame(html_table(table))
names(Czr1718) <- Czr1718[1,]
Czr1718 <- Czr1718[-1,]
Czr1718["Season"] <- "2017-2018"
Czr1718["Team"] <- "Crvena Zvezda mts BelgradeRED"

url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=RED&seasoncode=E2016#!stats'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
Czr1617 <- as.data.frame(html_table(table))
names(Czr1617) <- Czr1617[1,]
Czr1617 <- Czr1617[-1,]
Czr1617["Season"] <- "2016-2017"
Czr1617["Team"] <- "Crvena Zvezda mts BelgradeRED"

PlayersRedStar <- rbind(Czr1920,Czr1718,Czr1617)
}

Cska <- function(){
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=CSK&seasoncode=E2019#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  Csk1920 <- as.data.frame(html_table(table))
  names(Csk1920) <- Csk1920[1,]
  Csk1920 <- Csk1920[-1,]
  Csk1920["Season"] <- "2019-2020"
  Csk1920["Team"] <- "CSKA MoscowCSK"
  
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=CSK&seasoncode=E2018#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  Csk1819 <- as.data.frame(html_table(table))
  names(Csk1819) <- Csk1819[1,]
  Csk1819 <- Csk1819[-1,]
  Csk1819["Season"] <- "2018-2019"
  Csk1819["Team"] <- "CSKA MoscowCSK"
  
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=CSK&seasoncode=E2017#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  Csk1718 <- as.data.frame(html_table(table))
  names(Csk1718) <- Csk1718[1,]
  Csk1718 <- Csk1718[-1,]
  Csk1718["Season"] <- "2017-2018"
  Csk1718["Team"] <- "CSKA MoscowCSK"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=CSK&seasoncode=E2016#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  Csk1617 <- as.data.frame(html_table(table))
  names(Csk1617) <- Csk1617[1,]
  Csk1617 <- Csk1617[-1,]
  Csk1617["Season"] <- "2016-2017"
  Csk1617["Team"] <- "CSKA MoscowCSK"
  
  PlayersCska <- rbind(Csk1920,Csk1819,Csk1718,Csk1617)

}

Barcelona <- function(){
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=BAR&seasoncode=E2019#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  BAR1920 <- as.data.frame(html_table(table))
  names(BAR1920) <- BAR1920[1,]
  BAR1920 <- BAR1920[-1,]
  BAR1920["Season"] <- "2019-2020"
  BAR1920["Team"] <- "FC BarcelonaBAR"
  
  
  
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=BAR&seasoncode=E2018#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  BAR1819 <- as.data.frame(html_table(table))
  names(BAR1819) <- BAR1819[1,]
  BAR1819 <- BAR1819[-1,]
  BAR1819["Season"] <- "2018-2019"
  BAR1819["Team"] <- "FC Barcelona LassaBAR"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=BAR&seasoncode=E2017#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  BAR1718 <- as.data.frame(html_table(table))
  names(BAR1718) <- BAR1718[1,]
  BAR1718 <- BAR1718[-1,]
  BAR1718["Season"] <- "2017-2018"
  BAR1718["Team"] <- "FC Barcelona LassaBAR"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=BAR&seasoncode=E2016#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  BAR1617 <- as.data.frame(html_table(table))
  names(BAR1617) <- BAR1617[1,]
  BAR1617 <- BAR1617[-1,]
  BAR1617["Season"] <- "2016-2017"
  BAR1617["Team"] <- "FC Barcelona LassaBAR"
  
  PlayersBAR <- rbind(BAR1920, BAR1819,BAR1718,BAR1617)
}

Bayern <- function(){
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=MUN&seasoncode=E2019#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  MUN1920 <- as.data.frame(html_table(table))
  names(MUN1920) <- MUN1920[1,]
  MUN1920 <- MUN1920[-1,]
  MUN1920["Season"] <- "2019-2020"
  MUN1920["Team"] <- "FC Bayern MunichMUN"
  
  
  
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=MUN&seasoncode=E2018#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  MUN1819 <- as.data.frame(html_table(table))
  names(MUN1819) <- MUN1819[1,]
  MUN1819 <- MUN1819[-1,]
  MUN1819["Season"] <- "2018-2019"
  MUN1819["Team"] <- "FC Bayern MunichMUN"
  
  PlayersMUN <- rbind(MUN1920, MUN1819)

}

Fener <-  function(){
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=ULK&seasoncode=E2019#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  ULK1920 <- as.data.frame(html_table(table))
  names(ULK1920) <- ULK1920[1,]
  ULK1920 <- ULK1920[-1,]
  ULK1920["Season"] <- "2019-2020"
  ULK1920["Team"] <- "Fenerbahce Beko IstanbulULK"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=ULK&seasoncode=E2018#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  ULK1819 <- as.data.frame(html_table(table))
  names(ULK1819) <- ULK1819[1,]
  ULK1819 <- ULK1819[-1,]
  ULK1819["Season"] <- "2018-2019"
  ULK1819["Team"] <- "Fenerbahce Beko IstanbulULK"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=ULK&seasoncode=E2017#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  ULK1718 <- as.data.frame(html_table(table))
  names(ULK1718) <- ULK1718[1,]
  ULK1718 <- ULK1718[-1,]
  ULK1718["Season"] <- "2017-2018"
  ULK1718["Team"] <- "Fenerbahce Dogus IstanbulULK"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=ULK&seasoncode=E2016#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  ULK1617 <- as.data.frame(html_table(table))
  names(ULK1617) <- ULK1617[1,]
  ULK1617 <- ULK1617[-1,]
  ULK1617["Season"] <- "2016-2017"
  ULK1617["Team"] <- "Fenerbahce IstanbulULK"
  
  PlayersULK <- rbind(ULK1920, ULK1819,ULK1718,ULK1617)
}

Khimki <-  function(){
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=KHI&seasoncode=E2019#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  KHI1920 <- as.data.frame(html_table(table))
  names(KHI1920) <- KHI1920[1,]
  KHI1920 <- KHI1920[-1,]
  KHI1920["Season"] <- "2019-2020"
  KHI1920["Team"] <- "Khimki Moscow RegionKHI"
  
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=KHI&seasoncode=E2018#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  KHI1819 <- as.data.frame(html_table(table))
  names(KHI1819) <- KHI1819[1,]
  KHI1819 <- KHI1819[-1,]
  KHI1819["Season"] <- "2018-2019"
  KHI1819["Team"] <- "Khimki Moscow RegionKHI"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=KHI&seasoncode=E2017#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  KHI1718 <- as.data.frame(html_table(table))
  names(KHI1718) <- KHI1718[1,]
  KHI1718 <- KHI1718[-1,]
  KHI1718["Season"] <- "2017-2018"
  KHI1718["Team"] <- "Khimki Moscow RegionKHI"
  
  PlayersKHI <- rbind(KHI1920, KHI1819,KHI1718)
}

Baskonia <- function(){
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=BAS&seasoncode=E2019#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  BAS1920 <- as.data.frame(html_table(table))
  names(BAS1920) <- BAS1920[1,]
  BAS1920 <- BAS1920[-1,]
  BAS1920["Season"] <- "2019-2020"
  BAS1920["Team"] <- "KIROLBET Baskonia Vitoria-GasteizBAS"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=BAS&seasoncode=E2018#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  BAS1819 <- as.data.frame(html_table(table))
  names(BAS1819) <- BAS1819[1,]
  BAS1819 <- BAS1819[-1,]
  BAS1819["Season"] <- "2018-2019"
  BAS1819["Team"] <- "KIROLBET Baskonia Vitoria-GasteizBAS"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=BAS&seasoncode=E2017#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  BAS1718 <- as.data.frame(html_table(table))
  names(BAS1718) <- BAS1718[1,]
  BAS1718 <- BAS1718[-1,]
  BAS1718["Season"] <- "2017-2018"
  BAS1718["Team"] <- "KIROLBET Baskonia Vitoria GasteizBAS"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=BAS&seasoncode=E2016#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  BAS1617 <- as.data.frame(html_table(table))
  names(BAS1617) <- BAS1617[1,]
  BAS1617 <- BAS1617[-1,]
  BAS1617["Season"] <- "2016-2017"
  BAS1617["Team"] <- "Baskonia Vitoria GasteizBAS"
  
  PlayersBAS <- rbind(BAS1920, BAS1819,BAS1718,BAS1617)
}

Maccabi <- function(){
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=TEL&seasoncode=E2019#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  TEL1920 <- as.data.frame(html_table(table))
  names(TEL1920) <- TEL1920[1,]
  TEL1920 <- TEL1920[-1,]
  TEL1920["Season"] <- "2019-2020"
  TEL1920["Team"] <- "Maccabi FOX Tel AvivTEL"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=TEL&seasoncode=E2018#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  TEL1819 <- as.data.frame(html_table(table))
  names(TEL1819) <- TEL1819[1,]
  TEL1819 <- TEL1819[-1,]
  TEL1819["Season"] <- "2018-2019"
  TEL1819["Team"] <- "Maccabi FOX Tel AvivTEL"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=TEL&seasoncode=E2017#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  TEL1718 <- as.data.frame(html_table(table))
  names(TEL1718) <- TEL1718[1,]
  TEL1718 <- TEL1718[-1,]
  TEL1718["Season"] <- "2017-2018"
  TEL1718["Team"] <- "Maccabi FOX Tel AvivTEL"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=TEL&seasoncode=E2016#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  TEL1617 <- as.data.frame(html_table(table))
  names(TEL1617) <- TEL1617[1,]
  TEL1617 <- TEL1617[-1,]
  TEL1617["Season"] <- "2016-2017"
  TEL1617["Team"] <- "Maccabi FOX Tel AvivTEL"
  
  PlayersTEL <- rbind(TEL1920, TEL1819,TEL1718,TEL1617)
}

Panathinaikos <- function(){
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=PAN&seasoncode=E2019#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  PAN1920 <- as.data.frame(html_table(table))
  names(PAN1920) <- PAN1920[1,]
  PAN1920 <- PAN1920[-1,]
  PAN1920["Season"] <- "2019-2020"
  PAN1920["Team"] <- "Panathinaikos OPAP AthensPAN"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=PAN&seasoncode=E2018#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  PAN1819 <- as.data.frame(html_table(table))
  names(PAN1819) <- PAN1819[1,]
  PAN1819 <- PAN1819[-1,]
  PAN1819["Season"] <- "2018-2019"
  PAN1819["Team"] <- "Panathinaikos OPAP AthensPAN"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=PAN&seasoncode=E2017#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  PAN1718 <- as.data.frame(html_table(table))
  names(PAN1718) <- PAN1718[1,]
  PAN1718 <- PAN1718[-1,]
  PAN1718["Season"] <- "2017-2018"
  PAN1718["Team"] <- "Panathinaikos Superfoods AthensPAN"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=PAN&seasoncode=E2016#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  PAN1617 <- as.data.frame(html_table(table))
  names(PAN1617) <- PAN1617[1,]
  PAN1617 <- PAN1617[-1,]
  PAN1617["Season"] <- "2016-2017"
  PAN1617["Team"] <- "Panathinaikos Superfoods AthensPAN"
  
  PlayersPAN <- rbind(PAN1920, PAN1819,PAN1718,PAN1617)
}

Real <- function(){
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=MAD&seasoncode=E2019#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  MAD1920 <- as.data.frame(html_table(table))
  names(MAD1920) <- MAD1920[1,]
  MAD1920 <- MAD1920[-1,]
  MAD1920["Season"] <- "2019-2020"
  MAD1920["Team"] <- "Real MadridMAD"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=MAD&seasoncode=E2018#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  MAD1819 <- as.data.frame(html_table(table))
  names(MAD1819) <- MAD1819[1,]
  MAD1819 <- MAD1819[-1,]
  MAD1819["Season"] <- "2018-2019"
  MAD1819["Team"] <- "Real MadridMAD"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=MAD&seasoncode=E2017#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  MAD1718 <- as.data.frame(html_table(table))
  names(MAD1718) <- MAD1718[1,]
  MAD1718 <- MAD1718[-1,]
  MAD1718["Season"] <- "2017-2018"
  MAD1718["Team"] <- "Real MadridMAD"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=MAD&seasoncode=E2016#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  MAD1617 <- as.data.frame(html_table(table))
  names(MAD1617) <- MAD1617[1,]
  MAD1617 <- MAD1617[-1,]
  MAD1617["Season"] <- "2016-2017"
  MAD1617["Team"] <- "Real MadridMAD"
  
  PlayersMAD <- rbind(MAD1920, MAD1819,MAD1718,MAD1617)
}

Olympiakos <- function(){
url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=OLY&seasoncode=E2019#!stats'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
Oly1920 <- as.data.frame(html_table(table))
names(Oly1920) <- Oly1920[1,]
Oly1920 <- Oly1920[-1,]
Oly1920["Season"] <- "2019-2020"
Oly1920["Team"] <- "Olympiacos PiraeusOLY"


url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=OLY&seasoncode=E2018#!stats'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
Oly1819 <- as.data.frame(html_table(table))
names(Oly1819) <- Oly1819[1,]
Oly1819 <- Oly1819[-1,]
Oly1819["Season"] <- "2018-2019"
Oly1819["Team"] <- "Olympiacos PiraeusOLY"

url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=OLY&seasoncode=E2017#!stats'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
Oly1718 <- as.data.frame(html_table(table))
names(Oly1718) <- Oly1718[1,]
Oly1718 <- Oly1718[-1,]
Oly1718["Season"] <- "2017-2018"
Oly1718["Team"] <- "Olympiacos PiraeusOLY"

url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=OLY&seasoncode=E2016#!stats'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
Oly1617 <- as.data.frame(html_table(table))
names(Oly1617) <- Oly1617[1,]
Oly1617 <- Oly1617[-1,]
Oly1617["Season"] <- "2016-2017"
Oly1617["Team"] <- "Olympiacos PiraeusOLY"

PlayersOly <- rbind(Oly1920, Oly1819,Oly1718,Oly1617)
}

Zalgiris <- function(){
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=ZAL&seasoncode=E2019#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  ZAL1920 <- as.data.frame(html_table(table))
  names(ZAL1920) <- ZAL1920[1,]
  ZAL1920 <- ZAL1920[-1,]
  ZAL1920["Season"] <- "2019-2020"
  ZAL1920["Team"] <- "Zalgiris KaunasZAL"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=ZAL&seasoncode=E2018#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  ZAL1819 <- as.data.frame(html_table(table))
  names(ZAL1819) <- ZAL1819[1,]
  ZAL1819 <- ZAL1819[-1,]
  ZAL1819["Season"] <- "2018-2019"
  ZAL1819["Team"] <- "Zalgiris KaunasZAL"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=ZAL&seasoncode=E2017#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  ZAL1718 <- as.data.frame(html_table(table))
  names(ZAL1718) <- ZAL1718[1,]
  ZAL1718 <- ZAL1718[-1,]
  ZAL1718["Season"] <- "2017-2018"
  ZAL1718["Team"] <- "Zalgiris KaunasZAL"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=ZAL&seasoncode=E2016#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  ZAL1617 <- as.data.frame(html_table(table))
  names(ZAL1617) <- ZAL1617[1,]
  ZAL1617 <- ZAL1617[-1,]
  ZAL1617["Season"] <- "2016-2017"
  ZAL1617["Team"] <- "Zalgiris KaunasZAL"
  
  PlayersZAL <- rbind(ZAL1920, ZAL1819,ZAL1718,ZAL1617)
}

Valencia <- function(){
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=PAM&seasoncode=E2019#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  PAM1920 <- as.data.frame(html_table(table))
  names(PAM1920) <- PAM1920[1,]
  PAM1920 <- PAM1920[-1,]
  PAM1920["Season"] <- "2019-2020"
  PAM1920["Team"] <- "Valencia BasketPAM"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=PAM&seasoncode=E2017#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  PAM1718 <- as.data.frame(html_table(table))
  names(PAM1718) <- PAM1718[1,]
  PAM1718 <- PAM1718[-1,]
  PAM1718["Season"] <- "2017-2018"
  PAM1718["Team"] <- "Valencia BasketPAM"
 
  PlayersPAM <- rbind(PAM1920, PAM1718)
}

Darussafaka <- function(){
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=DAR&seasoncode=E2018#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  DAR1819 <- as.data.frame(html_table(table))
  names(DAR1819) <- DAR1819[1,]
  DAR1819 <- DAR1819[-1,]
  DAR1819["Season"] <- "2018-2019"
  DAR1819["Team"] <- "Darussafaka Tekfen IstanbulDAR"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=DAR&seasoncode=E2016#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  DAR1617 <- as.data.frame(html_table(table))
  names(DAR1617) <- DAR1617[1,]
  DAR1617 <- DAR1617[-1,]
  DAR1617["Season"] <- "2016-2017"
  DAR1617["Team"] <- "Darussafaka Dogus IstanbulDAR"
  
  PlayersDAR <- rbind(DAR1819, DAR1617)
}

Bamberg <- function(){
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=BAM&seasoncode=E2017#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  BAM1718 <- as.data.frame(html_table(table))
  names(BAM1718) <- BAM1718[1,]
  BAM1718 <- BAM1718[-1,]
  BAM1718["Season"] <- "2017-2018"
  BAM1718["Team"] <- "Brose BambergBAM"
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=BAM&seasoncode=E2016#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  BAM1617 <- as.data.frame(html_table(table))
  names(BAM1617) <- BAM1617[1,]
  BAM1617 <- BAM1617[-1,]
  BAM1617["Season"] <- "2016-2017"
  BAM1617["Team"] <- "Brose BambergBAM"
  
  PlayersBAM <- rbind(BAM1718, BAM1617)
}

Galata <- function(){
url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=GAL&seasoncode=E2016#!stats'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
GAL1617 <- as.data.frame(html_table(table))
names(GAL1617) <- GAL1617[1,]
GAL1617 <- GAL1617[-1,]
GAL1617["Season"] <- "2016-2017"
GAL1617["Team"] <- "Galatasaray Odeabank IstanbulGAL"

PlayersGAL <- rbind(GAL1617)
}

Unics <- function(){
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=UNK&seasoncode=E2016#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  UNK1617 <- as.data.frame(html_table(table))
  names(UNK1617) <- UNK1617[1,]
  UNK1617 <- UNK1617[-1,]
  UNK1617["Season"] <- "2016-2017"
  UNK1617["Team"] <- "Unics KazanUNK"
  
  PlayersUNK <- rbind(UNK1617)
}

Canaria <- function(){
url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=CAN&seasoncode=E2018#!stats'
results <- read_html(url)
table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
CAN1819 <- as.data.frame(html_table(table))
names(CAN1819) <- CAN1819[1,]
CAN1819 <- CAN1819[-1,]
CAN1819["Season"] <- "2018-2019"
CAN1819["Team"] <- "Herbalife Gran CanariaCAN"

PlayersCAN <- rbind(CAN1819)
}

Buduconst <- function(){
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=BUD&seasoncode=E2018#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  BUD1819 <- as.data.frame(html_table(table))
  names(BUD1819) <- BUD1819[1,]
  BUD1819 <- BUD1819[-1,]
  BUD1819["Season"] <- "2018-2019"
  BUD1819["Team"] <- "Buducnost VOLI PodgoricaBUD"
  
  PlayersBUD <- rbind(BUD1819)
}

Zenit <- function(){
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=DYR&seasoncode=E2019#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  DYR1920 <- as.data.frame(html_table(table))
  names(DYR1920) <- DYR1920[1,]
  DYR1920 <- DYR1920[-1,]
  DYR1920["Season"] <- "2019-2020"
  DYR1920["Team"] <- "Zenit St PetersburgDYR"
  
  PlayersDYR <- rbind(DYR1920) }
  
Asvel <- function(){
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=ASV&seasoncode=E2019#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  ASV1920 <- as.data.frame(html_table(table))
  names(ASV1920) <- ASV1920[1,]
  ASV1920 <- ASV1920[-1,]
  ASV1920["Season"] <- "2019-2020"
  ASV1920["Team"] <- "LDLC ASVEL VilleurbanneASV"
  
  PlayersASV <- rbind(ASV1920)}

Malaga <- function(){
  
  url <-'https://www.euroleague.net/competition/teams/showteam?clubcode=MAL&seasoncode=E2017#!stats'
  results <- read_html(url)
  table <-html_node(results,xpath ='//*[@id="tblClubPlayersSeasonStatistics"]')
  MAL1718 <- as.data.frame(html_table(table))
  names(MAL1718) <- MAL1718[1,]
  MAL1718 <- MAL1718[-1,]
  MAL1718["Season"] <- "2017-2018"
  MAL1718["Team"] <- "Unicaja MalagaMAL"
  
  PlayerMAL <- rbind(MAL1718)
}

Alba <- Alba()
Efes <- AnadoluEfes()
Armani <- Armani()
RedStar <- RedStar()
Cska <- Cska()
Barcelona <- Barcelona()
Bayern <- Bayern()
Fener <- Fener()
Khimki <- Khimki()
Baskonia <- Baskonia()
Maccabi <- Maccabi()
Olympiakos <- Olympiakos()
Panathinaikos <- Panathinaikos()
Real <- Real()
Zalgiris <- Zalgiris()
Malaga <- Malaga()
Valencia <- Valencia()
Asvel <- Asvel()
Zenit <- Zenit()
Galata <- Galata()
Unics <- Unics()
Canaria <- Canaria()
Buduconst <- Buduconst()
Darussafaka <- Darussafaka()
Bamberg <- Bamberg()


PlayerDfDirty <-rbind(Alba,Efes,Armani,RedStar,Cska,Barcelona,Bayern,Fener,
                      Olympiakos,Khimki,Baskonia,Maccabi,Panathinaikos,Real,
                      Zalgiris,Malaga,Valencia,Asvel,Zenit,Galata,Unics,
                      Canaria,Buduconst,Darussafaka,Bamberg) %>%
    separate('2FG',c("FGM2", "FGA2"),"/")%>%
  separate('3FG',c("FGM3","FGA3"),"/")%>%
  separate('FT',c("FTM","FTA"),"/")
PlayerDfDirty[is.na(PlayerDfDirty)] <- 0
PlayerDfDirty[PlayerDfDirty == ""] <- 0
PlayerDfDirty$Team <- substr(PlayerDfDirty$Team,1,nchar(PlayerDfDirty$Team)-3)


PlayerDf <- subset(PlayerDfDirty,Player!= "Average" & Player!="Totals")
write.csv(PlayerDf, file ="PlayersDatabase.csv",row.names = FALSE)
