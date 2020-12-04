library(readxl)
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(plotly)
library(rvest)
library(shinyWidgets)
library(stringr)
library(shinyBS)
library(BBmisc)
library(shinydashboard)
library(scales)



Players0db <- as.data.frame(read.csv("AdvPlayerStats.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
Playersdb <- Players0db[as.logical(rowSums(Players0db != 0)), ]
AP1920 <- as.data.frame(read.csv("AdvAP1920.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))                          

ui <- dashboardPage (
  dashboardHeader( title = "B-Ball Statistics"),
  dashboardSidebar (sidebarMenu(menuItem("Teams", tabName = "Teams"), menuItem("Players", tabName = "Players"))),
  dashboardBody(  tabItems(
    tabItem(tabName = "Teams",
            
    tags$head(tags$style(HTML(' 
            body{
            
         
              font-family: fantasy, sans-serif;
                }
                      /* Tab Color*/         
            .nav-tabs-custom>.nav-tabs>li.active {
              border-top-color: #EA7C14;
                }
                 
            table.dataTable th.dt-right, table.dataTable td.dt-right {
              text-align:center;
                }
                            
            .dataTables_wrapper.no-footer div.dataTables_scrollHead table.dataTable, .dataTables_wrapper.no-footer div.dataTables_scrollBody>table{
              text-align:center;
               }
            table.dataTable thead .sorting, table.dataTable thead .sorting_asc, table.dataTable thead .sorting_desc, table.dataTable thead .sorting_asc_disabled, table.dataTable thead .sorting_desc_disabled{
                text-align:center;
                }
                         
                         
              .shiny-notification {
             position:fixed;
             top: calc(50%);
             left: calc(50%);
              }
            
         
                        
            /* Title */
           .skin-blue .main-header .logo {
             background-color: black;
             color: #ECD8C5;
             font-family: fantasy, sans-serif;
             font-size: 24px;
              }

                  /* Title when hovered */
            .skin-blue .main-header .logo:hover {
              background-color: #EA7C14;
            }
              
              
              
              /* Header Bar*/
              .skin-blue .main-header .navbar {
                     background-color:#67685F;
                     color: #ECD8C5;
                   }        
                 
                 /* sidebar Background Color */
                   .skin-blue .main-sidebar {
                     background-color:#67685F;
                   }
                 
                  /* active selected tab in the sidebarmenu */
                   .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                     background-color: #ECD8C5;
                     border-left-color:#EA7C14;
                     color:black;
                   }
                   
                    .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                        border-left-color: black;
                    }
                 
                 /* other links in the sidebarmenu */
                   .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                     background-color:#67685F;
                       font-family: fantasy, sans-serif;
                       color: #ECD8C5; 
                   }
                                           
                 /* other links in the sidebarmenu when hovered */
                   .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                      background-color: #ECD8C5;
                      color: black;
                                             }
          div.box{
            border-top-color : #67685F;
          }
          
          .bg-red{
          background-color:#7E2E2E !important;
          }
          
          
          .bg-yellow{
            background-color: #347396 !important;
          }
          
          .bg-green{
            background-color: #507B6C !important;
          }
          div.small-box h3{
              font-size:29px;
          }  
          .small-box p{
            font-size: 20px;
          }
          .alert-info {
            background-color: #ECD8C5 !important;
            border-color: #67685F;
            color: #67685F !important;
          }
          
          .box-header .box-title, .box-header {
              font-size: 18px;
              text-align:center;
              font-weight: bold;
             }
          '))), fluidRow( box(width = 3, 
      selectInput("EuroleaguePhases", "Select Phase:",c("All Phases" ,"Regular Season",  "Playoffs", "Final 4")),
      selectInput("EuroleagueSeasons", "Select Season:",c("2019-2020" ,"2018-2019", "2017-2018", "2016-2017")),
      selectInput("HA", "Home/Away Option:",c("Both" ,"Home", "Away"))
      ),
    tabBox( width = 9,
        tabPanel(title = "Team Ratings", plotlyOutput("RatingPlot")),
        tabPanel(title = "Team Pace",plotlyOutput("PacePlot")),
        tabPanel(title = "True Shooting BubblePlot",plotlyOutput("ShootingPlot")),
        tabPanel(title = "Adv.Table",dataTableOutput("table"))
        ),
      box(width = 2,title = "Best Efg%",dataTableOutput("Efg")),box(width = 2,title = "Best Ast/TO ratio",dataTableOutput("ASTO")),box(width = 2,title ="Best Rebounding Teams",dataTableOutput("Reb")),
      box(width = 2,title ="Best Rebounding Teams",dataTableOutput("Rseb")), box(width = 2,title ="Best Rebounding Teams",dataTableOutput("Rsseb")),box(width = 2,title ="Best Rebounding Teams",dataTableOutput("Reeb")),
      box( width = 12,  valueBoxOutput("OffenseBox"),valueBoxOutput("DefenseBox"),valueBoxOutput("PaceBox"))
      )
    ),
    tabItem(tabName = "Players",fluidRow( box(width = 3,  bsAlert("alert"), 
                                              pickerInput("Player", "Select Player(s):",Playersdb$Player,  multiple = TRUE , options = list('live-Search' = TRUE,`actions-box` = TRUE)),
                                              pickerInput("Team", "Select Team(s):",substr(AP1920$Teams,1,nchar(AP1920$Teams)-3),selected = NULL,multiple = TRUE ,options = list('live-Search' = TRUE,`actions-box` = TRUE)),
                                              selectInput("Season", "Select Season:",c("2019-2020" ,"2018-2019", "2017-2018", "2016-2017"),selected = "2019-2020"),
                                              
                                              
    ),
      tabBox( width = 9,
              tabPanel(title = "Player Ratings ", plotlyOutput("PlayerRatingPlot")),
              tabPanel(title = "Player Comparison" ,plotlyOutput("Radar")),
              tabPanel(title = "True Shooting BubblePlot",plotlyOutput("TrueShootingPlot")), 
              tabPanel(title = "Usage-NRTG",plotlyOutput("UsagePlot"))),
    box( width = 12,valueBoxOutput("Tip")),
    
              )
         )
      )
    )
  )


  

 
server <- function(input, output,  session) {
  
  FloorPer <- round(rescale(Playersdb$FloorPer,to = c(0,5)),2)
  AstPer <- round(rescale(Playersdb$AstPer,to = c(0,5)),2)
  eFG <- round(rescale(Playersdb$eFGPer,to = c(0,5)),2)
  USG <- round(rescale(Playersdb$USG,to = c(0,5)),2)
  TRB <- round(rescale(Playersdb$TRB,to = c(0,5)),2)
  StlPer <- round(rescale(Playersdb$StlPer,to = c(0,5)),2)
 
  
  normalizePlayers <- data.frame(Player = c(Playersdb$Player),FloorPer =c(FloorPer),StlPer =c(StlPer),AstPer = c(AstPer),
                                 eFG =c(eFG),USG =c(USG),TRB =c(TRB),Season = c(Playersdb$Season),Team = c(Playersdb$Team))
  
  
  

  
  
  #Team Plots
  designScatterPlot <- function(appTable){
    if(nrow(appTable)== 0){}
    else{
    DefensiveRating<- appTable$DRTG
    OffensiveRating<- appTable$ORTG
    tCustom <- list( family = "sans serif",
                     size = 10,
                     color = toRGB("black"))
    Teams <- appTable$Teams
    AbbTeams <-str_sub(Teams, -3,-1)
    plot <- plot_ly(data = appTable, type="scatter", x= OffensiveRating,mode = "markers",
                    y = DefensiveRating,text = Teams,hoverinfo = "skip",showlegend = FALSE)%>%
      add_trace(mode= "markers", text = Teams, name=("Rating"),marker= list(    
        color = " #EA7C14",
        size =26,
        line = list(color = "#67685F",
                    width = 2)),
                        hovertemplate = paste(' <i>Team:</i> <b>%{text}</b>',
                                     '<br><i>ORTG: <b> %{x} </b></i></br>',
                                     '<i>DRTG: <b> %{y} </b></i>'))%>%
      add_text(text =  AbbTeams, textfont = tCustom,textposition = "center") %>%
      
      layout(    
          xaxis = list(          
          title = "Offensive Ratings",      
          showgrid = F),       
        yaxis = list(         
          title = "Defensive Ratings")     
      )
    }
      }
  
  designPacePlot <- function(appTable){
    if(nrow(appTable)== 0){}
    else{
    pace_y <- appTable$Pace 
        paceplot <- plot_ly(appTable,x = appTable$Teams,y= pace_y ,name= "Pace", mode="lines+markers",line = list(color= "#347396 ",width =3),
                            marker = list(color="#7E2E2E",size= 10,  symbol = 'square'),
                            type ='scatter',showlegend = F)%>%
          add_trace ( appTable, x = appTable$Teams , y = mean(appTable$Pace),mode = "markers+lines",line = list(color= "#67685F ",width =3), mode("lines"),
                      marker = list(opacity = 0),hoverinfo = "skip")}
  }
  
  desingBubblePlot <- function(appTable){
    if(nrow(appTable)== 0){}
    else{
    
    data <- appTable
    gapCounter <- median(data$FGA)
    actualSize <- data$FGA - gapCounter
    colorList<- data$TS
    TrueShootingP <-  as.numeric(sub("%", "",colorList,fixed=TRUE))/100
    Teams <- appTable$Teams
    AbbTeams <-str_sub(Teams, -3,-1)
    TwoPointP <- data$FGM2P
    ThreePointP <- data$FGM3P
    
    configureText <- list(
      family = "sans serif",
      size = 10,
      color = toRGB("black"))
    
    customColor <- c('#E9E61B','#DCD922','#EA7C14', '#C12424') 
    axiySet <- list(
      autotick = FALSE,
      showgrid = TRUE,
      ticks = "outside",
      tick0 = min(TwoPointP),
      dtick = 2,
      ticklen = 8,
      tickwidth = 2,
      tickcolor = toRGB("black")
    )
    axixSet <- list(
      
      autotick = FALSE,
      showgrid = TRUE,
      ticks = "outside",
      tick0 = min(ThreePointP),
      dtick = 1.5,
      ticklen = 8,
      tickwidth = 2,
      tickcolor = toRGB("black")
    )
  
    
    ShootingPlot <- plot_ly(data,x = TwoPointP, y = ThreePointP,type = 'scatter',name = "TrueShooting" ,
                            mode = 'markers',size = actualSize,showlegend = F,
                            color = TrueShootingP, colors = customColor,  sizes = c(10, 60),marker = list(opacity = 0)
                            )%>%
        add_trace(mode = "markers",marker = list(opacity = 0.7, sizemode = 'diameter'),hoverinfo = 'text',
                  text = ~paste('Team:', Teams, '<br>True Shooting:',
                                colorList, '<br> 2PT%: ',TwoPointP ,'<br> 3PT%: ', ThreePointP ,
                                '<br> FGA: ',data$FGA ))%>% 
      add_text(text = AbbTeams,textfont= configureText,hoverinfo = "skip")%>%
      layout(xaxis=axixSet,yaxis = axiySet)
    
    
    ax <- list(zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE)
    
    
    legendSize <- seq.default(from = min(data$FGA),to = max(data$FGA), by = ((max(data$FGA)-min(data$FGA))/2) )
    
    plotsLegend <- plot_ly(x = 1, y = legendSize, size = legendSize,type = "scatter", hoverinfo = "skip", showlegend = F,
                           mode ="markers", text = paste("<b>FGA Size:</b>",legendSize), 
                           sizes = c(10,60),mode = "markers",
                           marker = list(sizemode = 'diameter',color = "black",
                                         line = list(color = "#EA7C14", width = 2))) %>%
      add_text(textfont = configureText,textposition = "top center") %>%
      layout(xaxis = ax,
             yaxis = list(showgrid = FALSE,showticklabels = FALSE))
    subplot(ShootingPlot,plotsLegend,widths = c(0.8, 0.2))%>%
      layout(xaxis = list(title=  "2 Point Shots(%Made)"),yaxis = list(title = "3 Point Shots(%Made)"))
      
    }
    }
  
  decideTable <- function (){
  
    if(input$EuroleagueSeasons == "2019-2020"){
      
      
      #Both Option
      if(input$HA == "Both"){    
        
        if(input$EuroleaguePhases=="All Phases"){
          appTable <- AP1920
           }
        else if(input$EuroleaguePhases=="Regular Season"){
          appTable <- AP1920
        }
        else if(input$EuroleaguePhases=="Playoffs"){
          appTable <- data.frame(A = numeric())
         
        }
        else if(input$EuroleaguePhases=="Final 4"){
          appTable <- data.frame(A = numeric())
        }
        
      }
      #Home games only Option 
      else if(input$HA == "Home"){
        
        if(input$EuroleaguePhases=="All Phases"){}
        else if(input$EuroleaguePhases=="Regular Season"){}
        else if(input$EuroleaguePhases=="Playoffs"){
          appTable <- data.frame(A = numeric())
        }
        else if(input$EuroleaguePhases=="Final 4"){
          appTable <- data.frame(A = numeric())
        }
        
      }
      
      #Away games only Option
      else if(input$HA == "Away"){
        
        if(input$EuroleaguePhases=="All Phases"){
          
        }
        else if(input$EuroleaguePhases=="Regular Season"){
          
        }
        else if(input$EuroleaguePhases=="Playoffs"){
          appTable <- data.frame(A = numeric())
        }
        else if(input$EuroleaguePhases=="Final 4"){
          appTable <- data.frame(A = numeric())
                  }}
    }
    
    #Season 2018-2019 If Statements    
    else if(input$EuroleagueSeasons == "2018-2019"){
      
      #Both Option
      if(input$HA == "Both"){    
        
        if(input$EuroleaguePhases=="All Phases"){
          appTable <- AP1819  
           }
        else if(input$EuroleaguePhases=="Regular Season"){
          appTable <- RS1819
           }
        else if(input$EuroleaguePhases=="Playoffs"){
          appTable <- PO1819 
           }
        else if(input$EuroleaguePhases=="Final 4"){
          appTable <- FF1819
           }
        
      }
      
      #Home games only Option  
      else if(input$HA == "Home"){
        
        if(input$EuroleaguePhases=="All Phases"){
          appTable <- HAP1819
           }
        else if(input$EuroleaguePhases=="Regular Season"){
          appTable <- HRS1819
           }
        else if(input$EuroleaguePhases=="Playoffs"){
          appTable <- HPO1819
           }
        else if(input$EuroleaguePhases=="Final 4"){
          appTable <- FF1819
           }
      }
      
      #Away games only Option  
      else if(input$HA == "Away"){
        
        if(input$EuroleaguePhases=="All Phases"){
          appTable <- AAP1819
           }
        else if(input$EuroleaguePhases=="Regular Season"){
          appTable <- ARS1819
           
        }
        else if(input$EuroleaguePhases=="Playoffs"){
          appTable <- APO1819
           
        }
        else if(input$EuroleaguePhases=="Final 4"){
          appTable <- FF1819
           }
      }
    }
    
    #Season 2017-2018 If Statements    
    else if(input$EuroleagueSeasons == "2017-2018"){
      
      #Both Option
      if(input$HA == "Both"){    
        
        if(input$EuroleaguePhases=="All Phases"){
          appTable <- AP1718
           }
        else if(input$EuroleaguePhases=="Regular Season"){
          appTable <- RS1718
           }
        else if(input$EuroleaguePhases=="Playoffs"){
          appTable <- PO1718
           }
        else if(input$EuroleaguePhases=="Final 4"){
          appTable <- FF1718
           }
      }
      
      
      #Home games only Option 
      else if(input$HA == "Home"){
        
        if(input$EuroleaguePhases=="All Phases"){
          appTable <- HAP1718
           }
        else if(input$EuroleaguePhases=="Regular Season"){
          appTable <- HRS1718
           }
        else if(input$EuroleaguePhases=="Playoffs"){
          appTable <- HPO1718
           }
        
        else if(input$EuroleaguePhases=="Final 4"){
          appTable <- FF1718
           }
      }
      
      #Away games only Option  
      else if(input$HA == "Away"){
        
        if(input$EuroleaguePhases=="All Phases"){
          appTable <- AAP1718
           }
        else if(input$EuroleaguePhases=="Regular Season"){
          appTable <- ARS1718
           }
        else if(input$EuroleaguePhases=="Playoffs"){
          appTable <- APO1718
           }
        else if(input$EuroleaguePhases=="Final 4"){
          appTable <- FF1718
           }
      }
    }
    
    #Season 2016-2017 If Statements     
    else if(input$EuroleagueSeasons == "2016-2017"){
      
      
      #Both Option
      if(input$HA == "Both"){    
        
        if(input$EuroleaguePhases=="All Phases"){
          appTable <- AP1617
           }
        else if(input$EuroleaguePhases=="Regular Season"){
          appTable <- RS1617
           }
        else if(input$EuroleaguePhases=="Playoffs"){
          appTable <- PO1617
           }
        else if(input$EuroleaguePhases=="Final 4"){
          appTable <- FF1617
           }
      }
      
      #Home games only Option  
      else if(input$HA == "Home"){
        
        if(input$EuroleaguePhases=="All Phases"){
          appTable <- HAP1617
           }
        else if(input$EuroleaguePhases=="Regular Season"){
          appTable <- HRS1617
           }
        else if(input$EuroleaguePhases=="Playoffs"){
          appTable <- HPO1617
           }
        else if(input$EuroleaguePhases=="Final 4"){
          appTable <- FF1617
           }
        
      }
      
      #Away games only Option  
      else if(input$HA == "Away"){
        
        if(input$EuroleaguePhases=="All Phases"){
          appTable <- AAP1617
           }
        else if(input$EuroleaguePhases=="Regular Season"){
          appTable <- ARS1617
           }
        else if(input$EuroleaguePhases=="Playoffs"){
          appTable <- APO1617
           }
        else if(input$EuroleaguePhases=="Final 4"){
          appTable <- FF1617
           }
      }
    }
  }
  
  #Player Plots
  designPlayerScatterPlot <- function(appTable){
    closeAlert(session, "BubbleAlert")
    DefensiveRating<- appTable$DRTG
    OffensiveRating<- appTable$ORTG
    
    tCustom <- list( family = "sans serif",
                     size = 10,
                     color = toRGB("black"))
    Players <- appTable$Player
    Team <- appTable$Team
    PlayerName <- sub("\\,.*", "", Players)
    
   
    plot <- plot_ly(data = appTable, type="scatter", x= OffensiveRating,mode = "markers",
                    y = DefensiveRating,showlegend = FALSE, hoverinfo = 'text',  
                    text = paste('<br><b>Player:</b><i>',
                                 appTable$Player,'</i><br><b>ORTG:</b><i>', appTable$ORTG,'</i> <br> <b>DRTG: </b> <i>', appTable$DRTG,'</i>'),
                    transforms = list(
                      list(
                        type = 'groupby',
                        groups = appTable$Team,
                        styles = list(
                          list(target = "ALBA Berlin", value = list(marker =list(color = '#B4B8AB'))),
                          list( target = "Anadolu Efes Istanbul" , value = list(marker =list(color = '#153243'))),
                          list(target = "EA7 Emporio Armani Milan", value = list(marker =list(color = '#BA3B46'))),
                          list( target = "AX Armani Exchange Olimpia Milan" , value = list(marker =list(color = '#BA3B46'))),
                          list( target = "AX Armani Exchange Milan" , value = list(marker =list(color = '#BA3B46'))),
                          list( target = "CSKA Moscow" , value = list(marker =list(color = '#19297C'))),
                          list( target = "FC Barcelona Lassa" , value = list(marker =list(color = '#5B3758'))),
                          list( target = "FC Barcelona" , value = list(marker =list(color = '#5B3758'))),
                          list( target = "Fenerbahce Istanbul" , value = list(marker =list(color = '#0A1128'))),
                          list( target = "Fenerbahce Dogus Istanbul" , value = list(marker =list(color = '#0A1128'))),
                          list( target = "Fenerbahce Beko Istanbul" , value = list(marker =list(color = '#0A1128'))),
                          list( target = "Olympiacos Piraeus" , value = list(marker =list(color = '#F24236'))),
                          list( target = "Baskonia Vitoria Gasteiz" , value = list(marker =list(color = '#643A71'))),
                          list( target = "KIROLBET Baskonia Vitoria Gasteiz" , value = list(marker =list(color = '#643A71'))),
                          list( target = "KIROLBET Baskonia Vitoria-Gasteiz" , value = list(marker =list(color = '#643A71'))),
                          list( target = "Maccabi FOX Tel Aviv" , value = list(marker =list(color = '#F5F749'))),
                          list( target = "Panathinaikos Superfoods Athens" , value = list(marker =list(color = '#14591D'))),
                          list( target = "Panathinaikos OPAP Athens" , value = list(marker =list(color = '#14591D'))),
                          list( target = "Real Madrid" , value = list(marker =list(color = '#D6CFCB'))),
                          list( target = "Zalgiris Kaunas" , value = list(marker =list(color = '#83B692'))),
                          list( target = "Galatasaray Odeabank Istanbul" , value = list(marker =list(color = '#A6808C'))),
                          list( target = "Unics Kazan" , value = list(marker =list(color = '#61C9A8'))),
                          list( target = "Darussafaka Dogus Istanbul" , value = list(marker =list(color = '#C7EFCF'))),
                          list( target = "Darussafaka Tekfen Istanbul" , value = list(marker =list(color = '#C7EFCF'))),
                          list( target = "Brose Bamberg" , value = list(marker =list(color = '#E5EBEA'))),
                          list( target = "Khimki Moscow Region" , value = list(marker =list(color = '#507DBC'))),
                          list( target = "Zenit St Petersburg" , value = list(marker =list(color = '#BBD1EA'))),
                          list( target = "LDLC ASVEL Villeurbanne" , value = list(marker =list(color = '#333745'))),
                          list( target = "Unicaja Malaga" , value = list(marker =list(color = '#315659'))),
                          list( target = "Valencia Basket" , value = list(marker =list(color = '#FDD692'))),
                          list( target = "FC Bayern Munich" , value = list(marker =list(color = '#EC7357'))),
                          list( target = "Buducnost VOLI Podgorica" , value = list(marker =list(color = '#A7A2A9'))),
                          list( target = "Herbalife Gran Canaria" , value = list(marker =list(color = '#131B23'))),
                          list(target = "Crvena Zvezda mts Belgrade", value = list(marker =list(color = '#FE5F55')))
                        )
                      )
                    ),
    marker =list(opacity =0.7,size = 20))%>%
       layout( 
          xaxis = list(          
          title = "Offensive Ratings",      
          showgrid = F),       
        yaxis = list(         
          title = "Defensive Ratings")     
      )
    
  }
  
  designRadar <- function(appTable){
    
    closeAlert(session, "BubbleAlert")
    
    
    
   
    setNormalizedPlayer <-  normalizePlayers[ which(normalizePlayers$Player %in% appTable$Player
                                                    & normalizePlayers$Season %in% appTable$Season
                                                    & normalizePlayers$Team %in% appTable$Team), ]
 
    if(length(setNormalizedPlayer$Player) <= 4){
    
    radarPlot <- plot_ly(
      type = 'scatterpolar', fill = 'toself')%>% 
      add_trace(
      r = c(setNormalizedPlayer[1,]$FloorPer, setNormalizedPlayer[1,]$StlPer, setNormalizedPlayer[1,]$AstPer,
           setNormalizedPlayer[1,]$eFG, setNormalizedPlayer[1,]$USG, setNormalizedPlayer[1,]$TRB),
           mode = "markers", fillcolor = '#77CBB9',opacity = 0.7, marker= list(color= "#506C64",size =7),
      hoverinfo = "text", 
      text = ~paste( '<br><b> Player: </b><i>',appTable[1,1],'</i><br><b> Floor%: </b><i>',appTable[1,22],'%','</i><br><b> STL%: </b><i>',
                     appTable[1,19],'%','</i><br><b> AST%: </b><i>',appTable[1,20],'%','</i><br><b> USG%: </b><i>',appTable[1,10],'%',
                     '</i><br><b> eFG%%: </b><i>',appTable[1,21],'%','</i><br><b> TRB%: </b><i>',appTable[1,13],'%'),
      theta = c('<b>Floor%</b>','<b>Steals%</b>','<b>Assists%</b>', '<b>eFG%</b>', '<b>Usage%</b>', '<b>Tot.Rebound%</b>'),name = paste("",setNormalizedPlayer[1,]$Player))%>%
      add_trace(
        r = c(setNormalizedPlayer[2,]$FloorPer, setNormalizedPlayer[2,]$StlPer, setNormalizedPlayer[2,]$AstPer,
              setNormalizedPlayer[2,]$eFG, setNormalizedPlayer[2,]$USG, setNormalizedPlayer[2,]$TRB),
        mode = "markers",hoverinfo = "text",fillcolor = '#BF9ACA',opacity = 0.7,marker= list(color= "#8E4162",size =7),
        hoverinfo = "text", 
        text = ~paste( '<br><b> Player: </b><i>',appTable[2,1],'</i><br><b> Floor%: </b><i>',appTable[2,22],'%','</i><br><b> STL%: </b><i>',
                       appTable[2,19],'%','</i><br><b> AST%: </b><i>',appTable[2,20],'%','</i><br><b> USG%: </b><i>',appTable[2,10],'%',
                       '</i><br><b> eFG%%: </b><i>',appTable[2,21],'%','</i><br><b> TRB%: </b><i>',appTable[2,13],'%'),
        theta = c('<b>Floor%</b>','<b>Steals%</b>','<b>Assists%</b>', '<b>eFG%</b>', '<b>Usage%</b>', '<b>Tot.Rebound%</b>'),name = paste("",setNormalizedPlayer[2,]$Player))%>%
      add_trace(
        r = c(setNormalizedPlayer[3,]$FloorPer, setNormalizedPlayer[3,]$StlPer, setNormalizedPlayer[3,]$AstPer,
              setNormalizedPlayer[3,]$eFG, setNormalizedPlayer[3,]$USG, setNormalizedPlayer[3,]$TRB),
        mode = "markers",hoverinfo = "text",fillcolor = '#FAF2A1',opacity = 0.7,marker= list(color= "#FCAB10",size =7),
        hoverinfo = "text", 
        text = ~paste( '<br><b> Player: </b><i>',appTable[3,1],'</i><br><b> Floor%: </b><i>',appTable[3,22],'%','</i><br><b> STL%: </b><i>',
                       appTable[3,19],'%','</i><br><b> AST%: </b><i>',appTable[3,20],'%','</i><br><b> USG%: </b><i>',appTable[3,10],'%',
                       '</i><br><b> eFG%%: </b><i>',appTable[3,21],'%','</i><br><b> TRB%: </b><i>',appTable[3,13],'%'),
        theta = c('<b>Floor%</b>','<b>Steals%</b>','<b>Assists%</b>', '<b>eFG%</b>', '<b>Usage%</b>', '<b>Tot.Rebound%</b>'),name = paste("",setNormalizedPlayer[3,]$Player))%>%
      add_trace(
        r = c(setNormalizedPlayer[4,]$FloorPer, setNormalizedPlayer[4,]$StlPer, setNormalizedPlayer[4,]$AstPer,
              setNormalizedPlayer[4,]$eFG, setNormalizedPlayer[4,]$USG, setNormalizedPlayer[4,]$TRB),
        mode = "markers",hoverinfo = "text",opacity = 0.7,fillcolor = '#60935D',marker= list(color= "#14342B",size =7),
        hoverinfo = "text", 
        text = ~paste( '<br><b> Player: </b><i>',appTable[4,1],'</i><br><b> Floor%: </b><i>',appTable[4,22],'%','</i><br><b> STL%: </b><i>',
                       appTable[4,19],'%','</i><br><b> AST%: </b><i>',appTable[4,20],'%','</i><br><b> USG%: </b><i>',appTable[4,10],'%',
                       '</i><br><b> eFG%%: </b><i>',appTable[4,21],'%','</i><br><b> TRB%: </b><i>',appTable[4,13],'%'),
        theta = c('<b>Floor%</b>','<b>Steals%</b>','<b>Assists%</b>', '<b>eFG%</b>', '<b>Usage%</b>', '<b>Tot.Rebound%</b>'),name = paste("",setNormalizedPlayer[4,]$Player))%>%
  
      layout(
          polar = list(
            radialaxis = list(
            visible = F,
            range = c(0,5))
        ),
        
        showlegend = T
       
      )
    radarPlot
    }else{
      radarPlot <- plot_ly(
        type = 'scatterpolar', fill = 'toself',theta = c('','','', '', '', ''))%>%
        layout(
          polar = list(
            bgcolor ="black",
            radialaxis = list(
              visible = F,
              range = c(0,2))))
      createAlert(session, "alert", "BubbleAlert", title = "Info Box",
                  content = "Radar Chart supports up to 4 Players", append = FALSE)
      radarPlot
    }
    
    
  }
    
  designUsageScatterPlot <- function(appTable){
    closeAlert(session, "BubbleAlert")
    Usg <- appTable$USG
    Nrtg <- appTable$NRTG
    tCustom <- list( family = "sans serif",
                     size = 10,
                     color = toRGB("black"))
    Players <- appTable$Player
    Team <- appTable$Team
   
    
    plot <- plot_ly(data = appTable, type="scatter", x= Usg,mode = "markers",
                    y = Nrtg,showlegend = FALSE, hoverinfo = 'text',  
                    text = paste("<br> <b>Player:</b><i> ",
                                 appTable$Player,"</i><br><b>NRTG:</b><i> ", appTable$NRTG,"</i><br><b>Usage: </b><i>", appTable$USG, "% </i>"),
                    transforms = list(
                      list(
                        type = 'groupby',
                        groups = appTable$Team,
                        styles = list(
                          list(target = "ALBA Berlin", value = list(marker =list(color = '#B4B8AB'))),
                          list( target = "Anadolu Efes Istanbul" , value = list(marker =list(color = '#153243'))),
                          list(target = "EA7 Emporio Armani Milan", value = list(marker =list(color = '#BA3B46'))),
                          list( target = "AX Armani Exchange Olimpia Milan" , value = list(marker =list(color = '#BA3B46'))),
                          list( target = "AX Armani Exchange Milan" , value = list(marker =list(color = '#BA3B46'))),
                          list( target = "CSKA Moscow" , value = list(marker =list(color = '#19297C'))),
                          list( target = "FC Barcelona Lassa" , value = list(marker =list(color = '#5B3758'))),
                          list( target = "FC Barcelona" , value = list(marker =list(color = '#5B3758'))),
                          list( target = "Fenerbahce Istanbul" , value = list(marker =list(color = '#0A1128'))),
                          list( target = "Fenerbahce Dogus Istanbul" , value = list(marker =list(color = '#0A1128'))),
                          list( target = "Fenerbahce Beko Istanbul" , value = list(marker =list(color = '#0A1128'))),
                          list( target = "Olympiacos Piraeus" , value = list(marker =list(color = '#F24236'))),
                          list( target = "Baskonia Vitoria Gasteiz" , value = list(marker =list(color = '#643A71'))),
                          list( target = "KIROLBET Baskonia Vitoria Gasteiz" , value = list(marker =list(color = '#643A71'))),
                          list( target = "KIROLBET Baskonia Vitoria-Gasteiz" , value = list(marker =list(color = '#643A71'))),
                          list( target = "Maccabi FOX Tel Aviv" , value = list(marker =list(color = '#F5F749'))),
                          list( target = "Panathinaikos Superfoods Athens" , value = list(marker =list(color = '#14591D'))),
                          list( target = "Panathinaikos OPAP Athens" , value = list(marker =list(color = '#14591D'))),
                          list( target = "Real Madrid" , value = list(marker =list(color = '#D6CFCB'))),
                          list( target = "Zalgiris Kaunas" , value = list(marker =list(color = '#83B692'))),
                          list( target = "Galatasaray Odeabank Istanbul" , value = list(marker =list(color = '#A6808C'))),
                          list( target = "Unics Kazan" , value = list(marker =list(color = '#61C9A8'))),
                          list( target = "Darussafaka Dogus Istanbul" , value = list(marker =list(color = '#C7EFCF'))),
                          list( target = "Darussafaka Tekfen Istanbul" , value = list(marker =list(color = '#C7EFCF'))),
                          list( target = "Brose Bamberg" , value = list(marker =list(color = '#E5EBEA'))),
                          list( target = "Khimki Moscow Region" , value = list(marker =list(color = '#507DBC'))),
                          list( target = "Zenit St Petersburg" , value = list(marker =list(color = '#BBD1EA'))),
                          list( target = "LDLC ASVEL Villeurbanne" , value = list(marker =list(color = '#333745'))),
                          list( target = "Unicaja Malaga" , value = list(marker =list(color = '#315659'))),
                          list( target = "Valencia Basket" , value = list(marker =list(color = '#FDD692'))),
                          list( target = "FC Bayern Munich" , value = list(marker =list(color = '#EC7357'))),
                          list( target = "Buducnost VOLI Podgorica" , value = list(marker =list(color = '#A7A2A9'))),
                          list( target = "Herbalife Gran Canaria" , value = list(marker =list(color = '#131B23'))),
                          list(target = "Crvena Zvezda mts Belgrade", value = list(marker =list(color = '#FE5F55')))
                        )
                      )
                    ),
                    marker =list(opacity =0.7,size = 20))%>%
      
      layout( 
        xaxis = list(          
          title = "Player Usage %",      
          showgrid = F),       
        yaxis = list(         
          title = "Net Rating")     
      )
  }
  
  desingPlayerBubblePlot <- function(appTable){
    
    if (length(appTable$FGA)!=0){
      closeAlert(session, "BubbleAlert")
    data <- appTable
    gapCounter <- median(data$FGA)
    actualSize <- data$FGA - gapCounter
    colorList<- round(100*(data$TS),2)
    Players <- appTable$Player
    PlayerName <- sub("\\,.*", "", Players)
    TwoPointP <- round(100*(data$FGM2/data$FGA2),2)
    ThreePointP <- round(100*(data$FGM3/data$FGA3),2)

    
    configureText <- list(
      family = "sans serif",
      size = 10,
      color = toRGB("black"))
    
    customColor <- c('#E9E61B','#DCD922','#EA7C14', '#C12424') 
    axiySet <- list(
      autotick = T,
      showgrid = TRUE,
      ticks = "outside",
      tick0 = min(TwoPointP),
      dtick = 0.1,
      ticklen = 8,
      tickwidth = 2,
      tickcolor = toRGB("black")
    )
    axixSet <- list(
      
      autotick = T,
      showgrid = TRUE,
      ticks = "outside",
      tick0 = min(ThreePointP),
      ticklen = 8,
      tickwidth = 2,
      tickcolor = toRGB("black")
    )
    
    
    ShootingPlot <- plot_ly(data,x = TwoPointP, y = ThreePointP,type = 'scatter',name = "TrueShooting" ,
                            mode = 'markers',size = actualSize,showlegend =F,
                            color = colorList, colors = customColor,  sizes = c(10, 60),marker = list(opacity = 0)
    )%>%
      add_trace(mode = "markers",marker = list(opacity = 0.7, sizemode = 'diameter'),hoverinfo = 'text',
                text = ~paste('<b>Player</b><i>:', Players, '</i><br><b>True Shooting %:</b><i>',
                              colorList, '</i><br><b> 2PT%: </b><i> ',TwoPointP ,'</i><br><b> 3PT%: </b><i>', ThreePointP ,
                              '</i><br><b> FGA:</b> <i>',data$FGA ))%>% 
      add_text(mode = "markers",text = PlayerName,textfont= configureText,hoverinfo = "skip")%>%
           layout(xaxis=axixSet,yaxis = axiySet)
    
    
    ax <- list(zeroline = FALSE, showline = FALSE, showticklabels = FALSE, showgrid = FALSE)
    
   
    legendSize <- seq.default(from = min(data$FGA),to = max(data$FGA), by = ((max(data$FGA)-min(data$FGA))/2) )
    
    plotsLegend <- plot_ly(x = 1, y = legendSize, size = legendSize,type = "scatter", hoverinfo = "skip", showlegend = F,
                           mode ="markers", text = paste("<b>FGA Size:</b>",legendSize), 
                           sizes = c(10,60),mode = "markers",
                           marker = list(sizemode = 'diameter',color = "black",
                                         line = list(color = "#EA7C14", width = 2))) %>%
      add_text(mode = "markers",textfont = configureText,textposition = "top center") %>%
      layout(xaxis = ax,
             yaxis = list(showgrid = FALSE,showticklabels = FALSE))
    subplot(ShootingPlot,plotsLegend,widths = c(0.8, 0.2))%>%
      layout(xaxis = list(title=  "2 Point Shots(%Made)"),yaxis = list(title = "3 Point Shots(%Made)"))
    }else{
      createAlert(session, "alert", "BubbleAlert", title = "Info Box",
                  content = "Select Player(s) to initialize the Graph", append = FALSE)
    }
    
  }
  
  AP1819 <- as.data.frame(read.csv("AdvAP1819.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  RS1819 <- as.data.frame(read.csv("AdvRS1819.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  PO1819 <- as.data.frame(read.csv("AdvPO1819.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  FF1819 <- as.data.frame(read.csv("AdvFF1819.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  
  AP1718 <- as.data.frame(read.csv("AdvAP1718.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  RS1718 <- as.data.frame(read.csv("AdvRS1718.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  PO1718 <- as.data.frame(read.csv("AdvPO1718.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  FF1718 <- as.data.frame(read.csv("AdvFF1718.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  
  AP1617 <- as.data.frame(read.csv("AdvAP1617.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  RS1617 <- as.data.frame(read.csv("AdvRS1617.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  PO1617 <- as.data.frame(read.csv("AdvPO1617.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  FF1617 <- as.data.frame(read.csv("AdvFF1617.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  
  HAP1819 <- as.data.frame(read.csv("AdvHAP1819.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  HRS1819 <- as.data.frame(read.csv("AdvHRS1819.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  HPO1819 <- as.data.frame(read.csv("AdvHPO1819.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  AAP1819 <- as.data.frame(read.csv("AdvAAP1819.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  ARS1819 <- as.data.frame(read.csv("AdvARS1819.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  APO1819 <- as.data.frame(read.csv("AdvAPO1819.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  
  HAP1718 <- as.data.frame(read.csv("AdvHAP1718.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  HRS1718 <- as.data.frame(read.csv("AdvHRS1718.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  HPO1718 <- as.data.frame(read.csv("AdvHPO1718.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  AAP1718 <- as.data.frame(read.csv("AdvAAP1718.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  ARS1718 <- as.data.frame(read.csv("AdvARS1718.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  APO1718 <- as.data.frame(read.csv("AdvAPO1718.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  
  HAP1617 <- as.data.frame(read.csv("AdvHAP1617.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  HRS1617 <- as.data.frame(read.csv("AdvHRS1617.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  HPO1617 <- as.data.frame(read.csv("AdvHPO1617.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  AAP1617 <- as.data.frame(read.csv("AdvAAP1617.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  ARS1617 <- as.data.frame(read.csv("AdvARS1617.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  APO1617 <- as.data.frame(read.csv("AdvAPO1617.csv",header= TRUE, stringsAsFactors = FALSE,sep = ","))
  
  

 
    output$table <- renderDataTable({
      
      
      
      options(DT.options = list(searching = FALSE, columnDefs = list(
        list(targets = c(0,10,11,12), visible = FALSE)), lengthMenu = c(4,8,10,18), pageLength = 10,scrollX = T))
      
      
      decideTable()
      
   
     
    })
      
 
    output$RatingPlot <- renderPlotly({
      
       
       designScatterPlot(appTable =  decideTable())
      
     })
    output$PacePlot <- renderPlotly({
      
      designPacePlot(appTable = decideTable())
      
    })
    output$ShootingPlot <- renderPlotly({
      
      desingBubblePlot(appTable = decideTable())
      
    })
      
    output$Tip <- renderValueBox({
      valueBox("","Don't Forget in Defensive Rating, the smaller value is the better",  icon = icon("info-circle ", lib="font-awesome"),
               color = "red")
    })

        
    output$Efg <- renderDataTable({ 
      options(DT.options = list(searching = FALSE, columnDefs = list(
        list(targets = c(0,2,3,4,5,6,7,8,10,11,12), visible = FALSE)),lengthChange = FALSE,dom = 'ft', scrollX = T))
      iNFOtable <- decideTable()
      sortedTable <- iNFOtable[order(iNFOtable$eFG, decreasing = TRUE),]
      sortedTable[1:5,]
      
           }) 
    output$ASTO <- renderDataTable({ 
      options(DT.options = list(searching = FALSE, columnDefs = list(
        list(targets = c(0,2,3,4,5,6,7,8,10,11,12), visible = FALSE)),lengthChange = FALSE,dom = 'ft', scrollX = T))
      iNFOtable <- decideTable()
      sortedTable <- iNFOtable[order(iNFOtable$eFG, decreasing = TRUE),]
      sortedTable[1:5,]
      
    }) 
    output$Reb <- renderDataTable({ 
      options(DT.options = list(searching = FALSE, columnDefs = list(
        list(targets = c(0,2,3,4,5,6,7,8,10,11,12), visible = FALSE)),lengthChange = FALSE,dom = 'ft', scrollX = T))
      iNFOtable <- decideTable()
      sortedTable <- iNFOtable[order(iNFOtable$eFG, decreasing = TRUE),]
      sortedTable[1:5,]
      
    }) 
    
    
    output$OffenseBox <- renderValueBox({
      
        iNFOtable <- decideTable()
        if(nrow(iNFOtable)==0){valueBox("Best Offense","-", icon= icon("bomb", lib="font-awesome"),color = "red")}else{
        sortedTable <- iNFOtable[order(-iNFOtable$ORTG),]
        valueBox("Best Offense" , sortedTable$Teams[1] ,  icon = icon("bomb", lib="font-awesome"),
      color = "red"
    
      )}
    })
    
    output$DefenseBox <- renderValueBox({
     
        iNFOtable <- decideTable()
        if(nrow(iNFOtable)==0){valueBox("Best Defense","-", icon = icon("shield-alt", lib = "font-awesome"),
                                        color = "yellow")}
        else{
        sortedTable <- iNFOtable[order(iNFOtable$DRTG),]
      valueBox( "Best Defense", sortedTable$Teams[1] 
        , icon = icon("shield-alt", lib = "font-awesome"),
        color = "yellow")}
      
    })
    
    output$PaceBox <- renderValueBox({
      
      iNFOtable <- decideTable()
      if(nrow(iNFOtable)==0){valueBox("Highest Pace","-", icon = icon("tachometer-alt", lib = "font-awesome"),
                                      color = "green")}else{
      sortedTable <- iNFOtable[order(-iNFOtable$Pace),]
      valueBox( "Highest Pace",sortedTable$Teams[1], icon = icon("tachometer-alt", lib = "font-awesome"),
        color = "green")}
    })
    
    
    observeEvent(input$Player,{
       
      if(is.null(input$Player)){
        
        SelectedPlayers <- subset(Playersdb, Season %in% input$Season ,
                                  select = c(Player))
        
        
        output$PlayerRatingPlot <- renderPlotly({ designPlayerScatterPlot(appTable = SelectedPlayers )})
        output$Radar <- renderPlotly({ designRadar(appTable = SelectedPlayers) })
        output$UsagePlot <- renderPlotly({designUsageScatterPlot(appTable = SelectedPlayers) })
        output$TrueShootingPlot <- renderPlotly({desingPlayerBubblePlot(appTable = SelectedPlayers) })
        
      }else if(!is.null(input$Team) & !is.null(input$Player)){
       
      
        SelectedPlayers <- Playersdb[ which(Playersdb$Player %in% input$Player
                                            & Playersdb$Season %in% input$Season
                                            & Playersdb$Team %in% input$Team)
                                            , ]
        
        output$PlayerRatingPlot <- renderPlotly({ designPlayerScatterPlot(appTable = SelectedPlayers )})
        
        output$Radar <- renderPlotly({ designRadar(appTable = SelectedPlayers) })
        output$UsagePlot <- renderPlotly({designUsageScatterPlot(appTable = SelectedPlayers) })
        output$TrueShootingPlot <- renderPlotly({desingPlayerBubblePlot(appTable = SelectedPlayers) })
        
      }else if(is.null(input$Team)){
      
        
        SelectedPlayers <- Playersdb[ which(Playersdb$Player %in% input$Player
                                            & Playersdb$Season %in% input$Season), ]
       
        
        output$PlayerRatingPlot <- renderPlotly({ designPlayerScatterPlot(appTable = SelectedPlayers )})
        
        
        output$Radar <- renderPlotly({ designRadar(appTable = SelectedPlayers) })
        output$UsagePlot <- renderPlotly({designUsageScatterPlot(appTable = SelectedPlayers) })
        output$TrueShootingPlot <- renderPlotly({desingPlayerBubblePlot(appTable = SelectedPlayers) })
        
      }
      },ignoreNULL = FALSE)
      
    
    observeEvent(input$Team,{   
      
      if(is.null(input$Team)){
       
        
        SelectedPlayers <- subset(Playersdb, Season %in% input$Season ,
                                 select = c(Player))
        
        updatePickerInput(session, "Player",
                          choices = SelectedPlayers )
        
        output$PlayerRatingPlot <- renderPlotly({ designPlayerScatterPlot(appTable = SelectedPlayers )})
        output$UsagePlot <- renderPlotly({designUsageScatterPlot(appTable = SelectedPlayers) })
        
      }else{
        
        PlayersandSeason <- subset(Playersdb, Team %in% input$Team,
                                   select=c(Player,Season))
        
        correct <- subset(PlayersandSeason, Season %in% input$Season,
                          select = c(Player))
        
        SelectedPlayers <- Playersdb[ which(Playersdb$Team %in% input$Team
                                            & Playersdb$Season  %in% input$Season), ]
        updatePickerInput(session, "Player",
                          choices = correct )
        
        
       
       
        output$PlayerRatingPlot <- renderPlotly({ designPlayerScatterPlot(appTable = SelectedPlayers )})
        output$UsagePlot <- renderPlotly({designUsageScatterPlot(appTable = SelectedPlayers) })
      
      }
       
    }, ignoreNULL = FALSE)
    
    
    observeEvent(input$Season,{
      seasons <- Playersdb$Season
      players <- Playersdb$Player
      currentSeason <- input$Season
      
      if(input$Season == "2019-2020"){
        
        updatePickerInput(session, "Team",
                          choices = substr(AP1920$Teams,1,nchar(AP1920$Teams)-3))}
      else if(input$Season == "2018-2019"){
                   updatePickerInput(session, "Team",
                            choices = substr(AP1819$Teams,1,nchar(AP1819$Teams)-3))}
      else if(input$Season == "2017-2018"){
        
        updatePickerInput(session, "Team",
                          choices = substr(AP1718$Teams,1,nchar(AP1718$Teams)-3))}
      else if(input$Season == "2016-2017"){
       
        updatePickerInput(session, "Team",
                          choices = substr(AP1617$Teams,1,nchar(AP1617$Teams)-3))}
     
      SelectedPlayers <- subset(Playersdb, Season %in% input$Season ,
                                select = c(Player))
      
      updatePickerInput(session, "Player",
                        choices = SelectedPlayers )
    })
    
    
}
 
shinyApp(ui = ui, server = server)