library(readr)
library(dplyr)
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)


games_2021 <-
  read.csv(
    "https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Games.csv"
  )

# Odds <-
#   games_2021 %>% filter(Result == "TBD",!is.na(Money_Line)) %>%
#     select(
#       Date,
#       Team,
#       Opponent,
#       Line_Favored,
#       Line_Amount,
#       Over_Under,
#       Money_Line,
#       Money_Line_Opp,
#       Implied_Odds,
#       Implied_Odds_Opp
#     )


box_2021 <-
  read.csv(
    "https://raw.githubusercontent.com/MattC137/Open_Data/master/Data/Sports/NBA/NBA_2021_Box_Score.csv"
  )

# standings <-
#   games_2021 %>% filter(Season_Type == "Regular-Season", Result != "TBD") %>%
#   group_by(Team) %>%
#   summarize(
#     W = sum(Result == "W"),
#     L = sum(Result == "L"),
#     Pct = W / (W - L),
#     ppg = mean(Points_For),
#     opp_ppg = mean(Points_Against)
#   )

standings <- standings[order(-standings$W),]

# stat_leaders <-
#   box_2021 %>%  filter(Season_Type == "Regular-Season", Played == TRUE) %>%
#   group_by(Player_Id_Str) %>%
#   summarise(
#     Points = mean(Points),
#     Rebounds = mean(Rebounds),
#     Assists = mean(Assists),
#     Blocks = mean(Blocks),
#     Threes_Made = mean(Threes_Made),
#     Steals = mean(Steals)
#   )

# point_leader <- stat_leaders[,c(order[- stat_leaders$Points] , "Player_Id_Str")]
 point_leader <- stat_leaders[order(-stat_leaders$Points),] 
 pointLeader <- point_leader[1:5,c("Player_Id_Str", "Points"  )]
 
 rebound_leader <- stat_leaders[order(-stat_leaders$Rebounds),] 
 reboundLeader <- rebound_leader[1:5,c("Player_Id_Str", "Rebounds"  )]
 
 assist_leader <- stat_leaders[order(-stat_leaders$Assists),] 
 assistLeader <- assist_leader[1:5,c("Player_Id_Str", "Assists"  )]
 
 block_leader <- stat_leaders[order(-stat_leaders$Blocks),] 
 blockLeader <- block_leader[1:5,c("Player_Id_Str", "Blocks"  )]
 
 threeMade_leader <- stat_leaders[order(-stat_leaders$Threes_Made),] 
 threeMadeLeader <- threeMade_leader[1:5,c("Player_Id_Str", "Threes_Made"  )]
 
 steal_leader <- stat_leaders[order(-stat_leaders$Steals),] 
 stealLeader <- steal_leader[1:5,c("Player_Id_Str", "Steals"  )]
 
 

ui <- fluidPage(
  theme = shinytheme("darkly"),
  navbarPage(
    "2021 NBA",
    
    
    tabPanel(
      "Updates?",
      sidebarPanel(
        tags$h3("Input:"),
        textInput("username", "User Name:",""),
        textInput("email", "Email:",""),
        textInput("password", "Password:","")
      ), #Updates? sidePanel
      mainPanel(
        h1("Verify"),
        verbatimTextOutput("txtout"),
      ), #Updates? mainPanel
      radioButtons(
        "radio",
        label = h3("Receive updates how often"),
        choices = list(
          "Daily" = 1,
          "Weekly" = 2),
          selected = 1,
        
      ), #radioButton
        hr(),
        fluidRow(column(2, verbatimTextOutput("value")))
    

      ) , #Updates? tabPanel
    
    
    
    tabPanel("Total W's & L's",
             
             mainPanel(
               "Data", tableOutput("standings")
             ) #mainPanel
             ), #Total W's & L's tabPanel
    
             
             tabPanel("TBD ODDS",
                      mainPanel(
                        "Odds", tableOutput("Odds")
                      ) #mainPanel
                      ), #TBD ODDS tabPanel
    
             
             navbarMenu(
               "Stat Leaders",
               tabPanel("Points", tableOutput("pointLeader")),
               tabPanel("Rebounds", tableOutput("reboundLeader")),
               tabPanel("Assists", tableOutput("assistLeader")),
               tabPanel("Blocks", tableOutput("blockLeader")),
               tabPanel("Threes_Made", tableOutput("threeMadeLeader")),
               tabPanel("Steals", tableOutput("stealLeader"))
             ), #navbarMenu
    
            
    
            tabPanel("Videos",
                     sidebarPanel(
                       
                     ),
                     mainPanel(
                       tags$video(
                         HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/vScCsVwZldg" 
                              frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; 
                              gyroscope; picture-in-picture" allowfullscreen></iframe>'))
                     ),
                     )
   
  ) #navbarPage
  
) #fluidPage



server <- function(input, output) {
  output$txtout <- renderText({
    paste(input$username, input$email, input$password, sep = " ")
  })
  
  output$standings <- renderTable({
    standings
  })
  
  output$Odds <- renderTable({
    Odds
  })
  
  output$pointLeader <- renderTable({
    pointLeader
  })
  
  output$reboundLeader <- renderTable({
    reboundLeader
  })
  
  output$assistLeader <- renderTable({
    assistLeader
  })
  
  output$blockLeader <- renderTable({
    blockLeader
  })
  
  output$threeMadeLeader <- renderTable({
    threeMadeLeader
  })
  
  output$stealLeader <- renderTable({
    stealLeader
  })
} #server

shinyApp(ui = ui, server = server)
