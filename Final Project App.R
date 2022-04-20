
library(tidyverse)
library(dplyr)
library(stringr)



library(readr)
df <- read_csv("../data/qb_combine.csv")


target <- c("Kyler Murray", "Matt Ryan", "Lamar Jackson", "Josh Allen", "Sam Darnold", 
            "Andy Dalton", "Joe Burrow", "Baker Mayfield", "Dak Prescott", 
            "Drew Lock", "Jared Goff", "Aaron Rodgers", "Davis Mills", 
            "Carson Wentz", "Trevor Lawrence", "Patrick Mahomes","Derek Carr",
            "Justin Herbert", "Matthew Stafford", "Tua Tagovailoa", "Kirk Cousins",
            "Mac Jones", "Jamis Winston", "Daniel Jones", "Zach Wilson", "Jalen Hurts",
            "Ben Roethlisberger", "Jimmy Garoppolo", "Russell Wilson","Tom Brady",
            "Ryan Tannehill", "Taylor Heinicke")

QB_data <- df %>%
  filter(Player %in% target) %>%
  select(-(nfl_cav | Bench | Pos | School | College)) %>%
  select(-(drafted : cfb_Conf))
  
   

QB_data <- QB_data[!duplicated(QB_data$Player), ]     




heights<-str_split(QB_data$Ht, "-", simplify=FALSE) 
n.heights<-length(heights)
ft<-unlist(heights)[1+2*0:(n.heights-1)]
inch<-unlist(heights)[2*1:n.heights]
QB_data$height_in_inches=12*as.numeric(ft)+as.numeric(inch)


#Carson Wentz (https://www.sports-reference.com/cfb/players/carson-wentz-1.html)
which.CarsonW=which(QB_data$Player=="Carson Wentz")
QB_data$cfb_G[which.CarsonW]=35
QB_data$cfb_Completions[which.CarsonW]=392
QB_data$cfb_Attempts[which.CarsonW]=612
QB_data$cfb_CompPct[which.CarsonW]=64.1
QB_data$cfb_PassingYds[which.CarsonW]=5115
QB_data$cfb_YdsPerAtt[which.CarsonW]=8.4
QB_data$cfb_AdjPassPerAtt[which.CarsonW]=8.8
QB_data$cfb_Tds[which.CarsonW]=45
QB_data$cfb_Int[which.CarsonW]=14
QB_data$cfb_rating[which.CarsonW]=153.9

#Jimmy Garoppolo (https://web.archive.org/web/20161003063910/https://sports.yahoo.com/ncaaf/players/193289/)
which.JimmyG=which(QB_data$Player=="Jimmy Garoppolo")
QB_data$cfb_G[which.JimmyG]=45
QB_data$cfb_Completions[which.JimmyG]=1047
QB_data$cfb_Attempts[which.JimmyG]=1668
QB_data$cfb_CompPct[which.JimmyG]=62.8
QB_data$cfb_PassingYds[which.JimmyG]=13156
QB_data$cfb_YdsPerAtt[which.JimmyG]=7.9
QB_data$cfb_AdjPassPerAtt[which.JimmyG]=(QB_data$cfb_PassingYds[which.JimmyG]+20*QB_data$cfb_Tds[which.JimmyG]-45*QB_data$cfb_Int[which.JimmyG])/QB_data$cfb_Attempts[which.JimmyG]
QB_data$cfb_Tds[which.JimmyG]=118
QB_data$cfb_Int[which.JimmyG]=51
QB_data$cfb_rating[which.JimmyG]=146.3

QB_new <- tibble(Player = c("Your QB"),
                 height_in_inches = c(70),
                 Wt = c(215),
                 `40yd` = c(4.85),
                 Vertical = c(28.0),
                 `Broad Jump` = c(105),
                 `3Cone` = c(7.00),
                 Shuttle = c(4.30),
                 cfb_G = c(45),
                 cfb_Completions = c(500),
                 cfb_Attempts = c(1000),
                 cfb_CompPct = c(60.0),
                 cfb_PassingYds = c(9000),
                 cfb_YdsPerAtt = c(7.5),
                 cfb_AdjPassPerAtt = c(7.5),
                 cfb_Tds = c(50),
                 cfb_Int = c(25),
                 cfb_rating = c(130))


data.numcols <- QB_data[, sapply(QB_data, is.numeric)]

all.means <- apply(data.numcols, 2, function(x) mean(x, na.rm = TRUE))

all.means <- colMeans(data.numcols, na.rm = TRUE)
              
print(all.means)


ggplot(full_join(QB_data, QB_new), aes(fct_reorder(Player, cfb_AdjPassPerAtt), cfb_AdjPassPerAtt)) +
  geom_point() +
  geom_segment(aes(x = Player, xend = Player, y = 0, yend = cfb_AdjPassPerAtt)) +
  coord_flip() +
  scale_x_discrete(limits = rev) +
  geom_point(data = QB_new, color = "red") +
  geom_segment(data = QB_new, aes(x = Player, xend = Player, y = 0, yend = cfb_AdjPassPerAtt), color = "red")

ggplotly(ggplot(full_join(QB_data, QB_new), aes(fct_reorder(Player, cfb_AdjPassPerAtt), cfb_AdjPassPerAtt)) +
  geom_point() +
  geom_segment(aes(x = Player, xend = Player, y = 0, yend = cfb_AdjPassPerAtt)) +
  coord_flip() +
  xlab("Height (inches)") +
  ylab(NULL) +
  geom_point(data = QB_new, color = "red") +
  geom_segment(data = QB_new, aes(x = Player, xend = Player, y = 0, yend = cfb_AdjPassPerAtt), color = "red")
)

library(plotly)



library(shiny) 

ui <- fluidPage(
  titlePanel("Comparing Combine and College Performaces of 2021 Starting NFL Quarterbacks to a Prospect QB"),
  tabsetPanel(
    tabPanel("Combine", fluid = TRUE,
             fluidRow(
               column(2, numericInput("ht", "Height (inches)", value = 75),
                      numericInput("wt", "Weight (lbs)", value = 221)),
               column(2, offset = 0.5,
                      numericInput("40yd", "40 Yard Time (seconds)", value = 4.79),
                      numericInput("vert", "Vertical (inches)", value = 31.2)),
               column(2, offset = 0.5,
                      numericInput("broad", "Broad Jump (inches)", value = 113),
                      numericInput("3cone", "3 Cone Drill Time (seconds)", value = 7.05)),
               
               column(2, offset = 0.5,
                      numericInput("shuttle", "Shuttle Drill Time (seconds)", value = 4.33))
               ),
             
             
             fluidRow(column(12, tabsetPanel(
               tabPanel("Height", plotOutput("ht")),
               tabPanel("Weight", plotOutput("wt")),
               tabPanel("40 Yard", plotOutput("40yd")),
               tabPanel("Vertical", plotOutput("vert")),
               tabPanel("Broad Jump", plotOutput("broad")),
               tabPanel("3 Cone Drill", plotOutput("3cone")),
               tabPanel("Shuttle Drill", plotOutput("shuttle")))))),
            
    
    
    tabPanel("College Career", fluid = TRUE,
             fluidRow(
               column(2, numericInput("g", "# of Games Played", 38),
                      numericInput("comp", "# of Completions", 628)),
               column(2, offset = 0.5,
                      numericInput("att", "# of Attempts", 987),
                      numericInput("compper", "Completion Percentage", 64.4)),
               column(2, offset = 0.5,
                      numericInput("pass", "# of Passing Yards", 8220),
                      numericInput("yds", "Yards per Attempt", 8.6)),
               
               column(2, offset = 0.5,
                      numericInput("tds", "# of Touchdowns", 67),
                      numericInput("int", "# of Interceptions", 22)),
               column(2, offset = 0.5,
                      numericInput("qbr", "Quarterback Rating", 155.9),
                      numericInput("adj", "Adjusted Pass per Attempt", 9.1)
               )),
             
        
             fluidRow(column(12, tabsetPanel(
               tabPanel("Games", plotOutput("g")),
               tabPanel("Completions", plotOutput("comp")),
               tabPanel("Attempts", plotOutput("att")),
               tabPanel("Completion %", plotOutput("compper")),
               tabPanel("Passing Yards", plotOutput("pass")),
               tabPanel("Yards per Attempt", plotOutput("yds")),
               tabPanel("Tounchdowns", plotOutput("tds")),
               tabPanel("Interceptions", plotOutput("int")),
               tabPanel("QBR", plotOutput("qbr")),
               tabPanel("Adj Yards per Attempt", plotOutput("adj")))))
    )))


  
server <- function(input, output, session) {
  new_QB <- reactive({
    tibble(Player = c("Your QB"),
           height_in_inches = c(input$ht),
           Wt = c(input$wt),
           `40yd` = c(input$`40yd`),
           Vertical = c(input$vert),
           `Broad Jump` = c(input$broad),
           `3Cone` = c(input$`3cone`),
           Shuttle = c(input$shuttle),
           cfb_G = c(input$g),
           cfb_Completions = c(input$comp),
           cfb_Attempts = c(input$att),
           cfb_CompPct = c(input$compper),
           cfb_PassingYds = c(input$pass),
           cfb_YdsPerAtt = c(input$yds),
           cfb_AdjPassPerAtt = c(input$adj),
           cfb_Tds = c(input$tds),
           cfb_Int = c(input$int),
           cfb_rating = c(input$qbr))
  })
  
  output$ht <- renderPlot(
    ggplotly(ggplot(full_join(QB_data, new_QB()), aes(fct_reorder(Player, height_in_inches), height_in_inches)) +
      geom_point() +
      geom_segment(aes(x = Player, xend = Player, y = 0, yend = height_in_inches)) +
      coord_flip() +
      ylab("Height (inches)") +
      xlab(NULL) +
      geom_point(data = new_QB(), color = "red") +
      geom_segment(data = new_QB(), aes(x = Player, xend = Player, y = 0, yend = height_in_inches), color = "red")
  ))
  
  output$wt <- renderPlot(
    ggplot(full_join(QB_data, new_QB()), aes(fct_reorder(Player, Wt), Wt)) +
      geom_point() +
      geom_segment(aes(x = Player, xend = Player, y = 0, yend = Wt)) +
      coord_flip() +
      ylab("Weight (lbs)") +
      xlab(NULL) +
      geom_point(data = new_QB(), color = "red") +
      geom_segment(data = new_QB(), aes(x = Player, xend = Player, y = 0, yend = Wt), color = "red")
  )
  
  output$`40yd` <- renderPlot(
    ggplot((full_join(QB_data, new_QB()) %>% drop_na(`40yd`)), aes(fct_reorder(Player, `40yd`), `40yd`)) +
      geom_point() +
      geom_segment(aes(x = Player, xend = Player, y = 0, yend = `40yd`)) +
      coord_flip() +
      ylab("40 Yard Time (seconds)") +
      xlab(NULL) +
      scale_x_discrete(limits = rev) +
      geom_point(data = new_QB(), color = "red") +
      geom_segment(data = new_QB(), aes(x = Player, xend = Player, y = 0, yend = `40yd`), color = "red")
  )
  
  output$vert <- renderPlot(
    ggplot((full_join(QB_data, new_QB()) %>% drop_na(Vertical)), aes(fct_reorder(Player, Vertical), Vertical)) +
      geom_point() +
      geom_segment(aes(x = Player, xend = Player, y = 0, yend = Vertical)) +
      coord_flip() +
      ylab("Vertical (inches)") +
      xlab(NULL) +
      geom_point(data = new_QB(), color = "red") +
      geom_segment(data = new_QB(), aes(x = Player, xend = Player, y = 0, yend = Vertical), color = "red")
  )
  
  output$broad <- renderPlot(
    ggplot((full_join(QB_data, new_QB()) %>% drop_na(`Broad Jump`)), aes(fct_reorder(Player, `Broad Jump`), `Broad Jump`)) +
      geom_point() +
      geom_segment(aes(x = Player, xend = Player, y = 0, yend = `Broad Jump`)) +
      coord_flip() +
      ylab("Broad Jump (inches)") +
      xlab(NULL) +
      geom_point(data = new_QB(), color = "red") +
      geom_segment(data = new_QB(), aes(x = Player, xend = Player, y = 0, yend = `Broad Jump`), color = "red")
  )
  
  output$`3cone` <- renderPlot(
    ggplot((full_join(QB_data, new_QB()) %>% drop_na(`3Cone`)), aes(fct_reorder(Player, `3Cone`), `3Cone`)) +
      geom_point() +
      geom_segment(aes(x = Player, xend = Player, y = 0, yend = `3Cone`)) +
      coord_flip() +
      ylab("3 Cone Drill Time (seconds)") +
      xlab(NULL) +
      scale_x_discrete(limits = rev) +
      geom_point(data = new_QB(), color = "red") +
      geom_segment(data = new_QB(), aes(x = Player, xend = Player, y = 0, yend = `3Cone`), color = "red")
  )
  
  output$shuttle <- renderPlot(
    ggplot((full_join(QB_data, new_QB()) %>% drop_na(Shuttle)), aes(fct_reorder(Player, Shuttle), Shuttle)) +
      geom_point() +
      geom_segment(aes(x = Player, xend = Player, y = 0, yend = Shuttle)) +
      coord_flip() +
      ylab("Shuttle Drill Time (seconds)") +
      xlab(NULL) +
      scale_x_discrete(limits = rev) +
      geom_point(data = new_QB(), color = "red") +
      geom_segment(data = new_QB(), aes(x = Player, xend = Player, y = 0, yend = Shuttle), color = "red")
  )
  
  output$g <- renderPlot(
    ggplot(full_join(QB_data, new_QB()), aes(fct_reorder(Player, cfb_G), cfb_G)) +
      geom_point() +
      geom_segment(aes(x = Player, xend = Player, y = 0, yend = cfb_G)) +
      coord_flip() +
      ylab("# of Games Played") +
      xlab(NULL) +
      geom_point(data = new_QB(), color = "red") +
      geom_segment(data = new_QB(), aes(x = Player, xend = Player, y = 0, yend = cfb_G), color = "red")
  )
  
  output$comp <- renderPlot(
    ggplot(full_join(QB_data, new_QB()), aes(fct_reorder(Player, cfb_Completions), cfb_Completions)) +
      geom_point() +
      geom_segment(aes(x = Player, xend = Player, y = 0, yend = cfb_Completions)) +
      coord_flip() +
      ylab("# of Completions") +
      xlab(NULL) +
      geom_point(data = new_QB(), color = "red") +
      geom_segment(data = new_QB(), aes(x = Player, xend = Player, y = 0, yend = cfb_Completions), color = "red")
  )
  
  output$att <- renderPlot(
    ggplot(full_join(QB_data, new_QB()), aes(fct_reorder(Player, cfb_Attempts), cfb_Attempts)) +
      geom_point() +
      geom_segment(aes(x = Player, xend = Player, y = 0, yend = cfb_Attempts)) +
      coord_flip() +
      ylab("# of Attempts") +
      xlab(NULL) +
      geom_point(data = new_QB(), color = "red") +
      geom_segment(data = new_QB(), aes(x = Player, xend = Player, y = 0, yend = cfb_Attempts), color = "red")
  )
  
  output$compper <- renderPlot(
    ggplot(full_join(QB_data, new_QB()), aes(fct_reorder(Player, cfb_CompPct), cfb_CompPct)) +
      geom_point() +
      geom_segment(aes(x = Player, xend = Player, y = 0, yend = cfb_CompPct)) +
      coord_flip() +
      ylab("Completion Percentage (%)") +
      xlab(NULL) +
      geom_point(data = new_QB(), color = "red") +
      geom_segment(data = new_QB(), aes(x = Player, xend = Player, y = 0, yend = cfb_CompPct), color = "red")
  )
  
  
  output$pass <- renderPlot(
    ggplot(full_join(QB_data, new_QB()), aes(fct_reorder(Player, cfb_PassingYds), cfb_PassingYds)) +
      geom_point() +
      geom_segment(aes(x = Player, xend = Player, y = 0, yend = cfb_PassingYds)) +
      coord_flip() +
      ylab("# of Passing Yards") +
      xlab(NULL) +
      geom_point(data = new_QB(), color = "red") +
      geom_segment(data = new_QB(), aes(x = Player, xend = Player, y = 0, yend = cfb_PassingYds), color = "red")
  )
  
  output$yds <- renderPlot(
    ggplot(full_join(QB_data, new_QB()), aes(fct_reorder(Player, cfb_YdsPerAtt), cfb_YdsPerAtt)) +
      geom_point() +
      geom_segment(aes(x = Player, xend = Player, y = 0, yend = cfb_YdsPerAtt)) +
      coord_flip() +
      ylab("Yards Per Attempt") +
      xlab(NULL) +
      geom_point(data = new_QB(), color = "red") +
      geom_segment(data = new_QB(), aes(x = Player, xend = Player, y = 0, yend = cfb_YdsPerAtt), color = "red")
  )
  
  output$adj <- renderPlot(
    ggplot(full_join(QB_data, new_QB()), aes(fct_reorder(Player, cfb_AdjPassPerAtt), cfb_AdjPassPerAtt)) +
      geom_point() +
      geom_segment(aes(x = Player, xend = Player, y = 0, yend = cfb_AdjPassPerAtt)) +
      coord_flip() +
      ylab("Adjusted Yards Per Attempt") +
      xlab(NULL) +
      geom_point(data = new_QB(), color = "red") +
      geom_segment(data = new_QB(), aes(x = Player, xend = Player, y = 0, yend = cfb_AdjPassPerAtt), color = "red")
  )
  
  output$tds <- renderPlot(
    ggplot(full_join(QB_data, new_QB()), aes(fct_reorder(Player, cfb_Tds), cfb_Tds)) +
      geom_point() +
      geom_segment(aes(x = Player, xend = Player, y = 0, yend = cfb_Tds)) +
      coord_flip() +
      ylab("# of Touchdowns") +
      xlab(NULL) +
      geom_point(data = new_QB(), color = "red") +
      geom_segment(data = new_QB(), aes(x = Player, xend = Player, y = 0, yend = cfb_Tds), color = "red")
  )
  
  output$int <- renderPlot(
    ggplot(full_join(QB_data, new_QB()), aes(fct_reorder(Player, cfb_Int), cfb_Int)) +
      geom_point() +
      geom_segment(aes(x = Player, xend = Player, y = 0, yend = cfb_Int)) +
      coord_flip() +
      ylab("# of Interceptions") +
      xlab(NULL) +
      scale_x_discrete(limits = rev) +
      geom_point(data = new_QB(), color = "red") +
      geom_segment(data = new_QB(), aes(x = Player, xend = Player, y = 0, yend = cfb_Int), color = "red")
  )
  
  output$qbr <- renderPlot(
    ggplot(full_join(QB_data, new_QB()), aes(fct_reorder(Player, cfb_rating), cfb_rating)) +
      geom_point() +
      geom_segment(aes(x = Player, xend = Player, y = 0, yend = cfb_rating)) +
      coord_flip() +
      ylab("Quarterback Rating") +
      xlab(NULL) +
      geom_point(data = new_QB(), color = "red") +
      geom_segment(data = new_QB(), aes(x = Player, xend = Player, y = 0, yend = cfb_rating), color = "red")
  )
  
  
  
}

shinyApp(ui, server)




