## March 21

Tom Zhang - Working with covid data. Exploring the data set. Trying to compare peak cases to first case.

## March 23

Zetterquist - Trying to figure out data set. Working with NHL data. Thinking of doing a Plotly graph comparing teams over time.

## March 28

Wood - Currently trying to figure out how to change the inputs based on competition (suggested different tabs for each), also working to solve an issue with "NAs" in year based on competition, suggested subsetting based on competition input.

## April 6

Volpe - Working with Tennis data, current issue with joining datasets because of different name/ID variables.

 tabPanel("College Career", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(numericInput("g", "# of Games Played", 38),
                            numericInput("comp", "# of Completions", 628),
                            numericInput("att", "# of Attempts", 987),
                            numericInput("compper", "Completion Percentage", 64.4),
                            numericInput("pass", "# of Passing Yards", 8220),
                            numericInput("yds", "Yards per Attempt", 8.6),
                            numericInput("adj", "Adjusted Pass per Attempt", 9.1),
                            numericInput("tds", "# of Touchdowns", 67),
                            numericInput("int", "# of Interceptions", 22),
                            numericInput("qbr", "Quarterback Rating", 155.9)
                            , width = 2),
               mainPanel(
                 plotOutput("g"),
                 plotOutput("comp"),
                 plotOutput("att"),
                 plotOutput("compper"),
                 plotOutput("pass"),
                 plotOutput("yds"),
                 plotOutput("adj"),
                 plotOutput("tds"),
                 plotOutput("int"),
                 plotOutput("qbr")
               )
             )
    )
    )
    )