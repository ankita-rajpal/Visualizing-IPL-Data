# Install required packages if not installed
# install.packages(c("shiny", "dplyr", "tidyr", "ggplot2", "shinydashboard", "plotly"))

# Load required libraries
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(htmltools)

# Load datasets
matches <- read.csv("matches.csv")
deliveries <- read.csv("deliveries.csv")
overwise_statistics <- read.csv("overwise_statistics.csv")

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "IPL Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Introduction", tabName = "intro", icon = icon("info")),
      menuItem("Number of Wins for Each Team", tabName = "num_wins", icon = icon("trophy")),
      menuItem("Time Series Analysis", tabName = "time_series", icon = icon("line-chart")),
      menuItem("Year-wise Performance", tabName = "year_performance", icon = icon("bar-chart")),
      menuItem("Team toss Performance", tabName = "outcome_toss", icon = icon("pie-chart")),
      menuItem("Conclusion", tabName = "result", icon = icon("flag"))
    )
  ),
  dashboardBody(
    tabItems(
      # Introduction Tab
      tabItem(tabName = "intro",
              fluidPage(
                titlePanel("Introduction"),
                # Your introduction content goes here
                "Welcome to the IPL (Indian Premier League) Data Analysis 
                Dashboard! This interactive dashboard provides a comprehensive 
                overview of the IPL tournaments, allowing you to explore and 
                analyze various aspects of team performances, player statistics,
                and match outcomes.",
                tags$h3("About IPL"),
                tags$p("The Indian Premier League, commonly known as IPL, is
                       one of the most popular and exciting professional 
                       Twenty20 cricket leagues in the world. Established in 
                       2008, the IPL has witnessed intense competition among 
                       top cricket franchises, showcasing the talents of 
                       international and domestic players."),
                tags$h3("Dashboard Features"),
                tags$p("This dashboard offers a user-friendly interface with six
                       distinct sections, each designed to provide unique 
                       insights into IPL data:"),
                tags$ol(
                  tags$li("Number of Wins for Each Team: Explore the victories 
                          of IPL teams across different seasons using an 
                          intuitive bar graph."),
                  tags$li("Time Series Analysis - Team Comparison: Conduct a time
                          series analysis by selecting multiple teams to compare
                          their performance over the years, presented in an 
                          interactive line graph."),
                  tags$li("Year-wise Performance: Delve into detailed yearly 
                          statistics, including total runs, runs by fours, runs 
                          by sixes, and total wickets, through easy-to-navigate
                          tabs."),
                  tags$li("Team Toss Performance: Investigate the correlation 
                          between winning the toss and the subsequent match 
                          outcome for a specific team through a pie chart 
                          representation."),
                  tags$li("Conclusion and Result: Conclude your exploration with
                          insights and reflections on the rich data provided by 
                          this IPL Dashboard.")
                ),
                HTML("<p>This dashboard allows you to analyze IPL match data from 2008 to 2019. The dataset can be found <a href='https://www.kaggle.com/datasets/anuranroy/ipldatasets'>here</a>.</p>"),
                tags$p("Author: Ankita", style = "font-size: 14px; text-align: right;"),
                tags$p("Roll Number: MDS202309", style = "font-size: 14px; text-align: right;")
                
              )
      ),
      
      # Number of Wins Tab
      tabItem(tabName = "num_wins",
        fluidPage(
          titlePanel("Number of Wins for Each Team"),
          sliderInput("year_slider", "Select Year", min = 2008, max = 2019, value = 2008, step = 1, sep = ""),
          plotOutput("wins_plot"),
          textOutput("conclusion_text")
        )
),

      
      
      # Time Series Analysis Tab
      tabItem(tabName = "time_series",
              fluidPage(
                titlePanel("Time Series Analysis"),
                checkboxGroupInput("teams_checkbox", "Select Teams", choices = unique(matches$team1), selected = unique(matches$team1)[1]),
                plotOutput("time_series_plot")
              )
      ),
      
      # Year-wise Performance Tab
      tabItem(tabName = "year_performance",
              fluidPage(
                titlePanel("Year-wise Performance"),
                sliderInput("year_tab_slider", "Select Year", min = 2008, max = 2019, value = 2008, step = 1,sep = ""),
                tabsetPanel(
                  tabPanel("Total Runs", plotOutput("total_runs_plot")),
                  tabPanel("Runs by Fours", plotOutput("runs_by_fours_plot")),
                  tabPanel("Runs by Sixes", plotOutput("runs_by_sixes_plot")),
                  tabPanel("Total Wickets", plotOutput("total_wickets_plot"))
                )
              )
      ),
      
      # Composition of Total Wickets Tab
      tabItem(tabName = "outcome_toss",
              fluidPage(
                titlePanel("Outcome of matches after winning the toss"),
                selectInput("team_dropdown", "Select Team", choices = setdiff(unique(matches$team1), c("Rising Pune Supergiants", "Gujarat Lions", "Pune Warriors", "Kochi Tuskers Kerala","Rising Pune Supergiant")), selected = unique(matches$team1)[1]),
                plotOutput("outcome_toss_plot"),
                textOutput("toss_conclusion_text")
              )
      ),
      
      # Result Tab
      tabItem(tabName = "result",
              fluidPage(
                titlePanel("Conclusion"),
                # Your result content goes here
                'Our analysis of the IPL dataset spanning 2008 to 2019 has provided valuable insights into
                team performance, toss influence, dismissal patterns, and scoring strategies. Dominant teams
                like Mumbai Indians and Chennai Super Kings showcased consistency, while the toss’s impact varied across seasons. Common dismissals included ‘caught’ and ‘run out,’ and teams
                displayed diverse scoring approaches, emphasizing the importance of balanced line-ups. The
                passionate fan base continues to drive the IPL’s global appeal, reinforcing its status as a
                captivating blend of cricketing prowess and entertainment.'
              )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Number of Wins Plot
  output$wins_plot <- renderPlot({
    selected_year <- input$year_slider
    filtered_matches <- matches %>% filter(season == selected_year)
    
    ggplot(filtered_matches, aes(x = winner)) +
      geom_bar(fill = "skyblue", color = "black") +
      labs(title = paste("Number of Wins for Each Team in", selected_year),
           x = "Teams", y = "Number of Wins") +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  })
  output$conclusion_text <- renderText({
    # Your analysis code to determine the conclusion
    conclusion <- "In the above analysis, an intriguing observation emerges. Across multiple seasons, it is notable that the top two teams with the maximum number of wins often secure a spot in the finals for that particular season. This trend suggests a correlation between regular-season success and advancing to the finals, reflecting the competitive consistency of these teams."
    
    return(conclusion)
  })
  
  
  # Time Series Analysis Plot
  output$time_series_plot <- renderPlot({
    selected_teams <- input$teams_checkbox
    filtered_matches <- matches %>% filter(team1 %in% selected_teams | team2 %in% selected_teams)
    
    # Create a data frame for plotting
    plot_data <- filtered_matches %>%
      group_by(season, winner) %>%
      summarize(num_wins = n()) %>%
      filter(winner %in% selected_teams)
    
    # Create a line plot
    ggplot(plot_data, aes(x = as.factor(season), y = num_wins, color = winner, group = winner)) +
      geom_line() +
      geom_point() +
      labs(title = "Time Series Analysis",
           x = "Years", y = "Number of Wins",
           color = "Teams") +
      theme_minimal()
  })
  
 
  # Year-wise Performance Plots
  output$total_runs_plot <- renderPlot({
    selected_year <- input$year_tab_slider
    # Filter matches for the selected year
    filtered_matches <- matches %>% filter(season == selected_year)
    
    # Filter deliveries based on match_ids from the selected matches
    filtered_deliveries <- deliveries %>% filter(match_id %in% filtered_matches$match_id)
    
    # Summarize the total runs by batting_team
    runs_by_team <- filtered_deliveries %>%
      group_by(batting_team) %>%
      summarize(total_runs = sum(total_runs))
    
    # Plot the data
    # Plot the data with vertically displayed team names
    ggplot(runs_by_team, aes(x = batting_team, y = total_runs)) +
      geom_bar(stat = "identity", fill = "orange", color = "black") +
      labs(title = paste("Total Runs Scored by Each Team in", selected_year),
           x = "Teams", y = "Total Runs") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
    
  })
  
  output$runs_by_fours_plot <- renderPlot({
    selected_year <- input$year_tab_slider
    filtered_deliveries <- deliveries %>%
      filter(match_id %in% matches$match_id[matches$season == selected_year] & batsman_runs == 4)
    
    # Summarize the total fours by batting_team
    fours_by_team <- filtered_deliveries %>%
      group_by(batting_team) %>%
      summarize(total_fours = n())
    
    # Plot the data
    ggplot(fours_by_team, aes(x = batting_team, y = total_fours)) +
      geom_bar(stat = "identity", fill = "green", color = "black") +
      labs(title = paste("Total Runs Scored by Fours by Each Team in", selected_year),
           x = "Teams", y = "Total Runs by Fours") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
  })
  
  output$runs_by_sixes_plot <- renderPlot({
    selected_year <- input$year_tab_slider
    filtered_deliveries <- deliveries %>%
      filter(match_id %in% matches$match_id[matches$season == selected_year] & batsman_runs == 6)
    
    # Summarize the total sixes by batting_team
    sixes_by_team <- filtered_deliveries %>%
      group_by(batting_team) %>%
      summarize(total_sixes = n())
    
    # Plot the data
    ggplot(sixes_by_team, aes(x = batting_team, y = total_sixes)) +
      geom_bar(stat = "identity", fill = "blue", color = "black") +
      labs(title = paste("Total Runs Scored by Sixes by Each Team in", selected_year),
           x = "Teams", y = "Total Runs by Sixes") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
  })
  
  output$total_wickets_plot <- renderPlot({
    selected_year <- input$year_tab_slider
    
    # Filter deliveries for the selected year and exclude empty dismissal_kind
    filtered_deliveries <- deliveries %>%
      filter(match_id %in% matches$match_id[matches$season == selected_year] & player_dismissed != '')
    
    # Summarize the total wickets by bowling_team and dismissal_kind
    wickets_by_team <- filtered_deliveries %>%
      group_by(bowling_team, dismissal_kind) %>%
      filter(dismissal_kind != '') %>%  # Exclude rows with empty dismissal_kind
      summarize(total_wickets = n())
    
    # Plot the data
    ggplot(wickets_by_team, aes(x = bowling_team, y = total_wickets, fill = dismissal_kind)) +
      geom_bar(position = "stack", stat = "identity") +
      labs(title = paste("Different kind of Total Wickets Taken by Each Team in", selected_year),
           x = "Teams", y = "Total Wickets",
           fill = "Dismissal Kind") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
    
  })
  
  
  # Outcome after winning the Toss
  output$outcome_toss_plot <- renderPlot({
    selected_team <- input$team_dropdown
    # Filter matches for the selected team
    team_matches <- matches %>%
      filter(team1 == selected_team | team2 == selected_team)
    
    tossdf<- team_matches %>% filter(winner !='') %>%
      select(toss_winner,winner) %>%
      mutate(winning=if_else(toss_winner==winner,'won','loss'))
    tossdata<-tossdf %>% group_by(winning) %>% count(winning) %>%
      mutate(Percent = round(n/nrow(tossdf)*100,2)) 
    
    tossdata %>% ggplot(aes("",Percent,fill=winning))+
      geom_bar(stat = 'identity',width = 1)+
      coord_polar(theta = 'y')+
      theme_void()+
      geom_text(aes(label=paste0(Percent,'%')),position=position_stack(vjust =0.5))+
      labs(title='Outcome of matches after winning the toss',
           fill='Outcome')
    
  })
  output$toss_conclusion_text <- renderText({
    conclusion_toss <- "In the above analysis, a noteworthy observation arises. For most teams, the percentage of matches won after winning the toss hovers around 50%, with the highest deviation being 56% and the lowest being 44%. This distribution implies that cricket, being a sport influenced by various factors, goes beyond the mere outcome of the toss. It underscores the significance of skill, strategy, and on-field performance.\n\nWinning the toss, while providing an edge, does not guarantee victory. It serves as a factor that can influence the course of the game, but teams must complement it with strong batting, bowling, and fielding performances to secure a win."
    
    return(conclusion_toss)
  })
  
  
}

# Run the application
shinyApp(ui, server)
