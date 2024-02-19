library(shiny)
library(dplyr)
library(plotly)
library(shinythemes)
library(readr)

films_by_genre <- read_csv("films_by_genre.csv")
new_top <- read_csv("new_top.csv")
expanded_new_top <- read_csv("expanded_new_top.csv")
top_movies_dataset <- read_csv("top_movies_dataset.csv")

grouped_data <- expanded_new_top %>%
  group_by(month_published, year) %>%
  summarise(
    mean_success_rating = mean(success_rating_0_10, na.rm = TRUE),
    mean_profit_rating = mean(profit, na.rm = TRUE),
    mean_votes_rating = mean(votes, na.rm = TRUE),
    mean_avg_vote_rating = mean(avg_vote, na.rm = TRUE)
  )


# Define UI for application
ui <- fluidPage(
  theme = shinytheme("flatly"), 
  
  # Custom CSS to enhance style
  tags$head(
    tags$style(HTML("
            .navbar { background-color: #2C3E50; }  # Customize navbar color
            .navbar-default .navbar-nav > li > a { color: white; }  # Navbar text color
            .sidebar { background-color: #ECF0F1; }  # Customize sidebar color
            .main-panel { background-color: #FAFAFA; }  # Customize main panel color
            h1, h2, h3, h4, h5, h6 { color: #3498DB; }  # Heading colors
            .well { background-color: #BDC3C7; }  # Customize well component
        "))
  ),
  
  # Application title with custom HTML
  titlePanel(tags$h1("Movie Analysis Dashboard", style = "color: #3498DB; text-align: center;")),
  
  # Tabs for different analyses
  navbarPage("",
             tabPanel("Time",
                      navlistPanel(
                        tabPanel("Month and Genre",
                                 plotlyOutput("publicationMonthPlot")),
                        tabPanel("Years and Months",
                                 sidebarLayout(
                                   sidebarPanel(
                                     # Select input for choosing metric
                                     selectInput(
                                       inputId = "metric",
                                       label = "Select Metric",
                                       choices = c('Success Rating', 'Profit', 'Votes', 'Average Vote'),
                                       selected = 'Success Rating'
                                     )
                                   ),
                                   mainPanel(
                                     plotlyOutput("Years_MonthPlot")
                                   )
                                 )
                        )
                      )
                      # UI elements for Publication Month
             ),
             tabPanel("Duration",
                      plotlyOutput("durationPlot")
                      # UI elements for Duration
             ),
             tabPanel("Genre",
                      navlistPanel(
                        tabPanel("Top Genres", 
                                 # UI elements for General Genres
                        ),
                        tabPanel("Adventure", 
                                 # UI elements for Adventure
                        ),
                        tabPanel("Sci-Fi",
                        ),
                        tabPanel("Animation"
                                 # UI elements for Adventure
                        ),
                        
                      )
             ),
             tabPanel("Combinations",
                      # UI elements for Combinations
             ),
             tabPanel("Language",
                      sidebarLayout(
                        sidebarPanel(
                          # Select input for choosing metric
                          selectInput(
                            inputId = "metric",
                            label = "Select Metric",
                            choices = c('Success Rating', 'Profit', 'Votes', 'Average Vote'),
                            selected = 'Success Rating'
                          )
                        ),
                        mainPanel(
                          plotlyOutput("LanguagePlot")
                          
                        )
                      )
             )
             ,
             tabPanel("Description",
                      # UI elements for Description)
             )
  ))

# Define server logic 
server <- function(input, output) {
  output$publicationMonthPlot <- renderPlotly({
    films_by_genre_graph <- films_by_genre %>%
      group_by(genre, month_published) %>%
      summarise(AvgSuccess = mean(success_rating_0_10, na.rm = TRUE), .groups = 'drop')
    
    # Generate the plot with adjusted sizes and rotation
    plot_2 <- ggplot(films_by_genre_graph, aes(x = month_published, y = genre, fill = AvgSuccess)) +
      geom_tile() +
      scale_fill_viridis_c(option = "plasma") +
      labs(x = "Month published", y = "Genre") +
      theme_minimal() +
      theme(
        legend.key.size = unit(1, "cm"),  
        axis.text.x = element_text(size = 12, angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(size = 12),  
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
      )
    
    ggplotly(plot_2)
  })
  
  
  output$Years_MonthPlot<-renderPlotly({
    
    # Function to choose metric based on select input
    selected_metric <- reactive({
      switch(input$metric,
             "Success Rating" = "mean_success_rating",
             "Profit" = "mean_profit_rating",
             "Votes" = "mean_votes_rating",
             "Average Vote" = "mean_avg_vote_rating")
    })
    plot_data <- plot_ly(grouped_data, x = ~month_published, y = as.formula(paste0("~", selected_metric()))) %>%
      add_bars(color = ~ordered(year))
    plot_data
  })
  
  output$durationPlot <- renderPlotly({
    # Assuming new_top is available in your global environment or loaded within this function
    
    # Create your duration groups and summarise data
    new_top$duration_group <- cut(new_top$duration, breaks = c(seq(60, 285, by = 15), 288), include.lowest = TRUE)
    
    new_graph <- new_top %>%
      group_by(duration_group) %>%
      summarise(
        AverageSuccess = mean(success_rating_0_10, na.rm = TRUE),
        Count = n(),
        .groups = 'drop'  # Ensure that the result is no longer grouped
      )
    
    # Generate the plot
    p <- ggplot(new_graph, aes(x = duration_group, y = AverageSuccess, fill = Count)) +
      geom_col() +
      geom_text(aes(label = Count), vjust = -0.5, size = 4, color = "black") +
      labs(x = "Duration Group", y = "Average Success") +
      scale_fill_viridis_c() +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 17, face = "bold"),
        axis.title.y = element_text(size = 17, face = "bold"),
        panel.background = element_rect(fill = "lightblue")
      )
    ggplotly(p)
  })
  
  output$LanguagePlot <- renderPlotly({
    # Get the top 10 most frequent languages
    top_languages <- new_top %>%
      count(language_1) %>%
      top_n(10) %>%
      arrange(desc(n)) %>%
      pull(language_1)
    
    # Filter the data for the top 10 languages
    new_top_filtered <- new_top %>%
      filter(language_1 %in% top_languages)
    
    selected_metric <- reactive({switch(input$metric,
                                        "Success Rating" = "success_rating_0_10",
                                        "Profit" = "profit",
                                        "Votes" = "votes",
                                        "Average Vote" = "avg_vote")})
    
    plot3_2 <- ggplot(new_top_filtered, aes_string(x = "language_1", y = as.formula(paste0("~", selected_metric())), color = "language_1", text = "title")) +
      geom_jitter(position = position_jitter(width = 0.3), size = 5, alpha = 1) +
      labs(x = "Language", y = input$metric, face = "bold") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        legend.key.size = unit(1, 'cm'), # Change legend key size
        legend.key.height = unit(1, 'cm'),# Change legend key height
        legend.position = c(0.9, 0.7),
        legend.key.width = unit(1, 'cm'), # Change legend key width
        legend.title = element_text(size = 12, face = "bold"), # Change legend title font size
        legend.text = element_text(size = 12, face = "bold")
      )  +
      guides(fill = guide_legend(title.position = "top", title.hjust = 0.4))+
      coord_flip()
    
    ggplotly(plot3_2)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
