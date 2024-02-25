library(shiny)
library(dplyr)
library(plotly)
library(patchwork)
library(scales)
library(shinythemes)
library(readr)
library(tidyr)
library(ggradar)
library(RColorBrewer)
library(treemapify)

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

ui <- fluidPage(
  theme = shinytheme("flatly"), 
  tags$head(
    tags$style(HTML("
            .navbar { background-color: #2C3E50; }
            .navbar-default .navbar-nav > li > a { color: white; }
            .sidebar { background-color: #ECF0F1; }
            .main-panel { background-color: #FAFAFA; }
            h1, h2, h3, h4, h5, h6 { color: #3498DB; }
            .well { background-color: #BDC3C7; }
        "))
  ),
  
  titlePanel(tags$h1("Movie Analysis Dashboard", style = "color: #3498DB; text-align: center;")),
  
  navbarPage("",
             tabPanel("Time",
                      navlistPanel(
                        tabPanel("Month and Genre",
                                 plotlyOutput("publicationMonthPlot")),
                        tabPanel("Years and Months",
                                 sidebarLayout(
                                   sidebarPanel(
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
             ),
             tabPanel("Duration",
                      plotlyOutput("durationPlot")
             ),
             navbarMenu("Genres",
                        tabPanel("Top Genres",
                                 navlistPanel(
                                   tabPanel("Genre Metrics",
                                            selectInput("variableSelect", 
                                                        "Choose a metric:", 
                                                        choices = list("Success" = "AverageSuccess", 
                                                                       "Profit" = "AverageProfit",
                                                                       "Avg Vote" = "Average_Vote",
                                                                       "Number of Votes" = "Average_NrVotes")),
                                            plotlyOutput("topGenresPlot") 
                                   ),
                                   tabPanel("Genre Evolution",
                                            sliderInput("yearRange", 
                                                        "Select Year Range:", 
                                                        min = 2000, 
                                                        max = 2020, 
                                                        value = c(2000, 2020), 
                                                        step = 1),
                                            plotlyOutput("genreYearPlot", width = "100%", height = "500px")
                                   )
                                 )
                        ),
                        tabPanel("Adventure",
                                 selectInput("adventureSelect", 
                                             "Choose a category:", 
                                             choices = list("Actors" = "actors", 
                                                            "Directors" = "directors",
                                                            "Writers" = "writers")),
                                 plotlyOutput("adventurePlot") 
                        ),
                        tabPanel("Sci-Fi",
                                 selectInput("sciFiSelect", 
                                             "Choose a category:", 
                                             choices = list("Actors" = "actors", 
                                                            "Directors" = "directors",
                                                            "Writers" = "writers")),
                                 plotlyOutput("sciFiPlot")
                        ),
                        tabPanel("Animation",
                                 selectInput("animationSelect", 
                                             "Choose a category:", 
                                             choices = list("Actors" = "actors", 
                                                            "Directors" = "directors",
                                                            "Writers" = "writers")),
                                 plotlyOutput("animationPlot")
                        )
             ),
             tabPanel("Combinations",
                      navlistPanel(
                        tabPanel("Director and Writer",
                                 plotOutput("directorwritercombination", width = "100%", height = "500px")
                        ),
                        tabPanel("3D Director and Writer",
                                 plotlyOutput("director3d")
                        ),
                        tabPanel("Actor 1/Actor 2",
                                 plotOutput("actor12")
                        )
                      )
             ),
             tabPanel("Language",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            inputId = "selectedMetric",
                            label = "Select Metric",
                            choices = c('Success Rating' = 'success_rating_0_10', 'Profit' = 'profit', 'Votes' = 'votes', 'Average Vote' = 'avg_vote'),
                            selected = 'success_rating_0_10'
                          )
                        ),
                        mainPanel(
                          plotlyOutput("LanguagePlot")
                          
                        )
                      )
             )
             ,
             tabPanel("Description",
                      navlistPanel(
                        tabPanel("Most Frequent Words",
                                 tags$img(src = "mfw.png", height = "500px", width = "100%")
                                 #plotlyOutput("mostFrequentWordsPlot")
                        ),
                        tabPanel("Sentiment Analysis",
                                 plotlyOutput("sentimentAnalysisPlot", width= "100%", height= "500px")
                        )
                      )
             )
))

server <- function(input, output) {
  output$publicationMonthPlot <- renderPlotly({
    films_by_genre_graph <- films_by_genre %>%
      group_by(genre, month_published) %>%
      summarise(AvgSuccess = mean(success_rating_0_10, na.rm = TRUE), .groups = 'drop')
    
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
    
    new_top$duration_group <- cut(new_top$duration, breaks = c(seq(60, 285, by = 15), 288), include.lowest = TRUE)
    
    new_graph <- new_top %>%
      group_by(duration_group) %>%
      summarise(
        AverageSuccess = mean(success_rating_0_10, na.rm = TRUE),
        Count = n(),
        .groups = 'drop'
      )
    
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
  filtered_data <- reactive({
    top_languages <- new_top %>%
      count(language_1) %>%
      top_n(10) %>%
      arrange(desc(n)) %>%
      pull(language_1)
    
    new_top %>%
      filter(language_1 %in% top_languages)
  })
  output$LanguagePlot <- renderPlotly({
    data <- filtered_data()
    metric <- input$selectedMetric
    
    ggplot(data, aes_string(x = "language_1", y = metric, color = "language_1")) +
      geom_jitter(position = position_jitter(width = 0.3), size = 1.5, alpha = 1) +
      labs(x = "Language", y = metric, color = "Language") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(size = 15, face = "bold"),
        axis.text.y = element_text(size = 15, face = "bold"),
        axis.title.x = element_text(size = 15, face = "bold"),
        axis.title.y = element_text(size = 15, face = "bold"),
        legend.key.size = unit(1, 'cm'),
        legend.key.height = unit(1, 'cm'),
        legend.position = c(0.9, 0.7),
        legend.key.width = unit(1, 'cm'), 
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12, face = "bold")
      )  +
      guides(fill = guide_legend(title.position = "top", title.hjust = 0.4))+
      coord_flip()
  })
  output$publicationMonthPlot <- renderPlotly({
    films_by_genre_graph <- films_by_genre %>%
      group_by(genre, month_published) %>%
      summarise(AvgSuccess = mean(success_rating_0_10, na.rm = TRUE), .groups = 'drop')
    
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
  output$durationPlot <- renderPlotly({
    new_top$duration_group <- cut(new_top$duration, breaks = c(seq(60, 285, by = 15), 288), include.lowest = TRUE)
    
    new_graph <- new_top %>%
      group_by(duration_group) %>%
      summarise(
        AverageSuccess = mean(success_rating_0_10, na.rm = TRUE),
        Count = n(),
        .groups = 'drop'
      )
    
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
  genre_aggregated_data <- reactive({
    films_by_genre$month <- format(as.Date(films_by_genre$date_published), "%m")
    
    films_by_genre %>%
      group_by(genre) %>%
      summarise(
        AverageSuccess = mean(success_rating_0_10, na.rm = TRUE),
        AverageProfit = mean(profit, na.rm = TRUE),
        Average_Vote = mean(avg_vote, na.rm = TRUE),
        Average_NrVotes = mean(votes, na.rm = TRUE),
        NumFilms = n(),
        .groups = 'drop'
      )
  })
  
  create_stacked_bar_plot <- function(data, y, title, colors) {
    ggplot(data, aes(x = reorder(genre, !!sym(y)), y = !!sym(y), fill = genre)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(x = NULL, y = title, fill = "") +
      scale_fill_manual(values = colors) +
      coord_flip()
  }
  
  output$topGenresPlot <- renderPlotly({
    variable <- input$variableSelect
    title <- switch(variable,
                    "AverageSuccess" = "Success",
                    "AverageProfit" = "Profit",
                    "Average_Vote" = "Avg Vote",
                    "Average_NrVotes" = "Number of Votes")
    
    data <- genre_aggregated_data()
    
    tableau_colors <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
                        "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC",
                        "#D37295", "#AEC7E8", "#FFD92F", "#FF7F00", "#AFD1D2",
                        "#7F80CD", "#BEB9B5", "#C49C94", "#CAB2D6", "#FFFF99")
    genre_colors <- setNames(tableau_colors, unique(data$genre))
    
    selected_plot <- create_stacked_bar_plot(data, variable, title, genre_colors)
    
    ggplotly(selected_plot)
  })
  output$genreYearPlot <- renderPlotly({
    year_range <- input$yearRange
    selected_genres <- c("Adventure", "Sci-Fi", "Animation", "War", "Western")
    genre_colors <- c("Sci-Fi" = "blue", "Western" = "red2", "War" = "yellow2", "Adventure" = "green3", "Animation" = "deeppink")
    
    filtered_data <- films_by_genre %>%
      filter(genre %in% selected_genres) %>%
      group_by(year, genre) %>%
      summarise(
        AverageSuccess = mean(success_rating_0_10, na.rm = TRUE),
        AverageProfit = mean(profit, na.rm = TRUE),
        Average_Vote = mean(avg_vote, na.rm = TRUE),
        Average_NrVotes = mean(votes, na.rm = TRUE),
        NumFilms = n()
      ) %>%
      filter(year >= year_range[1], year <= year_range[2])
    
    p <- ggplot(filtered_data, aes(x = year, y = NumFilms, color = genre)) +
      geom_point(size = 5, alpha = 0.7) +  
      geom_line(aes(group = genre), size = 1, alpha = 0.7) +
      scale_color_manual(values = genre_colors) +
      theme(plot.margin = margin(t = 30, r = 20, b = 50, l = 80)) +  
      labs(x = "Year", y = "Number of Films", title = "Films by Genre Over Years")
    
    ggplotly(p, tooltip = "text")
  })
  output$adventurePlot <- renderPlotly({
    req(input$adventureSelect)
    
    if(input$adventureSelect == "actors") {

      adventure_movies <- expanded_new_top %>%
        filter(grepl("Adventure", genre))
      
      combined_actors <- adventure_movies %>%
        select(imdb_title_id, actors_1, actors_2, profit, avg_vote, votes) %>%
        mutate(across(c(actors_1, actors_2), ~trimws(.))) %>%
        pivot_longer(cols = c(actors_1, actors_2), names_to = "actor_role", values_to = "actor")
      
      actor_adventure <- combined_actors %>%
        group_by(actor) %>%
        summarise(
          mean_profit = mean(profit, na.rm = TRUE),
          mean_avg_vote = mean(avg_vote, na.rm = TRUE),
          mean_votes = mean(votes, na.rm = TRUE),
        ) %>%
        arrange(desc(mean_profit))
      
      top_7_actors_adventure <- head(actor_adventure, 7)
      
      radar_data <- top_7_actors_adventure %>%
        select(actor, mean_profit, mean_avg_vote, mean_votes)
      
      radar_data_normalized <- radar_data %>%
        mutate(across(-actor, ~ .x / max(.x, na.rm = TRUE)))
      
      actors_adventure_graph <- ggradar(radar_data_normalized, group.point.size = 4) +
        theme(legend.text = element_text(face = "bold"))
      
      ggplotly(actors_adventure_graph)
      
    } else if(input$adventureSelect == "directors") {
      adventure_movies <- expanded_new_top %>%
        filter(grepl("Adventure", genre))
      
      director_adventure <- adventure_movies %>%
        group_by(director) %>%
        summarise(
          mean_profit = mean(profit, na.rm = TRUE),
          mean_avg_vote = mean(avg_vote, na.rm = TRUE),
          mean_votes = mean(votes, na.rm = TRUE),
          mean_success = mean(success_rating_0_10, na.rm = TRUE)
        ) %>%
        arrange(desc(mean_success))
      
      top_10_directors <- head(director_adventure, 10)
      
      unique_directors_adventure <- unique(top_10_directors$director)
      colors_adventure <- grDevices::rainbow(length(unique_directors_adventure))
      color_mapping_adventure <- setNames(colors_adventure, unique_directors_adventure)
      
      bubble_chart <- ggplot(top_10_directors, aes(x = mean_profit, y = mean_avg_vote, size = mean_votes, color = director)) +
        geom_point(alpha = 0.7) +
        scale_size_continuous(range = c(3, 15)) +
        scale_color_manual(values = color_mapping_adventure) +
        theme_minimal() +
        labs(title = "Top 10 Directors in Adventure Genre",
             x = "Mean Profit",
             y = "Average Vote",
             size = "Total Votes",
             color = "Director") +
        theme(
          legend.text = element_text(face = "bold", size = 12),
          legend.title = element_text(size = 14),
          legend.key.size = unit(1.5, "lines"),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"),
          axis.text.x = element_text(face = "bold", size = 11.5), 
          axis.text.y = element_text(face = "bold", size = 11.5)
        )
      ggplotly(bubble_chart)
      
    } else if(input$adventureSelect == "writers") {
      adventure_movies <- expanded_new_top %>%
        filter(grepl("Adventure", genre))
      
      writer_adventure <- adventure_movies %>%
        group_by(writer) %>%
        summarise(
          mean_profit = mean(profit, na.rm = TRUE),
          mean_avg_vote = mean(avg_vote, na.rm = TRUE),
          mean_votes = mean(votes, na.rm = TRUE),
          mean_success = mean(success_rating_0_10, na.rm = TRUE)
        ) %>%
        arrange(desc(mean_success))
      
      top_15_writers <- head(writer_adventure, 15)
      
      normalize <- function(x) {
        return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
      }
      
      top_15_writers <- top_15_writers %>%
        mutate(
          normalized_profit = normalize(mean_profit),
          normalized_avg_vote = normalize(mean_avg_vote),
          normalized_votes = normalize(mean_votes),
          normalized_success_ranking = normalize(mean_success)
        )
      
      top_15_writers_long <- top_15_writers %>%
        select(writer, normalized_profit, normalized_avg_vote, normalized_votes, normalized_success_ranking) %>%
        pivot_longer(cols = c(normalized_profit, normalized_avg_vote, normalized_votes, normalized_success_ranking, ), names_to = "metric", values_to = "value")
      
      top_15_writers_long$metric <- factor(top_15_writers_long$metric, levels = c("normalized_success_ranking", "normalized_profit", "normalized_avg_vote", "normalized_votes"))
      
      stacked_bar_chart <- ggplot(top_15_writers_long, aes(x = writer, y = value, fill = metric)) +
        geom_bar(stat = "identity", color = "black") + 
        scale_fill_manual(
          values = c("normalized_success_ranking" = "red1", "normalized_profit" = "green3", 
                     "normalized_avg_vote" = "dodgerblue3", "normalized_votes" = "magenta3"),
          labels = c("success_ranking", "profit", "avg_vote", "votes")
        ) +
        theme_minimal() +
        labs(title = "Top 15 Writers in Adventure Genre",
             x = "Writer",
             y = "Normalized Metrics",
             fill = "Metric") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
          legend.text = element_text(face = "bold"), 
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"), 
          legend.position = c(0.9, 0.9), 
          legend.direction = "vertical" 
        )
      ggplotly(stacked_bar_chart)
    }
  })
  output$sciFiPlot <- renderPlotly({
    req(input$adventureSelect)
    
    if(input$sciFiSelect == "actors") {
      
      sci_fi_movies <- expanded_new_top %>%
        filter(grepl("Sci-Fi", genre))
      
      combined_actors_sci_fi <- sci_fi_movies %>%
        select(imdb_title_id, actors_1, actors_2, profit, avg_vote, votes) %>%
        mutate(across(c(actors_1, actors_2), ~trimws(.))) %>%
        pivot_longer(cols = c(actors_1, actors_2), names_to = "actor_role", values_to = "actor")
      
      actor_sci_fi <- combined_actors_sci_fi %>%
        group_by(actor) %>%
        summarise(
          mean_profit = mean(profit, na.rm = TRUE),
          mean_avg_vote = mean(avg_vote, na.rm = TRUE),
          mean_votes = mean(votes, na.rm = TRUE),
        ) %>%
        arrange(desc(mean_profit))
      
      top_7_actors_sci_fi <- head(actor_sci_fi, 7)
      
      radar_data_sci_fi <- top_7_actors_sci_fi %>%
        select(actor, mean_profit, mean_avg_vote, mean_votes)
      
      # normalization
      radar_data_normalized_sci_fi <- radar_data_sci_fi %>%
        mutate(across(-actor, ~ .x / max(.x, na.rm = TRUE)))
      
      actors_sci_fi_graph <- ggradar(radar_data_normalized_sci_fi, group.point.size = 4) +
        theme(
          legend.text = element_text(face = "bold")
        )
      ggplotly(actors_sci_fi_graph)
      
    } else if(input$sciFiSelect == "directors") {
      sci_fi_movies <- expanded_new_top %>%
        filter(grepl("Sci-Fi", genre))
      
      director_sci_fi <- sci_fi_movies %>%
        group_by(director) %>%
        summarise(
          mean_profit = mean(profit, na.rm = TRUE),
          mean_avg_vote = mean(avg_vote, na.rm = TRUE),
          mean_votes = mean(votes, na.rm = TRUE),
          mean_success = mean(success_rating_0_10, na.rm = TRUE)
        ) %>%
        arrange(desc(mean_success))
      
      top_10_directors_sci_fi <- head(director_sci_fi, 10)
      
      unique_directors_sci_fi <- unique(top_10_directors_sci_fi$director)
      colors_sci_fi <- grDevices::rainbow(length(unique_directors_sci_fi))
      color_mapping_sci_fi <- setNames(colors_sci_fi, unique_directors_sci_fi)
      
      bubble_chart_sci_fi <- ggplot(top_10_directors_sci_fi, aes(x = mean_profit, y = mean_avg_vote, size = mean_votes, color = director)) +
        geom_point(alpha = 0.7) +
        scale_size_continuous(range = c(3, 15)) +
        scale_color_manual(values = color_mapping_sci_fi) +
        theme_minimal() +
        labs(title = "Top 10 Directors in Sci-Fi Genre",
             x = "Mean Profit",
             y = "Average Vote",
             size = "Total Votes",
             color = "Director") +
        theme(
          legend.text = element_text(face = "bold", size = 12), 
          legend.title = element_text(size = 14), 
          legend.key.size = unit(1.5, "lines"),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"), 
          axis.text.x = element_text(face = "bold", size = 11.5), 
          axis.text.y = element_text(face = "bold", size = 11.5)
        )
      ggplotly(bubble_chart_sci_fi)
      
    } else if(input$sciFiSelect == "writers") {
      sci_fi_movies <- expanded_new_top %>%
        filter(grepl("Sci-Fi", genre))
      
      writer_sci_fi <- sci_fi_movies %>%
        group_by(writer) %>%
        summarise(
          mean_profit = mean(profit, na.rm = TRUE),
          mean_avg_vote = mean(avg_vote, na.rm = TRUE),
          mean_votes = mean(votes, na.rm = TRUE),
          mean_success = mean(success_rating_0_10, na.rm = TRUE)
        ) %>%
        arrange(desc(mean_success))
      
      top_15_writers_sci_fi <- head(writer_sci_fi, 15)
      
      normalize <- function(x) {
        return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
      }
      
      top_15_writers_sci_fi <- top_15_writers_sci_fi %>%
        mutate(
          normalized_profit = normalize(mean_profit),
          normalized_avg_vote = normalize(mean_avg_vote),
          normalized_votes = normalize(mean_votes),
          normalized_success_ranking = normalize(mean_success)
        )
      
      top_15_writers_long_sci_fi <- top_15_writers_sci_fi %>%
        select(writer, normalized_profit, normalized_avg_vote, normalized_votes, normalized_success_ranking) %>%
        pivot_longer(cols = c(normalized_profit, normalized_avg_vote, normalized_votes, normalized_success_ranking, ), names_to = "metric", values_to = "value")
      
      top_15_writers_long_sci_fi$metric <- factor(top_15_writers_long_sci_fi$metric, levels = c("normalized_success_ranking", "normalized_profit", "normalized_avg_vote", "normalized_votes"))
      
      stacked_bar_chart_sci_fi <- ggplot(top_15_writers_long_sci_fi, aes(x = writer, y = value, fill = metric)) +
        geom_bar(stat = "identity", color = "black") +
        scale_fill_manual(
          values = c("normalized_success_ranking" = "red1", "normalized_profit" = "green3", 
                     "normalized_avg_vote" = "dodgerblue3", "normalized_votes" = "magenta3"),
          labels = c("success_ranking", "profit", "avg_vote", "votes")
        ) +
        theme_minimal() +
        labs(title = "Top 15 Writers in Sci-Fi Genre",
             x = "Writer",
             y = "Normalized Metrics",
             fill = "Metric") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 14),
          legend.text = element_text(face = "bold"), 
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"), 
          legend.position = c(0.9, 0.9), 
          legend.direction = "vertical" 
        )
      
      ggplotly(stacked_bar_chart_sci_fi)
    }
  })
  output$animationPlot <- renderPlotly({
    req(input$animationSelect)
    
    if(input$animationSelect == "actors") {
      
      animation_movies <- expanded_new_top %>%
        filter(grepl("Animation", genre))
      
      combined_actors_animation <- animation_movies %>%
        select(imdb_title_id, actors_1, actors_2, profit, avg_vote, votes) %>%
        mutate(across(c(actors_1, actors_2), ~trimws(.))) %>%
        pivot_longer(cols = c(actors_1, actors_2), names_to = "actor_role", values_to = "actor")
      
      actor_animation <- combined_actors_animation %>%
        group_by(actor) %>%
        summarise(
          mean_profit = mean(profit, na.rm = TRUE),
          mean_avg_vote = mean(avg_vote, na.rm = TRUE),
          mean_votes = mean(votes, na.rm = TRUE),
        ) %>%
        arrange(desc(mean_profit))
      
      top_7_actors_animation <- head(actor_animation, 7)
      
      radar_data_animation <- top_7_actors_animation %>%
        select(actor, mean_profit, mean_avg_vote, mean_votes)
      
      # normalization
      radar_data_normalized_animation <- radar_data_animation %>%
        mutate(across(-actor, ~ .x / max(.x, na.rm = TRUE)))
      
      actors_animation_graph <- ggradar(radar_data_normalized_animation,
                                        group.point.size = 4) +
        theme(
          legend.text = element_text(face = "bold")
        )
      
      ggplotly(actors_animation_graph)
      
    } else if(input$animationSelect == "directors") {
      animation_movies <- expanded_new_top %>%
        filter(grepl("Animation", genre))
      
      director_animation <- animation_movies %>%
        group_by(director) %>%
        summarise(
          mean_profit = mean(profit, na.rm = TRUE),
          mean_avg_vote = mean(avg_vote, na.rm = TRUE),
          mean_votes = mean(votes, na.rm = TRUE),
          mean_success = mean(success_rating_0_10, na.rm = TRUE)
        ) %>%
        arrange(desc(mean_success))
      
      top_10_directors_animation <- head(director_animation, 10)
      
      unique_directors_animation <- unique(top_10_directors_animation$director)
      colors_animation <- grDevices::rainbow(length(unique_directors_animation))
      color_mapping_animation <- setNames(colors_animation, unique_directors_animation)
      
      bubble_chart_animation <- ggplot(top_10_directors_animation, aes(x = mean_profit, y = mean_avg_vote, size = mean_votes, color = director)) +
        geom_point(alpha = 0.7) +
        scale_size_continuous(range = c(3, 15)) +
        scale_color_manual(values = color_mapping_animation) +
        theme_minimal() +
        labs(title = "Top 10 Directors in Animation Genre",
             x = "Mean Profit",
             y = "Average Vote",
             size = "Total Votes",
             color = "Director") +
        theme(
          legend.text = element_text(face = "bold", size = 12),
          legend.title = element_text(size = 14),
          legend.key.size = unit(1.5, "lines"),
          axis.title.x = element_text(face = "bold"),
          axis.title.y = element_text(face = "bold"), 
          axis.text.x = element_text(face = "bold", size = 11.5), 
          axis.text.y = element_text(face = "bold", size = 11.5)
        )
      
      ggplotly(bubble_chart_animation)
      
    } else if(input$animationSelect == "writers") {
      animation_movies <- expanded_new_top %>%
        filter(grepl("Animation", genre))
      
      writer_animation <- animation_movies %>%
        group_by(writer) %>%
        summarise(
          mean_profit = mean(profit, na.rm = TRUE),
          mean_avg_vote = mean(avg_vote, na.rm = TRUE),
          mean_votes = mean(votes, na.rm = TRUE),
          mean_success = mean(success_rating_0_10, na.rm = TRUE)
        ) %>%
        arrange(desc(mean_success))
      
      top_15_writers_animation <- head(writer_animation, 15)
      
      normalize <- function(x) {
        return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
      }
      
      top_15_writers_animation <- top_15_writers_animation %>%
        mutate(
          normalized_profit = normalize(mean_profit),
          normalized_avg_vote = normalize(mean_avg_vote),
          normalized_votes = normalize(mean_votes),
          normalized_success_ranking = normalize(mean_success)
        )
      
      # reshape data for plotting
      top_15_writers_long_animation <- top_15_writers_animation %>%
        select(writer, normalized_profit, normalized_avg_vote, normalized_votes, normalized_success_ranking) %>%
        pivot_longer(cols = c(normalized_profit, normalized_avg_vote, normalized_votes, normalized_success_ranking, ), names_to = "metric", values_to = "value")
      
      top_15_writers_long_animation$metric <- factor(top_15_writers_long_animation$metric, levels = c("normalized_success_ranking", "normalized_profit", "normalized_avg_vote", "normalized_votes"))
      
      stacked_bar_chart_animation <- ggplot(top_15_writers_long_animation, aes(x = writer, y = value, fill = metric)) +
        geom_bar(stat = "identity", color = "black") +
        scale_fill_manual(
          values = c("normalized_success_ranking" = "red1", "normalized_profit" = "green3", 
                     "normalized_avg_vote" = "dodgerblue3", "normalized_votes" = "magenta3"),
          labels = c("success_ranking", "profit", "avg_vote", "votes")
        ) +
        theme_minimal() +
        labs(title = "Top 15 Writers in Animation Genre",
             x = "Writer",
             y = "Normalized Metrics",
             fill = "Metric") +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 12),
          legend.text = element_text(face = "bold"), 
          axis.title.x = element_text(face = "bold"), 
          axis.title.y = element_text(face = "bold"), 
          legend.position = c(0.95, 0.9), 
          legend.direction = "vertical" 
        )
      
      ggplotly(stacked_bar_chart_animation)
    }
  })
  output$directorwritercombination <- renderPlot({
    tableau_colors2 <- c("royalblue", "#F28E2B", "deeppink", "red1", "forestgreen",
                         "blue4", "#B07AA1", "plum", "indianred", "red4",
                         "turquoise", "#AEC7E8", "yellow2", "cyan4", "palegreen",
                         "purple", "yellowgreen", "magenta2", "#CAB2D6", "hotpink2")
    
    di_aggregated_data <- expanded_new_top %>%
      group_by(genre, director, writer) %>%
      summarise(
        AverageSuccess = mean(success_rating_0_10, na.rm = TRUE),
        AverageProfit = mean(profit, na.rm = TRUE),
        Average_Vote = mean(avg_vote, na.rm = TRUE),
        Average_NrVotes = mean(votes, na.rm = TRUE),
        NumFilms = n_distinct(imdb_title_id)
      )
    
    a1_a2_aggregated_data <- expanded_new_top %>%
      group_by(actors_1, actors_2) %>%
      summarise(
        AverageSuccess = mean(success_rating_0_10, na.rm = TRUE),
        AverageProfit = mean(profit, na.rm = TRUE),
        Average_Vote = mean(avg_vote, na.rm = TRUE),
        Average_NrVotes = mean(votes, na.rm = TRUE),
        NumFilms = n_distinct(imdb_title_id),
        director_writer_genre = paste(
          unique(director), 
          sprintf("*%s*", unique(writer)),
          collapse = " / "),
        ConcatenatedGenres = paste(unique(genre), collapse = ", ")
      )
    
    top_di_directors <- di_aggregated_data %>%
      arrange(desc(AverageSuccess)) %>%
      head(30)
    
    top_actors<- a1_a2_aggregated_data %>%
      arrange(desc(AverageSuccess)) %>%
      head(10)
    
    lollipop_plot_dw <- ggplot(top_di_directors, aes_string(x = "reorder(paste(director, writer), +AverageSuccess)", y = "AverageSuccess", color = "genre")) +
      geom_segment(aes_string(xend = "reorder(paste(director, writer), -AverageSuccess)", yend = "0"), size = 1) +  
      geom_point(size = 4, shape = 1, color = "black") +
      coord_flip() +
      labs(title = "",
           x = "Director Writer",
           y = "Success") +
      theme_minimal() +
      theme(axis.text.y = element_text(angle = 0, hjust = 1)) +  
      scale_color_manual(values = tableau_colors2)
    
    lollipop_plot_dw
  })
  output$director3d <- renderPlotly({
    dw_aggregated_data <- expanded_new_top %>%
      group_by(director, writer) %>%
      summarise(
        AverageSuccess = mean(success_rating_0_10, na.rm = TRUE),
        AverageProfit = mean(profit, na.rm = TRUE),
        Average_Vote = mean(avg_vote, na.rm = TRUE),
        Average_NrVotes = mean(votes, na.rm = TRUE),
        NumFilms = n_distinct(imdb_title_id), 
        genre = toString(unique(genre)) 
      )
    top_dw_directors <- dw_aggregated_data %>%
      arrange(desc(AverageSuccess)) %>%
      head(20)
    scatter_plot <- plot_ly(
      data = top_dw_directors,
      x = ~director,
      y = ~writer,
      z = ~AverageSuccess,
      type = "scatter3d",
      mode = "markers",
      marker = list(size = 10, color = ~AverageSuccess, colorscale = "Viridis"),
      text = ~paste("Director: ", director, "<br>Writer: ", writer, "<br>Average Success: ", AverageSuccess)
    ) %>%
      layout(
        scene = list(
          xaxis = list(title = ""), 
          yaxis = list(title = ""), 
          zaxis = list(title = "Success")
        ),
        title = "Scatter 3D Plot of Directors and Writers",
        margin = list(l = 0, r = 0, b = 0, t = 40)
      )
    
    ggplotly(scatter_plot)
  })
  output$actor12 <- renderPlot({
    a1_a2_aggregated_data2 <- expanded_new_top %>%
      group_by(actors_1, actors_2) %>%
      summarise(
        AverageSuccess = mean(success_rating_0_10, na.rm = TRUE),
        AverageProfit = mean(profit, na.rm = TRUE),
        Average_Vote = mean(avg_vote, na.rm = TRUE),
        Average_NrVotes = mean(votes, na.rm = TRUE),
        NumFilms = n_distinct(imdb_title_id),
        director_writer_genre = paste(
          unique(director), 
          sprintf("*%s*", unique(writer)),
          collapse = " / "),
        ConcatenatedGenres = paste(unique(genre), collapse = ", ")
      )
    
    top_actors2<- a1_a2_aggregated_data2 %>%
      arrange(desc(AverageSuccess)) %>%
      head(10)
    
    top_actors2$actor_pair <- paste(top_actors2$actors_1, top_actors2$actors_2, sep = "/")
    
    treemap <- ggplot(top_actors2, aes(area = NumFilms, fill = AverageSuccess, label = actor_pair)) +
      geom_treemap() +
      geom_treemap_text(fontface = "italic", colour = "black", place = "centre", grow = TRUE) +
      scale_fill_gradient(low = "darkkhaki", high = "forestgreen") +
      theme_minimal() +
      labs(title = "Top 10 Actor Pairs by Success",
           fill = "Average Success") +
      theme(
        legend.title = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 10) 
      )
    treemap
  })
  output$mostFrequentWordsPlot <- renderPlotly({
    library(tm)
    library(tidytext)
    library(textdata)
    clean_description <- new_top$description %>%
      tolower() %>%  
      removePunctuation() %>%  
      removeNumbers() %>%  
      removeWords(stopwords("en")) %>%  
      stripWhitespace()  
    
    tdm <- TermDocumentMatrix(Corpus(VectorSource(clean_description)))
    tdm_matrix <- as.matrix(tdm)
    word_freq <- sort(rowSums(tdm_matrix), decreasing = TRUE)
    word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)
    
    top_words <- head(word_freq_df, 15)
    
    calculate_means <- function(word, data, success_column) {
      with_word <- sapply(data$description, function(desc) word %in% strsplit(desc, " ")[[1]])
      without_word <- !with_word
      
      mean_success_with_word <- mean(data[with_word, success_column], na.rm = TRUE)
      mean_success_without_word <- mean(data[without_word, success_column], na.rm = TRUE)
      
      return(c(mean_success_with_word, mean_success_without_word))
    }
    
    means_df <- do.call(rbind, lapply(top_words$word, calculate_means, data = new_top, success_column = "success_rating_0_10"))
    colnames(means_df) <- c("Mean_Success_With_Word", "Mean_Success_Without_Word")
    means_df$Word <- top_words$word
    p <- ggplot(means_df, aes(x = Word, y = Mean_Success_With_Word, fill = Mean_Success_With_Word)) +
      geom_bar(stat = "identity", color = "black", size = 0.5) +   
      scale_fill_gradient(low = "lightblue", high = "blue") + 
      geom_text(aes(label = round(mean_success_with_word, 2)),
                position = position_stack(vjust = 0.5),
                color = "black", size = 5, fontface = "bold") + 
      theme_minimal() +
      labs(title = "Mean Success Rating with Frequent Words",
           x = "Word",
           y = "Mean Success Rating") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 17, face = "bold"),
            axis.text.y = element_text(size = 16, face = "bold"),
            legend.position = "none") +
      ylim(0, 2.5)
    ggplotly(p)
  })
  output$sentimentAnalysisPlot <- renderPlotly({
    library(tm)
    library(tidytext)
    library(textdata)
    new_top_tokens <- new_top %>%
      unnest_tokens(word, description)
    
    sentiment_lexicon <- get_sentiments("afinn")
    new_top_sentiment <- new_top_tokens %>%
      inner_join(sentiment_lexicon, by = "word")
    
    average_sentiment <- new_top_sentiment %>%
      group_by(imdb_title_id) %>%
      summarize(average_sentiment = mean(value))
    
    new_top <- new_top %>%
      left_join(average_sentiment, by = "imdb_title_id")
    
    new_top$sentiment_range <- cut(new_top$average_sentiment, breaks = c(-5, -3, -1, 1, 3, 5), 
                                   labels = c("Very Negative", "Negative", "Neutral", "Positive", "Very Positive"))
    
    mean_metrics_by_sentiment <- new_top %>%
      group_by(sentiment_range) %>%
      summarize(mean_profit = mean(profit, na.rm = TRUE),
                mean_avg_vote = mean(avg_vote, na.rm = TRUE),
                mean_votes = mean(votes, na.rm = TRUE),
                mean_success_rating = mean(success_rating_0_10, na.rm = TRUE))
    
    filtered_data_sentiment <- mean_metrics_by_sentiment %>%
      filter(!is.na(sentiment_range))
    
    sa <- ggplot(filtered_data_sentiment, aes(x = sentiment_range, y = mean_success_rating, fill = mean_success_rating)) +
      geom_bar(stat = "identity", color = "black") +
      scale_fill_gradient(low = "tan1", high = "red3") +
      geom_text(aes(label = sprintf("%.2f", mean_success_rating)),
                vjust = -0.5, 
                color = "black") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(size = 14, face = "bold"),
            axis.title = element_text(size = 12),
            axis.text = element_text(color = "black"),
            axis.ticks = element_line(color = "black")) +
      scale_y_continuous(limits = c(0, 2.5), breaks = seq(0, 5, by = 0.5)) +
      labs(title = "Mean Success Rating by Sentiment Range",
           x = "Sentiment Range",
           y = "Mean Success Rating")
    
    ggplotly(sa)
  })
}

shinyApp(ui = ui, server = server)
