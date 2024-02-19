library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)

# original dataset
df <- read.csv('theDataset.csv')

# missing values of each column
missing_data <- df %>%
  gather(key = "variable", value = "value") %>% 
  mutate(is_missing = is.na(value) | value == "") %>%
  group_by(variable) %>%
  summarize(missing_count = sum(is_missing))

ggplot(missing_data, aes(x = reorder(variable, -missing_count), y = missing_count)) +
  geom_bar(stat = "identity", fill = "#FF9999", color = "black") +  # Adjusted color and added outline
  geom_text(aes(label = missing_count), vjust = 0.5, hjust = 1.1, size = 2.5) +  # Adjusted position & size
  coord_flip() +
  labs(title = "Number of Missing Values per Column",
       x = "Columns",
       y = "Number of Missing Values") +
  theme_minimal()

# changing date_published on the movies with wrong values
df[df$imdb_title_id == "tt0399854", "date_published"] <- as.Date("2003-06-25")
df[df$imdb_title_id == "tt1121794", "date_published"] <- as.Date("2007-06-25")
df[df$imdb_title_id == "tt1146325", "date_published"] <- as.Date("2008-06-25")
df[df$imdb_title_id == "tt0291350", "date_published"] <- as.Date("2001-06-25")
df[df$imdb_title_id == "tt1194263", "date_published"] <- as.Date("2009-06-25")
df[df$imdb_title_id == "tt1724970", "date_published"] <- as.Date("2017-06-25")
df[df$imdb_title_id == "tt7825208", "date_published"] <- as.Date("2019-06-25")


# separate budget into budget_value and budget_currency
df <- df %>%
  mutate(
    budget_currency = ifelse(grepl("[A-Z]{3} ", budget), 
                             gsub(" ([0-9]+)", "", budget), 
                             "USD"),
    budget_value = as.numeric(gsub("[^0-9]", "", budget))
  )

# exchange rates dataset
exchanges <- read.csv('exchanges.csv')
# join both datasets
joined_df <- left_join(df, exchanges, by = c("year", "budget_currency" = "currency"))

# new column 'budget_in_usd'
joined_df <- joined_df %>%
  mutate(
    budget_in_usd = ifelse(is.na(exchange_rate) | budget_currency == "USD", 
                           budget_value, 
                           budget_value * exchange_rate)
  )

# remove redundant columns
joined_df$budget <- NULL
joined_df$exchange_rate <- NULL
joined_df$budget_currency <- NULL
joined_df$budget_value <- NULL
# rename 'budget_in_usd' to 'budget' since that will be our original 'budget' column
joined_df <- joined_df %>%
  rename(budget = budget_in_usd)

# see the exact row in which the values are missing for the column where the
# missing values are 4 or lower

missing_values_rows <- joined_df %>%
  filter(is.na(reviews_from_users) | reviews_from_users == "")
joined_df <- joined_df %>%
  mutate(reviews_from_users = ifelse(imdb_title_id == "tt1801051", 0, reviews_from_users))

missing_values_rows2 <- joined_df %>%
  filter(is.na(actors) | actors == "")
joined_df <- joined_df %>%
  mutate(actors = ifelse(imdb_title_id == "tt2396224", "No actors", actors))

missing_values_rows3 <- joined_df %>%
  filter(is.na(language_1) | language_1 == "")
joined_df <- joined_df %>%
  mutate(language_1 = ifelse(imdb_title_id == "tt1341710", "English", language_1))
joined_df <- joined_df %>%
  mutate(language_2 = ifelse(imdb_title_id == "tt1341710", "Polish", language_2))
joined_df <- joined_df %>%
  mutate(language_1 = ifelse(imdb_title_id == "tt2937366", "English", language_1))

missing_values_rows4 <- joined_df %>%
  filter(is.na(writer) | writer == "")
joined_df <- joined_df %>%
  mutate(writer = ifelse(imdb_title_id == "tt0411705", "Michael Winterbottom", writer))
joined_df <- joined_df %>%
  mutate(writer = ifelse(imdb_title_id == "tt1740047", "No writer", writer))
joined_df <- joined_df %>%
  mutate(writer = ifelse(imdb_title_id == "tt2396224", "No writer", writer))
joined_df <- joined_df %>%
  mutate(writer = ifelse(imdb_title_id == "tt4458206", "Ilkay Isik", writer))

# find duplicated values in the 'title' column
duplicates <- joined_df %>%
  group_by(title) %>%
  filter(n() > 1) %>%
  distinct(title)

# change the titles where the name is the same to "title + year"
joined_df$title[joined_df$title %in% duplicates$title] <- paste(joined_df$title[joined_df$title %in% duplicates$title], joined_df$year[joined_df$title %in% duplicates$title], sep = "")

joined_df$worlwide_gross_income[joined_df$worlwide_gross_income == "INR 600000000"] <- "7193070"
joined_df$worlwide_gross_income[joined_df$worlwide_gross_income == ""] <- NA

# create the profit column
joined_df <- joined_df %>%
  mutate(
    worlwide_gross_income = as.numeric(worlwide_gross_income),
    profit = worlwide_gross_income - budget
  )

joined_df$profit <- joined_df$profit / 1e6
joined_df$budget <- joined_df$budget / 1e6
joined_df$worlwide_gross_income <- joined_df$worlwide_gross_income / 1e6
joined_df$usa_gross_income <- joined_df$usa_gross_income / 1e6

summary(joined_df)


# ------------------- // -----------------------------  // -----------------------------
# ------------------- //   DATA DISTRIBUTION   // -----------------------------

# 'duration'
ggplot(joined_df, aes(x = duration)) +
  geom_histogram(fill = "#3498db", color = "black", bins = 30) +
  scale_x_continuous(breaks = seq(45, 365, by = 5), limits = c(45, 365)) +
  labs(title = "Distribution of Movie Durations",
       x = "Duration (minutes)",
       y = "Number of Movies") +
  theme_minimal()


# 'avg_votes'

ggplot(joined_df, aes(x = avg_vote)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of 'avg_vote'", 
       x = "'avg_vote'", 
       y = "Density") +
  theme_minimal()

ggplot(joined_df, aes(y = avg_vote)) +
  geom_boxplot(fill = "yellow", color = "black", alpha = 0.5) +
  scale_y_continuous(breaks = seq(min(joined_df$avg_vote, na.rm = TRUE), 
                                  max(joined_df$avg_vote, na.rm = TRUE), by = 0.5)) +
  labs(title = "Box Plot of 'avg_vote'", 
       x = "", 
       y = "'avg_vote'") +
  theme_minimal() +
  theme(plot.title.position = "plot", plot.title = element_text(hjust = 0.5))


#profit
ggplot(data = joined_df, aes(x = profit)) +
  geom_density(fill = "lightgreen") +
  xlim(-150, 300) +  # Set the x-axis limits
  labs(title = "Zoomed-In Density Plot of Profit") +
  theme_minimal()

# 'votes'
ggplot(joined_df, aes(x = votes)) +
  geom_histogram(fill = "#3498db", color = "black", bins = 50, alpha = 0.7) +
  labs(title = "Distribution of Votes for Movies",
       x = "Number of Votes",
       y = "Number of Movies") +
  theme_minimal()

ggplot(joined_df, aes(x = votes)) +
  geom_histogram(fill = "#3498db", color = "black", bins = 30, alpha = 1.3) +
  scale_x_log10(breaks = scales::trans_breaks("log10", function(x) 10^x),
                labels = scales::trans_format("log10", scales::math_format(10^.x))) +
  labs(title = "Distribution of Votes for Movies (Log Scale)",
       x = "Number of Votes (Log Scale)",
       y = "Number of Movies") +
  theme_minimal()

# 'reviews_from_users'
ggplot(joined_df, aes(x = reviews_from_users)) +
  geom_histogram(fill = "#3498db", color = "black", bins = 55, alpha = 0.3) +
  labs(title = "Histogram of Reviews from Users",
       x = "Number of Reviews from Users",
       y = "Number of Movies") +
  theme_minimal()

ggplot(joined_df, aes(x = reviews_from_users)) +
  geom_density(fill = "#3498db", alpha = 0.5) +
  labs(title = "Density Plot of Reviews from Users",
       x = "Number of Reviews from Users",
       y = "Density") +
  theme_minimal()

# "language_1'
filtered_data_language <- joined_df %>%
  group_by(language_1) %>%
  filter(n() >= 3)

ggplot(filtered_data_language, aes(x = language_1)) +
  geom_bar(fill = "chocolate", color = "black", alpha = 0.7) +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  labs(title = "Number of Movies Produced in Each Language",
       x = "Language",
       y = "Number of Movies") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ------------------- // -----------------------------  // -----------------------------
# ------------------- //   SUBSET OF THE BEST MOVIES   // -----------------------------

#Success function
top_movies_dataset <- joined_df[joined_df$profit > 15.867 & joined_df$avg_vote > 5.9 & joined_df$votes > 17947, ]
top_movies_dataset <- top_movies_dataset[!apply(is.na(top_movies_dataset), 1, all), ]

# Scatter plot
ggplot(joined_df, aes(x = budget, y = profit)) +
  geom_point(aes(color = ifelse(profit > 0, "Profitable", "Not Profitable")), alpha = 0.5) +
  scale_color_manual(values = c("Profitable" = "green", "Not Profitable" = "red")) +
  labs(title = "Relation between Budget and Profit",
       x = "Budget (in millions)",
       y = "Profit (in millions)",
       color = "Movie Profitability") +
  theme_minimal() +
  geom_smooth(method = "lm", col = "blue", se = FALSE) +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        legend.text = element_text(face = "bold"))

# Create a new column indicating if language_1 is English or not
joined_df$language_status <- ifelse(joined_df$language_1 == "English", "English", "Not English")

# Boxplot
ggplot(joined_df, aes(x = language_status, y = avg_vote)) +
  geom_boxplot(aes(fill = language_status)) +
  labs(title = "Distribution of Average Votes by Language",
       x = "Language Status",
       y = "Average Vote") +
  scale_fill_manual(values = c("English" = "#3498db", "Not English" = "#e74c3c")) +
  theme_minimal() +
  theme(legend.position = "none")

library(plyr)
# Summarizing the data
data_summary <- ddply(joined_df, "year", summarise,
                      usa_income = sum(usa_gross_income, na.rm = TRUE),
                      total_income = sum(worlwide_gross_income, na.rm = TRUE))

data_summary$percentage <- (data_summary$usa_income / data_summary$total_income) * 100

ggplot(data_summary, aes(x = year)) +
  geom_col(aes(y = total_income, fill = "Worldwide Gross Income"), position = "stack", color = "black") +
  geom_col(aes(y = usa_income, fill = "USA Gross Income"), position = "stack", color = "black") +
  geom_text(aes(y = usa_income/2, label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.9), size = 3, fontface="bold") +
  labs(title = "Worldwide Gross Income with USA Contribution",
       x = "Year",
       y = "Income") + 
  scale_fill_manual(values = c("Worldwide Gross Income" = "coral", "USA Gross Income" = "cornflowerblue")) +
  theme_minimal() 

detach(package:plyr, unload = TRUE)

# create country_category column
joined_df$country_category <- ifelse(joined_df$country == "USA", "USA", "Non-USA")
# median budget for each category
median_budget_data <- joined_df %>%
  dplyr::group_by(country_category) %>%
  dplyr::summarise(median_budget = median(budget, na.rm = TRUE))

ggplot(median_budget_data, aes(x = country_category, y = median_budget, fill = country_category)) +
  geom_bar(stat = "identity", color="black", width=0.7) +
  labs(title = "Median Budget for USA vs Non-USA Movies",
       x = "Country Category",
       y = "Median Budget (in millions)") +
  scale_fill_manual(values = c("darkcyan", "darkolivegreen2")) +
  theme_minimal()

# ------------------- // -----------------------------  // -----------------------------
# ------------------- //   SUCCESS RATING FORMULA   // -----------------------------

success_rating <- function(top_movies_dataset) {
  normalize <- function(x) {
    (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  }
  
  top_movies_dataset %>%
    mutate(
      normalized_profit = normalize(profit),
      normalized_avg_vote = normalize(avg_vote),
      normalized_votes = normalize(votes),
      success_rating = (0.5 * normalized_profit) + 
        (0.25 * normalized_avg_vote) + 
        (0.25 * normalized_votes)
    )
}

new_top <- success_rating(top_movies_dataset)

success_rating_0_10 <- function(new_top) {
  new_top %>%
    mutate(
      success_rating_0_10 = ((success_rating - min(success_rating, na.rm = TRUE)) / (max(success_rating, na.rm = TRUE) - min(success_rating, na.rm = TRUE))) * 10
    )
}
new_top <- success_rating_0_10(new_top)

numeric_columns <- sapply(new_top, is.numeric)
new_top[numeric_columns] <- lapply(new_top[numeric_columns], round, 3)

new_top <- new_top %>%
  select(-normalized_profit, -normalized_avg_vote, -normalized_votes, -success_rating)

# ------------------- // -----------------------------  // -----------------------------
# ------------------- //         MONTH COLUMN         // -----------------------------
library(lubridate)

# Exctract the month in a new column
new_top$month_published <- month(new_top$date_published)

# Transform numeric months to month names
month_names <- c("January", "February", "March", "April", "May", "June", 
                 "July", "August", "September", "October", "November", "December")
new_top$month_published <- month_names[new_top$month_published]
new_top$month_published <- factor(new_top$month_published, 
                                  levels = month.name, 
                                  ordered = TRUE)



new_top <- new_top %>%
  mutate(country = sapply(strsplit(country, ","), `[`, 1))

# ------------------- // -----------------------------  // -----------------------------
# ------------------- //         LONG FORMAT         // -----------------------------

library(splitstackshape)

# split 'actors_f2' column by comma
new_top$actors_f2 <- sapply(strsplit(new_top$actors_f2, ","), function(x) ifelse(length(x) > 1, x[2], ""))

colnames(new_top)[colnames(new_top) == 'actors_f2'] <- 'actors_2'

# expand all the combinations by genre, director, writer
expanded_new_top <- new_top %>%
  separate_rows(genre, sep = ", ") %>%
  separate_rows(director, sep = ", ") %>%
  separate_rows(writer, sep = ", ")

films_by_genre <- expanded_new_top %>%
  distinct(imdb_title_id, genre, .keep_all = TRUE) %>%
  add_count(imdb_title_id, genre, name = "num_films") %>%
  distinct(imdb_title_id, genre, .keep_all = TRUE)

# ------------------- // -----------------------------  // -----------------------------
# ------------------- // -----TOP MOVIES DATASET-----  // ------------------------------------

# bar graph of success grouping by year and month
grouped_data <- expanded_new_top %>%
  group_by(month_published, year) %>%
  summarise(mean_success_rating = mean(success_rating_0_10, na.rm = TRUE))

grouped_data$year <- factor(grouped_data$year, levels = rev(unique(grouped_data$year)))

plot_1<-plot_ly(grouped_data, x=~month_published, y=~mean_success_rating ) %>%
  add_bars(color=~ordered(year))
plot_1

# ------------------- // -----------------------------

# Genre and Month Heatmap
# The creation of a heatmap makes easy the comparision of success between each month of release and each genre.
# The visualization shows clearly that films whose genre is animation and were released in July are very prone to have a lot of success.
# The main reason could be related to the school break for kids during this month and the fact that kids are the main audience of this type of movies.

films_by_genre_graph<-films_by_genre %>%
  group_by(genre,month_published) %>%
  summarise(
    AvgSuccess=mean(success_rating_0_10, na.rm = TRUE))


plot_2<-ggplot(films_by_genre_graph, aes(x = month_published, y = genre, fill = AvgSuccess)) +
  geom_tile() +scale_fill_viridis_c(option = "plasma") + 
  labs(x = "Month published", y = "Genre") + theme_minimal()+theme(legend.key.size = unit(2.5, "cm"),
                                                                   axis.text.x = element_text(size = 17, face = "bold"),
                                                                   axis.text.y = element_text(size = 17, face = "bold"),
                                                                   axis.title.x = element_text(size = 17, face = "bold"),
                                                                   axis.title.y = element_text(size = 17, face = "bold"))
plot_2
ggplotly(plot_2)

# ------------------- // ----------------------------- 

# language only
top_languages <- new_top %>%
  count(language_1) %>%
  top_n(10) %>%
  arrange(desc(n)) %>%
  pull(language_1)

new_top_filtered_language <- new_top %>%
  filter(language_1 %in% top_languages)


plot3_2<-ggplot(new_top_filtered_language, aes(x = language_1, y = success_rating_0_10, color = language_1, face = "bold")) +
  geom_jitter(position = position_jitter(width = 0.3), size = 5, alpha = 1) +
  labs(x = "Language", y = "Success Rating (0-10)", color = "Language", face = "bold") +
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

plot3_2

# ------------------- // -----------------------------

# bar graph of duration
# new column 'duration_group' with duration values grouped in bins of 15, including the 288 value
new_top$duration_group <- cut(new_top$duration, breaks = c(seq(60, 285, by = 15), 288), include.lowest = TRUE)

new_graph<-new_top %>%
  group_by(duration_group)%>%
  summarise(
    AverageSuccess = mean(success_rating_0_10, na.rm = TRUE),
    Count=n()
  )

ggplot(new_graph, aes(x = duration_group, y = AverageSuccess, fill = Count)) +
  geom_col() +
  geom_text(aes(label = Count), vjust = -0.5, size = 4, color = "black") +
  labs(x = "Duration Group", y = "Average Success", size = 50, face = "bold") +
  scale_fill_viridis_c() +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 15, face = "bold"),
    axis.title.x = element_text(size = 17, face = "bold"),
    axis.title.y = element_text(size = 17, face = "bold"),
    panel.background = element_rect(fill = "lightblue")  
  )
# The bar graph cant show a very clear optimum range, but we can afirm that movies between 140 to 190 minutes are the most prone to success.
# Make the same graph with profit and number of votes.

# ------------------- // -----------------------------

# Year column
year_aggregated_data <- expanded_new_top %>%
  group_by(year) %>%
  summarise(
    AverageSuccess = mean(success_rating_0_10, na.rm = TRUE),
    AverageProfit = mean(profit, na.rm = TRUE),
    Average_Vote = mean(avg_vote, na.rm = TRUE),
    Average_NrVotes = mean(votes, na.rm = TRUE),
    NumFilms = n()
  )

tableau_colors2 <- c("royalblue", "#F28E2B", "deeppink", "red1", "forestgreen",
                     "blue4", "#B07AA1", "plum", "indianred", "red4",
                     "turquoise", "#AEC7E8", "yellow2", "cyan4", "palegreen",
                     "purple", "yellowgreen", "magenta2", "#CAB2D6", "hotpink2")

tableau_colors <- c("#4E79A7", "#F28E2B", "#E15759", "#76B7B2", "#59A14F",
                    "#EDC948", "#B07AA1", "#FF9DA7", "#9C755F", "#BAB0AC",
                    "#D37295", "#AEC7E8", "#FFD92F", "#FF7F00", "#AFD1D2",
                    "#7F80CD", "#BEB9B5", "#C49C94", "#CAB2D6", "#FFFF99")

# plot the number of films by genre and year
bar_y_plot<- ggplot(films_by_genre, aes(x = ..count.., y = factor(year), fill = genre)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  labs(x = "Number of Films", y = "year", fill = "genre") +
  scale_fill_manual(values = tableau_colors, guide = guide_legend()) +
  theme(legend.position = "right") +
  scale_x_continuous(labels = scales::comma)

bar_y_plot

# ------------------- // -----------------------------

# Top Genres exploration
library(patchwork)

films_by_genre$month <- format(as.Date(films_by_genre$date_published), "%m")

genre_aggregated_data <- films_by_genre %>%
  group_by(genre) %>%
  summarise(
    AverageSuccess = mean(success_rating_0_10, na.rm = TRUE),
    AverageProfit = mean(profit, na.rm = TRUE),
    Average_Vote = mean(avg_vote, na.rm = TRUE),
    Average_NrVotes = mean(votes, na.rm = TRUE),
    NumFilms = n()
  )

genre_colors <- setNames(tableau_colors, unique(genre_aggregated_data$genre))

create_stacked_bar_plot <- function(data, y, title, colors) {
  ggplot(data, aes(x = reorder(genre, !!rlang::sym(y)), y = !!rlang::sym(y), fill = genre)) +
    geom_bar(stat = "identity") +
    theme_minimal() +
    labs(x = NULL, y = title, fill = "") +
    scale_fill_manual(values = colors) +
    coord_flip()
}

# 4 stacked bar plots
plot1 <- create_stacked_bar_plot(genre_aggregated_data, "AverageSuccess", "Success", genre_colors)
plot2 <- create_stacked_bar_plot(genre_aggregated_data, "AverageProfit", "Profit", genre_colors)
plot3 <- create_stacked_bar_plot(genre_aggregated_data, "Average_Vote", "Avg_Vote", genre_colors)
plot4 <- create_stacked_bar_plot(genre_aggregated_data, "Average_NrVotes", "Nr. Votes", genre_colors)

combined_plots <- plot1 + plot2 + plot3 + plot4 +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Genre Metrics",
    theme = theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right" 
    )
  )

print(combined_plots)

# Movies released by year
genre_year_aggregated_data <- films_by_genre %>%
  group_by(year, genre) %>%
  summarise(
    AverageSuccess = mean(success_rating_0_10, na.rm = TRUE),
    AverageProfit = mean(profit, na.rm = TRUE),
    Average_Vote = mean(avg_vote, na.rm = TRUE),
    Average_NrVotes = mean(votes, na.rm = TRUE),
    NumFilms = n()
  )

genre_colors <- c("Sci-Fi" = "blue", "Western" = "red2", "War" = "yellow2", "Adventure" = "green3", "Animation" = "pink")

selected_genres_SWWAA <- c("Sci-Fi", "Western", "War", "Adventure", "Animation")
filtered_data <- genre_year_aggregated_data %>% filter(genre %in% selected_genres_SWWAA)

p <- ggplot(filtered_data, aes(x = year, y = NumFilms, color = genre, text = paste("Success:", round(AverageSuccess, 4), "\nProfit:", round(AverageProfit, 2)))) +
  geom_point(size = 9, alpha = 0.7) +  
  geom_line(aes(group = genre), size = 4, alpha = 0.7) +
  scale_color_manual(values = genre_colors) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(2000, 2020, by = 1)) +
  scale_y_continuous(name = "Num Films", breaks = pretty_breaks(), limits = c(0, 40))

print(p)

# ------------------- // -----------------------------
library(devtools)
#install.packages("rlang")
#devtools::install_github("ricardo-bion/ggradar",dependencies = TRUE)
library(ggradar)

# Top 3 genres study

# Genre: Adventure
adventure_movies <- expanded_new_top %>%
  filter(grepl("Adventure", genre))
# get the top 7 actors of the 'Adventure' genre
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

# normalization
radar_data_normalized <- radar_data %>%
  mutate(across(-actor, ~ .x / max(.x, na.rm = TRUE)))

actors_adventure_graph <- ggradar(radar_data_normalized,
                                  group.point.size = 4) +
  theme(
    legend.text = element_text(face = "bold")
  )

print(actors_adventure_graph)
ggplotly(actors_adventure_graph)

# get the top 15 directors of the 'Adventure' genre
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

print(bubble_chart)
ggplotly(bubble_chart)

# get the top 15 writers of the 'Adventure' genre
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

# reshape data for plotting
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

print(stacked_bar_chart)

# Genre: Sci-Fi

sci_fi_movies <- expanded_new_top %>%
  filter(grepl("Sci-Fi", genre))

# get the top 7 actors of the 'Sci-Fi' genre
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

print(actors_sci_fi_graph)
ggplotly(actors_sci_fi_graph)

# get the top 15 directors of the 'Sci_Fi' genre
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

# unique directors
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

print(bubble_chart_sci_fi)
ggplotly(bubble_chart_sci_fi)

# get the top 15 writers of the 'Sci-Fi' genre
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

print(stacked_bar_chart_sci_fi)

# Genre: Animation

animation_movies <- expanded_new_top %>%
  filter(grepl("Animation", genre))

# get the top 7 actors of the 'Animation' genre
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

print(actors_animation_graph)
ggplotly(actors_animation_graph)

# get the top 15 directors of the 'Animation' genre
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

print(bubble_chart_animation)
ggplotly(bubble_chart_animation)

# get the top 15 writers of the 'Animation' genre
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

print(stacked_bar_chart_animation)

ggplotly(stacked_bar_chart_animation)

# ------------------- // -----------------------------
library(treemapify)

# Combinations study

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
    ConcatenatedGenres = paste(unique(genre), collapse = ", ")  # Concatenate all unique genres
  )

top_di_directors <- di_aggregated_data %>%
  arrange(desc(AverageSuccess)) %>%
  head(30)

top_actors<- a1_a2_aggregated_data %>%
  arrange(desc(AverageSuccess)) %>%
  head(10)

# Director/Writer
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

# Actor 1/Actor 2
library(RColorBrewer)

top_actors$actor_pair <- paste(top_actors$actors_1, top_actors$actors_2, sep = "/")
treemap <- ggplot(top_actors, aes(area = NumFilms, fill = AverageSuccess, label = actor_pair)) +
  geom_treemap() +
  geom_treemap_text(fontface = "italic", colour = "black", place = "centre", grow = TRUE) +
  scale_fill_gradient(low = "darkkhaki", high = "forestgreen") +
  theme_minimal() +
  labs(title = "Top 10 Actor Pairs by Success",
       fill = "Average Success") +
  theme(
    legend.title = element_text(face = "bold", size = 12),  # Bolder and slightly larger legend title
    legend.text = element_text(size = 10)  # Slightly larger legend text
  )

print(treemap)

# ------------------- // -----------------------------
library(tm)
library(tidytext)
library(textdata)

# Description
clean_description <- new_top$description %>%
  tolower() %>%  
  removePunctuation() %>%  
  removeNumbers() %>%  
  removeWords(stopwords("en")) %>%  
  stripWhitespace()  

# term document matrix
tdm <- TermDocumentMatrix(Corpus(VectorSource(clean_description)))
tdm_matrix <- as.matrix(tdm)
word_freq <- sort(rowSums(tdm_matrix), decreasing = TRUE)
word_freq_df <- data.frame(word = names(word_freq), freq = word_freq)

top_words <- head(word_freq_df, 15)

for(word in top_words$word) {
  new_top[[word]] <- grepl(word, new_top$description, ignore.case = TRUE)
}

word_success_correlation <- lapply(top_words$word, function(word) {
  data.frame(
    word = word,
    mean_success_with_word = mean(new_top[grepl(word, new_top$description, ignore.case = TRUE), "success_rating_0_10"], na.rm = TRUE),
    mean_success_without_word = mean(new_top[!grepl(word, new_top$description, ignore.case = TRUE), "success_rating_0_10"], na.rm = TRUE)
  )
})

word_success_correlation_df <- do.call(rbind, word_success_correlation)
word_success_correlation_df$word <- factor(word_success_correlation_df$word, levels = word_success_correlation_df$word[order(-word_success_correlation_df$mean_success_with_word)])

ggplot(word_success_correlation_df, aes(x = word, y = mean_success_with_word, fill = mean_success_with_word)) +
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


# Sentiment Analysis on Description

# tokenize
new_top_tokens <- new_top %>%
  unnest_tokens(word, description)

sentiment_lexicon <- get_sentiments("afinn")
new_top_sentiment <- new_top_tokens %>%
  inner_join(sentiment_lexicon, by = "word")

# Calculate average sentiment score for each movie
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

ggplot(filtered_data_sentiment, aes(x = sentiment_range, y = mean_success_rating, fill = mean_success_rating)) +
  geom_bar(stat = "identity", color = "black") +  # Added a black border to the bars
  scale_fill_gradient(low = "tan1", high = "red3") +
  geom_text(aes(label = sprintf("%.2f", mean_success_rating)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.3, 
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
