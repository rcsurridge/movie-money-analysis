# plotting_functions.R

library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(lubridate)
library(stringr)
library(scales)
library(maps)
library(viridis)

box_plot <- function(data,
                     genre_input,
                     metric_input,
                     director_input,
                     decade_input,
                     score_input,
                     rating_input,
                     budget_input,
                     profit_input,
                     runtime_input,
                     season_input,
                     country_input) {

    if (genre_input == "All") {
        filtered_data <- data %>%
            filter(
                if (director_input != "All" && !is.null(director_input)) director == director_input else TRUE,
                if (country_input != "All" && !is.null(country_input)) country == country_input else TRUE,
                if (decade_input != "All" && !is.null(decade_input)) decade == decade_input else TRUE,
                if (rating_input != "All" && !is.null(rating_input)) rating == rating_input else TRUE,
                if (season_input != "All" && !is.null(season_input)) season == season_input else TRUE,
                score >= score_input[1] & score <= score_input[2],
                Budget >= budget_input[1] & Budget <= budget_input[2],
                Profit >= profit_input[1] & Profit <= profit_input[2],
                runtime >= runtime_input[1] & runtime <= runtime_input[2],
            )
    } else {
        filtered_data <- data %>%
            filter(
                if (genre_input != "All" && !is.null(genre_input)) genre == genre_input else TRUE,
                if (country_input != "All" && !is.null(country_input)) country == country_input else TRUE,
                if (director_input != "All" && !is.null(director_input)) director == director_input else TRUE,
                if (decade_input != "All" && !is.null(decade_input)) decade == decade_input else TRUE,
                if (rating_input != "All" && !is.null(rating_input)) rating == rating_input else TRUE,
                if (season_input != "All" && !is.null(season_input)) season == season_input else TRUE,
                score >= score_input[1] & score <= score_input[2],
                Budget >= budget_input[1] & Budget <= budget_input[2],
                Profit >= profit_input[1] & Profit <= profit_input[2],
                runtime >= runtime_input[1] & runtime <= runtime_input[2],
            )
      }

    if (nrow(filtered_data) == 0) {
        message <- "Data not available for selected input(s)"
        return(
            ggplot() +
            annotate("text", 
                     x = 0.5,
                     y = 0.5,
                     label = message,
                     size = 10,
                     vjust = 0.5,
                     hjust = 0.5
                     ) +
            theme_void()
        )
    }

    top_genres <- tail(names(sort(table(filtered_data$genre))), 7)

    filtered_data <- filtered_data %>%
        filter(genre %in% top_genres)

    y_label <- ifelse(metric_input == "Budget", "Budget (Millions)", "Profit (Millions)")
    title_prefix <- ifelse(metric_input == "Budget", "Cost of", "Profitability of")

    genre_title <- ifelse(genre_input != "All", paste(genre_input, "Movies"), "All Movies")
    director_title <- ifelse(director_input != "All", paste("Directed by", director_input), "")
    decade_title <- ifelse(decade_input != "All", paste("in the", decade_input), "")
    rating_title <- ifelse(rating_input != "All", paste("Rated", decade_input), "")
    season_title <- ifelse(season_input != "All", paste("during", season_input), "")
    country_title <- ifelse(country_input != "All", paste("in", country_input), "")

    title = str_squish(paste(title_prefix,
                             genre_title,
                             director_title,
                             decade_title,
                             rating_title,
                             season_title,
                             country_title
                            )
                      )

    caption <- ifelse(metric_input == "Budget",
                      "Dark blue points represent mean budgets for each genre (in millions)",
                      "Dark blue points represent mean profitabilities for each genre (in millions)"
                      )

    filtered_data <- filtered_data %>%
    mutate(color_category = if (metric_input == "Profit") {
        cut(Budget / 1e6,
            breaks = c(-Inf, 10, 100, Inf),
            labels = c("Budget less than $10M", 
                       "Budget between $10-100M", 
                       "Budget greater than $100M" ),
            include.lowest = TRUE)
    } else {
        cut(Profit / 1e6,
            breaks = c(-Inf, 50, 500, Inf),
            labels = c("Profit less than $50M", 
                       "Profit between $50-500M", 
                       "Profit greater than $500M" ),
            include.lowest = TRUE)
    })

    
    if (metric_input == "Profit") {

        box_plot <- ggplot(filtered_data, aes(x = genre, y = get(metric_input) / 1e6)) +
            geom_jitter(width = 0.2, alpha = 0.7, aes(colour=color_category)) +
            stat_summary(geom = "point", fun = "mean", size = 10, color = "darkblue") +
            scale_color_manual(values = c("Budget less than $10M" = "IndianRed", 
                                      "Budget between $10-100M" = "Gold", 
                                      "Budget greater than $100M" = "darkgreen")) +
            labs(title = title,
                x = "Genre",
                y = y_label,
                caption = caption,
                color = "Budget Category",
                size = 5
                ) +
            scale_y_continuous(trans = 'log10', labels = scales::dollar_format()) +
            theme_bw() +
            theme(axis.title = element_text(size = 12)) 
    }

    if (metric_input == "Budget") {

        box_plot <- ggplot(filtered_data, aes(x = genre, y = get(metric_input)  / 1e6)) +
            geom_jitter(width = 0.2, alpha = 0.7, aes(colour=color_category)) +
            stat_summary(geom = "point", fun = "mean", size = 10, color = "darkblue") +
            scale_color_manual(values = c("Profit less than $50M" = "IndianRed", 
                                      "Profit between $50-500M" = "Gold", 
                                      "Profit greater than $500M" = "darkgreen")) +
            labs(title = title,
                x = "Genre",
                y = y_label,
                caption = caption,
                color = "Profit Category",
                ) +
            scale_y_continuous(trans='log10', labels = scales::dollar_format()) +
            theme_bw()
    }

    return(box_plot)
}

season_box_plot <- function(data,
                            genre_input,
                            metric_input,
                            director_input,
                            decade_input,
                            score_input,
                            rating_input,
                            budget_input,
                            profit_input,
                            runtime_input,
                            season_input,
                            country_input) {

    filtered_data <- data %>%
        filter(
            if (genre_input != "All" && !is.null(genre_input)) genre == genre_input else TRUE,
            if (country_input != "All" && !is.null(country_input)) country == country_input else TRUE,
            if (director_input != "All" && !is.null(director_input)) director == director_input else TRUE,
            if (decade_input != "All" && !is.null(decade_input)) decade == decade_input else TRUE,
            if (rating_input != "All" && !is.null(rating_input)) rating == rating_input else TRUE,
            if (season_input != "All" && !is.null(season_input)) season == season_input else TRUE,
            score >= score_input[1] & score <= score_input[2],
            Budget >= budget_input[1] & Budget <= budget_input[2],
            Profit >= profit_input[1] & Profit <= profit_input[2],
            runtime >= runtime_input[1] & runtime <= runtime_input[2]
        ) %>%
        group_by(decade, season)

    if (nrow(filtered_data) == 0) {

        message <- "Data not available for selected input(s)"
        return(
            ggplot() +
            annotate("text", 
                     x = 0.5, 
                     y = 0.5, 
                     label = message, 
                     size = 10, 
                     vjust = 0.5, 
                     hjust = 0.5
                     ) +
            theme_void()
        )
    }

    genre_title <- ifelse(genre_input != "All", paste("for", genre_input, "Movies"), "")
    director_title <- ifelse(director_input != "All", paste("Directed by", director_input), "")
    decade_title <- ifelse(decade_input != "All", paste("in the", decade_input), "")
    rating_title <- ifelse(rating_input != "All", paste("Rated", decade_input), "")
    season_title <- ifelse(season_input != "All", paste("during", season_input), "")
    country_title <- ifelse(country_input != "All", paste("in", country_input), "")
    
    y_label <- ifelse(metric_input == "Budget", "Budget (Millions)", "Profit (Millions)")
    title_prefix <- ifelse(metric_input == "Budget", "Cost Range by Season", "Profit Range by Season")
    title <- str_squish(paste(title_prefix,
                              genre_title,
                              director_title,
                              decade_title,
                              rating_title,
                              season_title,
                              country_title
                              )
                        )
        
    season_box_plot <- ggplot(filtered_data, 
                              aes(x = as.factor(decade), y = get(metric_input) / 1e6, fill = season)
                              ) +
        geom_boxplot() +
        labs(title = title,
             x = "Decade",
             y = y_label,
             fill = "Season",
             caption = "Plot excludes the year 2020 because the dataset only included a few movies for that year."
            ) +
        scale_fill_manual(values = c("#8DA0CB", "#66C2A5", "#E78AC3", "#FC8D62")) +
        scale_y_continuous(trans = 'log10', labels = scales::dollar_format()) +
        theme_bw()
}

render_finance_map <- function(data,
                            genre_input,
                            metric_input,
                            director_input,
                            decade_input,
                            score_input,
                            rating_input,
                            budget_input,
                            profit_input,
                            runtime_input,
                            season_input,
                            country_input) {

    filtered_data <- data %>%
        filter(
            if (genre_input != "All" && !is.null(genre_input)) genre == genre_input else TRUE,
            if (director_input != "All" && !is.null(director_input)) director == director_input else TRUE,
            if (decade_input != "All" && !is.null(decade_input)) decade == decade_input else TRUE,
            if (rating_input != "All" && !is.null(rating_input)) rating == rating_input else TRUE,
            if (season_input != "All" && !is.null(season_input)) season == season_input else TRUE,
            if (country_input != "All" && !is.null(country_input)) country == country_input else TRUE,
            score >= score_input[1] & score <= score_input[2],
            Budget >= budget_input[1] & Budget <= budget_input[2],
            Profit >= profit_input[1] & Profit <= profit_input[2],
            runtime >= runtime_input[1] & runtime <= runtime_input[2],
            year %in% 1980:2019
            )

    if (nrow(filtered_data) == 0) {
        message <- "Data not available for selected input(s)"
        return(
            ggplot() +
            annotate("text", 
                     x = 0.5, 
                     y = 0.5, 
                     label = message, 
                     size = 10, 
                     vjust = 0.5, 
                     hjust = 0.5) +
            theme_void()
        )
    }

    genre_title <- ifelse(genre_input != "All", paste(genre_input, "Movies"), "All Movies")
    director_title <- ifelse(director_input != "All", paste("Directed by", director_input), "")
    decade_title <- ifelse(decade_input != "All", paste("in the", decade_input), "")
    rating_title <- ifelse(rating_input != "All", paste("Rated", decade_input), "")
    season_title <- ifelse(season_input != "All", paste("during", season_input), "")
    country_title <- ifelse(country_input != "All", paste("in", country_input), "")

    y_label <- ifelse(metric_input == "Budget", "Budget (Millions)", "Profit (Millions)")
    title_prefix <- ifelse(metric_input == "Budget", "Cost of", "Profitability of")

    title <- str_squish(paste(title_prefix,
                              genre_title,
                              director_title,
                              decade_title,
                              rating_title,
                              season_title,
                              country_title
                             )
                        )

    world_map <- map_data("world")

    all_country_outlines <- world_map %>%
        distinct(region, .keep_all = TRUE)

    column_to_aggregate <- paste0("filtered_data$", metric_input)

    avg_metric_data <- aggregate(x = eval(parse(text = column_to_aggregate)),
                             by = list(country = filtered_data$country),
                             FUN = mean)

    avg_metric_data$log_avg_metric <- log1p(avg_metric_data$x)

    merged_data <- merge(all_country_outlines, avg_metric_data, by.x = "region", by.y = "country", all.x = TRUE)

    breaks <- seq(0, max(merged_data$log_avg_metric, na.rm = TRUE), length.out = 6)
    labels <- scales::label_number(accuracy = 1000000, scale = 1e6)

    map <- ggplot() +
        geom_map(data = merged_data, aes(map_id = region, fill = log_avg_metric), 
                map = world_map, 
                color = "grey") +
        scale_fill_distiller(palette = "YlGn", 
                            name = paste("Average", metric_input), 
                            breaks = breaks, 
                            labels = labels,
                            na.value = "lightgrey",
                            direction = 1
                            ) +
        expand_limits(x = world_map$long, y = world_map$lat) +
        theme_void() +
        theme(legend.position = "bottom") +
        ggtitle(title)
}

budget_profit_plot <- function(data,
                               genre_input,
                               director_input,
                               decade_input,
                               score_input,
                               rating_input,
                               budget_input,
                               profit_input,
                               runtime_input,
                               season_input,
                               country_input) {

    filtered_data <- data %>%
        filter(
            if (genre_input != "All" && !is.null(genre_input)) genre == genre_input else TRUE,
            if (country_input != "All" && !is.null(country_input)) country == country_input else TRUE,
            if (director_input != "All" && !is.null(director_input)) director == director_input else TRUE,
            if (decade_input != "All" && !is.null(decade_input)) decade == decade_input else TRUE,
            if (rating_input != "All" && !is.null(rating_input)) rating == rating_input else TRUE,
            score >= score_input[1] & score <= score_input[2],
            Budget >= budget_input[1] & Budget <= budget_input[2],
            Profit >= profit_input[1] & Profit <= profit_input[2],
            runtime >= runtime_input[1] & runtime <= runtime_input[2],
            year %in% 1980:2019
            ) %>%
        mutate(profit_labeled = ifelse(Profit > 1500000000, TRUE, FALSE))

    if (nrow(filtered_data) == 0) {
        message <- "Data not available for selected input(s)"
        return(
            ggplot() +
            annotate("text", 
                     x = 0.5, 
                     y = 0.5, 
                     label = message, 
                     size = 10, 
                     vjust = 0.5, 
                     hjust = 0.5) +
            theme_void()
        )
    }

    genre_title <- ifelse(genre_input != "All", paste("for", genre_input, "Movies"), "")
    director_title <- ifelse(director_input != "All", paste("Directed by", director_input), "")
    decade_title <- ifelse(decade_input != "All", paste("in the", decade_input), "")
    rating_title <- ifelse(rating_input != "All", paste("Rated", decade_input), "")
    season_title <- ifelse(season_input != "All", paste("during", season_input), "")
    country_title <- ifelse(country_input != "All", paste("in", country_input), "")

    title <- str_squish(paste("Budget versus Profit", 
                              genre_title,
                              director_title,
                              decade_title,
                              rating_title,
                              season_title,
                              country_title
                             )
                        )
                        
    budget_profit_plot <- ggplot(filtered_data, 
                                 aes(x = Budget / 1e6, y = Profit / 1e6, label = name)
                                ) +
        geom_point() +
        geom_smooth(color = "darkblue") +
        annotate(geom = "rect",
                 xmin = -Inf,
                 xmax = Inf,
                 ymin = -Inf,
                 ymax = 100,
                 fill = "red",
                 alpha = 0.2
                ) +
        annotate(geom = "rect",
                 xmin = -Inf,
                 xmax = Inf,
                 ymin = 100,
                 ymax = 1000, 
                 fill = "yellow",
                 alpha = 0.2
                ) +
        annotate(geom = "rect",
                 xmin = -Inf,
                 xmax = Inf,
                 ymin = 1000,
                 ymax = Inf, 
                 fill = "green",
                 alpha = 0.2
                ) +
        labs(title = title,
            x = "Budget (Millions)", 
            y = "Profit (Millions)",
            caption = "Dark blue curve represents the nonlinear relationship between Budget and Profit"
            ) +
        theme_bw() +
        geom_label_repel(data = filter(filtered_data, 
                                       profit_labeled == TRUE
                                       ),
                         aes(label = name)
                        ) +
        annotate("text",
                 x = Inf,
                 y = 50,
                 label = "Profit less than $100M",
                 vjust = 1.5,
                 hjust = 1.05,
                 color = "darkred",
                 size = 5
                ) +
        annotate("text", 
                 x = Inf,
                 y = 600,
                 label = "Profit less than $1B",
                 vjust = 1.5,
                 hjust = 1.05,
                 color = "darkorange",
                 size = 5
                ) +
        annotate("text",
                 x = Inf,
                 y = 1950,
                 label = "Profit greater than $1B",
                 vjust = -1,
                 hjust = 1.05,
                 color = "darkgreen",
                 size = 5
                )
  }


filtered_data_table <- function(data) {
    
    table_data <- data %>%
        select(name, genre, director, decade, rating, season, score, Budget, Profit, runtime)
    
    table_data$Budget <- dollar_format()(table_data$Budget)
    table_data$Profit <- dollar_format()(table_data$Profit)

    colnames(table_data) <- toupper(colnames(table_data))

    return(table_data)
}