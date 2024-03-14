# server.R

# Load necessary libraries
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(lubridate)
library(stringr)
library(scales)

source("plotting_functions.R")

source("data_process.R")
movie_df <- load_and_clean_data()

shinyServer(function(input, output, session) {

observeEvent({
    c(
    input$genreInput,
    input$decadeInput,
    input$seasonInput,
    input$directorInput,
    input$countryInput,
    input$ratingInput
  )
},
{
  filtered_data <- movie_df %>%
    filter(
      (input$genreInput == "All" | genre == input$genreInput) &
      (input$decadeInput == "All" | decade == input$decadeInput) &
      (input$seasonInput == "All" | season == input$seasonInput) &
      (input$directorInput == "All" | director == input$directorInput) &
      (input$countryInput == "All" | country == input$countryInput) &
      (input$ratingInput == "All" | rating == input$ratingInput)
    )
  
  updateSelectInput(
    session,
    "genreInput",
    choices = c("All", sort(unique(filtered_data$genre))),
    selected = input$genreInput
  )

  updateSelectInput(
    session,
    "decadeInput",
    choices = c("All", sort(unique(filtered_data$decade))),
    selected = input$decadeInput
  )

  filtered_data$season <- as.character(filtered_data$season)

  updateSelectInput(
    session,
    "seasonInput",
    choices = c("All", unique(filtered_data$season)),
    selected = input$seasonInput
  )

  updateSelectInput(
    session,
    "directorInput",
    choices = c("All", sort(unique(filtered_data$director))),
    selected = input$directorInput
  )

  updateSelectInput(
    session,
    "countryInput",
    choices = c("All", sort(unique(filtered_data$country))),
    selected = input$countryInput
  )
 
  updateSelectInput(
    session,
    "ratingInput",
    choices = c("All", sort(unique(filtered_data$rating))),
    selected = input$ratingInput
  )

    })


    output$BoxPlot <- renderPlot({
        box <- box_plot(movie_df,
                        input$genreInput,
                        input$budgetProfitInput,
                        input$directorInput,
                        input$decadeInput,
                        input$scoreInput,
                        input$ratingInput,
                        input$budgetInput,
                        input$profitInput,
                        input$runtimeInput,
                        input$seasonInput,
                        input$countryInput
                        )
        if (!is.null(box)) {
            print(box)
        } else {
            print("No data available.")
        }
    })

    output$SeasonBoxPlot <- renderPlot({
        season_box <- season_box_plot(movie_df,
                                      input$genreInput,
                                      input$budgetProfitInput,
                                      input$directorInput, 
                                      input$decadeInput,
                                      input$scoreInput,
                                      input$ratingInput,
                                      input$budgetInput,
                                      input$profitInput,
                                      input$runtimeInput,
                                      input$seasonInput,
                                      input$countryInput
                                     )
        if (!is.null(season_box)) {
            print(season_box)
        } else {
            print("No data available.")
        }
    })

    output$FinanceMap <- renderPlot({
        finance_map <- render_finance_map(movie_df,
                                        input$genreInput,
                                        input$budgetProfitInput,
                                        input$directorInput,
                                        input$decadeInput,
                                        input$scoreInput,
                                        input$ratingInput,
                                        input$budgetInput,
                                        input$profitInput,
                                        input$runtimeInput,
                                        input$seasonInput,
                                        input$countryInput
                                        )
        if (!is.null(finance_map)) {
            print(finance_map)
        } else {
            print("No data available.")
        }
    })

    output$BudgetProfitPlot <- renderPlot({
        budget_profit <- budget_profit_plot(movie_df,
                                            input$genreInput,
                                            input$directorInput,
                                            input$decadeInput,
                                            input$scoreInput,
                                            input$ratingInput,
                                            input$budgetInput,
                                            input$profitInput,
                                            input$runtimeInput,
                                            input$seasonInput,
                                            input$countryInput
                                           )
        if (!is.null(budget_profit)) {
            print(budget_profit)
        } else {
            print("No data available.")
        }
    })

    output$FilteredDataTable <- renderTable({
        filtered_data <- movie_df %>%
        filter(
            if (input$genreInput != "All") genre == input$genreInput else TRUE,
            if (input$countryInput != "All") country == input$countryInput else TRUE,
            if (input$directorInput != "All") director == input$directorInput else TRUE,
            if (input$decadeInput != "All") decade == input$decadeInput else TRUE,
            if (input$ratingInput != "All") rating == input$ratingInput else TRUE,
            if (input$seasonInput != "All") season == input$seasonInput else TRUE,
            score >= input$scoreInput[1] & score <= input$scoreInput[2],
            Budget >= input$budgetInput[1] & Budget <= input$budgetInput[2],
            Profit >= input$profitInput[1] & Profit <= input$profitInput[2],
            runtime >= input$runtimeInput[1] & runtime <= input$runtimeInput[2]
        )

        filtered_data_table(filtered_data)
    })
    

    output$learnMore <- renderText({
        if (input$learnMoreToggle == "Overview") {
            return("
            
            <h4>Project Overview</h4>

            <p><strong>Introduction</strong></p>

            <p>In the ever-evolving landscape of the film industry, understanding the financial dynamics of movies is crucial for filmmakers, producers, and 
            enthusiasts alike. This paper introduces a Shiny web application designed to explore and analyze financial trends in movies across various dimensions, including genres, countries, and time periods. The primary motivation behind creating this application is to provide users with a powerful tool that allows them to delve into the intricate details of the film industry, enabling a nuanced understanding of the financial performance of movies.</p>

            <p>Throughout this paper, we will delve into the methods employed to create these visualizations, showcasing how the design choices, including the use 
            of log scales, color categories, and specific plot types, enhance the user experience and contribute to a more nuanced understanding of financial patterns within the realm of cinema. Subsequently, we will present the results and discussions derived from the visualizations, highlighting their truthfulness, functionality, beauty, insightfulness, and enlightening nature. The subsequent sections will provide a detailed account of the methodology employed and the implications of the findings, ultimately shedding light on the intricate financial trends within the realm of cinema.</p>


            <p><strong>Problem Statement and Research Question</strong></p>

            <p>The film industry is marked by its complexity, with myriad factors influencing the success or failure of a movie. As such, the research question 
            addressed by this application is: 'How can we comprehensively visualize and analyze the financial trends in movies, considering diverse attributes such 
            as genres, countries of release, directors, and release decades?'</p>

            <p><strong>Shiny App Overview</strong></p>

            <p>The Shiny app developed for this purpose offers users the capability to narrow down their exploration through intuitive filtering mechanisms. 
            Users can focus their analysis based on various film attributes, including release decade, genre, release country, director, and more. The interactive 
            and user-friendly nature of the application facilitates a dynamic exploration of financial patterns within the film industry.</p>

            <p><strong>Importance of Financial Analysis in the Film Industry</strong></p>

            <p>Understanding the financial aspects of movies is paramount for stakeholders in the film industry. Producers need insights into budgeting and 
            profitability, directors aim to make informed decisions, and audiences are curious about the financial success of their favorite genres. This Shiny 
            app aims to address these needs by providing a platform for detailed financial analysis.</p>
            
            ")
    
        }

        if (input$learnMoreToggle == "Visualization Design Choices") {
            return("

            <h4>Visualization Design Choices</h4>

            <p><strong>Use of Log Scales</strong></p>

            <p>Several visualizations within the Shiny app employ logarithmic scales, particularly in scenarios where the data spans multiple orders of magnitude. 
            Logarithmic scales are effective in revealing patterns and trends in data with a wide range, ensuring that smaller values are not overshadowed by 
            larger ones. For instance, when visualizing budgets and profits in the scatter plots, log scales enhance the visibility of relationships between 
            variables, providing a more comprehensive understanding of the financial landscape.</p>

            <p><strong>Color Categories for Enhanced Interpretation</strong></p>

            <p>To enhance interpretability, color categories have been thoughtfully assigned based on logical associations with positive and negative financial 
            indicators. In the Season Box Plot, different seasons are represented by distinct and easily distinguishable colors, aiding users in quickly grasping 
            the seasonal variations in movie financials. Additionally, in the Budget vs. Profit Scatter Plot, colors categorize movies into different profitability 
            ranges, such as low, medium, and high, providing a quick visual cue to users about the financial success of movies.</p>

            <p><strong>Choice of Specific Plot Types</strong></p>

            <p><u>Jitter Plot with Genre Data</u></p>

            <p>Jitter plots were selected to offer a detailed view of the distribution of financial metrics within each genre. Jittering the data points allows 
            users to observe the concentration and spread of financial values for individual movies within a genre. By color-coding the jitter data points with the 
            opposite metric (profit jitter with budget color-coding and vice versa), users can quickly discern the relationship between these two critical 
            financial aspects for each movie in a specific genre.</p>

            <p><u>Box Plot for Seasonal Analysis</u></p>

            <p>The choice of a box plot serves the specific focus on the distribution of financial metrics across different seasons within a decade. Unlike jitter 
            plots, box plots emphasize the central tendency, dispersion, and potential outliers within the data. This is particularly relevant when exploring 
            seasonal variations, as the primary interest lies in understanding the overall distribution of financial metrics without the distraction of individual 
            data points.</p>

            <p><u>Worldwide Movie Profitability and Cost Distribution</u></p>

            <p>This map utilizes a green color scale (darker indicating higher profits/budgets) and a logarithmic scale to depict the distribution of average 
            profits and budgets across countries. To keep the user focused on the data with values, light gray color with dark gray borders is employed for 
            countries without data.</p>

            <p><u>Movie Profit-Budget Relationship</u></p>

            <p>With this plot, a linear scale is used to compare movie profits to their budgets. The background features logarithmic breaks (100 million and 1 
            billion) to create colored areas highlighting poor, medium, and high profits. A non-linear trendline is included to best visualize the general 
            relationship, given the clustering of data in the bottom-left portion of the plot. This plot is particularly useful for users to observe variations 
            in the profit-budget relationship as filters are applied.</p>

            ")
        
        }

        if (input$learnMoreToggle == "Results Across Genres") {
            return("
            
            <h4>Results Discussion: Analysis Across Genres</h4>

            <p><strong>Financial Trends</strong></p>

            <p>The jitter plot reveals insights into the distribution of profits and budgets for different movie genres, especially after applying logarithmic 
            scales to both profit and budget. Both comedy and horror genres exhibit a wide distribution in profits. However, the budget distribution in these 
            genres is notably diverse. Comedy movies, in particular, showcase a longer tail for lower budgets, reflecting the presence of a considerable number 
            of low-budget productions that either result in box office bombs or significant profit gains. This trend, often observed in production houses like 
            Blumhouse, signifies the potential for cult followings to turn low-budget productions into unexpected financial successes.</p>

            <p>Like comedy and horror, drama movies follow a distinct budget pattern, with a slightly less pronounced lower tail. This suggests a more restrained 
            distribution of drama budgets. In contrast, action movies demonstrate a significantly less distributed budget landscape, with most 
            movies having budgets exceeding $10 million. This trend intensifies in the 2000s, aligning with the rise of big-budget blockbuster action films.</p>

            <p>The use of color scales, with discrete logarithmic breaks for the opposite metric (profit for budget and vice versa), brings additional insight.
            Highly profitable action movies tend to correlate with higher budgets, indicating a trend where larger investments lead to substantial returns. In 
            these genres, a noteworthy observation emerges. While highly profitable action movies generally have higher budgets, the same isn't universally true 
            for horror and comedy. Medium and even some low-budget horror and comedy movies exhibit profits comparable to highly profitable action films, 
            showcasing the potential for significant returns even with constrained budgets.</p>

            <p>Horror movies tend to have lower budgets compared to comedy, yet the average profit for horror movies surpasses that of comedy. This could come 
            from a lower overall production volume of horror movies, especially in recent decades. Animated films stand out as having higher-than-average profits. 
            However, they tend to have at least medium budgets, notably over $10 million. This aligns with the inherent upfront costs associated with producing 
            full-length animated films, a trend particularly prominent in recent decades with advancements in digital technology.</p>

            <p><strong>Considerations and Conclusions</strong></p>

            <p>The jitter plot not only dissects financial landscapes across genres but also uncovers intricate relationships between budgets and profits. 
            It offers a truthful and insightful portrayal of the film industry, showcasing the diversity in financial patterns and the potential for both high 
            and low-budget productions to yield substantial profits. The use of color scales enhances the interpretability of the data, allowing users 
            to glean valuable insights into the financial dynamics of different genres. The plot, therefore, serves as a functional and enlightening tool for 
            industry stakeholders seeking to navigate the complex terrain of movie finances.</p>
            
            ")
        }
        if (input$learnMoreToggle == "Results Across Seasons") {
            return("
            
            <h4>Results Discussion: Analysis Across Seasons</h4>

            <p><strong>Financial Trends</strong></p>

            <p>Analyzing the season box plot reveals discernible trends in profits across different decades and seasons. Notably, profits exhibit a consistent 
            upward trajectory over the decades, reflecting an overall increase in financial returns. However, it's crucial to acknowledge that this increase 
            occurs at a diminishing rate, suggesting a potential impact of factors such as lower cinema attendance, the rise of streaming platforms, and 
            increased piracy on the film industry's profit margins.</p>

            <p>Despite the overarching increase in profits, a distinct seasonal pattern persists. The summer season consistently stands out with a 
            higher-than-average profit range. This aligns with the industry's historical trend of releasing blockbuster movies during the summer months, attracting 
            larger audiences and generating substantial profits. The prominence of summer as the most lucrative season underscores the enduring appeal of 
            big-budget films released during this period.</p>

            <p>In contrast to profits, the distribution of movie budgets exhibits a wider range, evident by the logarithmic scale increasing by a factor of 10. 
            This observation highlights the substantial variability in production budgets, emphasizing the diverse financial investments made in the creation of 
            movies. Unlike profits, the season appears to have a comparatively limited impact on a movie's budget. The distribution of budgets remains relatively 
            consistent across seasons, indicating that the financial resources allocated to film production are less influenced by the time of release. This 
            contrasts with the observed impact of seasonality on profits, suggesting that budget decisions are driven by factors other than seasonal trends.</p>

            <p><strong>Considerations and Conclusions</strong></p>

            <p>The season box plot illuminates crucial insights into the financial dynamics of the film industry. The consistent rise in profits, albeit at a 
            slower pace, reflects the enduring profitability of the industry over the years. The seasonal influence on profits, with summer consistently leading 
            in profitability, underscores the strategic importance of timing in movie releases.On the other hand, the wide distribution of budgets highlights the 
            diversity in financial strategies employed in filmmaking. The limited impact of seasonality on budgets suggests that filmmakers and producers make 
            budgetary decisions based on factors beyond seasonal trends, such as genre, expected audience appeal, and production requirements.</p>

            <p>In conclusion, the season box plot contributes to a holistic understanding of the financial landscape of the film industry, showcasing both 
            overarching trends and nuanced variations in profits and budgets. This visual representation serves as a valuable tool for industry professionals and 
            enthusiasts seeking to navigate the multifaceted world of movie finances.</p>

            ")
        }
        if (input$learnMoreToggle == "Results Across Countries") {
            return("

            <h4>Results Discussion: Analysis Across Countries</h4>

            <p><strong>Financial Trends</strong></p>

            <p>The world map financial plot provides a comprehensive view of the film industry's financial landscape across different countries. Average profits 
            and budgets have been analyzed to identify trends and variations, revealing intriguing insights into the global dynamics of movie finances. Average 
            profits emerge highest in countries such as Japan, the United States, China, the UK, and surprisingly, Finland, with figures approaching $20 million. 
            A parallel pattern is observed in average budgets, where countries like Australia, South Africa, France, and Germany also exhibit higher budgets 
            compared to their counterparts worldwide.</p>

            <p>Upon scrutinizing the data across decades, notable shifts in relative movie profits become apparent. Australia and the US experience a decrease in 
            relative profits, while Japan and China witness a substantial increase. Japan stands out as a dominant force in more profitable genres, particularly 
            action and adventure films, closely followed by the US and China. The United States asserts its dominance in one of the consistently lucrative 
            categories—animation. This supremacy can be attributed to the monumental success of major animation studios like Disney, Pixar, and DreamWorks, which 
            have consistently dominated the market throughout the decades covered in the dataset.</p>

            <p><strong>Considerations and Conclusions</strong></p>

            <p>The absence of animation profit data for Japan poses a limitation, especially considering the popularity and profitability of anime, notably from 
            studios like Studio Ghibli. This underscores the need for a more comprehensive dataset, including a diverse range of movies, to present a more 
            accurate and nuanced portrayal of genre-specific financial trends.</p>

            <p>Examining the data for early decades, a surprising observation unfolds for countries like India and all of South America, renowned for their 
            contributions to the world of cinema. Notably, during the 1980s, these regions appear empty in the dataset, signifying a remarkable growth in the movie 
            industry within just a few decades. This absence during earlier periods highlights the rapid development and expansion of the film industries in these 
            regions, demonstrating their emergence as significant players on the global cinematic stage in relatively recent times.</p>

            <p>The world map financial plot serves as a valuable tool for understanding the global distribution of movie profits and budgets. It highlights both 
            overarching trends and specific country-based nuances, offering insights that can guide industry professionals and enthusiasts in navigating the 
            complex world of movie finances. As the industry evolves, ongoing updates to the dataset will contribute to a more accurate and detailed portrayal 
            of the financial dynamics shaping the global film landscape.</p>

            ")
        }
        if (input$learnMoreToggle == "Results Between Metrics") {
            return("
            
            <h4>Results and Discussion: Budget-Profit Relationship</h4>

            <p><strong>Financial Trends</strong></p>

            <p>For this analysis, a non-logarithmic scale is employed, as the objective is to directly compare the budget and profit values on the same scale. The
            profits trendline curves upwards due to high-budget movies with incredibly high profits, notably labeled movies such as Avatar, Avengers, and Titanic. 
            These films are notable for their ambitious budgets and remarkable box office performances.</p>

            <p>Most movies are clustered in a region with less than $1 billion in profit and less than $200 million in budget. This corresponds to the background 
            color coding, where high-profit movies are defined as greater than $1 billion, medium-profit movies fall between $100 million and $1 billion, and 
            low-profit movies are anything below $100 million. The majority of the data falls below the high-profit margin, consistent with the jitter plot that 
            examined finances across genres.</p>

            <p>This graph presents a holistic view of the financial landscape, showcasing the nonlinear relationship between budget and profitability. It is 
            intended for in-depth analysis when the user has selected specific filters. For example, action movies follow a very similar exponential pattern as 
            all genres aggregated together. However, comedy and horror genres exhibit relatively flat trends and even tend to trend downwards towards higher 
            budgets.</p>

            <p>When examining each decade, the 1980s show a relatively flat trendline as budgets increase. However, in the 1990s and beyond, more dramatic 
            exponential curves emerge, driven by movies like Titanic in the 90s, Avatar in the 2000s, and Avengers: Endgame in the 2010s. These iconic films 
            contribute to the evolving financial dynamics of the film industry over the years, influencing the observed trends in the budget-profit relationship.</p>

            <p><strong>Considerations and Conclusions</strong></p>

            <p>The budget-profit relationship analysis underscores the influence of blockbuster movies with exceptionally high budgets on the overall profitability 
            trend. While the majority of films fall within a range of less than $1 billion in profit and less than $200 million in budget, the presence of a few 
            mega-hits significantly impacts the upward trajectory of the profits trendline. This emphasizes the pivotal role of high-budget films in shaping the 
            financial landscape of the film industry.</p>

            <p>Furthermore, the genre-specific analysis reveals nuanced trends, with action movies mirroring the overall pattern, while comedy and horror genres 
            exhibit distinct behaviors. The examination of each decade brings attention to specific periods of exponential growth, driven by cinematic phenomena 
            like Titanic, Avatar, and Avengers: Endgame. In conclusion, the budget-profit relationship is a dynamic aspect of the film industry, influenced by both 
            overarching trends and the unique characteristics of individual genres and iconic movies.</p>
            
            ")

        }
    })

})