library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Graphical Dashboard for eDNA Grouping Data"),
  
  sidebarLayout(
    sidebarPanel(h2("Criteria Selector"),
                 selectInput("modelSelect", "What clustering model should be used?",
                             choices = c("6 Cluster K-medoid", "2 Cluster Dirichlet Multinomial Mixture"),
                             multiple = FALSE),
                 selectInput("critSelect", 
                             "What should the data be grouped by?", 
                             choices = c("Clustering", "Season", "Month", "Year", "Location"),
                             multiple = FALSE),
                 conditionalPanel(
                   condition = "input.critSelect == 'Clustering'",
                   numericInput("clusterNumber", "Which Cluster?", 1, 1, 6, 1)
                 ),
                 conditionalPanel(
                   condition = "input.critSelect == 'Season'",
                   selectInput("seasonSelect", 
                               "Which Season?", 
                               choices = c("Spring", "Summer", "Fall"),
                               multiple = FALSE)
                 ),
                 conditionalPanel(
                   condition = "input.critSelect == 'Month'",
                   selectInput("monthSelect", 
                               "Which Month?", 
                               choices = c("Jun", "Jul", "Aug", "Sep", "Oct"),
                               multiple = FALSE)
                 ),
                 conditionalPanel(
                   condition = "input.critSelect == 'Year'",
                   numericInput("yearNumber", "Which Year?", 2016, 2016, 2018, step = 1)
                 ),
                 conditionalPanel(
                   condition = "input.critSelect == 'Location'",
                   selectInput("locationSelect", 
                               "Which Location?", 
                               choices = c("Bagaduce R.", "Damariscotta R.", "Jacks Point", "Webhannet R.", "New Meadows R.", "Prentiss Isl.", "Weskeag R."),
                               multiple = FALSE)
                 )
                 ),
    
    mainPanel(h2("Visualizations"),
              plotOutput("selected_plot"),
              textOutput("maxCluster"),
              textOutput("maxSeason"),
              textOutput("maxMonth"),
              textOutput("maxYear"),
              textOutput("maxLocation")
              )
  )
)

# Define server logic ----
server <- function(input, output) {
  output$selected_plot <- renderPlot({
    if (input$critSelect == 'Clustering'){
      data_row <- meta_data[6,]
      eval_data_row <- data_row == input$clusterNumber
      meta_data_clustered <<- meta_data[,eval_data_row]
      
      getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
      }
      modeCluster <<- as.integer(getmode(meta_data_clustered[6,]))
      modeMonth <<- getmode(meta_data_clustered[2,])
      modeSeason <<- getmode(meta_data_clustered[3,])
      modeYear <<- as.integer(getmode(meta_data_clustered[4,]))
      modeLocation <<- getmode(meta_data_clustered[5,])
      
      samp_months_labels <- c("Jun", "Jul", "Aug", "Sep", "Oct")
      samp_jun <- sum(meta_data_clustered[2,] == "Jun")
      samp_jul <- sum(meta_data_clustered[2,] == "Jul")
      samp_aug <- sum(meta_data_clustered[2,] == "Aug")
      samp_sep <- sum(meta_data_clustered[2,] == "Sep")
      samp_oct <- sum(meta_data_clustered[2,] == "Oct")
      samp_months <- c(samp_jun, samp_jul, samp_aug, samp_sep, samp_oct)
      matrix_month <- cbind(samp_months_labels, samp_months)
      
      samp_seasons_labels <- c("Spring", "Summer", "Fall")
      samp_spring <- sum(meta_data_clustered[3,] == "Spring")
      samp_summer <- sum(meta_data_clustered[3,] == "Summer")
      samp_fall <- sum(meta_data_clustered[3,] == "Fall")
      samp_seasons <- c(samp_spring, samp_summer, samp_fall)
      matrix_season <- cbind(samp_seasons_labels, samp_seasons)
      
      samp_years_labels <- c(2016, 2017, 2018)
      samp_2016 <- sum(meta_data_clustered[4,] == 2016)
      samp_2017 <- sum(meta_data_clustered[4,] == 2017)
      samp_2018 <- sum(meta_data_clustered[4,] == 2018)
      samp_years <- c(samp_2016, samp_2017, samp_2018)
      matrix_year <- cbind(samp_years_labels, samp_years)
      
      samp_locations_labels <- c("Bagaduce R.", "Damariscotta R.", "Jacks Point", "Webhannet R.", "New Meadows R.", "Prentiss Isl.", "Weskeag R.")
      samp_baga <- sum(meta_data_clustered[5,] == "Bagaduce R.")
      samp_dama <- sum(meta_data_clustered[5,] == "Damariscotta R.")
      samp_jack <- sum(meta_data_clustered[5,] == "Jacks Point")
      samp_web <- sum(meta_data_clustered[5,] == "Webhannet R.")
      samp_newm <- sum(meta_data_clustered[5,] == "New Meadows R.")
      samp_prent <- sum(meta_data_clustered[5,] == "Prentiss Isl.")
      samp_wesk <- sum(meta_data_clustered[5,] == "Weskeag R.")
      samp_locations <- c(samp_baga, samp_dama, samp_jack, samp_web, samp_newm, samp_prent, samp_wesk)
      matrix_location <- cbind(samp_locations_labels, samp_locations)
      
      bar_data <- c(as.integer(matrix_month[,2]), as.integer(matrix_season[,2]), as.integer(matrix_year[,2]), as.integer(matrix_location[,2]))
      bar_labels <- c(matrix_month[,1],matrix_season[,1], as.integer(matrix_year[,1]), matrix_location[,1])
      
      barplot(bar_data, 
              main = "Presence of Criteria in Selected Cluster", 
              names.arg = bar_labels,
              col = c(rep("dark green", length(matrix_month[,2])), 
                      rep("purple", length(matrix_season[,2])), 
                      rep("dark blue", length(matrix_year[,2])),
                      rep("dark red", length(matrix_location[,2]))), 
              space = c(rep(0, 5), 2, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 0)
      )
    } else if (input$critSelect == 'Season') {
      data_row <- meta_data[3,]
      eval_data_row <- data_row == input$seasonSelect
      meta_data_clustered <<- meta_data[,eval_data_row]
      
      getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
      }
      modeCluster <<- as.integer(getmode(meta_data_clustered[6,]))
      modeMonth <<- getmode(meta_data_clustered[2,])
      modeSeason <<- getmode(meta_data_clustered[3,])
      modeYear <<- as.integer(getmode(meta_data_clustered[4,]))
      modeLocation <<- getmode(meta_data_clustered[5,])

      samp_clusters_labels <- c(1:6)
      samp_1 <- sum(meta_data_clustered[6,] == 1)
      samp_2 <- sum(meta_data_clustered[6,] == 2)
      samp_3 <- sum(meta_data_clustered[6,] == 3)
      samp_4 <- sum(meta_data_clustered[6,] == 4)
      samp_5 <- sum(meta_data_clustered[6,] == 5)
      samp_6 <- sum(meta_data_clustered[6,] == 6)
      samp_clusters <- c(samp_1, samp_2, samp_3, samp_4, samp_5, samp_6)
      matrix_cluster <- cbind(samp_clusters_labels, samp_clusters)
      
      samp_months_labels <- c("Jun", "Jul", "Aug", "Sep", "Oct")
      samp_jun <- sum(meta_data_clustered[2,] == "Jun")
      samp_jul <- sum(meta_data_clustered[2,] == "Jul")
      samp_aug <- sum(meta_data_clustered[2,] == "Aug")
      samp_sep <- sum(meta_data_clustered[2,] == "Sep")
      samp_oct <- sum(meta_data_clustered[2,] == "Oct")
      samp_months <- c(samp_jun, samp_jul, samp_aug, samp_sep, samp_oct)
      matrix_month <- cbind(samp_months_labels, samp_months)
      
      samp_years_labels <- c(2016, 2017, 2018)
      samp_2016 <- sum(meta_data_clustered[4,] == 2016)
      samp_2017 <- sum(meta_data_clustered[4,] == 2017)
      samp_2018 <- sum(meta_data_clustered[4,] == 2018)
      samp_years <- c(samp_2016, samp_2017, samp_2018)
      matrix_year <- cbind(samp_years_labels, samp_years)
      
      samp_locations_labels <- c("Bagaduce R.", "Damariscotta R.", "Jacks Point", "Webhannet R.", "New Meadows R.", "Prentiss Isl.", "Weskeag R.")
      samp_baga <- sum(meta_data_clustered[5,] == "Bagaduce R.")
      samp_dama <- sum(meta_data_clustered[5,] == "Damariscotta R.")
      samp_jack <- sum(meta_data_clustered[5,] == "Jacks Point")
      samp_web <- sum(meta_data_clustered[5,] == "Webhannet R.")
      samp_newm <- sum(meta_data_clustered[5,] == "New Meadows R.")
      samp_prent <- sum(meta_data_clustered[5,] == "Prentiss Isl.")
      samp_wesk <- sum(meta_data_clustered[5,] == "Weskeag R.")
      samp_locations <- c(samp_baga, samp_dama, samp_jack, samp_web, samp_newm, samp_prent, samp_wesk)
      matrix_location <- cbind(samp_locations_labels, samp_locations)
      
      bar_data <- c(as.integer(matrix_cluster[,2]), as.integer(matrix_month[,2]), as.integer(matrix_year[,2]), as.integer(matrix_location[,2]))
      bar_labels <- c(as.integer(matrix_cluster[,1]), matrix_month[,1], as.integer(matrix_year[,1]), matrix_location[,1])
      barplot(bar_data, 
              main = "Presence of Criteria in Selected Season", 
              names.arg = bar_labels,
              col = c(rep("dark green", length(matrix_cluster[,2])), 
                      rep("purple", length(matrix_month[,2])), 
                      rep("dark blue", length(matrix_year[,2])),
                      rep("dark red", length(matrix_location[,2]))), 
              space = c(1, rep(0, (length(matrix_cluster[,2]) - 1)),
                        2,
                        rep(0, (length(matrix_month[,2]) - 1)),
                        2,
                        rep(0, (length(matrix_year[,2]) - 1)),
                        2,
                        rep(0, (length(matrix_location[,2]) - 1)))
              
      )
    } else if (input$critSelect == 'Month') {
      data_row <- meta_data[2,]
      eval_data_row <- data_row == input$monthSelect
      meta_data_clustered <<- meta_data[,eval_data_row]
      
      getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
      }
      modeCluster <<- as.integer(getmode(meta_data_clustered[6,]))
      modeMonth <<- getmode(meta_data_clustered[2,])
      modeSeason <<- getmode(meta_data_clustered[3,])
      modeYear <<- as.integer(getmode(meta_data_clustered[4,]))
      modeLocation <<- getmode(meta_data_clustered[5,])
      
      samp_clusters_labels <- c(1:6)
      samp_1 <- sum(meta_data_clustered[6,] == 1)
      samp_2 <- sum(meta_data_clustered[6,] == 2)
      samp_3 <- sum(meta_data_clustered[6,] == 3)
      samp_4 <- sum(meta_data_clustered[6,] == 4)
      samp_5 <- sum(meta_data_clustered[6,] == 5)
      samp_6 <- sum(meta_data_clustered[6,] == 6)
      samp_clusters <- c(samp_1, samp_2, samp_3, samp_4, samp_5, samp_6)
      matrix_cluster <- cbind(samp_clusters_labels, samp_clusters)
      
      samp_seasons_labels <- c("Spring", "Summer", "Fall")
      samp_spring <- sum(meta_data_clustered[3,] == "Spring")
      samp_summer <- sum(meta_data_clustered[3,] == "Summer")
      samp_fall <- sum(meta_data_clustered[3,] == "Fall")
      samp_seasons <- c(samp_spring, samp_summer, samp_fall)
      matrix_season <- cbind(samp_seasons_labels, samp_seasons)
      
      samp_years_labels <- c(2016, 2017, 2018)
      samp_2016 <- sum(meta_data_clustered[4,] == 2016)
      samp_2017 <- sum(meta_data_clustered[4,] == 2017)
      samp_2018 <- sum(meta_data_clustered[4,] == 2018)
      samp_years <- c(samp_2016, samp_2017, samp_2018)
      matrix_year <- cbind(samp_years_labels, samp_years)
      
      samp_locations_labels <- c("Bagaduce R.", "Damariscotta R.", "Jacks Point", "Webhannet R.", "New Meadows R.", "Prentiss Isl.", "Weskeag R.")
      samp_baga <- sum(meta_data_clustered[5,] == "Bagaduce R.")
      samp_dama <- sum(meta_data_clustered[5,] == "Damariscotta R.")
      samp_jack <- sum(meta_data_clustered[5,] == "Jacks Point")
      samp_web <- sum(meta_data_clustered[5,] == "Webhannet R.")
      samp_newm <- sum(meta_data_clustered[5,] == "New Meadows R.")
      samp_prent <- sum(meta_data_clustered[5,] == "Prentiss Isl.")
      samp_wesk <- sum(meta_data_clustered[5,] == "Weskeag R.")
      samp_locations <- c(samp_baga, samp_dama, samp_jack, samp_web, samp_newm, samp_prent, samp_wesk)
      matrix_location <- cbind(samp_locations_labels, samp_locations)
      
      bar_data <- c(as.integer(matrix_cluster[,2]), as.integer(matrix_season[,2]), as.integer(matrix_year[,2]), as.integer(matrix_location[,2]))
      bar_labels <- c(as.integer(matrix_cluster[,1]), matrix_season[,1], as.integer(matrix_year[,1]), matrix_location[,1])
      
      barplot(bar_data, 
              main = "Presence of Criteria in Selected Month", 
              names.arg = bar_labels,
              col = c(rep("dark green", length(matrix_cluster[,2])), 
                      rep("purple", length(matrix_season[,2])), 
                      rep("dark blue", length(matrix_year[,2])),
                      rep("dark red", length(matrix_location[,2]))), 
              space = c(1, rep(0, (length(matrix_cluster[,2]) - 1)),
                        2,
                        rep(0, (length(matrix_season[,2]) - 1)),
                        2,
                        rep(0, (length(matrix_year[,2]) - 1)),
                        2,
                        rep(0, (length(matrix_location[,2]) - 1)))
              
      )
      
      
    } else if (input$critSelect == 'Year') {
      data_row <- meta_data[4,]
      eval_data_row <- data_row == input$yearNumber
      meta_data_clustered <<- meta_data[,eval_data_row]
      
      getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
      }
      modeCluster <<- as.integer(getmode(meta_data_clustered[6,]))
      modeMonth <<- getmode(meta_data_clustered[2,])
      modeSeason <<- getmode(meta_data_clustered[3,])
      modeYear <<- as.integer(getmode(meta_data_clustered[4,]))
      modeLocation <<- getmode(meta_data_clustered[5,])
 
      samp_clusters_labels <- c(1:6)
      samp_1 <- sum(meta_data_clustered[6,] == 1)
      samp_2 <- sum(meta_data_clustered[6,] == 2)
      samp_3 <- sum(meta_data_clustered[6,] == 3)
      samp_4 <- sum(meta_data_clustered[6,] == 4)
      samp_5 <- sum(meta_data_clustered[6,] == 5)
      samp_6 <- sum(meta_data_clustered[6,] == 6)
      samp_clusters <- c(samp_1, samp_2, samp_3, samp_4, samp_5, samp_6)
      matrix_cluster <- cbind(samp_clusters_labels, samp_clusters)
      
      samp_months_labels <- c("Jun", "Jul", "Aug", "Sep", "Oct")
      samp_jun <- sum(meta_data_clustered[2,] == "Jun")
      samp_jul <- sum(meta_data_clustered[2,] == "Jul")
      samp_aug <- sum(meta_data_clustered[2,] == "Aug")
      samp_sep <- sum(meta_data_clustered[2,] == "Sep")
      samp_oct <- sum(meta_data_clustered[2,] == "Oct")
      samp_months <- c(samp_jun, samp_jul, samp_aug, samp_sep, samp_oct)
      matrix_month <- cbind(samp_months_labels, samp_months)
      
      samp_seasons_labels <- c("Spring", "Summer", "Fall")
      samp_spring <- sum(meta_data_clustered[3,] == "Spring")
      samp_summer <- sum(meta_data_clustered[3,] == "Summer")
      samp_fall <- sum(meta_data_clustered[3,] == "Fall")
      samp_seasons <- c(samp_spring, samp_summer, samp_fall)
      matrix_season <- cbind(samp_seasons_labels, samp_seasons)
      
      samp_locations_labels <- c("Bagaduce R.", "Damariscotta R.", "Jacks Point", "Webhannet R.", "New Meadows R.", "Prentiss Isl.", "Weskeag R.")
      samp_baga <- sum(meta_data_clustered[5,] == "Bagaduce R.")
      samp_dama <- sum(meta_data_clustered[5,] == "Damariscotta R.")
      samp_jack <- sum(meta_data_clustered[5,] == "Jacks Point")
      samp_web <- sum(meta_data_clustered[5,] == "Webhannet R.")
      samp_newm <- sum(meta_data_clustered[5,] == "New Meadows R.")
      samp_prent <- sum(meta_data_clustered[5,] == "Prentiss Isl.")
      samp_wesk <- sum(meta_data_clustered[5,] == "Weskeag R.")
      samp_locations <- c(samp_baga, samp_dama, samp_jack, samp_web, samp_newm, samp_prent, samp_wesk)
      matrix_location <- cbind(samp_locations_labels, samp_locations)
      
      bar_data <- c(as.integer(matrix_cluster[,2]), as.integer(matrix_month[,2]), as.integer(matrix_season[,2]), as.integer(matrix_location[,2]))
      bar_labels <- c(as.integer(matrix_cluster[,1]), matrix_month[,1], matrix_season[,1], matrix_location[,1])
      
      barplot(bar_data, 
              main = "Presence of Criteria in Selected Year", 
              names.arg = bar_labels,
              col = c(rep("dark green", length(matrix_cluster[,2])), 
                      rep("purple", length(matrix_month[,2])), 
                      rep("dark blue", length(matrix_season[,2])),
                      rep("dark red", length(matrix_location[,2]))), 
              space = c(1, rep(0, (length(matrix_cluster[,2]) - 1)),
                        2,
                        rep(0, (length(matrix_month[,2]) - 1)),
                        2,
                        rep(0, (length(matrix_season[,2]) - 1)),
                        2,
                        rep(0, (length(matrix_location[,2]) - 1)))
              
      )
      
    } else if (input$critSelect == 'Location') {
      data_row <- meta_data[5,]
      eval_data_row <- data_row == input$locationSelect
      meta_data_clustered <<- meta_data[,eval_data_row]
    
      getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
      }
      modeCluster <<- as.integer(getmode(meta_data_clustered[6,]))
      modeMonth <<- getmode(meta_data_clustered[2,])
      modeSeason <<- getmode(meta_data_clustered[3,])
      modeYear <<- as.integer(getmode(meta_data_clustered[4,]))
      modeLocation <<- getmode(meta_data_clustered[5,])

      samp_clusters_labels <- c(1:6)
      samp_1 <- sum(meta_data_clustered[6,] == 1)
      samp_2 <- sum(meta_data_clustered[6,] == 2)
      samp_3 <- sum(meta_data_clustered[6,] == 3)
      samp_4 <- sum(meta_data_clustered[6,] == 4)
      samp_5 <- sum(meta_data_clustered[6,] == 5)
      samp_6 <- sum(meta_data_clustered[6,] == 6)
      samp_clusters <- c(samp_1, samp_2, samp_3, samp_4, samp_5, samp_6)
      matrix_cluster <- cbind(samp_clusters_labels, samp_clusters)
      
      samp_months_labels <- c("Jun", "Jul", "Aug", "Sep", "Oct")
      samp_jun <- sum(meta_data_clustered[2,] == "Jun")
      samp_jul <- sum(meta_data_clustered[2,] == "Jul")
      samp_aug <- sum(meta_data_clustered[2,] == "Aug")
      samp_sep <- sum(meta_data_clustered[2,] == "Sep")
      samp_oct <- sum(meta_data_clustered[2,] == "Oct")
      samp_months <- c(samp_jun, samp_jul, samp_aug, samp_sep, samp_oct)
      matrix_month <- cbind(samp_months_labels, samp_months)
      
      samp_seasons_labels <- c("Spring", "Summer", "Fall")
      samp_spring <- sum(meta_data_clustered[3,] == "Spring")
      samp_summer <- sum(meta_data_clustered[3,] == "Summer")
      samp_fall <- sum(meta_data_clustered[3,] == "Fall")
      samp_seasons <- c(samp_spring, samp_summer, samp_fall)
      matrix_season <- cbind(samp_seasons_labels, samp_seasons)
      
      samp_years_labels <- c(2016, 2017, 2018)
      samp_2016 <- sum(meta_data_clustered[4,] == 2016)
      samp_2017 <- sum(meta_data_clustered[4,] == 2017)
      samp_2018 <- sum(meta_data_clustered[4,] == 2018)
      samp_years <- c(samp_2016, samp_2017, samp_2018)
      matrix_year <- cbind(samp_years_labels, samp_years)
      
      bar_data <- c(as.integer(matrix_cluster[,2]), as.integer(matrix_month[,2]), as.integer(matrix_season[,2]), as.integer(matrix_year[,2]))
      bar_labels <- c(as.integer(matrix_cluster[,1]), matrix_month[,1], matrix_season[,1], as.integer(matrix_year[,1]))
      
      barplot(bar_data, 
              main = "Presence of Criteria in Selected Location", 
              names.arg = bar_labels,
              col = c(rep("dark green", length(matrix_cluster[,2])), 
                      rep("purple", length(matrix_month[,2])), 
                      rep("dark blue", length(matrix_season[,2])),
                      rep("dark red", length(matrix_year[,2]))), 
              space = c(1, rep(0, (length(matrix_cluster[,2]) - 1)),
                        2,
                        rep(0, (length(matrix_month[,2]) - 1)),
                        2,
                        rep(0, (length(matrix_season[,2]) - 1)),
                        2,
                        rep(0, (length(matrix_year[,2]) - 1)))
              
      )
    }
  })
  output$maxCluster <- renderText({
    inputs <- c(input$critSelect, input$clusterNumber, input$monthSelect, input$seasonSelect, input$yearNumber, input$locationSelect)
    paste("The most common cluster is", modeCluster)
  })
  output$maxMonth <- renderText({
    inputs <- c(input$critSelect, input$clusterNumber, input$monthSelect, input$seasonSelect, input$yearNumber, input$locationSelect)
    paste("The most common month is", modeMonth)
  })
  output$maxSeason <- renderText({
    inputs <- c(input$critSelect, input$clusterNumber, input$monthSelect, input$seasonSelect, input$yearNumber, input$locationSelect)
    paste("The most common season is", modeSeason)
  })
  output$maxYear <- renderText({
    inputs <- c(input$critSelect, input$clusterNumber, input$monthSelect, input$seasonSelect, input$yearNumber, input$locationSelect)
    paste("The most common year is", modeYear)
  })
  output$maxLocation <- renderText({
    inputs <- c(input$critSelect, input$clusterNumber, input$monthSelect, input$seasonSelect, input$yearNumber, input$locationSelect)
    paste("The most common location is", modeLocation)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)