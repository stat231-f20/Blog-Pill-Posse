library(tidyverse)
library(shiny)
library(shinybusy)
library(shinyWidgets)
library(plotly)
library(maps)
library(ggrepel) 

# Read in data
prescription_data <- readRDS(file = "prescription_data.rds")
overdose_data <- readRDS(file = "all_overdoses.rds")

# Wrangle data for clustering
full_data <- left_join(prescription_data, overdose_data, by = c("year", "State")) %>% 
  select(State, year, prescription_rate, Age.Adjusted.Rate) %>% 
  janitor::clean_names() %>% 
  mutate_if(is.numeric, funs(`std`=scale(.) %>% as.vector())) %>% 
  select(-c(year_std, prescription_rate, age_adjusted_rate)) %>% 
  mutate(state = tolower(state))
  
# Initialize map data
usa_states <- map_data(map = "state", region = ".")

# ui 
ui <- fluidPage( 
  
  setBackgroundImage(
    src = "https://cdn.cjr.org/wp-content/uploads/2019/08/AdobeStock_239680334-686x371.jpeg"
  ),
  
  # CSS
  tags$head(
    tags$style(HTML("
      h1, h3 {
        color: black;
        text-align: center;
        font-family: Arial Black;
      }
      p {
        color: black;
        background-color: white;
        text-align: center;
        border: solid;
        font-weight: 900;
        font-size: 15px;
        margin-top: 15px;
        margin-bottom: 20px;
      }
      a {
        color: black;
        font-weight: 900;
        background-color: white;
      }
      .nav-tabs>li.active>a {
        background-color: black !important;
        color: white !important;
      }
    "))
  ),
  
  HTML("<h1>Opioid Analysis From 2014-2018</h1>
        <h3>Pill Posse: Sean Wei, Tamer Sullivan, Chris Murphy</h3>"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "year",
                  label = "Choose a Year:",
                  choices = c(
                    "ALL",
                    unique(as.character(str_sort(prescription_map$year)))
                  )
      ),
      img(src = "https://www.clipartkey.com/mpngs/m/290-2909884_opioid-icons2x-opioid-symbol.png", width = "100%")
    ),
    
    mainPanel(
      add_busy_spinner(spin = "double-bounce"), # let user know things may take a while to load
      tabsetPanel(type = "tabs", 
                  tabPanel("Prescription Rate vs. Overdose Rate",
                           HTML("<p>The visualization below depicts the opioid prescription rate across the U.S. for a given year. The prescription rate is defined as MME (Morphine Milligram Equivalents) prescribed per 100 people. This visualization is interactive - you can hover over a particular state and see its prescription rate.</p>"),
                           plotlyOutput("prescriptions"),
                           div(style = "margin-bottom: 15px;"),
                           HTML("<p>The visualization below depicts the opioid overdose rate across the U.S. for a given year. The overdose rate is adjusted for differences in age distributions. This visualization is also interactive - you can hover over a particular state to see the number of deaths and its age adjusted overdose rate.</p>"),
                           plotlyOutput("overdoses")
                  ),
                  tabPanel("K-Means Clustering",
                           HTML("<p>We used k-means clustering to determine similiar opioid characteristics among states. The first visualization is a scatterplot of the clusters, with the optimal k being chosen by a function that picks the highest silhouette score from possible k values of 2-5.</p>"),
                           plotOutput("clusterplot"),
                           div(style = "margin-bottom: 15px;"),
                           HTML("<p>The second visualization depicts the states colored by cluster number, which is based off of the scatterplot above.</p>"),
                           plotOutput("clustermap")     
                  )
              
      )
    )
  )
)

# server
server <- function(input, output){ 
  
  # Tamer's Portion
  prescription_graph <- reactive({
    prescription_map <- prescription_data %>% 
      req(input$year) 
    if (input$year != "ALL") {
      prescription_map <- prescription_map %>% 
        filter(year == input$year) %>% 
        inner_join(usa_states, by = c("state" = "region")) %>%
        rename(`Prescription Rate` = prescription_rate)
    }
    else {
      prescription_map <- prescription_map %>% 
        group_by(state) %>% 
        # If there is no filter, average all the values
        summarise(prescription_rate = mean(prescription_rate)) %>% 
        inner_join(usa_states, by = c("state" = "region")) %>%
        rename(`Prescription Rate` = prescription_rate)
    }
  })
  
  output$prescriptions <- renderPlotly({
    ggplotly(
      ggplot(prescription_graph(), aes(x = long, y = lat, group = group, 
                                       fill = `Prescription Rate`)) +
        geom_polygon(color = "white", aes(text = paste0('<b>State</b>: ', str_to_title(state), '<br>', '<b>Prescription Rate: ', `Prescription Rate`))) +
        theme_void() +
        coord_fixed(ratio = 1.3) +
        labs(fill = "Prescription Rate") +
        theme(legend.position = "bottom") +
        scale_fill_distiller(palette = "Oranges", direction = 2) + 
        ggtitle(ifelse(
          input$year == "ALL",
          paste("Average Opioid Prescription Rates From 2014-2018"),
          paste("Opioid Prescription Rates in", input$year)
        )),
      tooltip = "text"
    )
  })
  
  # Chris' Portion
  overdose_graph <- reactive({
    overdose_map <- overdose_data %>% 
      req(input$year) 
    if (input$year != "ALL") {
      overdose_map <- overdose_map %>% 
        filter(year == input$year) %>% 
        mutate(State = tolower(State)) %>% 
        inner_join(usa_states, by = c("State" = "region")) %>% 
        rename(`Overdose Rate` = Age.Adjusted.Rate)
    }
    else {
      overdose_map <- overdose_map %>% 
        mutate(State = tolower(State),
               Deaths = gsub(",", "", Deaths)) %>% 
        group_by(State) %>% 
        summarise(Age.Adjusted.Rate = mean(Age.Adjusted.Rate), Deaths = sum(as.numeric(as.character(Deaths)))) %>% 
        inner_join(usa_states, by = c("State" = "region")) %>%
        rename(`Overdose Rate` = `Age.Adjusted.Rate`)
    }
  })
  
  output$overdoses <- renderPlotly({
    ggplotly(
      ggplot(overdose_graph(), aes(x = long, y = lat, group = group,
                                   fill = `Overdose Rate`)) +
        geom_polygon(color = "white", aes(text = paste0('<b>State</b>: ', str_to_title(State), '<br>', 
                                                        '<b>Deaths</b>: ', Deaths, '<br>',
                                                        '<b>Overdose Rate</b>: ', `Overdose Rate`))) +
        theme_void() +
        coord_fixed(ratio = 1.3) +
        labs(fill = "Overdose Rate") +
        theme(legend.position = "bottom") +
        ggtitle(ifelse(
          input$year == "ALL",
          paste("Average Opioid Overdose Rates From 2014-2018"),
          paste("Opioid Overdose Rates in", input$year)
        )) +
        scale_fill_distiller(palette = "Blues", direction = 2),
      tooltip = "text"
    )
  })
  
  # Sean's Portion
  clustering_data <- reactive({
    clustering_data <- full_data %>% 
      req(input$year)
    if (input$year != "ALL") {
      clustering_data <- full_data %>% 
        filter(year == input$year) %>% 
        select(-c(year))
    }
    else {
      clustering_data <- clustering_data %>% 
        group_by(state) %>% 
        # If there is no filter, average all the values
        summarise(prescription_rate_std = mean(prescription_rate_std), age_adjusted_rate_std = mean(age_adjusted_rate_std))
    }
  }) 
  
  output$clusterplot <- renderPlot({
    set.seed(1106)
    data <- clustering_data()
    
    # Determine optimal k
    silhouette_score <- function(k){
      km <- kmeans(data[,2:3], centers = k, nstart = 20)
      score <- cluster::silhouette(km$cluster, dist(data[, 2:3]))
      mean(score[, 3])
    }
    avg_sil <- sapply(2:5, silhouette_score)
    optimal_k <- which(as.data.frame(avg_sil)$avg_sil == max(avg_sil)) + 1
    
    # Perform k-means with optimal k
    km <- kmeans(data[, 2:3], centers = optimal_k, nstart = 20)
    
    data <- mutate(data, 
                   cluster = as.character(km$cluster),
                   state = str_to_title(state))
    
    ggplot(data = data, aes(x = prescription_rate_std, y = age_adjusted_rate_std)) + 
      geom_point(aes(color = cluster)) +
      geom_text_repel(aes(label = state, color = cluster), size = 3) +
      geom_point(data = as.data.frame(km$centers)
                 , aes(x = prescription_rate_std, y = age_adjusted_rate_std)
                 , pch = "X"
                 , size = 3) +
      labs(x = "Standardized Prescription Rate", y = "Standardized Overdose Rate", color = "Cluster Assignment")
  })
  
  output$clustermap <- renderPlot({
    set.seed(1106)
    data <- clustering_data()
    
    # Determine optimal k
    silhouette_score <- function(k){
      km <- kmeans(data[, 2:3], centers = k, nstart = 20)
      score <- cluster::silhouette(km$cluster, dist(data[, 2:3]))
      mean(score[, 3])
    }
    avg_sil <- sapply(2:5, silhouette_score)
    optimal_k <- which(as.data.frame(avg_sil)$avg_sil == max(avg_sil)) + 1
    
    # Perform k-means with optimal k
    km <- kmeans(data[, 2:3], centers = optimal_k, nstart = 20)
    
    data <- mutate(data, cluster = as.character(km$cluster))
    cluster_map <- data %>%
      inner_join(usa_states, by = c("state" = "region"))
    
    ggplot(cluster_map, aes(x = long, y = lat, group = group, fill = cluster)) +
      geom_polygon(color = "white") +
      theme_void() +
      coord_fixed(ratio = 1.3) +
      labs(title = "Visualizing the Clusters",
           fill = "Cluster #") +
      theme(legend.position = "right")
  })

}

# call to shinyApp
shinyApp(ui = ui, server = server)
