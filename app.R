library(tidyverse)
library(shiny)
library(shinybusy)
library(mdsr)
library(shinyWidgets)
library(plotly)
library(maps)
library(datasets)
library(ggrepel) 
library(viridis)

# Read in Data
prescription_data <- readRDS(file = "prescription_data.rds")
overdose_data <- readRDS(file = "all_overdoses.rds")
full_data <- left_join(prescription_data, overdose_data, by = c("year", "State")) %>% 
  select(State, year, prescription_rate, Age.Adjusted.Rate) %>% 
  janitor::clean_names() %>% 
  mutate_if(is.numeric, funs(`std`=scale(.) %>% as.vector())) %>% 
  select(-c(year_std, prescription_rate, age_adjusted_rate)) %>%
  rename(`Prescription Rate` = prescription_rate,
         `Overdose Rate` = age_adjusted_rate)
  
# Set up map data
usa_states <- map_data(map = "state"                       
                       , region = ".")

# Join map data to prescription data
prescription_map <- prescription_data %>%
  inner_join(usa_states, by = c("state" = "region")) %>%
  rename(`Prescription Rate` = prescription_rate)

# Join map data to Overdose data
colnames(overdose_data)[1] <- "region"
overdose_data[[1]] <- tolower(overdose_data[[1]])
overdose_map<- inner_join(usa_states, overdose_data)

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
  
  HTML("<h1>Opioid Overdose Analysis</h1>
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
                           HTML("<p>Is there a correlation between opioid prescription rate and overdose death rate?</p>"),
                           plotlyOutput("prescriptions")
                  ),
                  tabPanel("Clustering",
                           HTML("<p>Something about clustering"),
                           plotOutput("clusters"),
                           plotOutput("clustermap")     
                  ),
                  tabPanel("Opioid Deaths rate by State",
                                plotlyOutput("overdoses")
                  )
              
      )
    )
  )
)

# server
server <- function(input, output){ 
  
  # Tamer's Tab
  prescription_graph <- reactive({
    prescription_map <- prescription_map %>% 
      req(input$year) 
    if (input$year != "ALL") {
      prescription_map <- prescription_map %>% 
        filter(year == input$year) 
    }
    else {
      prescription_map <- prescription_map
    }
  })
  
  output$prescriptions <- renderPlotly({
    ggplotly(
    ggplot(prescription_graph(), aes(x = long, y = lat, group = group
                              , fill = `Prescription Rate`)) +
      geom_polygon(color = "white") +
      theme_void() +
      coord_fixed(ratio = 1.3) +
      labs(title = "Opioid Prescription Rate by State",
           subtitle = "MME Prescribed per 100 People",
           fill = "") +
      theme(legend.position="bottom") +
      scale_fill_distiller(palette = "Oranges", direction = 2)
    )
  })
  
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
        summarise(prescription_rate_std = mean(prescription_rate_std), age_adjusted_rate_std = mean(age_adjusted_rate_std))
    }
  }) 
  
  output$clusters <- renderPlot({
    set.seed(1106)
    data <- clustering_data()
    silhouette_score <- function(k){
      km <- kmeans(data[,2:3], centers = k, nstart = 20)
      score <- cluster::silhouette(km$cluster, dist(data[, 2:3]))
      mean(score[, 3])
    }
    
    k <- 2:10
    avg_sil <- sapply(k, silhouette_score)
    optimal_k <- which(as.data.frame(avg_sil)$avg_sil == max(avg_sil)) + 1
    
    km <- kmeans(data[, 2:3], centers = optimal_k, nstart = 20)
    
    data <- mutate(data, cluster = as.character(km$cluster))
    
    ggplot(data = data, aes(x = prescription_rate_std, y = age_adjusted_rate_std)) + 
      geom_point(aes(color = cluster)) +
      geom_text_repel(aes(label = state, color = cluster), size = 3) +
      geom_point(data = as.data.frame(km$centers)
                 , aes(x = prescription_rate_std, y = age_adjusted_rate_std)
                 , pch = "X"
                 , size = 3) +
      labs(x = "Prescription Rate", y = "Age Adjusted Overdose Rate", color = "Cluster Assignment")
  })
  
  output$clustermap <- renderPlot({
    set.seed(1106)
    data <- clustering_data()
    silhouette_score <- function(k){
      km <- kmeans(data[,2:3], centers = k, nstart = 20)
      score <- cluster::silhouette(km$cluster, dist(data[, 2:3]))
      mean(score[, 3])
    }
    
    k <- 2:10
    avg_sil <- sapply(k, silhouette_score)
    optimal_k <- which(as.data.frame(avg_sil)$avg_sil == max(avg_sil)) + 1
    
    km <- kmeans(data[, 2:3], centers = optimal_k, nstart = 20)
    
    data <- mutate(data, cluster = as.character(km$cluster))
    
    usa_states <- map_data(map = "state", region = ".") %>% 
      mutate(state = stringr::str_to_title(region))
    
    cluster_map <- data %>%
      inner_join(usa_states, by = "state")
    
    ggplot(cluster_map, aes(x = long, y = lat, group = group, fill = cluster)) +
      geom_polygon(color = "white") +
      theme_void() +
      coord_fixed(ratio = 1.3) +
      labs(title = "Opioid Prescription Rate by State",
           subtitle = "MME Prescribed per 100 People",
           fill = "") +
      theme(legend.position="right")
    
  })
    
    # Chris' Tab
    overdose_graph <- reactive({
      data <-overdose_map %>% 
        req(input$year) 
      if (input$year != "ALL") {
        data <- overdose_map %>% 
          filter(year == input$year) 
      }
      else {
        data <- overdose_map
      }
    })
    output$overdoses <- renderPlotly({
      ggplotly(
        ggplot(overdose_graph(), aes(x = long, y = lat, group = group,
                               fill = Age.Adjusted.Rate)) +
          geom_polygon(color = "white") +
          theme_void() +
          coord_fixed(ratio = 1.3) +
          labs(fill = "Age Adjusted DR") +
          scale_fill_viridis(option = "magma", direction = -1)+
          theme(legend.position="right")+
          ggtitle(label = "Overdose Rates in the United States")
      )
    })
}

# call to shinyApp
shinyApp(ui = ui, server = server)
