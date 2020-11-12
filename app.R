library(tidyverse)
library(shiny)
library(shinybusy)
library(mdsr)
library(shinyWidgets)
library(plotly)
library(maps)
library(datasets)

# Read in Data
prescription_data <- readRDS(file = "prescription_data.rds") %>%
  mutate(state = tolower(State))
  
# Set up map data
usa_states <- map_data(map = "state"                       
                       , region = ".")

# Join map data to prescription data
prescription_map <- prescription_data %>%
  inner_join(usa_states, by = c("state" = "region"))

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
                           plotOutput("prescriptions")
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
        filter(
          year == input$year
        ) 
    }
    else {
      prescription_map <- prescription_map
    }
  })
  
  output$prescriptions <- renderPlot({
    
    ggplot(prescription_graph(), aes(x = long, y = lat, group = group
                              , fill = prescription_rate)) +
      geom_polygon(color = "white") +
      theme_void() +
      coord_fixed(ratio = 1.3) +
      labs(title = "Opioid Prescription Rate by State",
           subtitle = "MME Prescribed per 100 People",
           fill = "") +
      theme(legend.position="bottom") +
      scale_fill_distiller(palette = 1, direction = 2)
    
  })

}

# call to shinyApp
shinyApp(ui = ui, server = server)
