# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# 1. Libraries
library(shiny)
library(leaflet)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# 2. Dataset
satellite_data <- data.frame(
  Country = c("Algeria", "Argentina", "Australia", "Azerbaijan", "Bangladesh",
              "Belarus", "Bolivia", "Brazil", "Bulgaria", "Canada", "Chile", 
              "Czech Republic", "Czechia", "Denmark", "Ecuador", "Egypt", 
              "Estonia", "France", "Germany", "Greece", "Hungary", "India", 
              "Indonesia", "Iran", "Iraq", "Israel", "Italy", "Japan", 
              "Kazakhstan", "Kenya", "Kuwait", "Laos", "Lithuania", 
              "Luxembourg", "Malaysia", "Mexico", "Morocco", "Netherlands", 
              "New Zealand", "Nigeria", "North Korea", "Norway", "Pakistan", 
              "China", "Peru", "Philippines", "Poland", 
              "Portugal", "Qatar", "Rwanda", "Slovenia", 
              "Tunisia", "Saudi Arabia", "Singapore", "South Africa", 
              "South Korea", "Spain", "Sweden", "Taiwan", "Thailand", 
              "Turkey", "United Arab Emirates", "United Kingdom", "United States", 
              "Uruguay", "Venezuela", "Vietnam"),
  Satellites = c(6, 37, 32, 2, 1, 1, 1, 20, 6, 67, 2, 3, 2, 9, 2, 11, 1, 104, 76, 
                 3, 2, 106, 19, 4, 1, 20, 51, 208, 8, 1, 1, 1, 6, 13, 9, 12, 4, 
                 8, 2, 5, 1, 19, 8, 757, 2, 2, 4, 3, 1, 6, 2, 1, 15, 18, 6, 40, 
                 42, 13, 16, 12, 25, 12, 658, 8782, 1, 3, 3)
)


# 3. Country Coordinates
world <- ne_countries(scale = "medium", returnclass = "sf")

# 4. Match coordinates with dataset
world_data <- world %>%
  left_join(satellite_data, by = c("name_long" = "Country"))

# 5. Color palette based on number of satellites
palette <- colorBin(
  palette = "YlOrRd", # Yellow-Orange-Red color palette
  domain = world_data$Satellites,
  bins = c(1, 15, 90, 150, 500, 1000, Inf),
  na.color = "transparent"
)

# Define UI
ui <- fluidPage(
  
  tags$style(
    HTML("
      body {
        background-color: #bcd4e6;
      }
      .well {
        background-color: #ffffff;
        border: 3px solid #0f0f0f;
      }
    ")
    
  ), 
  
  titlePanel("Satellites in Orbit by Country"),
  sidebarLayout(
    sidebarPanel(
      p("This map shows the number of satellites in orbit by country."),
      p("Hover over the country to see number of satelites.")
    ),
    mainPanel(
      leafletOutput("map", width = "100%", height = "500px")
    )
  )
)

# Define server
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(world_data) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolygons(
        fillColor = ~palette(Satellites), # Fill color based on number of satellites
        weight = 2,
        color = "#f2f3f4",
        fillOpacity = 0.8,
        label = ~paste0(
          "<strong>", name_long, "</strong><br/>",
          "Satellites: ", ifelse(is.na(Satellites), "No Data", Satellites)
        ) %>% lapply(htmltools::HTML),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "black",
          fillOpacity = 1,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        pal = palette,
        values = ~Satellites,
        position = "bottomright",
        title = "Number of Satellites in Orbit"
      )
  })
}

# Run the Shiny app
shinyApp(ui, server)

