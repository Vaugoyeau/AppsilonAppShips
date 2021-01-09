### Aller on se lance ----

### Packages uses
library(shiny)
library(shiny.semantic)
library(dplyr)
library(magrittr)
library(geosphere)
library(leaflet)
library(ggplot2)

## data
# ships <- readr::read_csv("ship_movement.csv")
load("ship_movement.RData")

## Test réalisation appli
ui <- semanticPage(
    title = "Shiny Developer @Appsilon / Marine",
    card(
        div(
            class = "content", 
            div(
                a(class="ui massive blue ribbon label", "Choose a vessel type")
            ), 
            dropdown_input(
                "type", 
                levels(ship_movement$ship_type %>% as.factor()))
        )
    ),
    card(
        div(
            class = "content", 
            div(
                a(class="ui huge teal ribbon label", "Choose a vessel")
            ),
            shiny::uiOutput("vessel")
        )
    ),
    leafletOutput("map"),
    plotOutput("graph")
)

server <- function(input, output) {
    observeEvent(input$type, {
        output$vessel <- renderUI({
            selectInput(
                "vessel",
                label = NULL,
                levels((ship_movement %>% 
                            filter(
                                ship_type == input$type
                            ) %>%
                            droplevels()
                )$SHIP_ID %>%
                    as.factor())
            )
        })
        
    })
    
    
    observeEvent(input$vessel, {
        
        vessel_data <- 
            ship_movement %>% 
            filter(
                ship_type == input$type,
                SHIP_ID == input$vessel
            )
        
        maximum <- vessel_data %>% 
            arrange(
                desc(distance),
                DATETIME 
            ) %>% 
            slice(1)
        
        output$map <- renderLeaflet({
            leaflet(data = vessel_data) %>% 
                addTiles() %>% 
                addMarkers(
                    lng = maximum$longitude_departure,
                    lat = maximum$latitude_departure,
                    popup = glue::glue("Ship {maximum$SHIP_ID} ({maximum$SHIPNAME}) sailed {maximum$distance %>% round()} m from here at {maximum$date} (speed: {maximum$SPEED} knots)")
                ) %>% 
                addMarkers(
                    lng = maximum$longitude_end,
                    lat = maximum$latitude_end,
                    popup = glue::glue("Ship {maximum$SHIP_ID} ({maximum$SHIPNAME}) sailed {maximum$distance %>% round()} m to here at {maximum$date}")
                ) %>% 
                addCircles(
                    lng = ~longitude_departure,
                    lat = ~latitude_departure,
                    popup = ~ glue::glue("Ship {SHIP_ID} ({SHIPNAME}) at {DATETIME}")
                )
        })
        
        output$graph <- renderPlot(
            vessel_data %>% 
                ggplot() +
                aes(x = DATETIME, y = SPEED, color = date %>% as.factor()) +
                geom_line() +
                geom_point() +
                theme(
                    legend.title = element_blank(),
                    panel.background = element_blank()
                ) 
        )
        
    })
    
}

shinyApp(ui, server)
