library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(leaflet)
library(raster)
library(data.table)
#----------------------------------------Base Code--------------------------------------------------#

# Shared data read
# Mapas
political <- shapefile("../Barrio_Vereda/Barrio_Vereda.shp")
Encoding(political@data$NOMBRE) <- "UTF-8"
political$NOMBRE <- political$NOMBRE %>% toupper() %>% str_replace("DE  MESA", "DE MESA")

# Salud
db_mapa_salud <- fread("../databases/db_mapa_salud.csv", encoding = "UTF-8") %>% as_tibble()
Encoding(db_mapa_salud$descripcion) <- "latin-1"

# ------------------------------------- Salud ------------------------------------------------------#
colores_salud <- db_mapa_salud %>%
    dplyr::select(-nombre_barrio) %>% 
    distinct(color, .keep_all = TRUE) %>% 
    arrange(desc(grupo))

mapa_salud <- leaflet(data = political) %>% 
    addTiles() %>% 
    addProviderTiles(providers$OpenStreetMap) %>% 
    addPolygons(fill = TRUE, stroke = TRUE, weight = 2, color = db_mapa_salud$color, 
                label = as.character(political$NOMBRE),
                popup = as.character(political$NOMBRE)) %>% 
    addLegend("bottomright", colors = colores_salud$color, labels = as.character(colores_salud$descripcion))


#-----------------------------------------Shiny App-------------------------------------------------#
# Define UI for application that draws a histogram
ui <- dashboardPage(title = "Realidad de los barrios de Medellín: Diferencia de clases estadísticas",
                    skin = "black",
    dashboardHeader(title = "Dimensiones de ciudad"),
    dashboardSidebar(
        sidebarMenu(
            id = "Tipos de accidente",
            menuItem(text = "Salud", tabName = "salud", icon = icon("ambulance")),
            menuItem(text = "Escolaridad", tabName = "escolaridad", icon = icon("book")),
            menuItem(text = "Ingresos", tabName = "ingresos", icon = icon("coins")),
            menuItem(text = "Movilidad", tabName = "movilidad", icon = icon("car")),
            menuItem(text = "Seguridad y libertad", tabName = "seguridad", icon = icon("landmark"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "salud",
                    leafletOutput("leaflet_salud", height = 600),
            tabItem(tabName = "escolaridad"),
            tabItem(tabName = "ingresos"),
            tabItem(tabName = "movilidad"),
            tabItem(tabName = "seguridad")
            )
    )
)
)

# Server logic - Programming
server <- function(input, output) {
    # Salud ----------------------------------------------------------------------------------------
    output$leaflet_salud <- renderLeaflet(mapa_salud)
}

# Run the application 
shinyApp(ui = ui, server = server)