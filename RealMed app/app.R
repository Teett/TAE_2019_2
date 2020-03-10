library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(leaflet)
library(raster)
library(data.table)
library(knitr)
#----------------------------------------Base Code--------------------------------------------------#

# Shared data read
paletas_cualitativas <- RColorBrewer::brewer.pal.info %>%
    rownames_to_column(var = "palette") %>% 
    filter(category == "qual")
# Mapas
political <- shapefile("../Barrio_Vereda/Barrio_Vereda.shp")
Encoding(political@data$NOMBRE) <- "UTF-8"
political$NOMBRE <- political$NOMBRE %>% toupper() %>% str_replace("DE  MESA", "DE MESA")


# Salud
db_salud <- fread("../databases/db_salud.csv", encoding = "UTF-8") %>% as_tibble()
db_mapa_salud <- fread("../databases/db_mapa_salud.csv", encoding = "UTF-8") %>% as_tibble()
clustering_salud <- fread("../databases/clustering_salud.csv") %>% as_tibble()

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

media_sd <- function(variable) {
    paste0(round(mean(variable), 4), " (", round(sd(variable), 4), ")")
}

#-----------------------------------------Shiny App-------------------------------------------------#
# User Interface
ui <- dashboardPage(title = "Realidad de los barrios de Medellín: Diferencia de clases en dimensiones de ciudad",
                    skin = "black",
    dashboardHeader(title = "Dimensiones"),
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
    dashboardBody(titlePanel(h1("Realidad de los barrios de Medellín: Diferencia de clases en dimensiones de ciudad", align = "center")),
                  width = 12, 
        tabItems( 
            tabItem(tabName = "salud",
                    box(leafletOutput("leaflet_salud"), width = 12),
                    tabBox(title = "Características de los grupos",
                           tabPanel("Grupo 1", uiOutput("grupo_1")),
                           tabPanel("Grupo 2", uiOutput("grupo_2")),
                           tabPanel("Grupo 3", uiOutput("grupo_3")),
                           tabPanel("Grupo 4", uiOutput("grupo_4")),
                           tabPanel("Grupo 5", uiOutput("grupo_5")),
                           width = 12, height = 500),
                    box(title = h2("¡Explora tu propio clustering!", align = "center"),
                        leafletOutput("leaflet_salud_dinamico"),
                        "Los grupos que selecciones aquí comparten características en salud detectadas por el algoritmo, es decir que no necesariamente están ordenados por la calidad, 
                        para concluir correctamente sobre tus grupos puedes analizar los resultados de cada cluster en la tabla",
                        sliderInput(inputId = "k_salud", 
                                    label = h3("Selecciona el número de grupos"),
                                    min = 2, 
                                    max = 10, 
                                     value = 5),
                        DTOutput("tabla_salud_dinamica"),
                        width = 12
                        )
                    ),
            tabItem(tabName = "escolaridad"),
            tabItem(tabName = "ingresos"),
            tabItem(tabName = "movilidad"),
            tabItem(tabName = "seguridad")
            )
    )
)

# Server logic - Programming ---------------------------------------------------------------------------------------------------------#
server <- function(input, output) {
    # Salud --------------------------------------------------------------------------------------------------------------#
    output$leaflet_salud <- renderLeaflet(mapa_salud)
    output$leaflet_salud_dinamico <- renderLeaflet({
        # Input de usuario
        set.seed(3)
        kmeans_salud <- kmeans(clustering_salud, centers = eval(parse(text = input$k_salud)), nstart = 10, iter.max = 15)
        db_salud_kmeans <- bind_cols(db_salud, cluster = kmeans_salud$cluster)
        grupos_barrios_salud <- data.frame(barrio_nombre = db_salud$encuesta_calidad.barrio, grupo = kmeans_salud$cluster)
        nombres_mapa_salud <- data.frame(nombre_barrio = political$NOMBRE)
        
        vector_nombres_salud = c()
        vector_grupos_salud = c()
        
        for(nombre_mapa in nombres_mapa_salud$nombre_barrio) {
            grupo <- grupos_barrios_salud[grupos_barrios_salud$barrio_nombre == nombre_mapa, 2][1]
            vector_nombres_salud <- c(vector_nombres_salud, nombre_mapa)
            vector_grupos_salud <- c(vector_grupos_salud, grupo)
        }
        factpal_salud <- colorFactor(rainbow(eval(parse(text = input$k_salud))), vector_grupos_salud)
        
        mapa_dinamico_salud <- tibble(
            nombre_barrio = vector_nombres_salud, 
            grupo = vector_grupos_salud,
            color = factpal_salud(grupo)
        )
        
        colores_dinamico_salud <- mapa_dinamico_salud %>%
            dplyr::select(-nombre_barrio) %>% 
            distinct(color, .keep_all = TRUE) %>% 
            arrange(desc(grupo))
        
        leaflet(data = political) %>% 
            addTiles() %>% 
            addProviderTiles(providers$OpenStreetMap) %>% 
            addPolygons(fill = TRUE, stroke = TRUE, weight = 2, color = mapa_dinamico_salud$color, 
                        label = as.character(political$NOMBRE),
                        popup = as.character(political$NOMBRE)) %>% 
            addLegend("bottomright", colors = colores_dinamico_salud$color, labels = as.character(colores_dinamico_salud$grupo))
    })
    output$tabla_salud_dinamica <- renderDT({
        set.seed(3)
        kmeans_salud <- kmeans(clustering_salud, centers = eval(parse(text = input$k_salud)), nstart = 10, iter.max = 15)
        db_salud_kmeans <- bind_cols(db_salud, cluster = kmeans_salud$cluster)
        db_salud_kmeans %>%
        dplyr::select(-encuesta_calidad.barrio, -encuesta_calidad.comuna, -n) %>% 
        group_by(cluster) %>% 
        summarise_all(~media_sd(.))
        }, options = list(scrollX = TRUE))
    ##-------------------------------------------------Text outputs-------------------------------------------#
    output$grupo_1 <- renderUI({
        HTML(markdown::markdownToHTML(knit("grupo_1.rmd", quiet = TRUE)))
    })
    output$grupo_2 <- renderUI({
        HTML(markdown::markdownToHTML(knit("grupo_2.rmd", quiet = TRUE)))
    })
    output$grupo_3 <- renderUI({
        HTML(markdown::markdownToHTML(knit("grupo_3.rmd", quiet = TRUE)))
    })
    output$grupo_4 <- renderUI({
        HTML(markdown::markdownToHTML(knit("grupo_4.rmd", quiet = TRUE)))
    })
    output$grupo_5 <- renderUI({
        HTML(markdown::markdownToHTML(knit("grupo_5.rmd", quiet = TRUE)))
    })
}
# Run the application 
shinyApp(ui = ui, server = server)