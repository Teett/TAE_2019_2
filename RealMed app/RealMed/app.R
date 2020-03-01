library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)


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
            tabItem(tabName = "salud"),
            tabItem(tabName = "escolaridad"),
            tabItem(tabName = "ingresos"),
            tabItem(tabName = "movilidad"),
            tabItem(tabName = "seguridad")
            )
    )
)

# Server logic - Programming
server <- function(input, output) {
    
}

# Run the application 
shinyApp(ui = ui, server = server)