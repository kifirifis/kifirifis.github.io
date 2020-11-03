# Esta es mi app con un ggplot sencillo

# Librerías y datos ----
library(shiny)
library(tidyverse)
library(ggrepel)
library(DT)
library(shinythemes)

datos <- read.csv("datos.csv", header = T)
nome <- levels(datos$nome)

# .ui.R ----

ui <- fluidPage(
        
        # Título de la app
        titlePanel(title = "Nombres de niña en Galicia"),
        
        # Sidebar layout con las definiciones de entrada y salida ----
        sidebarLayout(
                
                # Posicion del sidebarLayout ----
                #position = "right",
                
                # sidebarPanel con las entradas ----
                sidebarPanel(
                        
                        # Entrada: selectInput alguno de los niveles de la variable nome
                        selectInput(inputId = "nome",
                                    label = "Selecciona el nombre de niña:",
                                    choices = nome, 
                                    selected = "LUCIA"), #selectInput
                        
                        # Entrada: Texto de ayuda
                        helpText("Nota: Los nombres que aparecen aquí son los nombres",
                                 "que porporciona el IGE en su página.")
                        
                ), # sidebarPanel
                
                # mainPanel con las salidas ----
                mainPanel(
                        
                        # Salida: Plot de ggplot
                        plotOutput(outputId = "Plot"),
                        
                        # Salida: Tabla nombres más comunes por año
                        h4("El nombre más común de cada año:"),
                        dataTableOutput("tabla")
                        
                ) # mainPanel
                
        ) # sidebarLayout
        
) # fluidPage

# server.R ----

# Definir lógica del servidor para que dibuje un ggplot
server <- function(input, output) {
        
        output$Plot <- renderPlot({
                
                nombre <- datos %>% filter(nome == input$nome)
                todos <- datos %>% filter(nome != input$nome)        
                
                todos %>% 
                        ggplot(aes(tempo, porcentaxe / 100, group = nome)) +
                        geom_line(colour = "gray70", alpha = 0.5) +
                        geom_point(colour = "gray70" , alpha = 0.5) +
                        #Nombre específico        
                        geom_line(data = nombre, aes(tempo, porcentaxe / 100, colour = nome)) +
                        geom_point(data = nombre, aes(tempo, porcentaxe / 100, colour = nome)) +
                        scale_color_manual(name = "Nombre:", values = "blue") +
                        geom_text_repel(aes(label = paste(as.character(porcentaxe), "%", sep = "")), 
                                        data = nombre) +
                        theme(legend.position = "top") +
                        
                        scale_x_continuous(breaks = seq(2000, 2018, 2)) +
                        scale_y_continuous(labels = scales::percent) +
                        
                        labs(x = "", y = "", title = "Nombres de niña en Galicia 2000-2018",
                             subtitle = "Fuente: IGE", caption = "Elaboración: Javier Kniffki | KStats®")  
        }) # renderPlot
        
        output$tabla <- renderDataTable(
                
                datos %>% group_by(tempo) %>% 
                        filter(frecuencia ==  max(frecuencia)) %>% 
                        select(-porcentaxe, Nombre = nome, Frecuencia = frecuencia, Año = tempo)
                
        ) # renderDataTable
        
} # server

# Crear Shiny app ----
shinyApp(ui = ui, server = server)