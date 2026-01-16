library(shiny)
library(tidyverse)
library(readxl)
library(jtools)
library(lubridate)

#----------------- Carga y preparación de datos -----------------
analisis <- read_excel("datos/datos.xlsx", sheet = "Hoja")  # aquí usa tu ruta real
etiquetas <- read_excel("datos/datos.xlsx", sheet = "Sheet1")

analisis <- analisis |>
  mutate(
    fecha = parse_date_time(
      paste("1", Mes, Año),   # en tu archivo la columna es "Ao" [file:1]
      orders = "d B Y",
      locale = "es_ES"
    ),
    fecha_label = format(fecha, "%b - %y")
  )

analisis_long <- analisis |>
  pivot_longer(
    cols = 3:10,      # igual que en tu script
    names_to = "Variable",
    values_to = "Valor"
  ) |>
  drop_na()

analisis_long$Mes <- factor(
  analisis_long$Mes,
  levels = c("Enero","Febrero","Marzo",
             "Abril","Mayo","Junio",
             "Julio","Agosto","Septiembre",
             "Octubre","Noviembre","Diciembre")
)

# Para usar en selectInput
vars_disponibles <- unique(analisis_long$Variable)
anios_disponibles <- sort(unique(analisis_long$Año))  # columna Ao en el archivo [file:1]

#----------------- UI -----------------
ui <- fluidPage(
  titlePanel("Evolución mensual por año y variable"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("anio",
                  "Seleccione el año:",
                  choices = anios_disponibles,
                  selected = max(anios_disponibles)),
      selectInput("var",
                  "Seleccione la variable:",
                  choices = vars_disponibles,
                  selected = vars_disponibles[1])
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Año seleccionado",
                 plotOutput("grafico_g2_anio")),
        tabPanel("Todos los años",
                 plotOutput("grafico_g2_todos"))
      )
    )
  )
)

#----------------- SERVER -----------------
server <- function(input, output, session){
  
  # --- Datos reactivos ---
  
  # 1) Para un solo año
  datos_anio <- reactive({
    analisis_long |>
      filter(Año == input$anio,
             Variable == input$var) |>
      mutate(Año2 = factor(Año))
  })
  
  # 2) Para todos los años (solo filtra variable)
  datos_todos <- reactive({
    analisis_long |>
      filter(Variable == input$var) |>
      mutate(Año2 = factor(Año))
  })
  
  # Etiqueta eje Y
  etiqueta_var <- reactive({
    etq <- etiquetas |>
      filter(Variable == input$var)
    paste0(etq[[1, 2]])
  })
  
  # --- Gráfico 1: año seleccionado ---
  output$grafico_g2_anio <- renderPlot({
    df <- datos_anio()
    
    ggplot(df, aes(x = Mes, y = Valor, group = Año2)) +
      geom_point(aes(col = Año2)) +
      geom_line(aes(col = Año2)) +
      labs(y = etiqueta_var(), x = "Mes") +
      theme_apa() +
      theme(legend.position = "bottom")
  })
  
  # --- Gráfico 2: todos los años ---
  output$grafico_g2_todos <- renderPlot({
    df <- datos_todos()
    
    ggplot(df, aes(x = Mes, y = Valor, group = Año2)) +
      geom_point(aes(col = Año2)) +
      geom_line(aes(col = Año2)) +
      labs(y = etiqueta_var(), x = "Mes") +
      theme_apa() +
      theme(legend.position = "bottom")
  })
}


shinyApp(ui, server)
