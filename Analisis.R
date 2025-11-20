library(agricolae)
library(tidyverse)
library(readxl)
library(writexl)
library(jtools)
library(dplyr)
library(janitor) # Util para limpiar nombres de columnas
library(broom) # Útil para extraer resultados de modelos (tidy, glance)
library(readr) # Necesario para parse_number

analisis <- read_excel("datos/datos.xlsx", 
                       sheet = "Hoja")
etiquetas <- read_excel("datos/datos.xlsx", 
                       sheet = "Sheet1")

analisis <- analisis |>
  mutate(
    fecha = parse_date_time(
      paste("1", Mes, Año),
      orders = "d B Y",
      locale = "es_ES"     # importante para meses en español
    ),
    fecha_label = format(fecha, "%b - %y")
  ) 

analisis_long <- analisis |>
  pivot_longer(
    3:10,
    names_to = "Variable",
    values_to = "Valor"
  ) |>
  drop_na() 

stats_anual <- analisis_long |>
  group_by(Año) |>
  summarise(
    Media = mean(Valor),
    Desv = sd(Valor)
  )

variables <- unique(analisis_long$Variable)

yrs <- unique(analisis_long$Año)

analisis_long$Mes <- factor(analisis_long$Mes, 
                            levels = c("Enero","Febrero","Marzo",
                                       "Abril","Mayo","Junio",
                                       "Julio","Agosto","Septiembre",
                                       "Octubre","Noviembre","Diciembre"))


for(i in seq_along(variables)){
  df <- analisis_long |>
    filter(Variable == variables[i]) |>
    mutate(
      Año2 = factor(Año)
    )
  
  etiqueta <- etiquetas |>
    filter(Variable == variables[i])
  etiqueta <- paste0(etiqueta[1,2])
  nombre1 <- paste0("figures/evol1_",variables[i],".png")
  nombre2 <- paste0("figures/evol2_",variables[i],".png")
  g1 <- ggplot(df, aes(x = fecha, y = Valor)) +
    geom_point() + 
    geom_line() +
    theme_apa() +
    labs(x = "Año", y = etiqueta)  + 
    scale_x_date(
      breaks = as.Date(paste0(unique(year(df$fecha)), "-01-01")),
      labels = unique(year(df$fecha))
    ) 
  
  g2 <- ggplot(df, aes(x = Mes, y = Valor, group = Año2)) +
    geom_point(aes(col = Año2)) + 
    geom_line(aes(col = Año2)) + 
    labs(y = etiqueta) +
    theme_apa() +
    theme(
      legend.position = "bottom"
    )
  
  ggsave(nombre1, g1, width = 6, height = 5)
  ggsave(nombre2, g2, width = 6, height = 5)
}



