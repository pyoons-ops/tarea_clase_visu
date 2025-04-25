


# 0 - Usando el dataframe visto en clase sobre delitos en CABA en 2023####
library(tidyverse)
library(skimr)
library(sf)
library(tidytext)
library(scales)
library(ggnewscale)

## 0.1 - dataframe delitos caba####
df_delitos  <-  read.csv2("../data/delitos_2023.csv", encoding = "UTF-8")

glimpse(df_delitos)

unique(df_delitos$franja)
unique(df_delitos$tipo)
unique(df_delitos$comuna)

df_delitos2  <-  df_delitos %>% 
  mutate(tipo_delito = as.factor(tipo),
         fecha = ymd(fecha),
         horario = as.numeric(franja),
         comuna = case_when(
           comuna == "CC-08" ~ "8",
           comuna == "CC-09" ~ "9",
           comuna == "CC-01 NORTE" ~ "1",
           comuna == "CC-01 SUR" ~ "1",
           comuna == "CC-04" ~ "4",
           comuna == "CC-07" ~ "7",
           comuna == "CC-15" ~ "15",
           comuna == "CC-02" ~ "2",
           comuna == "CC-12" ~ "12",
           comuna == "CC-10" ~ "10",
           comuna == "CC-06" ~ "6",
           comuna == "CC-13" ~ "13",
           comuna == "CC-05" ~ "5",
           comuna == "CC-03" ~ "3",
           comuna == "CC-14" ~ "14",
           comuna == "CC-11" ~ "11",
           TRUE ~ comuna)) %>%
  mutate(comuna = as.numeric(comuna)) %>%
  select(-c(tipo, franja))

unique(df_delitos2$comuna)

df_delitos2  <-  df_delitos2 %>% 
  mutate(
    latitud = str_remove_all(latitud, "\\."),
    latitud = str_squish(latitud),
    latitud = str_replace(latitud, "(^-?\\d{2})", "\\1."),
    latitud = as.numeric(latitud),
    longitud = as.numeric(str_replace_all(str_squish(str_remove_all(longitud, "\\.")),"(^-?\\d{2})", "\\1."))
  )

df2_delitos_delitos_fecha$fecha <- as.Date(df2_delitos_delitos_fecha$fecha)





# 1 - Mejorar visualmente el siguiente gráfico (pueden usar colores, acomodar labels de los ejes, agregar titulos, etc)####

df2_delitos_delitos_fecha <- df_delitos2 %>% 
  group_by(fecha, tipo_delito) %>% 
  summarise(cantidad=n())

p2_delitos_fecha <- ggplot(df2_delitos_delitos_fecha, 
                           aes(x = fecha, 
                               y = cantidad, 
                               color = tipo_delito))+
  geom_line(size = 1, alpha = 0.8) +
  theme_minimal() +
  labs(
    title= "Delitos por fecha en CABA",
    caption = "Fuente:datos abiertos GCBA",
    x = "Fecha",
    y = "Cantidad",
    color = "Tipo de Delito"
  ) +
  scale_color_brewer(palette = "Set2") +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %y",  
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )
# geom_smooth()

p2_delitos_fecha



# 2 - Cuál fue el mes con mayor cantidad de delitos?####

# El mes con mayor cantidad de delitos fue Marzo, lo cual puede comprobarse con el grafico siguiente:
p1_delitos_mensuales


## 2.1 - Agrupar mes y tipo de delito para poder evaluar con mas precision####

library(dplyr)
library(lubridate)

{
  df_delitos_mensuales <- df2_delitos_delitos_fecha %>%
    mutate(mes = floor_date(fecha, "month")) %>%  
    group_by(mes, tipo_delito) %>%
    summarise(cantidad = sum(cantidad), .groups = "drop")
}


## 2.2 - Armar el grafico nuevamente con datos mas precisos#### 
{
  p1_delitos_mensuales <- ggplot(df_delitos_mensuales, 
                                 aes(x = mes, 
                                     y = cantidad, 
                                     fill = tipo_delito))+
    geom_bar(stat = "identity", position = "stack")+
    #geom_bar(stat="identity", position = "stack") +
    #scale_x_reordered() +
    coord_flip() +
    #facet_wrap(~tipo_delito, scales = "free") +
    theme_minimal() +
    labs(
      title= "Delitos por fecha",
      caption = "Fuente:datos abiertos GCBA",
      x = "mes",
      y = "Cantidad",
      fill = "Tipo de Delito"
    ) +
    scale_fill_manual(values = c(
      "Homicidios" = "#900C3F", 
      "Hurto" = "#f4d03f", 
      "Robo" = "#d35400", 
      "Lesiones" = "#2e86c1", 
      "Amenazas" = "#117a65", 
      "Vialidad" = "#6c3483")) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b %y",  
      expand = expansion(mult = c(0.01, 0.01))
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      axis.text = element_text(size = 12),
      legend.position = "bottom"
    )
}
p1_delitos_mensuales


# 3 - En cuál comuna, los robos son el tipo de delito menos usual?####

#La comuna con menor cantidad de robos es la comuna 10, lo cual lo hace el tipo de delito menos usual


## 3.1 - Armar dataframe de delitos por comuna ####

df1_delitos_comuna <- df_delitos2 %>%
  group_by(comuna, tipo_delito) %>%
  summarise(cantidad=n())%>%
  drop_na()


## 3.2 - Grafico de cantidad y tipos de delito por comuna####

{
  p1_delito_comuna_c<- ggplot(df1_delitos_comuna,
                              aes(x = reorder_within(comuna, cantidad, tipo_delito),
                                  y = cantidad, 
                                  fill = tipo_delito))+
    geom_bar(stat="identity", position = "stack")+
    scale_x_reordered() +
    #coord_flip() +
    facet_wrap(~tipo_delito, scales = "free") +  
    theme_minimal()+
    labs(x = "Comuna", 
         y = "Cantidad",
         fill = "Tipo de delito",
         title = "Cantidad y tipo de delitos por comuna",
         caption = "Fuente:datos abiertos GCBA") +
    scale_fill_manual(values = c(
      "Homicidios" = "#900C3F", 
      "Hurto" = "#f4d03f", 
      "Robo" = "#d35400", 
      "Lesiones" = "#2e86c1", 
      "Amenazas" = "#117a65", 
      "Vialidad" = "#6c3483")) +
    scale_y_continuous(labels = scales::comma) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text = element_text(size = 10),
      legend.position = "bottom"
    )
}

p1_delito_comuna_c


# 4 - Generar una preguna analítica y responderla con un gráfico####

## En que comunas se presenta una mayor concentracion de delitos y como se relaciona esto con el ingreso promedio por comuna?



## 4.1 - dataframe de los ingresos por comuna####

comunas_ingreso <- read.csv("C:\\Users\\LENOVO\\Documents\\R\\OPAL\\comunas por ingreso\\data\\Comunas_por_ingreso_2.csv")

df_comunas_ingreso <- comunas_ingreo %>%
  rename(comuna = numerocomuna)



## 4.2 - dataframe del total de delitos por comuna####

df_total_delitos_comuna <- df1_delitos_comuna %>%
  group_by(comuna) %>%
  summarise(cantidad = sum(cantidad, na.rm = TRUE)) %>%
  ungroup()



## 4.3 - unificacion de ambos dataframe####

df_delitos_ingreso <- df_total_delitos_comuna %>%
  left_join(df_comunas_ingreso, by = "comuna")


## 4.4 - Elaboracion de grafico de dispercion entre las variables seleccionadas####

{
  p1_delitos_ingreso <-  ggplot(df_delitos_ingreso, aes(
    x = ingreso_promedio, 
    y = cantidad, 
    label = comuna)) +
    geom_point(size = 4, color = "#ee0000", alpha = 0.7) +  # Puntos con transparencia
    geom_text(vjust = -0.8, size = 3, color = "black") +    # Etiquetas claras
    #geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "red") +  # Tendencia
    scale_x_continuous(labels = label_comma(prefix = "$")) +  # Ingreso con formato monetario
    scale_y_continuous(labels = label_comma()) +  # Delitos con miles separados
    labs(
      title = "Relación entre ingresos y cantidad de delitos por comuna",
      subtitle = "CABA - Datos por comuna",
      x = "Ingreso promedio mensual por comuna",
      y = "Cantidad total de delitos",
      caption = "Fuente: datos abiertos GCBA"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
      plot.caption = element_text(size = 10, hjust = 1),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "gray20"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "none"
    )
}

p1_delitos_ingreso

## 4.5 - Mapa de calor ####

df_heatmap <- df_delitos_ingreso %>%
  pivot_longer(cols = c(cantidad, ingreso_promedio), 
               names_to = "variable", 
               values_to = "valor")


## 4.6 - desglosar los dataframe por cantidad de delitos y por ingreso ####
df_delitos_b <- df_heatmap %>% filter(variable == "cantidad")
df_ingresos_b <- df_heatmap %>% filter(variable == "ingreso_promedio")


## 4. 7 - elaborar el mapa de calor ####
{
  p1_delitos_ingreso_b <- ggplot() +
    geom_tile(data = df_delitos_b, aes(x = variable, y = reorder(comuna, valor), fill = valor), color = "white") +
    scale_fill_gradient(
      name = "Cantidad de delitos",
      low = "#fadbd8",
      high = "#943126"
    ) +
    ggnewscale::new_scale_fill() +
    geom_tile(data = df_ingresos_b, aes(x = variable, y = reorder(comuna, valor), fill = valor), color = "white") +
    scale_fill_gradient(
      name = "Ingreso promedio",
      low = "#d5f5e3",
      high = "#1d8348",
      labels = scales::comma_format(prefix = "$")
    ) +
    labs(
      title = "Mapa de calor por comuna: delitos e ingresos",
      x = "Variable",
      y = "Comuna",
      caption = "Fuente: datos abiertos GCBA"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 11),
      legend.position = "right"
    )
}

