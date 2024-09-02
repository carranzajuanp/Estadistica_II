#Cargamos los datos
library(googlesheets4)
datos <- read_sheet('https://docs.google.com/spreadsheets/d/1FW4EtmIWLB5bo0mXUDiOGJeI9g6uCTxEM00clT2V9ug/edit?usp=sharing')

# Creamos una variable categórica a partir de la edad de las personas encuestadas
library(tidyverse)
datos$rango = case_when(datos$edad >= 16 &  datos$edad <= 25 ~ "16 a 25 años",
                        datos$edad >= 26 &  datos$edad <= 34 ~ "24 a 34 años",
                        datos$edad >= 35 &  datos$edad <= 45 ~ "35 a 45 años",
                        datos$edad >= 46 &  datos$edad <= 60 ~ "46 a 60 años",
                        datos$edad >= 61  ~ "+60 años")

# Convertimos a esta nueva variable en un factor
datos$rango = factor(datos$rango, levels = c("16 a 25 años", "24 a 34 años", "35 a 45 años", "46 a 60 años", "+60 años"))

# Verificamos la cantidad de casos que tenemos en cada rango etario
table(datos$rango)

# Verificamos que no haya algún NA dentro de estas categorías
table(is.na(datos$rango))

# Creamos una tabla para ver la cantidad total de encuestas realizadas por cada estudiante
control_total = datos %>% 
  group_by(mail) %>% 
  summarise(n = n())
control_total
library(xlsx)
write.xlsx(control_total, "control_total.xlsx")

# Creamos una tabla para ver la cantidad de encuestas realizadas por cada estudiante en cada rango etario
control_desagregado = datos %>% 
  group_by(mail, rango) %>% 
  summarise(n = n())
control_desagregado
write.xlsx(control_total, "control_desagregado.xlsx")

