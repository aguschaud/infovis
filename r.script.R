install.packages("ggiraph")
install.packages("tidyverse")
install.packages("duckdb")
install.packages("ggplot2")
install.packages("dplyr")
library("duckdb")
library("tidyverse")
library("ggiraph")
library("ggplot2")
library("dplyr")

con<-dbConnect(duckdb())
prueba<-dbGetQuery(con,"SELECT 1+1")
prueba

setwd("C:/Agus/2023 2do cuatri/Info vis/tp final/parquets")
res1<-dbGetQuery(con,"SELECT count(*) FROM read_parquet(ResultadosElectorales_1v.parquet);")
res1

prim_10 <- dbGetQuery(con, "SELECT * FROM read_parquet('ResultadosElectorales_1v.parquet') LIMIT 10")

resL <- dbGetQuery(con, "SELECT * FROM read_parquet('ResultadosElectorales_1v.parquet') WHERE agrupacion_nombre='LA LIBERTAD AVANZA'")


#votos por distrito
votos_por_distrito <- dbGetQuery(con, "SELECT distrito_nombre, SUM(votos_cantidad) AS total_votos FROM read_parquet('ResultadosElectorales_1v.parquet') GROUP BY distrito_nombre")
votos_por_distrito

# votos por agrupacion
votos_x_p <- dbGetQuery(con, "SELECT agrupacion_nombre, SUM(votos_cantidad) as votos FROM read_parquet('ResultadosElectorales_1v.parquet') WHERE cargo_id = 3 AND agrupacion_nombre IS NOT NULL GROUP BY agrupacion_nombre;")

# votos por agrupacion por distrito
votos_agrupacion_distrito <- dbGetQuery(con, "SELECT distrito_id, agrupacion_nombre, SUM(votos_cantidad) AS total_votos FROM read_parquet('ResultadosElectorales_1v.parquet') WHERE agrupacion_nombre IS NOT NULL GROUP BY distrito_id, agrupacion_nombre ORDER BY distrito_id, agrupacion_nombre")
#---------------grafico votos distrito---------

datos <- dbGetQuery(con, "
  SELECT distrito_id, agrupacion_nombre, SUM(votos_cantidad) AS total_votos 
  FROM read_parquet('ResultadosElectorales_1v.parquet') 
  WHERE agrupacion_nombre IN ('UNION POR LA PATRIA', 'JUNTOS POR EL CAMBIO', 'LA LIBERTAD AVANZA', 'HACEMOS POR NUESTRO PAIS', 'FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD') 
  GROUP BY distrito_id, agrupacion_nombre
  ORDER BY distrito_id, agrupacion_nombre
")


agrupaciones <- c("UNION POR LA PATRIA", "JUNTOS POR EL CAMBIO", "LA LIBERTAD AVANZA", "HACEMOS POR NUESTRO PAIS", "FRENTE DE IZQUIERDA Y DE TRABAJADORES")


#diccionario de distritos
dict_distritos <- dbGetQuery(con, "SELECT distrito_id, distrito_nombre FROM read_parquet('ResultadosElectorales_1v.parquet') GROUP BY distrito_id, distrito_nombre ORDER BY distrito_id")
partidos <- c("UNION POR LA PATRIA","JUNTOS POR EL CAMBIO","LA LIBERTAD AVANZA","HACEMOS POR NUESTRO PAIS","FRENTE DE IZQUIERDA Y DE TRABAJADORES" )
bancas<-C()

colores_partidos <- c(
  "JUNTOS POR EL CAMBIO" = "#fedd00",
  "UNION POR LA PATRIA" = "#009cde",
  "LA LIBERTAD AVANZA" = "#753bbd",
  "FRENTE DE IZQUIERDA Y DE TRABAJADORES" = "#f95461",
  "HACEMOS POR NUESTRO PAIS" = "#43488f"
)
# Crear el gráfico
ggplot(resultados, aes(fill = partido, y = bancas, x = distrito)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = colores_partidos) +
  labs(x = 'Distrito', y = 'Número de bancas', fill = 'Partido') +
  theme_minimal()


# Número de bancas por provincia (según la información proporcionada)
bancas_por_provincia <- c('Provincia de Buenos Aires' = 35, 'Ciudad de Buenos Aires' = 12,
                           'Santa Fe' = 10, 'Córdoba' = 9, 
                           'Mendoza' = 5, 'Corrientes' = 5, 'Entre Ríos' = 5, 
                           'Misiones' = 5, 'Salta' = 5, 'Santiago del Estero' = 5, 
                           'Tucumán' = 5, 'Chaco' = 5, 'Chubut' = 5, 'Formosa' = 5, 
                           'Jujuy' = 5, 'La Rioja' = 5, 'Río Negro' = 5, 'San Juan' = 5, 
                           'Tierra del Fuego' = 5, 'Catamarca' = 5, 'La Pampa' = 5, 
                           'Neuquén' = 5, 'Santa Cruz' = 5, 'San Luis' = 5)


#----obtener datos-------------------


# Obtener los datos de la base de datos
datos <- dbGetQuery(con, "
  SELECT distrito_id, agrupacion_nombre, SUM(votos_cantidad) AS total_votos 
  FROM read_parquet('ResultadosElectorales_1v.parquet') 
  WHERE agrupacion_nombre IN ('UNION POR LA PATRIA', 'JUNTOS POR EL CAMBIO', 'LA LIBERTAD AVANZA', 'HACEMOS POR NUESTRO PAIS', 'FRENTE DE IZQUIERDA Y DE TRABAJADORES - UNIDAD') 
  GROUP BY distrito_id, agrupacion_nombre
  ORDER BY distrito_id, agrupacion_nombre
")

#--------------grafico bancas diputados ----------------
# Configuraciones basadas en pcParams
width <- 600
height <- 350
center_x <- 300
center_y <- 175
min_radius <- 80
seat_radius <- 5
seat_spacing <- 2

# Distribución de bancas por partido
distribucion_bancas <- c(
  "UNION POR LA PATRIA" = 105,
  "JUNTOS POR EL CAMBIO" = 93,
  "LA LIBERTAD AVANZA" = 38,
  "HACEMOS POR NUESTRO PAIS" = 16,
  "FRENTE DE IZQUIERDA Y DE TRABAJADORES" = 5
)

# Colores de los partidos
colores_partidos <- c(
  "JUNTOS POR EL CAMBIO" = "#fedd00",
  "UNION POR LA PATRIA" = "#009cde",
  "LA LIBERTAD AVANZA" = "#753bbd",
  "FRENTE DE IZQUIERDA Y DE TRABAJADORES" = "#f95461",
  "HACEMOS POR NUESTRO PAIS" = "#43488f"
)

# Función para calcular coordenadas
get_seat_coordinates <- function(row, seat_num, total_seats_row, min_radius, center_x, center_y) {
  radius <- min_radius + (row - 1) * (2 * seat_radius + seat_spacing)
  angle_per_seat <- pi / (total_seats_row - 1)
  theta <- angle_per_seat * (seat_num - 1)
  x <- center_x + radius * cos(pi - theta)
  y <- center_y + radius * sin(pi - theta)
  return(c(x, y))
}

# Crear datos de las bancas
bancas <- data.frame(partido = character(), x = numeric(), y = numeric())
# Asignar bancas a partidos con bloques de partidos
current_seat <- 1
for (row in 1:5) {
  for (seat in 1:seats_per_row) {
    if (all(distribucion_bancas == 0)) {
      break
    }
    
    partido <- names(distribucion_bancas)[which.max(distribucion_bancas)]
    while (distribucion_bancas[partido] == 0) {
      partido <- names(distribucion_bancas)[which.max(distribucion_bancas)]
    }
    
    distribucion_bancas[partido] <- distribucion_bancas[partido] - 1
    coords <- get_seat_coordinates(row, seat, seats_per_row, min_radius, center_x, center_y)
    bancas <- rbind(bancas, data.frame(partido, x = coords[1], y = coords[2]))
  }
}

# Crear el gráfico
dip <- ggplot(bancas, aes(x = x, y = y, color = partido)) +
  geom_point() +
  scale_color_manual(values = colores_partidos) +
  theme_void() +
  coord_fixed(ratio = 1)

print(dip)

#---------------grafico bancas senadores---------------------------------
library(ggplot2)

# Configuraciones para la Cámara de Senadores
width <- 600
height <- 350
center_x <- 300
center_y <- 175
min_radius <- 60
seat_radius <- 5
seat_spacing <- 2

# Distribución de bancas por partido
distribucion_bancas_sen <- c(
  "UNION POR LA PATRIA" = 33,
  "JUNTOS POR EL CAMBIO" = 21,
  "LA LIBERTAD AVANZA" = 7,
  "CAMBIO FEDERAL" = 4,
  "UNIDAD FEDERAL" = 3,
  "MISIONES" = 2,
  "JUNTOS SOMOS RIO NEGRO" = 1,
  "PRODUCCION Y TRABAJO" = 1
)

# Colores de los partidos
colores_partidos_sen <- c(
  "UNION POR LA PATRIA" = "#009cde",
  "MISIONES" = "#a5c8e1",
  "JUNTOS SOMOS RIO NEGRO" = "#3498db",
  "JUNTOS POR EL CAMBIO" = "#fedd00",
  "CAMBIO FEDERAL" = "#e74c3c",
  "Hay Futuro Argentina" = "#f39c12",
  "PRODUCCION Y TRABAJO" = "#f7dc6f",
  "Partido de la Justicia Social" = "#f1c40f",
  "UNIDAD FEDERAL" = "#27ae60",
  "LA LIBERTAD AVANZA" = "#753bbd"
)

# Función para calcular coordenadas
get_seat_coordinates_sen <- function(row, seat_num, total_seats_row, min_radius, center_x, center_y) {
  radius <- min_radius + (row - 1) * (2 * seat_radius + seat_spacing)
  angle_per_seat <- pi / (total_seats_row - 1)
  theta <- angle_per_seat * (seat_num - 1)
  x <- center_x + radius * cos(pi - theta)
  y <- center_y + radius * sin(pi - theta)
  return(c(x, y))
}

# Crear datos de los bancas
bancas_sen <- data.frame(partido = character(), x = numeric(), y = numeric())


# Asignar bancas a partidos
total_seats <- sum(distribucion_bancas_sen)
seats_per_row <- round(total_seats / 4)  #  4 filas para 72 bancas
current_seat <- 1
for (row in 1:4) {
  for (seat in 1:seats_per_row) {
    partido <- names(distribucion_bancas_sen)[which.max(distribucion_bancas_sen)]
    distribucion_bancas_sen[partido] <- distribucion_bancas_sen[partido] - 1
    coords <- get_seat_coordinates_sen(row, seat, seats_per_row, min_radius, center_x, center_y)
    bancas_sen <- rbind(bancas_sen, data.frame(partido, x = coords[1], y = coords[2]))
  }
}

sen <- ggplot(bancas_sen, aes(x = x, y = y, color = partido)) +
  geom_point(size = 4) +  
  scale_color_manual(values = colores_partidos_sen) +
  theme_void() +
  coord_fixed(ratio = 1) +
  theme(legend.position = "right") +  # Ajustar la posición de los nombres de los partidos
  guides(color = guide_legend(override.aes = list(size=6))) # Asegurarse de que la leyenda sea legible

print(sen)


