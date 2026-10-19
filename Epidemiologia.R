#Epidemiologia  Curso 
# se debe installar los paquetes necesarios para poder correr el script
library(readxl)
library(janitor)
library(dplyr)
getwd()#Verificar donde estoy trabajando
setwd("C:/Users/Gabriel/Documents/Rstudio")

# 1.1.Preparar la sesión e importar la base

library(readxl)
dengue <- read_excel("Base_simulada_dengue_Piura_SE01_32_2026.xlsx")
View(dengue) #ver la base de datos
clean_names(dengue) #limpiar nombres de variables

 
dim(dengue) #cuantificar registros y nro de variables
names(dengue) # ver las variables 
head(dengue) # llamar las primeras filas o los 6 registros 

dengue

# 1.2.Seleccionar, filtrar y ordenar con varias condiciones
#Subgrupo: casos graves

casos_priorizados <- dengue |>
  filter(
    hospitalizacion == "Sí",
    diagnostic %in% c(
      "Dengue confirmado con signos de alarma",
      "Dengue grave"
    )
  ) |>
  select(
    semana, provincia, distrito, edad, sexo,
    diagnostic, hospitalizacion, resultado
  ) |>
  arrange(semana, provincia)

casos_priorizados

#1.3 Crear variables derivadas

dengue <- dengue |>
  mutate(
    grupo_edad = case_when(
      is.na(edad) ~ NA_character_,
      edad < 18 ~ "Menor de 18",
      edad < 60 ~ "18 a 59",
      TRUE ~ "60 o más"
    ),
    demora_notificacion = as.numeric(
      difftime(
        fecha_notificacion,
        fecha_inicio_sintomas,
        units = "days"
      )
    )
  )

# 1.4. Resúmenes por grupos

resumen_provincia <- dengue |>
  group_by(provincia) |>
  summarise(
    casos = n(),
    edad_mediana = median(edad, na.rm = TRUE),
    hospitalizados = sum(hospitalizacion == "Sí", na.rm = TRUE),
    .groups = "drop"
  )

resumen_provincia

###  Resolucion: Manipulacion encadenada

#1.	Filtre los casos notificados entre las semanas epidemiológicas 10 y 20.
#2.	Seleccione las variables semana, provincia, distrito, edad, sexo y diagnostic.
#3.	Cree una nueva variable llamada adulto_mayor que identifique a las personas de 60 años o más.
#4.	Obtenga el número de casos según provincia y sexo.



#2.1 Formato largo y formato ancho

library(tidyr)

casos_semana_provincia <- dengue |>
  count(semana, provincia)

casos_ancho <- casos_semana_provincia |>
  pivot_wider(
    names_from = provincia,
    values_from = n,
    values_fill = 0
  )

casos_ancho

#2.2 Volver de ancho a largo

casos_largo <- casos_ancho |>
  pivot_longer(
    cols = -semana,
    names_to = "provincia",
    values_to = "casos"
  )

casos_largo

#Resolucion:
#1. Conteo de casos por semana y sexo
casos_semana_sexo <- dengue |>
  count(semana, sexo)

head(casos_semana_sexo)


#2. Transformar de formato largo a ancho
casos_ancho1 <- casos_semana_sexo |>
  pivot_wider(
    names_from = sexo,
    values_from = n,
    values_fill = 0
  )

head(casos_ancho1)

#3. Casos: Volver de formato ancho a largo

casos_largo1 <- casos_ancho1 |>
  pivot_longer(
    cols = -semana,
    names_to = "sexo",
    values_to = "n"
  )

head(casos_largo1)

#3. Integración y control de calidad de bases
#3.1 Integrar información mediante joins

#Primero agregamos una variable: 

catalogo_distritos <- tibble(
  distrito = c(
    "Castilla",
    "Catacaos",
    "Piura",
    "Veintiséis de Octubre",
    "Bellavista",
    "Marcavelica",
    "Sullana",
    "La Brea",
    "Pariñas"
  ),
  zona = c(
    "Zona 1",
    "Zona 1",
    "Zona 1",
    "Zona 1",
    "Zona 2",
    "Zona 2",
    "Zona 2",
    "Zona 3",
    "Zona 3"
  )
)

catalogo_distritos

#Hacemos el join
dengue_integrado <- dengue |>
  left_join(
    catalogo_distritos,
    by = "distrito"
  )

head(dengue_integrado)

#Pequeña tabla que muestra resultados
dengue_integrado |>
  select(provincia, distrito, zona) |>
  distinct() |>
  arrange(provincia, distrito)

#Comprobacion:

dengue_integrado |>
  filter(is.na(zona)) |>
  distinct(distrito)

#3.2 Revisar duplicados
duplicados_exactos <- dengue |>
  group_by(across(everything())) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1)

duplicados_exactos

# 3.3 Datos faltante
faltantes <- dengue |>
  summarise(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  )

faltantes # todos los datos esta completos todos loscuadros del excel esta lleno sino se agrega NA

#3.4 Fechas y variables categóricas
dengue <- dengue |>
  mutate(
    fecha_inicio_sintomas = as.Date(fecha_inicio_sintomas),
    fecha_notificacion = as.Date(fecha_notificacion),
    sexo = factor(sexo),
    hospitalizacion = factor(hospitalizacion)
  )

str(dengue)

#Resolucion
# 3. Control de calidad
# 1. Comprobar duplicados exactos

duplicados1 <- dengue |>
  group_by(across(everything())) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1)

duplicados1

# 2. Contar valores faltantes en todas las variables

faltantes1 <- dengue |>
  summarise(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  )

faltantes1

## 3. Revisar valores únicos de variables categóricas

dengue |>
  distinct(sexo)

dengue |>
  distinct(hospitalizacion)

dengue |>
  distinct(resultado)

#4.1 La lógica de ggplot2
#En ggplot2, una figura se construye por capas: datos, estética, geometría, etiquetas y tema.
library(ggplot2)
library(dplyr)

casos_semana1 <- dengue |>
  count(semana)

grafico_semana1 <- ggplot(
  casos_semana1,
  aes(x = semana, y = n)
) +
  geom_area(
    fill = "lightyellow",
    alpha = 0.4
  ) +
  geom_line(
    color = "seagreen",
    linewidth = 1
  ) +
  geom_point(
    color = "seagreen",
    size = 2
  ) +
  scale_x_continuous(
    breaks = seq(
      min(casos_semana1$semana),
      max(casos_semana1$semana),
      by = 1
    )
  ) +
  labs(
    title = "Casos notificados de Dengue por semana epidemiológica (SE) en Piura",
    x = "Semana epidemiológica (SE)",
    y = "Nº de casos"
  ) +
  theme_minimal()

grafico_semana1 # agrandar la ventana de visualizacion para ver mejor el grafico

# Aprendiendo a hacer tablas resumenes: 

library(gtsummary)

tabla_resumen <- dengue |>
  select(
    edad,
    sexo,
    provincia,
    hospitalizacion,
    resultado
  ) |>
  tbl_summary(
    label = list(
      edad ~ "Edad",
      sexo ~ "Sexo",
      provincia ~ "Provincia",
      hospitalizacion ~ "Hospitalización",
      resultado ~ "Resultado"
    ),
    statistic = list(
      edad ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "ifany"
  ) |>
  bold_labels()

# label = list para arreglar las mayuscculas o si estan en ingles

tabla_resumen

#Variables Numericas:
#Instalamos Paquetes Necesarios

library(dplyr)
library(gtsummary)
library(gt)

tabla_numericas <- dengue |>
  select(
    edad,
    demora_notificacion
  ) |>
  tbl_summary(
    statistic = all_continuous() ~ "{mean} ({sd})",
    digits = all_continuous() ~ 1,
    label = list(
      edad ~ "Edad (años)",
      demora_notificacion ~ "Demora de notificación (días)"
    ),
    missing = "no"
  ) |>
  modify_header(
    label ~ "**Variable**",
    stat_0 ~ "**Media (DE)**"
  ) |>
  modify_caption(
    "**Tabla 1. Resumen de variables numéricas**"
  ) |>
  as_gt() |>
  tab_options(
    table.border.top.style = "solid",
    table.border.bottom.style = "solid",
    column_labels.border.bottom.style = "solid"
  )

tabla_numericas

print(tabla_numericas)

#Instalando paquete especial
install.packages("webshot2")
#intalando libreria

library(gtsummary)
library(gt)

#guardando tabla como pnj
gtsave(
  tabla_numericas,
  "resultados/tabla_numericas.png"
)

#Variables cualitativas
tabla_cualitativas <- dengue |>
  select(
    sexo,
    hospitalizacion,
    resultado
  ) |>
  tbl_summary(
    statistic = all_categorical() ~ "{n} ({p}%)",
    digits = all_categorical() ~ c(0, 1),
    label = list(
      sexo ~ "Sexo",
      hospitalizacion ~ "Hospitalización",
      resultado ~ "Resultado"
    ),
    missing = "no"
  ) |>
  modify_header(
    label ~ "**Variable**",
    stat_0 ~ "**n (%)**"
  ) |>
  modify_caption(
    "**Tabla 2. Resumen de variables cualitativas**"
  )

tabla_cualitativas

#guardamos en pnj
tabla_cualitativas_gt <- tabla_cualitativas |>
  as_gt()

gtsave(
  tabla_cualitativas_gt,
  "resultados/tabla_cualitativas.png"
)

#Perfil epidemiologico: Solo tomamos en cuenta los casos Confirmados

tabla_perfil <- dengue |>
  filter(
    diagnostic %in% c(
      "Dengue confirmado sin signos de alarma",
      "Dengue confirmado con signos de alarma",
      "Dengue grave"
    )
  ) |>
  select(
    edad,
    grupo_edad,
    sexo,
    provincia,
    diagnostic,
    hospitalizacion,
    resultado,
    demora_notificacion
  ) |>
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ c(0, 1)
    ),
    label = list(
      edad ~ "Edad (años)",
      grupo_edad ~ "Grupo de edad",
      sexo ~ "Sexo",
      provincia ~ "Provincia",
      diagnostic ~ "Clasificación del dengue",
      hospitalizacion ~ "Hospitalización",
      resultado ~ "Resultado",
      demora_notificacion ~ "Demora de notificación (días)"
    ),
    missing = "no"
  ) |>
  modify_header(
    label ~ "**Característica**",
    stat_0 ~ "**Total**"
  ) |>
  modify_caption(
    "**Tabla 3. Perfil epidemiológico de los casos confirmados de dengue**"
  ) |>
  bold_labels()

tabla_perfil


tabla_perfil_png <- gtsummary::as_gt(tabla_perfil)

gt::gtsave(
  tabla_perfil_png,
  "resultados/perfil_epidemiologico_dengue.png",
  zoom = 2
)

#####################
#Referencias:
#Posit. RStudio IDE User Guide. https://docs.posit.co/ide/user/
#Gerencia Regional de Salud Lambayeque. (2026). Calendario epidemiológico año 2026. Gobierno Regional Lambayeque. Calendario epidemiológico año 2026
#https://www.datosabiertos.gob.pe/dataset/vigilancia-epidemiol%C3%B3gica-de-dengue/resource/79e21c8d-3a94-4ac5-b234-bace71fe9675

###########################################################
################

# VARIABLE CATERGÓRICA: SEXO
# VARIABLE NUMÉRICA: EDAD
# COUNT = CONTEO 
