# Instalar y cargar las bibliotecas necesarias
if (!require("readxl")) install.packages("readxl") # Lectura de archivos Excel
if (!require("ggplot2")) install.packages("ggplot2")   # Creación de gráficos
if (!require("dplyr")) install.packages("dplyr")  # Manipulación y transformación de datos
if (!require("tidyr")) install.packages("tdyr")
# Carga de librerías
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

# Ruta al archivo Excel
path <- "C:/Users/ASUS VIVOBOOK PRO/Downloads/PruebaDatos.xlsx"
path2 <- "C:/Users/ASUS VIVOBOOK PRO/Downloads/Datos_Agrupados.csv"
# --- CARGA Y COMBINACIÓN DE DATOS ---

datos <- bind_rows(lapply(hojas, function(hoja) {
  read_excel(path, sheet = hoja)
}))
datos2 <- read.csv(path2) 

# Leer las hojas del archivo Excel y añadir etiquetas para pierna y parte del pie
punta_no_dominante <- read_excel(path, sheet = "Punta - No Dominante") %>%
  mutate(Pierna = "No Dominante", Parte = "Punta")
punta_dominante <- read_excel(path, sheet = "Punta - Dominante") %>%
  mutate(Pierna = "Dominante", Parte = "Punta")
interno_no_dominante <- read_excel(path, sheet = "Interno - No Dominante") %>%
  mutate(Pierna = "No Dominante", Parte = "Interno")
interno_dominante <- read_excel(path, sheet = "Interno - Dominante") %>%
  mutate(Pierna = "Dominante", Parte = "Interno")
externo_no_dominante <- read_excel(path, sheet = "Externo - No Dominante") %>%
  mutate(Pierna = "No Dominante", Parte = "Externo")
externo_dominante <- read_excel(path, sheet = "Externo - Dominante") %>%
  mutate(Pierna = "Dominante", Parte = "Externo")

# Combinar todas las hojas en un solo dataframe
datos <- bind_rows(punta_no_dominante, punta_dominante, 
                   interno_no_dominante, interno_dominante, 
                   externo_no_dominante, externo_dominante)

# Convertir la columna 'Resultados' a formato numérico
datos$Resultados <- as.numeric(as.character(datos$Resultados))

# --- PARTE 1: GRÁFICO DE BARRAS AGRUPADAS ---
# Clasificar los resultados como "Éxito" o "Fallo"
datos <- datos %>%
  mutate(Estado = ifelse(Resultados == 1, "Éxito", "Fallo"))

# Crear un gráfico de barras agrupadas por pierna y parte del pie
ggplot(datos, aes(x = interaction(Pierna, Parte), fill = Estado)) +
  geom_bar(position = "dodge") +  # Barras agrupadas (no apiladas)
  labs(
    title = "Éxito y fallo por combinación de pierna y parte del pie",
    x = "Combinación de pierna y parte del pie",
    y = "Frecuencia",
    fill = "Resultado"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Texto inclinado en el eje X
    plot.title = element_text(hjust = 0.5)  # Título centrado
  )

# --- PARTE 2: GRÁFICO DE BARRAS SIMPLES ---
# Calcular la proporción de éxito para cada parte del pie
proporciones <- datos %>%
  group_by(Parte) %>%
  summarise(Proporcion_Exito = mean(Resultados == 1))

# Crear el gráfico de barras simples
ggplot(proporciones, aes(x = Parte, y = Proporcion_Exito)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +  # Barras proporcionales
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )+
  labs(
    title = "Proporción de Éxito por Parte del Pie",
    x = "Parte del Pie",
    y = "Proporción de Éxito"
  ) 

# --- PARTE 3: MAPA DE CALOR ---
# Calcular la proporción de éxito combinada por pierna y parte del pie
heatmap_data <- datos %>%
  group_by(Pierna, Parte) %>%
  summarise(Proporcion_Exito = mean(Resultados == 1)) %>%
  ungroup()

# Crear un mapa de calor para visualizar las proporciones de éxito
ggplot(heatmap_data, aes(x = Pierna, y = Parte, fill = Proporcion_Exito)) +
  geom_tile(color = "white") +  # Celdas con bordes blancos
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Proporción\nÉxito") +
  labs(
    title = "Proporción de Éxito por Pierna y Parte del Pie",
    x = "Pierna Usada",
    y = "Parte del Pie"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    panel.grid.major = element_blank(),  # Quitar líneas de cuadrícula
    panel.grid.minor = element_blank()
  )

# --- PARTE 4: GRÁFICO DE BARRAS POR PIERNA ---
# Calcular la proporción de éxito por pierna
pierna_data <- datos %>%
  group_by(Pierna) %>%
  summarise(Proporcion_Exito = mean(Resultados == 1))

# Crear un gráfico de barras para la proporción de éxito por pierna
ggplot(pierna_data, aes(x = Pierna, y = Proporcion_Exito, fill = Pierna)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Proporción de Éxito entre Piernas",
    x = "Pierna Usada",
    y = "Proporción de Éxito",
    fill = "Pierna"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# --- PARTE 5: DIAGRAMA DE CAJA Y BIGOTES ---
# Convertir la columna 'Tiempo' a formato numérico y filtrar valores NA
datos$Tiempo <- as.numeric(as.character(datos$Tiempo))
datos <- datos %>% filter(!is.na(Tiempo))

# Crear el diagrama de caja y bigotes para los tiempos por pierna y parte del pie
ggplot(datos, aes(x = interaction(Pierna, Parte), y = Tiempo, fill = Pierna)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +  # Resaltar valores atípicos
  labs(
    title = "Diagrama de Caja y Bigotes para Tiempo",
    x = "Combinación de Pierna y Parte del Pie",
    y = "Tiempo",
    fill = "Pierna"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
# Función para convertir columnas de un data frame a numérico (si no lo son)
convertir_a_numerico <- function(data) {
  for (col in names(data)) {
    if (!is.numeric(data[[col]])) {
      data[[col]] <- as.numeric(as.character(data[[col]]))
    }
  }
  return(data)
}

# Frecuencia absoluta para datos enteros
generar_grafico_absoluto_enteros <- function(data, titulo_hoja, columna) {
  data <- data[!is.na(data[[columna]]), ]
  hist_data <- hist(data[[columna]], 
                    breaks = seq(floor(min(data[[columna]]) - 0.5), ceiling(max(data[[columna]]) + 0.5), by = 1), 
                    plot = FALSE)
  
  hist_df <- data.frame(
    mids = hist_data$mids,
    counts = hist_data$counts,
    breaks = hist_data$breaks[-length(hist_data$breaks)],
    width = diff(hist_data$breaks)
  )
  
  ggplot(hist_df, aes(x = mids, y = counts)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black", width = 1) +
    geom_text(aes(label = counts), vjust = -0.5, size = 3.5, color = "black") +
    labs(title = paste("Frecuencia Absoluta -", titulo_hoja), x = columna, y = "Frecuencia Absoluta") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}

# Frecuencia absoluta para datos decimales
# Frecuencia absoluta mejorada para datos decimales
generar_grafico_absoluto_decimales <- function(data, titulo_hoja, columna, binwidth = 0.2) {
  # Eliminar valores NA e infinitos
  data <- data[!is.na(data[[columna]]) & is.finite(data[[columna]]), ]
  
  # Crear histograma con bins ajustados
  ggplot(datos, aes(x = Tiempo)) +
    geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black", alpha = 0.8) +  # Color sólido
    geom_text(stat = "bin", binwidth = 0.2, aes(label = ..count..), 
              vjust = -0.5, size = 3) +
    labs(title = "Frecuencia Absoluta - Tiempo",
         x = "Tiempo",
         y = "Frecuencia Absoluta") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  
}
# Frecuencia relativa para datos enteros
generar_grafico_relativo_enteros <- function(data, titulo_hoja, columna) {
  # Eliminar valores NA de la columna seleccionada
  data <- data[!is.na(data[[columna]]), ]
  
  # Crear el histograma sin graficar
  hist_data <- hist(data[[columna]], 
                    breaks = seq(floor(min(data[[columna]]) - 0.5), ceiling(max(data[[columna]]) + 0.5), by = 1), 
                    plot = FALSE)
  
  # Calcular frecuencias relativas
  frecuencias_relativas <- hist_data$density * diff(hist_data$breaks)
  
  # Crear DataFrame para ggplot2
  hist_df <- data.frame(
    mids = hist_data$mids,                         # Puntos medios de los intervalos
    density = frecuencias_relativas,              # Frecuencia relativa
    breaks = hist_data$breaks[-length(hist_data$breaks)],  # Límites de intervalos
    width = diff(hist_data$breaks)               # Ancho de los intervalos
  )
  
  # Crear el gráfico
  ggplot(hist_df, aes(x = mids, y = density)) +
    geom_bar(stat = "identity", fill = "lightgreen", color = "black", width = 1, alpha = 0.8) +  # Barras verdes
    geom_line(aes(x = mids, y = density), color = "blue", size = 1) +  # Línea de densidad azul
    geom_point(aes(x = mids, y = density), color = "blue", size = 2) +  # Puntos de densidad
    geom_text(aes(label = round(density, 3)), vjust = -0.5, size = 3) +  # Etiquetas de frecuencia
    labs(
      title = paste("Frecuencia Relativa -", titulo_hoja),
      x = columna,
      y = "Frecuencia Relativa"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
}


# Frecuencia relativa para datos decimales (mejorada)
generar_grafico_relativo_decimales <- function(data, titulo_hoja, columna, binwidth = 0.1) {
  # Eliminar valores NA e infinitos
  data <- data[!is.na(data[[columna]]) & is.finite(data[[columna]]), ]
  
  # Crear histograma sin graficar con bins ajustados
  hist_data <- hist(data[[columna]], 
                    breaks = seq(floor(min(data[[columna]])) - (binwidth / 2), 
                                 ceiling(max(data[[columna]])) + (binwidth / 2), 
                                 by = binwidth), 
                    plot = FALSE, freq = FALSE)
  
  # Calcular frecuencias relativas
  frecuencias_relativas <- hist_data$density * diff(hist_data$breaks)
  
  # Crear DataFrame para ggplot2
  hist_df <- data.frame(
    mids = hist_data$mids,            # Puntos medios de los bins
    density = frecuencias_relativas,  # Frecuencia relativa
    breaks = hist_data$breaks[-length(hist_data$breaks)],  # Límites de los bins
    width = diff(hist_data$breaks)    # Ancho de los bins
  )
  
  # Crear gráfico mejorado con ggplot2
  ggplot(hist_df, aes(x = mids, y = density)) +
    geom_bar(stat = "identity", fill = "lightgreen", color = "black", width = binwidth, alpha = 0.8) +  # Barras sólidas
    geom_line(aes(x = mids, y = density), color = "blue", size = 1) +   # Línea de densidad azul
    geom_point(aes(x = mids, y = density), color = "blue", size = 2) +  # Puntos de densidad
    geom_text(aes(label = round(density, 3)), vjust = -0.5, size = 3, color = "black", angle = 45) +  # Etiquetas rotadas
    labs(
      title = paste("Frecuencia Relativa -", titulo_hoja),
      x = columna,
      y = "Frecuencia Relativa"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotar etiquetas del eje X
    )
}

# DataFrame para pierna Dominante
pierna_dominante <- subset(datos2, Tipo.de.Pierna == "Dominante")

# DataFrame para pierna No dominante
pierna_no_dominante <- subset(datos2, Tipo.de.Pierna == "No Dominante")

# DataFrame para Borde externo
parte_borde_externo <- subset(datos2, Parte.del.Pie == "Externo")

# DataFrame para Borde interno
parte_borde_interno <- subset(datos2, Parte.del.Pie == "Interno")

# DataFrame para Punta
parte_punta <- subset(datos2, Parte.del.Pie == "Punta")

# Dominante - Borde externo
dominante_borde_externo <- subset(datos2, Tipo.de.Pierna == "Dominante" & Parte.del.Pie == "Externo")

# Dominante - Borde interno
dominante_borde_interno <- subset(datos2, Tipo.de.Pierna == "Dominante" & Parte.del.Pie == "Interno")

# Dominante - Punta
dominante_punta <- subset(datos2, Tipo.de.Pierna == "Dominante" & Parte.del.Pie == "Punta")

# No dominante - Borde externo
no_dominante_borde_externo <- subset(datos2, Tipo.de.Pierna == "No Dominante" & Parte.del.Pie == "Externo")

# No dominante - Borde interno
no_dominante_borde_interno <- subset(datos2, Tipo.de.Pierna == "No Dominante" & Parte.del.Pie == "Interno")

# No dominante - Punta
no_dominante_punta <- subset(datos2, Tipo.de.Pierna == "No Dominante" & Parte.del.Pie == "Punta")

# Especificar la columna que se analizará 
columna <- "Tiempo"
columna2 <- "Resultados"

# Gráficos de Frecuencia Absoluta (Datos Enteros - datos2)
print(generar_grafico_absoluto_enteros(datos2, "Generales", columna2))
print(generar_grafico_absoluto_enteros(pierna_dominante, "Pierna Dominante", columna2))
print(generar_grafico_absoluto_enteros(pierna_no_dominante, "Pierna No Dominante", columna2))
print(generar_grafico_absoluto_enteros(parte_borde_externo, "Parte Borde Externo", columna2))
print(generar_grafico_absoluto_enteros(parte_borde_interno, "Parte Borde Interno", columna2))
print(generar_grafico_absoluto_enteros(parte_punta, "Parte Punta", columna2))
print(generar_grafico_absoluto_enteros(dominante_borde_externo, "Dominante - Borde Externo", columna2))
print(generar_grafico_absoluto_enteros(dominante_borde_interno, "Dominante - Borde Interno", columna2))
print(generar_grafico_absoluto_enteros(dominante_punta, "Dominante - Punta", columna2))
print(generar_grafico_absoluto_enteros(no_dominante_borde_externo, "No Dominante - Borde Externo", columna2))
print(generar_grafico_absoluto_enteros(no_dominante_borde_interno, "No Dominante - Borde Interno", columna2))
print(generar_grafico_absoluto_enteros(no_dominante_punta, "No Dominante - Punta", columna2))

# Gráfico de Frecuencia Absoluta para Datos Decimales (Tiempo)
print(generar_grafico_absoluto_decimales(datos, "Tiempo", columna))


# Gráficos de Frecuencia Relativa (Datos Enteros - datos2)
print(generar_grafico_relativo_enteros(datos2, "Generales", columna2))
print(generar_grafico_relativo_enteros(pierna_dominante, "Pierna Dominante", columna2))
print(generar_grafico_relativo_enteros(pierna_no_dominante, "Pierna No Dominante", columna2))
print(generar_grafico_relativo_enteros(parte_borde_externo, "Parte Borde Externo", columna2))
print(generar_grafico_relativo_enteros(parte_borde_interno, "Parte Borde Interno", columna2))
print(generar_grafico_relativo_enteros(parte_punta, "Parte Punta", columna2))
print(generar_grafico_relativo_enteros(dominante_borde_externo, "Dominante - Borde Externo", columna2))
print(generar_grafico_relativo_enteros(dominante_borde_interno, "Dominante - Borde Interno", columna2))
print(generar_grafico_relativo_enteros(dominante_punta, "Dominante - Punta", columna2))
print(generar_grafico_relativo_enteros(no_dominante_borde_externo, "No Dominante - Borde Externo", columna2))
print(generar_grafico_relativo_enteros(no_dominante_borde_interno, "No Dominante - Borde Interno", columna2))
print(generar_grafico_relativo_enteros(no_dominante_punta, "No Dominante - Punta", columna2))

# Gráfico de Frecuencia Relativa para Datos Decimales (Tiempo)
print(generar_grafico_relativo_decimales(datos, "Tiempo", columna))
# Convertir columnas a los tipos adecuados
datos2 <- datos2 %>%
  mutate(
    Resultados = as.numeric(Resultados),  # Convertir Resultados a numérico
    Parte.del.Pie = as.factor(Parte.del.Pie)  # Convertir Parte.del.Pie a factor
  )

# Función para generar gráficos de línea
generar_grafico_linea <- function(data, categoria_col, categoria_valor, result_col) {
  # Filtrar datos por categoría
  data_filtrada <- data %>% filter(!!sym(categoria_col) == categoria_valor)
  
  # Verificar si hay datos suficientes después del filtrado
  if (nrow(data_filtrada) == 0) {
    warning(paste("No hay datos disponibles para la categoría:", categoria_valor))
    return(NULL)
  }
  
  # Calcular la cantidad de resultados por valor
  datos_agrupados <- data_filtrada %>%
    group_by(!!sym(result_col)) %>%
    summarise(Conteo = n(), .groups = "drop")
  
  # Crear el gráfico de línea
  ggplot(datos_agrupados, aes(x = !!sym(result_col), y = Conteo)) +
    geom_line(color = "blue", size = 1) +
    geom_point(color = "red", size = 2) +
    labs(
      title = paste("Distribución de Resultados -", categoria_valor),
      x = "Resultados",
      y = "Frecuencia"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))  # Centrar el título
}

# Generar gráficos para cada categoría en Parte.del.Pie
categorias <- unique(datos2$Parte.del.Pie)
graficos <- list()

for (categoria in categorias) {
  grafico <- generar_grafico_linea(
    data = datos2,
    categoria_col = "Parte.del.Pie",
    categoria_valor = categoria,
    result_col = "Resultados"
  )
  if (!is.null(grafico)) {
    graficos[[categoria]] <- grafico
    print(grafico)
  }
}
#-----------------------------------

# Filtrar los datos para los éxitos (Resultados mayores a 0)
datos_exitos <- datos2 %>%
  filter(Resultados > 0)

# Calcular la cantidad de éxitos por cada valor en "Resultados"
datos_agrupados <- datos_exitos %>%
  group_by(Resultados) %>%
  summarise(Conteo = n(), .groups = "drop")

# Crear un gráfico general de los éxitos
grafico_exitos <- ggplot(datos_agrupados, aes(x = Resultados, y = Conteo)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "Distribución General de Éxitos",
    x = "Resultados (Éxitos)",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  # Centrar el título
print(grafico_exitos)

# Función para cargar y añadir columnas 'Pierna' y 'Parte'
leer_datos <- function(sheet_name, pierna, parte) {
  read_excel(path, sheet = sheet_name) %>%
    mutate(Pierna = pierna, Parte = parte)
}

# Cargar las hojas del Excel y agregar las columnas correspondientes
punta_no_dominante <- leer_datos("Punta - No Dominante", "No Dominante", "Punta")
punta_dominante <- leer_datos("Punta - Dominante", "Dominante", "Punta")
interno_no_dominante <- leer_datos("Interno - No Dominante", "No Dominante", "Interno")
interno_dominante <- leer_datos("Interno - Dominante", "Dominante", "Interno")
externo_no_dominante <- leer_datos("Externo - No Dominante", "No Dominante", "Externo")
externo_dominante <- leer_datos("Externo - Dominante", "Dominante", "Externo")

# Combinar todas las hojas en un solo dataframe
datos <- bind_rows(
  punta_no_dominante, punta_dominante, 
  interno_no_dominante, interno_dominante, 
  externo_no_dominante, externo_dominante
)

# Convertir y filtrar la columna 'Resultados'
datos$Resultados <- as.numeric(as.character(datos$Resultados))
datos <- datos %>% filter(!is.na(Resultados))

# Validar que los resultados son binarios (0 o 1)
if (!all(datos$Resultados %in% c(0, 1))) {
  stop("La columna 'Resultados' tiene valores fuera de 0 y 1. Verifica los datos.")
}

# Cálculo de frecuencias observadas
f_exito_observada <- mean(datos$Resultados == 1, na.rm = TRUE)
f_fracaso_observada <- 1 - f_exito_observada

# Probabilidades teóricas
p_exito_teorico <- 0.515
p_fracaso_teorico <- 1 - p_exito_teorico

# Calcular frecuencias absolutas y guardarlas en un DataFrame
frecuencias_absolutas_observadas <- data.frame(
  Resultado = c("Éxito", "Fracaso"),
  Frecuencia_Absoluta = c(f_exito_observada * n,
                          f_fracaso_observada * n)
)

# Crear dataframes de frecuencias observadas y teóricas
frecuencias_relativas_observadas <- data.frame(
  Resultado = c("Éxito", "Fracaso"),
  Probabilidad = c(f_exito_observada, f_fracaso_observada)
)
frecuencias_relativas_teoricas <- data.frame(
  Resultado = c("Éxito", "Fracaso"),
  Probabilidad = c(p_exito_teorico, p_fracaso_teorico)
)

# Crear dataframe combinado para el gráfico
frecuencias_relativas_combinadas <- data.frame(
  Resultado = c("Éxito", "Fracaso", "Éxito", "Fracaso"),
  Frecuencia = c(f_exito_observada, f_fracaso_observada, p_exito_teorico, p_fracaso_teorico),
  Tipo = c("Observada", "Observada", "Teórica", "Teórica")
)

# Generar gráfico de barras
ggplot(frecuencias_relativas_combinadas, aes(x = Resultado, y = Frecuencia, fill = Tipo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Comparación de Resultados Teóricos y Observados",
    x = "Resultado",
    y = "Probabilidad",
    fill = "Frecuencia"
  ) +
  theme_minimal()

# Crear datasets combinados
externo <- bind_rows(externo_dominante, externo_no_dominante)
interno <- bind_rows(interno_dominante, interno_no_dominante)
punta <- bind_rows(punta_dominante, punta_no_dominante)
no_dominante <- bind_rows(punta_no_dominante, interno_no_dominante, externo_no_dominante)
dominante <- bind_rows(punta_dominante, interno_dominante, externo_dominante)

# Prueba de Hipótesis 1: Prueba de proporciones
x <- 576
n <- 1500
p0 <- 0.515
resultado_prueba_proporciones <- prop.test(x = x, n = n, p = p0, correct = TRUE)
print(resultado_prueba_proporciones)

# Prueba de Hipótesis 2: Chi-cuadrado
exitos_dominante <- sum(dominante$Resultados == 1, na.rm = TRUE)
exitos_no_dominante <- sum(no_dominante$Resultados == 1, na.rm = TRUE)
fracasos_dominante <- 750 - exitos_dominante
fracasos_no_dominante <- 750 - exitos_no_dominante

tabla_resultados <- data.frame(
  Categoria = c("Dominante", "No Dominante"),
  Exitos = c(exitos_dominante, exitos_no_dominante),
  Fracasos = c(fracasos_dominante, fracasos_no_dominante)
)
rownames(tabla_resultados) <- tabla_resultados$Categoria
tabla_resultados <- tabla_resultados[, -1]
prueba_chi <- chisq.test(as.matrix(tabla_resultados))
print(prueba_chi)

# Prueba de Hipótesis 3: Mann-Whitney
datos_RT <- datos[, !(colnames(datos) %in% c("Pierna", "Parte"))]
datos_RT$Tiempo <- as.numeric(datos_RT$Tiempo)
datos_RT$Resultados <- as.factor(datos_RT$Resultados)
prueba_mann_whitney <- wilcox.test(Tiempo ~ Resultados, data = datos_RT)
print(prueba_mann_whitney)

# Gráfico Boxplot
ggplot(datos_RT, aes(x = Resultados, y = Tiempo, fill = Resultados)) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightblue", "lightcoral")) +
  labs(
    title = "Distribución de Tiempo por Resultado (Éxito vs. Fracaso)",
    x = "Resultado",
    y = "Tiempo"
  ) +
  theme_minimal()

# Prueba de Bondad de ajuste Chi-cuadrado
# Definir parámetros
n2 <- 5  # Número de ensayos (máximo valor de 'Resultados')
p_teorico <- 0.515  # Probabilidad teórica de éxito

# Calcular frecuencias observadas
frecuencias_obs <- table(datos2$Resultados)

# Calcular frecuencias esperadas bajo la distribución binomial con p = 0.515
frecuencias_esp <- dbinom(0:n2, size = n2, prob = p_teorico) * sum(frecuencias_obs)

# Verificar que las frecuencias observadas y esperadas tengan la misma longitud
if (length(frecuencias_obs) != length(frecuencias_esp)) {
  stop("Error: Las frecuencias observadas y esperadas no tienen la misma longitud.")
}

# Prueba de Chi-cuadrado
chi_test <- chisq.test(frecuencias_obs, p = frecuencias_esp / sum(frecuencias_esp), rescale.p = TRUE)

# Mostrar resultado de la prueba
cat("\nPrueba de Bondad de Ajuste a la Distribución Binomial\n")
print(chi_test)

# Gráficos Q-Q Antiguo (Lo dejo aqui porque no se logro quitar en el informe)
n_ensayos <- 5
datos_observados <- c(rep(5, 576), rep(0, 924))
cuantiles_teoricos <- qbinom(ppoints(length(datos_observados)), size = n_ensayos, prob = p_exito_teorico)
qqplot(cuantiles_teoricos, datos_observados, main = "Q-Q Plot para Distribución Binomial", 
       xlab = "Cuantiles Teóricos (Binomial)", ylab = "Datos Observados")
abline(0, 1, col = "red", lwd = 2)

n_ensayos <- 5
# Probabilidad observada
p_exito_observado <- 0.384

datos_observados <- c(rep(5, 576), rep(0, 924))

# Calcular los cuantiles teóricos con la probabilidad observada
cuantiles_teoricos <- qbinom(ppoints(length(datos_observados)), size = n_ensayos, prob = p_exito_observado)

# Generar el Q-Q Plot
qqplot(cuantiles_teoricos, datos_observados, 
       main = "Q-Q Plot para Distribución Binomial con Probabilidad Observada", 
       xlab = "Cuantiles Teóricos (Binomial)", 
       ylab = "Datos Observados")
abline(0, 1, col = "blue", lwd = 2)

#Grafico Q-Q nuevo
# Parámetros de la distribución binomial
n2 <- 5          # Número de ensayos
p_teorico <- 0.515  # Probabilidad teórica de éxito

# Generar una muestra aleatoria de la distribución binomial con p = 0.515
set.seed(123)  # Asegura reproducibilidad
muestra_binomial <- rbinom(n = length(datos2$Resultados), size = n2, prob = p_teorico)

# QQ-Plot: Comparar los cuantiles de los datos con la distribución binomial teórica
qqplot(muestra_binomial, datos2$Resultados,
       main = "QQ-Plot: Datos vs Distribución Binomial (p = 0.515)",
       xlab = "Cuantiles Teóricos (Binomial)",
       ylab = "Cuantiles de los Datos",
       pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)



# Prueba de Kolmogorov-Smirnov
datos_normalizados <- scale(datos_RT$Tiempo)
ks_test <- ks.test(datos_normalizados, "pnorm", mean = 0, sd = 1)
print(ks_test)
qqnorm(datos_normalizados)
qqline(datos_normalizados, col = "red")

# Prueba de Hipótesis 6: Chi-cuadrado por partes del pie
exitos_externo <- sum(externo$Resultados == 1, na.rm = TRUE)
fracasos_externo <- sum(externo$Resultados == 0, na.rm = TRUE)
exitos_interno <- sum(interno$Resultados == 1, na.rm = TRUE)
fracasos_interno <- sum(interno$Resultados == 0, na.rm = TRUE)
exitos_punta <- sum(punta$Resultados == 1, na.rm = TRUE)
fracasos_punta <- sum(punta$Resultados == 0, na.rm = TRUE)

tabla_pie <- data.frame(
  Parte = c("Externo", "Interno", "Punta"),
  Exitos = c(exitos_externo, exitos_interno, exitos_punta),
  Fracasos = c(fracasos_externo, fracasos_interno, fracasos_punta)
)

rownames(tabla_pie) <- tabla_pie$Parte
tabla_prueba <- tabla_pie[, -1]
prueba_chi_pie <- chisq.test(as.matrix(tabla_prueba))
print(prueba_chi_pie)

# Prueba de Hipótesis 7: Mann-Whitney
tiempo_dominante <- as.numeric(dominante$Tiempo)
tiempo_no_dominante <- as.numeric(no_dominante$Tiempo)
prueba_tiempos <- wilcox.test(tiempo_dominante, tiempo_no_dominante, exact = FALSE)
print(prueba_tiempos)

# Crear un data frame llamado datos_tiempo_piernas
datos_tiempo_piernas <- data.frame(
  Dominante = tiempo_dominante,
  No_Dominante = tiempo_no_dominante
)

# Convertir el data frame a formato largo con el nombre datos_largos_pierna
datos_largos_pierna <- datos_tiempo_piernas %>%
  pivot_longer(cols = everything(), names_to = "Grupo", values_to = "Tiempo")

# Crear el boxplot con ggplot2
ggplot(datos_largos_pierna, aes(x = Grupo, y = Tiempo, fill = Grupo)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Dominante" = "lightblue", 
                               "No_Dominante" = "lightgreen")) +
  labs(
    title = "Comparación de Tiempos por Grupo",
    y = "Tiempo",
    x = "Grupo"
  ) +
  theme_minimal()

# Prueba de Hipótesis 8: Kruskal-Wallis
# Asegurarse de que los vectores sean numéricos
tiempo_interno <- as.numeric(interno$Tiempo)
tiempo_externo <- as.numeric(externo$Tiempo)
tiempo_punta <- as.numeric(punta$Tiempo)

# Crear el data frame en formato largo directamente
datos_tiempo <- data.frame(
  Tiempo = c(tiempo_interno, tiempo_externo, tiempo_punta),
  Grupo = c(rep("Interno", length(tiempo_interno)), 
            rep("Externo", length(tiempo_externo)), 
            rep("Punta", length(tiempo_punta)))
)

# Prueba de Kruskal-Wallis
prueba_kruskal <- kruskal.test(Tiempo ~ Grupo, data = datos_tiempo)
print(prueba_kruskal)

datos_tiempo$Grupo <- factor(datos_tiempo$Grupo, levels = c("Interno", "Externo", "Punta"))

# Crear el boxplot con ggplot2
ggplot(datos_tiempo, aes(x = Grupo, y = Tiempo, fill = Grupo)) +
  geom_boxplot() +
  scale_fill_manual(values = c("Interno" = "skyblue", 
                               "Externo" = "lightgreen", 
                               "Punta" = "salmon")) +
  labs(
    title = "Comparación de Tiempos por Grupo",
    y = "Tiempo",
    x = "Grupo"
  ) +
  theme_minimal()
# Normalización de los datos para la prueba Kolmogorov-Smirnov
resultados_norm <- scale(datos2$Resultados) # Escalar y centrar los datos

# Prueba de Kolmogorov-Smirnov para distribución normal en datos normalizados
ks_test_normal <- ks.test(resultados_norm, "pnorm") # Prueba KS con distribución normal estándar

# Resultados
cat("\nPrueba de Kolmogorov-Smirnov para distribución normal (Datos Normalizados):\n")
print(ks_test_normal)

# Normalizar los datos (media 0, desviación estándar 1)
datos_normalizados2 <- scale(datos2$Resultados)

# QQ-Plot con datos normalizados
qqnorm(datos_normalizados2,
       main = "QQ-Plot: Datos Normalizados vs Distribución Normal",
       xlab = "Cuantiles Teóricos (Normal)",
       ylab = "Cuantiles de los Datos Normalizados",
       col = "green", pch = 19)
qqline(datos_normalizados, col = "red", lwd = 2)

# Comparación de promedios entre pierna dominante y no dominante
ttest_pierna <- t.test(pierna_dominante$Resultados, pierna_no_dominante$Resultados, var.equal = FALSE)

cat("\nPrueba T para comparación de promedios entre pierna dominante y no dominante:\n")
print(ttest_pierna)

# Crear el boxplot
ggplot(datos2, aes(x = Tipo.de.Pierna, y = Resultados, fill = Tipo.de.Pierna)) +
  geom_boxplot() +
  labs(title = "Comparación de Resultados por Tipo de Pierna",
       x = "Tipo de Pierna",
       y = "Resultados") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set2")

# Realizar ANOVA
anova_model <- aov(Resultados ~ Parte.del.Pie, data = datos2)
anova_summary <- summary(anova_model)

cat("\nPrueba ANOVA para comparación de promedios entre partes del pie:\n")
print(anova_summary)

ggplot(datos2, aes(x = Parte.del.Pie, y = Resultados, fill = Parte.del.Pie)) +
  geom_boxplot() +
  labs(title = "Comparación de Resultados por Parte del Pie",
       x = "Parte del Pie",
       y = "Resultados") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette = "Set2")
