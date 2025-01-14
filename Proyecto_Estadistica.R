# Instalar y cargar las bibliotecas necesarias
if (!require("readxl")) install.packages("readxl") # Lectura de archivos Excel
if (!require("ggplot2")) install.packages("ggplot2")   # Creación de gráficos
if (!require("dplyr")) install.packages("dplyr")  # Manipulación y transformación de datos
if (!require("tdyr")) install.packages("tdyr")
# Carga de librerías
library(readxl)
library(ggplot2)
library(dplyr)
library(tdyr)

# Ruta al archivo Excel
path <- "C:/Users/ASUS VIVOBOOK PRO/Downloads/PruebaDatos.xlsx"

# --- CARGA Y COMBINACIÓN DE DATOS ---

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
  labs(
    title = "Proporción de Éxito por Parte del Pie",
    x = "Parte del Pie",
    y = "Proporción de Éxito"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
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

# Función para generar gráficos de frecuencia absoluta con ggplot2 (sin línea azul)
generar_grafico_absoluto <- function(data, titulo_hoja, columna) {
  # Eliminar valores NA
  data <- data[!is.na(data[[columna]]), ]
  
  # Crear histograma sin graficar
  hist_data <- hist(data[[columna]], plot = FALSE)
  
  # Crear data frame para ggplot2
  hist_df <- data.frame(
    mids = hist_data$mids,
    counts = hist_data$counts,
    breaks = hist_data$breaks[-length(hist_data$breaks)],
    width = diff(hist_data$breaks)
  )
  
  # Crear gráfico
  ggplot(hist_df, aes(x = mids, y = counts)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black", width = hist_df$width[1]) +
    geom_text(aes(label = counts), vjust = -0.5, size = 3.5, color = "black") +
    labs(
      title = paste("Frecuencia Absoluta -", titulo_hoja),
      x = columna,
      y = "Frecuencia Absoluta"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))  # Centrar el título
}

# Función para generar gráficos de frecuencia relativa con ggplot2
generar_grafico_relativo <- function(data, titulo_hoja, columna) {
  # Eliminar valores NA
  data <- data[!is.na(data[[columna]]), ]
  
  # Crear histograma sin graficar
  hist_data <- hist(data[[columna]], plot = FALSE, freq = FALSE)
  
  # Calcular frecuencias relativas
  frecuencias_relativas <- hist_data$density * diff(hist_data$breaks)
  suma_areas <- sum(frecuencias_relativas)
  cat("La suma de las frecuencias relativas (debe ser 1):", suma_areas, "\n")
  
  # Crear data frame para ggplot2
  hist_df <- data.frame(
    mids = hist_data$mids,
    density = frecuencias_relativas,
    breaks = hist_data$breaks[-length(hist_data$breaks)],
    width = diff(hist_data$breaks)
  )
  
  # Crear gráfico
  ggplot(hist_df, aes(x = mids, y = density)) +
    geom_bar(stat = "identity", fill = "lightgreen", color = "black", width = hist_df$width[1]) +
    geom_line(aes(x = mids, y = density), color = "blue", size = 1) +
    geom_point(aes(x = mids, y = density), color = "blue", size = 2) +
    geom_text(aes(label = round(density, 3)), vjust = -0.5, size = 3.5, color = "black") +
    labs(
      title = paste("Frecuencia Relativa -", titulo_hoja),
      x = columna,
      y = "Frecuencia Relativa"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))  # Centrar el título
}

# Cargar y convertir los datos de cada hoja del archivo
externo_dominante <- convertir_a_numerico(read_excel(path, sheet = "Externo - Dominante"))
externo_no_dominante <- convertir_a_numerico(read_excel(path, sheet = "Externo - No Dominante"))
interno_dominante <- convertir_a_numerico(read_excel(path, sheet = "Interno - Dominante"))
interno_no_dominante <- convertir_a_numerico(read_excel(path, sheet = "Interno - No Dominante"))
punta_dominante <- convertir_a_numerico(read_excel(path, sheet = "Punta - Dominante"))
punta_no_dominante <- convertir_a_numerico(read_excel(path, sheet = "Punta - No Dominante"))

# Especificar la columna que se analizará en cada hoja
columna <- "Tiempo"

# Generar gráficos de frecuencia absoluta
print(generar_grafico_absoluto(externo_dominante, "Externo - Dominante", columna))
print(generar_grafico_absoluto(externo_no_dominante, "Externo - No Dominante", columna))
print(generar_grafico_absoluto(interno_dominante, "Interno - Dominante", columna))
print(generar_grafico_absoluto(interno_no_dominante, "Interno - No Dominante", columna))
print(generar_grafico_absoluto(punta_dominante, "Punta - Dominante", columna))
print(generar_grafico_absoluto(punta_no_dominante, "Punta - No Dominante", columna))

# Generar gráficos de frecuencia relativa
print(generar_grafico_relativo(externo_dominante, "Externo - Dominante", columna))
print(generar_grafico_relativo(externo_no_dominante, "Externo - No Dominante", columna))
print(generar_grafico_relativo(interno_dominante, "Interno - Dominante", columna))
print(generar_grafico_relativo(interno_no_dominante, "Interno - No Dominante", columna))
print(generar_grafico_relativo(punta_dominante, "Punta - Dominante", columna))
print(generar_grafico_relativo(punta_no_dominante, "Punta - No Dominante", columna))
# Tamaño del intervalo para agrupar los datos
interval_size <- 0.5  # Intervalo de agrupación en segundos

# Función para procesar los datos y generar el gráfico
generar_grafico <- function(sheet_name) {
  # Leer los datos desde la hoja especificada
  data <- read_excel(path, sheet = sheet_name)
  
  # Convertir las columnas a numérico y manejar posibles valores no válidos
  data <- data %>%
    mutate(
      Tiempo = as.numeric(Tiempo),  # Convertir columna "Tiempo" a numérico
      Resultados = as.numeric(Resultados)  # Convertir columna "Resultados" a numérico
    ) %>%
    filter(!is.na(Tiempo), !is.na(Resultados))  # Eliminar filas con valores NA
  
  # Agrupar los datos en intervalos de tiempo y calcular el promedio de aciertos
  data <- data %>%
    mutate(Intervalo = floor(Tiempo / interval_size) * interval_size) %>%  # Crear intervalos de tiempo
    group_by(Intervalo) %>%  # Agrupar por intervalos
    summarise(Promedio_Aciertos = mean(Resultados, na.rm = TRUE))  # Calcular promedio de "Resultados"
  
  # Crear y devolver el gráfico
  ggplot(data, aes(x = Intervalo, y = Promedio_Aciertos)) +
    geom_line(color = "blue") +  # Línea azul para representar la tendencia
    geom_point(color = "red") +  # Puntos rojos para resaltar los valores individuales
    labs(
      title = paste("Evolución del promedio de aciertos\n(", sheet_name, ")", sep = ""),  # Título del gráfico
      x = "Intervalo de Tiempo (s)",  # Etiqueta del eje X
      y = "Promedio de aciertos"  # Etiqueta del eje Y
    ) +
    theme_minimal()  # Tema minimalista para un diseño limpio
}

# Generar gráficos para cada hoja del archivo Excel
grafico_punta_no_dominante <- generar_grafico("Punta - No Dominante")
grafico_punta_dominante <- generar_grafico("Punta - Dominante")
grafico_interno_no_dominante <- generar_grafico("Interno - No Dominante")
grafico_interno_dominante <- generar_grafico("Interno - Dominante")
grafico_externo_no_dominante <- generar_grafico("Externo - No Dominante")
grafico_externo_dominante <- generar_grafico("Externo - Dominante")

# Mostrar los gráficos en la consola
print(grafico_punta_no_dominante)
print(grafico_punta_dominante)
print(grafico_interno_no_dominante)
print(grafico_interno_dominante)
print(grafico_externo_no_dominante)
print(grafico_externo_dominante)
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
  labs(
    title = "Distribución de Tiempo por Resultado (Éxito vs. Fracaso)",
    x = "Resultado",
    y = "Tiempo"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightcoral"))

# Prueba de Bondad de ajuste Chi-cuadrado
observadas <- c(n * f_exito_observada, n * (1 - f_exito_observada))
esperadas <- c(n * p_exito_teorico, n * (1 - p_exito_teorico))
prueba_chi_ajuste <- chisq.test(x = observadas, p = esperadas / sum(esperadas))
print(prueba_chi_ajuste)

# Gráfico Q-Q
n_ensayos <- 5
datos_observados <- c(rep(5, 576), rep(0, 924))
cuantiles_teoricos <- qbinom(ppoints(length(datos_observados)), size = n_ensayos, prob = p_exito_teorico)
qqplot(cuantiles_teoricos, datos_observados, main = "Q-Q Plot para Distribución Binomial", 
       xlab = "Cuantiles Teóricos (Binomial)", ylab = "Datos Observados")
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
tiempo_interno <- as.numeric(interno$Tiempo)
tiempo_externo <- as.numeric(externo$Tiempo)
tiempo_punta <- as.numeric(punta$Tiempo)

datos_tiempo <- data.frame(
  Tiempo = c(tiempo_interno, tiempo_externo, tiempo_punta),
  Grupo = c(rep("Interno", length(tiempo_interno)), 
            rep("Externo", length(tiempo_externo)), 
            rep("Punta", length(tiempo_punta)))
)

prueba_kruskal <- kruskal.test(Tiempo ~ Grupo, data = datos_tiempo)
print(prueba_kruskal)

library(ggplot2)
library(dplyr)
library(tidyr)

# Suponiendo que tiempo_interno, tiempo_externo y tiempo_punta son vectores numéricos
# Crear un data frame para ggplot
datos_tiempo_pie <- data.frame(
  Interno = tiempo_interno,
  Externo = tiempo_externo,
  Punta = tiempo_punta
)

# Convertir el data frame a formato largo
datos_largos_pie <- datos_tiempo_pie %>%
  pivot_longer(cols = everything(), names_to = "Grupo", values_to = "Tiempo")

# Crear el boxplot con ggplot2
ggplot(datos_largos_pie, aes(x = Grupo, y = Tiempo, fill = Grupo)) +
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