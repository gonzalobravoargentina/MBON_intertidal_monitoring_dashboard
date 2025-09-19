#----------------------------------------#
# MATRIZ DE CONFUSION MODELOS VIEJO Y NUEVO
#----------------------------------------#

# Cargar librerías
library(readr)
library(dplyr)
library(caret)
library(ggplot2)
library(forcats)

# Ruta de los datos
Data <- "DATA"

# Cargar datos
annotations_new <- read_csv(file.path(Data, "annotations_MONTEMAR_new.csv"))
annotations_old <- read_csv(file.path(Data, "annotations_MONTEMAR_old.csv"))

# Unificar niveles de categorías (etiquetas y predicciones)
all_levels <- union(
  unique(c(annotations_old$`Label code`, annotations_old$`Machine suggestion 1`)),
  unique(c(annotations_new$`Label code`, annotations_old$`Label code`))
)

# Aplicar niveles unificados a factores para ambos datasets
annotations_old <- annotations_old %>%
  mutate(
    `Label code` = factor(`Label code`, levels = all_levels),
    `Machine suggestion 1` = factor(`Machine suggestion 1`, levels = all_levels)
  )

annotations_new <- annotations_new %>%
  mutate(
    `Label code` = factor(`Label code`, levels = all_levels)
  )

# MATRIZ DE CONFUSIÓN – MODELO VIEJO (Argentina)
cm_old <- confusionMatrix(
  annotations_old$`Machine suggestion 1`,
  annotations_old$`Label code`
)
confusion_df_old <- as.data.frame(cm_old$table)

# MATRIZ DE CONFUSIÓN – MODELO NUEVO (Chile)
# Compara las predicciones del nuevo modelo con las etiquetas humanas originales
cm_new <- confusionMatrix(
  annotations_new$`Label code`,  # Predicción del modelo nuevo
  annotations_old$`Label code`   # Etiqueta humana original
)
confusion_df_new <- as.data.frame(cm_new$table)

# VISUALIZACIÓN – MODELO VIEJO
ggplot(confusion_df_old, aes(x = Prediction, y = fct_rev(Reference), fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#deebf7", high = "#08306b") +
  geom_text(aes(label = Freq), color = "white", size = 3) +
  theme_minimal() +
  labs(
    title = "Matriz de Confusión - Modelo Argentina",
    x = "IA", y = "Humano", fill = "Frecuencia"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# VISUALIZACIÓN – MODELO NUEVO
ggplot(confusion_df_new, aes(x = Prediction, y = fct_rev(Reference), fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#deebf7", high = "#BF4627") +
  geom_text(aes(label = Freq), color = "white", size = 3) +
  theme_minimal() +
  labs(
    title = "Matriz de Confusión - Modelo Chile",
    x = "IA", y = "Humano", fill = "Frecuencia"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# OPCIONAL: Mostrar métricas generales
print(cm_old$overall)  # Accuracy, Kappa del modelo viejo
print(cm_new$overall)  # Accuracy, Kappa del modelo nuevo

# Extraer métricas promedio para cada modelo
metricas_old <- data.frame(
  Modelo = "Viejo",
  Accuracy = cm_old$overall["Accuracy"],
  Sensibilidad = mean(cm_old$byClass[,"Sensitivity"], na.rm = TRUE),
  Precision = mean(cm_old$byClass[,"Precision"], na.rm = TRUE),
  F1 = mean(cm_old$byClass[,"F1"], na.rm = TRUE)
)

metricas_new <- data.frame(
  Modelo = "Nuevo",
  Accuracy = cm_new$overall["Accuracy"],
  Sensibilidad = mean(cm_new$byClass[,"Sensitivity"], na.rm = TRUE),
  Precision = mean(cm_new$byClass[,"Precision"], na.rm = TRUE),
  F1 = mean(cm_new$byClass[,"F1"], na.rm = TRUE)
)

# Unir resultados en una sola tabla
metricas_comparadas <- bind_rows(metricas_old, metricas_new)

# Ver tabla resumen
print(metricas_comparadas)



# Crear una matriz de confusión  con valores absolutos
conf_matrix_completa <- cm_new$table

#Usar clustering para agrupar categorías confusas
#Puedes agrupar clases que tienden a confundirse en la matriz de confusión usando clustering jerárquico.
# Filtrar filas con suma > 0
valid_rows <- rowSums(conf_matrix_completa) > 0
valid_cols <- colSums(conf_matrix_completa) > 0

# Filtrar matriz
conf_matrix_filtrada <- conf_matrix_completa[valid_rows, valid_cols]

# Normalizar por filas
conf_matrix_prop <- prop.table(as.matrix(conf_matrix_filtrada), margin = 1)

library(pheatmap)
# Volver a intentar el pheatmap
pheatmap(
  conf_matrix_prop,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  display_numbers = TRUE,
  number_format = "%.2f",
  main = "Clustering de Categorías Confusas (Ambos modelos)"
)



#Gráfico comparativo de métricas por clase
# Extraer sensibilidad por clase y reorganizar en data frame largo
# Extraer sensibilidad por clase y reorganizar en data frame largo
sensibilidades <- data.frame(
  Categoria = rownames(cm_old$byClass),
  Sensibilidad_viejo = cm_old$byClass[, "Sensitivity"],
  Sensibilidad_nuevo = cm_new$byClass[, "Sensitivity"]
)

# Convertir a formato largo para ggplot
library(tidyr)
library(forcats)

sensibilidades_long <- sensibilidades %>%
  pivot_longer(
    cols = starts_with("Sensibilidad"),
    names_to = "Modelo",
    values_to = "Sensibilidad"
  ) %>%
  mutate(
    Modelo = case_when(
      Modelo == "Sensibilidad_viejo" ~ "Modelo Argentina",
      Modelo == "Sensibilidad_nuevo" ~ "Modelo Chile"
    )
  )

# Filtrar NA antes del gráfico
sensibilidades_long_filtrado <- sensibilidades_long %>%
  filter(!is.na(Sensibilidad))

# Graficar
library(ggplot2)
ggplot(sensibilidades_long_filtrado, aes(x = Sensibilidad, y = fct_reorder(Categoria, Sensibilidad), fill = Modelo)) +
  geom_col(position = "dodge") +
  labs(
    title = "Comparación de Sensibilidad por Categoría",
    x = "Sensibilidad",
    y = "Categoría"
  ) +
  scale_fill_manual(values = c("Modelo Argentina" = "#1f77b4", "Modelo Chile" = "#ff7f0e")) +
  theme_minimal()


library(dplyr)
# Convertir la matriz a un data.frame
confusion_long <- as.data.frame(as.table(conf_matrix_prop))

# Renombrar columnas para claridad
colnames(confusion_long) <- c("Referencia", "Prediccion", "Proporcion")

confusions_only <- confusion_long %>%
  mutate(
    Referencia = as.character(Referencia),
    Prediccion = as.character(Prediccion)
  ) %>%
  filter(Referencia != Prediccion) %>%
  arrange(desc(Proporcion))  # Ordenar de mayor a menor

# Mostrar las 10 confusiones más altas
head(confusions_only, 10)

#----------------------------------------#
# INDICES DIVERSIDAD-------
#----------------------------------------#
# Cargar las bibliotecas necesarias
library(dplyr)
library(ggplot2)
library(vegan)        # Para índices de diversidad
library(tidyr)
library(reshape2)
library(broom)        # Para obtener el resumen de los modelos


# Partimos del dataset annotations_new


# Creamos dos datasets: uno para IA y otro para Humano
ia_data <- annotations_new %>%
  group_by(Name, Label = `Label code`) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(source = "IA")

humano_data <- annotations_old %>%
  group_by(Name, Label = `Label code`) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(source = "Humano")

# Unir ambos datasets
data_combined <- bind_rows(ia_data, humano_data)


# Crear matriz especie x muestra


# Convertir a formato wide: filas = fotos, columnas = especies, valores = abundancias
abund_matrix <- data_combined %>%
  pivot_wider(names_from = Label, values_from = count, values_fill = 0)

# Guardar columnas de metadatos (foto y fuente)
metadata <- abund_matrix %>% select(Name, source)

# Matriz solo de abundancias
matriz_abundancias <- abund_matrix %>% select(-Name, -source)


# Calcular índices de diversidad


# Riqueza (número de especies con abundancia > 0)
S <- specnumber(matriz_abundancias)

# Índice de Shannon
shannon <- diversity(matriz_abundancias, index = "shannon")

# Índice de Simpson (probabilidad de dominancia, aquí usamos 1 - D para diversidad)
simpson <- diversity(matriz_abundancias, index = "simpson")


# Crear dataframe con todos los índices
indices <- data.frame(
  Name = metadata$Name,
  Source = metadata$source,
  S = S,
  Shannon = shannon,
  Simpson = simpson
)


# Graficar Boxplots por índice
# Pasar a formato largo para graficar fácilmente
indices_long <- indices %>%
  pivot_longer(cols = c(S, Shannon, Simpson), names_to = "Indice", values_to = "Valor")

ggplot(indices_long, aes(x = Source, y = Valor, fill = Source)) +
  geom_boxplot(alpha = 0.8) +
  facet_wrap(~Indice, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Comparación de índices de diversidad por foto",
    x = "Fuente", y = "Valor del índice"
  ) +
  scale_fill_manual(values = c("Humano" = "#1b9e77", "IA" = "#d95f02")) +
  theme(legend.position = "none")


# Pruebas estadísticas por índice (IA vs Humano) -- GLM

# Crear lista de modelos GLM por índice
modelos_glm <- indices_long %>%
  group_by(Indice) %>%
  group_split() %>%
  setNames(unique(indices_long$Indice)) %>%
  lapply(function(df) {
    glm(Valor ~ Source, data = df, family = gaussian())
  })

# Extraer resumen de coeficientes
resultados_glm <- lapply(modelos_glm, function(mod) {
  tidy(mod)
})

# Mostrar resultados en un solo data frame
resultados_glm_df <- bind_rows(resultados_glm, .id = "Indice") %>%
  mutate(p.value = format(round(p.value, 4), nsmall = 4))  # Mostrar p sin notación científica

print(resultados_glm_df)



#----------------------------------------#
# % COBERTURA-------
#----------------------------------------#
library(dplyr)
library(tidyr)
library(ggplot2)


# 1. Crear dataset de cobertura por foto y categoría


# Datos IA
ia_cover <- annotations_new %>%
  group_by(Name, Categoria = `Label code`) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Name) %>%
  mutate(total = sum(n),
         cobertura = (n / total) * 100,
         fuente = "IA") %>%
  select(Name, Categoria, cobertura, fuente)

# Datos Humano
humano_cover <- annotations_old %>%
  group_by(Name, Categoria = `Label code`) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Name) %>%
  mutate(total = sum(n),
         cobertura = (n / total) * 100,
         fuente = "Humano") %>%
  select(Name, Categoria, cobertura, fuente)

# Combinar ambos
cobertura_comb <- bind_rows(ia_cover, humano_cover)


# 2. Calcular las 12 categorías más abundantes en total

top12_categorias <- cobertura_comb %>%
  group_by(Categoria) %>%
  summarise(cobertura_total = sum(cobertura)) %>%
  arrange(desc(cobertura_total)) %>%
  slice_head(n = 12) %>%
  pull(Categoria)

# Filtrar solo esas 12 categorías
cobertura_top12 <- cobertura_comb %>%
  filter(Categoria %in% top12_categorias)


# 3. Graficar boxplots de % de cobertura estimado


ggplot(cobertura_top12, aes(x = fuente, y = cobertura, fill = fuente)) +
  geom_boxplot(alpha = 0.8) +
  facet_wrap(~Categoria, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "% de cobertura estimado por Humano e IA",
    x = "Fuente",
    y = "% de cobertura"
  ) +
  scale_fill_manual(values = c("Humano" = "#1b9e77", "IA" = "#d95f02")) +
  theme(legend.position = "none")


#ESTADISTICA
# Generar modelos GLM por categoría
modelos_glm <- cobertura_top12 %>%
  group_by(Categoria) %>%
  group_split() %>%
  setNames(top12_categorias) %>%
  lapply(function(df) {
    glm(cobertura ~ fuente, data = df, family = gaussian())
  })

# Extraer resultados de los modelos
resultados_glm <- lapply(modelos_glm, tidy)

# Unir en un solo dataframe
resultados_glm_df <- bind_rows(resultados_glm, .id = "Categoria") %>%
  mutate(p.value = format(round(p.value, 4), nsmall = 4))  # para evitar notación científica

# Mostrar resultados
print(resultados_glm_df)



