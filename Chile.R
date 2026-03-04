#DATA----
##Data folder
Data <- "DATA"

library(readr)
cover <- read_csv(file.path(Data, "percent_covers_chile.csv"))#read cover data
metadata <- read_csv(file.path(Data,"metadata_chile.csv"))#read metadata

#Merge photoquadrat.metadata and photoquadrat.cover
AMP<- merge(metadata,cover, by.x = "Name", by.y ="Image name", all.x = TRUE) 

#Remove original data frames from enviroment
rm(cover,metadata)

library(dplyr)
library(lubridate)

AMP <- AMP %>%
  mutate(
    Date = as.Date(Date),  # asegurar formato Date
    month = month(Date),
    season = case_when(
      month >= 3 & month <= 6 ~ "FALL",
      month >= 10 & month <= 12 ~ "SPRING",
      TRUE ~ NA_character_
    )
  )%>%
  select(Name,Date,season,month,everything())


#DATA VALPARAISO - MONTEMAR
valpo_data <- AMP %>%
  filter(locality == "VALPARAISO", !is.na(season), site=="MONTEMAR")

# Identificar columnas CATAMI
catami_cols <- names(valpo_data)[which(names(valpo_data) == "BRY"):ncol(valpo_data)]

# Calcular las 6 más abundantes
top5_catami <- valpo_data %>%
  select(all_of(catami_cols)) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "categoria", values_to = "suma") %>%
  arrange(desc(suma)) %>%
  slice_head(n = 6) %>%
  pull(categoria)


library(dplyr)
library(tidyr)
library(purrr)
library(broom)

# Filtrar y preparar los datos largos
valpo_long <- valpo_data %>%
  select(season, strata, all_of(top5_catami)) %>%
  pivot_longer(cols = all_of(top5_catami),
               names_to = "categoria",
               values_to = "cobertura") %>%
  filter(!is.na(season), !is.na(strata))

# Realizar el test de Wilcoxon por categoría y estrato
test_resultados <- valpo_long %>%
  group_by(categoria, strata) %>%
  filter(length(unique(season)) == 2) %>%  # asegurar que hay ambos grupos
  nest() %>%
  mutate(
    test = map(data, ~ wilcox.test(cobertura ~ season, data = .x)),
    tidy_test = map(test, tidy)
  ) %>%
  unnest(tidy_test) %>%
  select(categoria, strata, p.value, statistic)

# Ver resultados ordenados por significancia
test_resultados <- test_resultados %>%
  arrange(p.value)


library(scales)
test_resultados %>%
  mutate(p.value = number(p.value, accuracy = 0.0001))

#agregar * al boxplot
library(dplyr)
library(tidyr)

test_resultados <- valpo_long %>%
  group_by(categoria, strata) %>%
  summarise(p.value = tryCatch(
    t.test(cobertura ~ season)$p.value,
    error = function(e) NA
  ), .groups = "drop") %>%
  mutate(signif = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE            ~ ""
  ))

# Calcular el máximo valor de cobertura para cada categoría y estrato
coords_asteriscos <- valpo_long %>%
  group_by(categoria, strata) %>%
  summarise(y_pos = max(cobertura, na.rm = TRUE) + 5, .groups = "drop")  # ajustá el "+5" si querés más espacio

# Unir los datos del test con las coordenadas
anotaciones <- left_join(test_resultados, coords_asteriscos, by = c("categoria", "strata"))


ggplot(valpo_long, aes(x = season, y = cobertura, fill = season)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  facet_grid(categoria ~ strata, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Cobertura de las 5 categorías CATAMI más abundantes en MONTEMAR",
    x = "Estación (Season)",
    y = "Cobertura (%)"
  ) +
  scale_fill_manual(values = c("FALL" = "#FFA500", "SPRING" = "#00BFC4")) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  geom_text(
    data = anotaciones %>% filter(signif != ""),
    aes(x = 1.5, y = y_pos, label = signif),
    inherit.aes = FALSE,
    size = 5
  )



#MSD-----
library(vegan)
library(dplyr)
library(ggplot2)

# Filtrar Valparaíso
valpo_data <- AMP %>% filter(locality == "VALPARAISO")

# Detectar columnas CATAMI (asumen que empiezan con "BRY")
catami_cols <- grep("^BRY", names(valpo_data)):ncol(valpo_data)

# Seleccionar solo columnas CATAMI
catami_matrix <- valpo_data[, catami_cols]

# Remover columnas con solo ceros
catami_matrix <- catami_matrix[, colSums(catami_matrix, na.rm = TRUE) > 0]

# Correr nMDS sin transformación y con distancia Bray-Curtis
nMDS_valpo <- metaMDS(catami_matrix, k = 2, trymax = 50, try = 50,
                      distance = "bray", autotransform = FALSE)

# Extraer coordenadas
NMDS1 <- nMDS_valpo$points[, 1]
NMDS2 <- nMDS_valpo$points[, 2]

# Combinar con info extra
MDS_plot <- cbind(valpo_data, NMDS1, NMDS2)

# Plot tipo publicación
ggplot(MDS_plot, aes(x = NMDS1, y = NMDS2, color = strata, shape = strata)) +
  geom_point(position = position_jitter(width = 0.1, height = 0.1), size = 3, alpha = 0.9) +
  stat_ellipse(type = "t", size = 1, linetype = "solid") +
  scale_color_brewer(palette = "Set1", name = "Estrato") +
  scale_shape_manual(values = c(16, 17, 15)) +
  annotate("text",
           x = max(NMDS1, na.rm = TRUE) - 0.3,
           y = min(NMDS2, na.rm = TRUE) - 0.3,
           label = paste("Stress =", round(nMDS_valpo$stress, 3)),
           hjust = 1, size = 4) +
  ggtitle("   nMDS - Arrecifes de Valparaíso por estrato") +
  theme_bw() +
  theme(
    legend.position = "top",
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_blank()
  )


#PIE PLOTS------
library(dplyr)
library(tidyr)
library(plotly)

# Seleccionar columnas CATAMI desde "BRY" en adelante
catami_cols <- which(names(valpo_data) == "BRY"):ncol(valpo_data)

# Reestructurar y resumir datos por season, strata y categoría
catami_long <- valpo_data %>%
  select(season, strata, all_of(catami_cols)) %>%
  pivot_longer(cols = -c(season, strata), names_to = "CATAMI", values_to = "cover") %>%
  group_by(season, strata, CATAMI) %>%
  summarise(cover.mean = mean(cover, na.rm = TRUE), .groups = "drop") %>%
  filter(cover.mean > 0, CATAMI != "algae")

# Asignar colores (opcional si no tenés columna Color)
catami_long$Color <- RColorBrewer::brewer.pal(n = length(unique(catami_long$CATAMI)), "Set3")[as.numeric(factor(catami_long$CATAMI))]

# Filtrar por estación y estrato
data_FL <- filter(catami_long, season == "FALL", strata == "LOWTIDE")
data_FM <- filter(catami_long, season == "FALL", strata == "MIDTIDE")
data_FH <- filter(catami_long, season == "FALL", strata == "HIGHTIDE")
data_SL <- filter(catami_long, season == "SPRING", strata == "LOWTIDE")
data_SM <- filter(catami_long, season == "SPRING", strata == "MIDTIDE")
data_SH <- filter(catami_long, season == "SPRING", strata == "HIGHTIDE")

# Crear gráfico de 6 tortas
p <- plot_ly(labels = ~CATAMI, values = ~cover.mean, legendgroup = ~CATAMI,
             textinfo = 'label+percent', marker = list(colors = ~Color)) %>%
  
  # FALL
  add_pie(data = data_FL, name = "FALL - Bajo", title = 'FALL - Bajo', domain = list(row = 0, column = 0)) %>%
  add_pie(data = data_FM, name = "FALL - Medio", title = 'FALL - Medio', domain = list(row = 0, column = 1)) %>%
  add_pie(data = data_FH, name = "FALL - Alto", title = 'FALL - Alto', domain = list(row = 0, column = 2)) %>%
  
  # SPRING
  add_pie(data = data_SL, name = "SPRING - Bajo", title = 'SPRING - Bajo', domain = list(row = 1, column = 0)) %>%
  add_pie(data = data_SM, name = "SPRING - Medio", title = 'SPRING - Medio', domain = list(row = 1, column = 1)) %>%
  add_pie(data = data_SH, name = "SPRING - Alto", title = 'SPRING - Alto', domain = list(row = 1, column = 2)) %>%
  
  # Layout general
  layout(title = "Cobertura % por Estación y Estrato (VALPO)",
         showlegend = TRUE,
         grid = list(rows = 2, columns = 3),
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

# Mostrar gráfico
p

#Tabla
library(dplyr)
library(tidyr)
library(writexl)  # Asegúrate de tener esta librería instalada

# Seleccionar columnas CATAMI desde "BRY" en adelante
catami_cols <- which(names(valpo_data) == "BRY"):ncol(valpo_data)

# Reestructurar y calcular % por combinación season + strata
catami_table <- valpo_data %>%
  select(season, strata, all_of(catami_cols)) %>%
  pivot_longer(cols = -c(season, strata), names_to = "CATAMI", values_to = "cover") %>%
  group_by(season, strata, CATAMI) %>%
  summarise(mean_cover = mean(cover, na.rm = TRUE), .groups = "drop") %>%
  group_by(season, strata) %>%
  mutate(percentage = (mean_cover / sum(mean_cover, na.rm = TRUE)) * 100) %>%
  filter(percentage > 0) %>%
  arrange(season, strata, desc(percentage)) %>%
  mutate(percentage = round(percentage, 1))  # redondear al final

# Exportar a un archivo Excel
write_xlsx(catami_table, "catami_coverages_table.xlsx")




#ANNOTATIONS------
library(readr)
annotations <- read_csv(file.path(Data, "annotations_chile.csv"))

unique(annotations$Annotator)vperez2019

names(annotations)
library(dplyr)

# Filtrar solo el sitio MONTEMAR
montemar_data <- annotations %>%
  filter(site == "MONTEMAR")

# Crear columna "match" para comparar humano vs IA
montemar_data <- montemar_data %>%
  mutate(match = if_else(`Label code` == `Machine suggestion 1`, "yes", "no"))

# Resumen general de aciertos y errores
accuracy_summary <- montemar_data %>%
  count(match) %>%
  mutate(percent = round(100 * n / sum(n), 1))

print(accuracy_summary)

# Categorías en las que más se equivoca la IA (según el humano)
errors_by_label <- montemar_data %>%
  filter(match == "no") %>%
  count(`Label code`, sort = TRUE)

print(errors_by_label)

# Categorías en las que más acierta la IA (según el humano)
correct_by_label <- montemar_data %>%
  filter(match == "yes") %>%
  count(`Label code`, sort = TRUE)

print(correct_by_label)

# Tabla de confusión simplificada (combinaciones Label code vs IA)
confusion_simple <- montemar_data %>%
  count(`Label code`, `Machine suggestion 1`) %>%
  arrange(desc(n))

print(confusion_simple)

# Tabla estilo matriz de confusión (opcional)
print(table(montemar_data$`Label code`, montemar_data$`Machine suggestion 1`))


# Exportar a un archivo Excel
write_xlsx(confusion_simple, "confusion_simple.xlsx")



#VALEN-----

library(readr)
library(dplyr)

# Leer el archivo
annotations <- read_csv(file.path(Data, "annotations_chile.csv"))

# Filtrar por Annotator "vperez2019"
vperez_data <- annotations %>%
  filter(Annotator == "vperez2019")

# Crear columna "match" para comparar humano vs IA
vperez_data <- vperez_data %>%
  mutate(match = if_else(`Label code` == `Machine suggestion 1`, "yes", "no"))

# Resumen general de aciertos y errores
accuracy_summary <- vperez_data %>%
  count(match) %>%
  mutate(percent = round(100 * n / sum(n), 1))

print(accuracy_summary)

# Categorías en las que más se equivoca la IA (según el humano)
errors_by_label <- vperez_data %>%
  filter(match == "no") %>%
  count(`Label code`, sort = TRUE)

print(errors_by_label)

# Categorías en las que más acierta la IA (según el humano)
correct_by_label <- vperez_data %>%
  filter(match == "yes") %>%
  count(`Label code`, sort = TRUE)

print(correct_by_label)

# Tabla de confusión simplificada (combinaciones Label code vs IA)
confusion_simple <- vperez_data %>%
  count(`Label code`, `Machine suggestion 1`) %>%
  arrange(desc(n))

print(confusion_simple)

# Tabla estilo matriz de confusión
confusion_matrix <- table(vperez_data$`Label code`, vperez_data$`Machine suggestion 1`)
print(confusion_matrix)


library(caret)

# Obtener los niveles comunes entre las dos columnas
common_levels <- union(levels(vperez_data$`Machine suggestion 1`), levels(vperez_data$`Label code`))

# Asegurarse de que ambas columnas tengan los mismos niveles
vperez_data$`Machine suggestion 1` <- factor(vperez_data$`Machine suggestion 1`, levels = common_levels)
vperez_data$`Label code` <- factor(vperez_data$`Label code`, levels = common_levels)

# Crear la matriz de confusión
cm <- confusionMatrix(vperez_data$`Machine suggestion 1`, vperez_data$`Label code`)

# Ver el resultado
print(cm)

# Convertir la matriz de confusión a formato data frame
confusion_df <- as.data.frame(cm$table)

# Graficar la matriz de confusión
ggplot(confusion_df, aes(x =Prediction , y =Reference, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#deebf7", high = "#08306b") +
  geom_text(aes(label = Freq), color = "white", size = 3) +
  theme_minimal() +
  labs(title = "Matriz de Confusión: Humano vs IA",
       x = "IA (Machine suggestion 1)",
       y = "Humano (Label code)",
       fill = "Frecuencia") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#Codigo KAI----
# Librerías necesarias
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(broom)

# Carpeta donde están los datos
Data <- "DATA"

# Leer datos
cover <- read_csv(file.path(Data, "percent_covers_chile_v2.csv"))
metadata <- read_csv(file.path(Data, "metadata_chile.csv"))

# Unir metadata con cobertura
AMP <- merge(metadata, cover, by.x = "Name", by.y = "Image name", all.x = TRUE)

# Limpiar entorno
rm(cover, metadata)

# Procesar fechas y estaciones
AMP <- AMP %>%
  mutate(
    Date = as.Date(Date),
    month = month(Date),
    season = case_when(
      month >= 3 & month <= 6 ~ "FALL",
      month >= 10 & month <= 12 ~ "SPRING",
      TRUE ~ NA_character_
    )
  ) %>%
  select(Name, Date, season, month, everything())

# Eliminar columna "unc" (insensible a mayúsculas)
unc_col <- grep("^unc$", names(AMP), ignore.case = TRUE)
if (length(unc_col) > 0) {
  AMP <- AMP[, -unc_col]
}

# Identificar columnas CATAMI desde "BRY" en adelante
catami_cols <- which(names(AMP) == "BRY"):ncol(AMP)

# Recalcular porcentajes sin "unc"
AMP <- AMP %>%
  rowwise() %>%
  mutate(
    total_cover = sum(c_across(all_of(catami_cols)), na.rm = TRUE)
  ) %>%
  ungroup()

# Reescalar a nuevo 100%
AMP <- AMP %>%
  mutate(across(
    .cols = all_of(catami_cols),
    .fns = ~ . / total_cover * 100,
    .names = "{.col}"
  )) %>%
  select(-total_cover)

# Verificar que los porcentajes sumen aproximadamente 100%
AMP <- AMP %>%
  rowwise() %>%
  mutate(cover_sum = sum(c_across(all_of(catami_cols)), na.rm = TRUE)) %>%
  ungroup()

summary(AMP$cover_sum)

AMP <- AMP %>% select(-cover_sum)

#DATA VALPARAISO - MONTEMAR
valpo_data <- AMP %>%
  filter(locality == "VALPARAISO", !is.na(season), site=="MONTEMAR")

# Identificar columnas CATAMI
catami_cols <- names(valpo_data)[which(names(valpo_data) == "BRY"):ncol(valpo_data)]

# Calcular las 6 más abundantes
top5_catami <- valpo_data %>%
  select(all_of(catami_cols)) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "categoria", values_to = "suma") %>%
  arrange(desc(suma)) %>%
  slice_head(n = 6) %>%
  pull(categoria)

# Filtrar y preparar los datos largos
valpo_long <- valpo_data %>%
  select(season, strata, all_of(top5_catami)) %>%
  pivot_longer(cols = all_of(top5_catami),
               names_to = "categoria",
               values_to = "cobertura") %>%
  filter(!is.na(season), !is.na(strata))




#PIE PLOTS------
library(dplyr)
library(tidyr)
library(ggplot2)

# Reestructurar y resumir datos
catami_cols <- which(names(valpo_data) == "BRY"):ncol(valpo_data)

catami_long <- valpo_data %>%
  select(season, strata, all_of(catami_cols)) %>%
  pivot_longer(cols = -c(season, strata), names_to = "CATAMI", values_to = "cover") %>%
  group_by(season, strata, CATAMI) %>%
  summarise(cover.mean = mean(cover, na.rm = TRUE), .groups = "drop") %>%
  filter(cover.mean > 0, CATAMI != "algae") %>%
  mutate(grupo = paste(season, strata, sep = " - "),
         strata = factor(strata, levels = c("HIGHTIDE", "MIDTIDE", "LOWTIDE")),
         grupo = factor(grupo, levels = unique(paste(rep(c("FALL", "SPRING"), each = 3),
                                                     c("HIGHTIDE", "MIDTIDE", "LOWTIDE"),
                                                     sep = " - "))))

# Calcular porcentaje y posiciones para etiquetas
catami_long <- catami_long %>%
  group_by(grupo) %>%
  mutate(
    porcentaje = cover.mean / sum(cover.mean) * 100,
    label = ifelse(porcentaje >= 1, paste0(CATAMI, "\n", round(porcentaje, 1), "%"), "")
  ) %>%
  ungroup()

# Gráfico limpio
ggplot(catami_long, aes(x = "", y = porcentaje, fill = CATAMI)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  facet_wrap(~ grupo, nrow = 2) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
  theme_void() +
  theme(strip.text = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Cobertura (>1%) por estación y estrato (VALPO)")



#BOX PLOTS------
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(scales)
library(purrr)

# 1. Test de Wilcoxon solo donde hay ambas seasons
test_resultados <- valpo_long %>%
  group_by(categoria, strata) %>%
  filter(length(unique(season)) == 2) %>%
  nest() %>%
  mutate(
    test = map(data, ~ wilcox.test(cobertura ~ season, data = .x)),
    tidy_test = map(test, tidy)
  ) %>%
  unnest(tidy_test) %>%
  select(categoria, strata, p.value)

# 2. Agregar asteriscos de significancia
test_resultados <- test_resultados %>%
  mutate(signif = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    TRUE            ~ ""
  ))

# 3. Coordenadas de los asteriscos encima del boxplot
coords_asteriscos <- valpo_long %>%
  group_by(categoria, strata) %>%
  summarise(y_pos = max(cobertura, na.rm = TRUE) + 0, .groups = "drop")

# 4. Unir resultados del test con coordenadas
anotaciones <- left_join(test_resultados, coords_asteriscos, by = c("categoria", "strata"))

# 5. Ordenar niveles de strata
valpo_long <- valpo_long %>%
  mutate(strata = factor(strata, levels = c("HIGHTIDE", "MIDTIDE", "LOWTIDE")))

anotaciones <- anotaciones %>%
  mutate(strata = factor(strata, levels = c("HIGHTIDE", "MIDTIDE", "LOWTIDE")))

# 6. Crear el gráfico final
ggplot(valpo_long, aes(x = season, y = cobertura, fill = season)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  facet_grid(strata ~ categoria,
             scales = "free_y",
             space = "free_y",
             switch = "y") +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = c(0, 50, 100)
  ) +
  scale_fill_manual(values = c("FALL" = "#FFA500", "SPRING" = "#00BFC4")) +
  theme_minimal(base_size = 14) +
  theme(
    strip.placement = "outside",
    strip.text.y.right = element_text(angle = 0, hjust = 0),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.spacing.y = unit(1.5, "lines"),
    legend.position = "none",
    plot.title = element_blank(),
    plot.margin = margin(10, 40, 10, 10),
    strip.switch.pad.wrap = unit(0.3, "cm")
  ) +
  geom_text(
    data = anotaciones %>% filter(signif != "", y_pos <= 100),
    aes(x = 1.5, y = y_pos, label = signif),
    inherit.aes = FALSE,
    size = 5
  )


#codigo Valen---

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






