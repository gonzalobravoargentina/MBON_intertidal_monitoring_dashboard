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


