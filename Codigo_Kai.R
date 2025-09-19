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




