# Install and upload needed packages
# install.packages(c("afex", "tidyverse"))
# install.packages(c("see"))
# install.packages("effectsize")
library(afex)
library(tidyverse)
library(readr)
library(performance)
library(effectsize)
library(ggplot2)

# Load the data
data <- read_delim("C:/Users/carlo/OneDrive/Documentos/Carlota/CEU/TFG/Table with everything.csv", 
                   delim = ";", escape_double = FALSE, trim_ws = TRUE)

# Categorical variables into factors
data <- data %>%
  mutate(
    Subject_ID = factor(SubjectID),
    Sex = factor(Sex),
    Exo_num = ifelse(Exo == "Yes", 1, 0),
    Weight_num = ifelse(Weight == "Yes", 1, 0),
    Condition = case_when(
      Exo_num == 1 & Weight_num == 1 ~ "Exo - Weight",
      Exo_num == 1 & Weight_num == 0 ~ "Exo - No Weight",
      Exo_num == 0 & Weight_num == 1 ~ "No Exo - Weight",
      Exo_num == 0 & Weight_num == 0 ~ "No Exo - No Weight"
    ),
    Condition = factor(Condition, levels = c(
      "Exo - Weight", "Exo - No Weight",
      "No Exo - Weight", "No Exo - No Weight"))
  )

# Create a folder for graphs and reuslts if it doesn´t exist
output_dir <- "C:/Users/carlo/OneDrive/Documentos/Carlota/CEU/TFG/Gráficas/Anova"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Function to save the graphs with white background 
guardar_plot <- function(nombre_archivo, grafico, ancho = 8, alto = 5) {
  ggsave(
    filename = file.path(output_dir, nombre_archivo),
    plot = grafico,
    device = "png",
    width = ancho,
    height = alto,
    bg = "white"
  )
}

# ---- ANOVAS ----
# ANOVA CC_Flex_Accel
anova_cc_flex_accel <- aov_ez(
  id = "Subject_ID",
  dv = "CC_Flex_Accel",
  data = data,
  within = c("Exo", "Weight"),
  #between = "Sex"
)
summary(anova_cc_flex_accel)
# to check if model is okay (normality of residuals, (variance that the model doesn´t explain) of the 2 graphs below)
performance::check_model(anova_cc_flex_accel)
tabla_cc_flex_accel <- as.data.frame(anova_cc_flex_accel$anova_table)
# tam of effect for ANOVA in general
eta_squared(anova_cc_flex_accel, partial = TRUE)
print(tabla_cc_flex_accel)
write.csv(tabla_cc_flex_accel, file.path(output_dir, "Resultados_ANOVA_CC_Flex_Accel.csv"), row.names = FALSE)

# ANOVA CC_Flex_Decel
anova_cc_flex_decel <- aov_ez(
  id = "Subject_ID",
  dv = "CC_Flex_Decel",
  data = data,
  within = c("Exo", "Weight"),
  #between = "Sex"
)
summary(anova_cc_flex_decel)
# to check if model is okay (normality of residuals, (variance that the model doesn´t explain) of the 2 graphs below)
performance::check_model(anova_cc_flex_decel)
tabla_cc_flex_decel <- as.data.frame(anova_cc_flex_decel$anova_table)
print(tabla_cc_flex_decel)
write.csv(tabla_cc_flex_decel, file.path(output_dir, "Resultados_ANOVA_CC_Flex_Decel.csv"), row.names = FALSE)

# ANOVA CC_Ext_Accel
anova_cc_ext_accel <- aov_ez(
  id = "Subject_ID",
  dv = "CC_Ext_Accel",
  data = data,
  within = c("Exo", "Weight"),
  #between = "Sex"
)
summary(anova_cc_ext_accel)
# to check if model is okay (normality of residuals, (variance that the model doesn´t explain) of the 2 graphs below)
performance::check_model(anova_cc_ext_accel)
tabla_cc_ext_accel <- as.data.frame(anova_cc_ext_accel$anova_table)
print(tabla_cc_ext_accel)
write.csv(tabla_cc_ext_accel, file.path(output_dir, "Resultados_ANOVA_CC_Ext_Accel.csv"), row.names = FALSE)

# ANOVA CC_Ext_Decel
anova_cc_ext_decel <- aov_ez(
  id = "Subject_ID",
  dv = "CC_Ext_Decel",
  data = data,
  within = c("Exo", "Weight"),
  #between = "Sex"
)
summary(anova_cc_ext_decel)
# to check if model is okay (normality of residuals, (variance that the model doesn´t explain) of the 2 graphs below)
performance::check_model(anova_cc_ext_decel)
tabla_cc_ext_decel <- as.data.frame(anova_cc_ext_decel$anova_table)
print(tabla_cc_ext_decel)
write.csv(tabla_cc_ext_decel, file.path(output_dir, "Resultados_ANOVA_CC_Ext_Decel.csv"), row.names = FALSE)

# ANOVA Synergy Biceps Flexion
anova_synergy_biceps_flex <- aov_ez(
  id = "Subject_ID",
  dv = "Synergy_Biceps_Flexion",
  data = data,
  within = c("Exo", "Weight"),
  #between = "Sex"
)
summary(anova_synergy_biceps_flex)
# to check if model is okay (normality of residuals, (variance that the model doesn´t explain) of the 2 graphs below)
performance::check_model(anova_synergy_biceps_flex)
tabla_synergy_biceps_flex <- as.data.frame(anova_synergy_biceps_flex$anova_table)
print(tabla_synergy_biceps_flex)
write.csv(tabla_synergy_biceps_flex, file.path(output_dir, "Resultados_ANOVA_SynergyBiceps_Flexion.csv"), row.names = FALSE)

# ANOVA Synergy Biceps Extension
anova_synergy_biceps_ext <- aov_ez(
  id = "Subject_ID",
  dv = "Synergy_Biceps_Extension",
  data = data,
  within = c("Exo", "Weight"),
  #between = "Sex"
)
summary(anova_synergy_biceps_ext)
# to check if model is okay (normality of residuals, (variance that the model doesn´t explain) of the 2 graphs below)
performance::check_model(anova_synergy_biceps_ext)
tabla_synergy_biceps_ext <- as.data.frame(anova_synergy_biceps_ext$anova_table)
print(tabla_synergy_biceps_ext)
write.csv(tabla_synergy_biceps_ext, file.path(output_dir, "Resultados_ANOVA_SynergyBiceps_Extension.csv"), row.names = FALSE)

# ANOVA Synergy Triceps Flexion
anova_synergy_triceps_flex <- aov_ez(
  id = "Subject_ID",
  dv = "Synergy_Triceps_Flexion",
  data = data,
  within = c("Exo", "Weight"),
  #between = "Sex"
)
summary(anova_synergy_triceps_flex)
# to check if model is okay (normality of residuals, (variance that the model doesn´t explain) of the 2 graphs below)
performance::check_model(anova_synergy_triceps_flex)
tabla_synergy_triceps_flex <- as.data.frame(anova_synergy_triceps_flex$anova_table)
print(tabla_synergy_triceps_flex)
write.csv(tabla_synergy_triceps_flex, file.path(output_dir, "Resultados_ANOVA_SynergyTriceps_Flexion.csv"), row.names = FALSE)

# ANOVA Synergy Triceps Extension
anova_synergy_triceps_ext <- aov_ez(
  id = "Subject_ID",
  dv = "Synergy_Triceps_Extension",
  data = data,
  within = c("Exo", "Weight"),
  #between = "Sex"
)
summary(anova_synergy_triceps_ext)
# to check if model is okay (normality of residuals, (variance that the model doesn´t explain) of the 2 graphs below)
performance::check_model(anova_synergy_triceps_ext)
tabla_synergy_triceps_ext <- as.data.frame(anova_synergy_triceps_ext$anova_table)
print(tabla_synergy_triceps_ext)
write.csv(tabla_synergy_triceps_ext, file.path(output_dir, "Resultados_ANOVA_SynergyTriceps_Extension.csv"), row.names = FALSE)

# ANOVA ROM
anova_rom <- aov_ez(
  id = "Subject_ID",
  dv = "ROM_TOTAL",
  data = data,
  within = c("Exo", "Weight"),
  #between = "Sex"
)
summary(anova_rom)
# to check if model is okay (normality of residuals, (variance that the model doesn´t explain) of the 2 graphs below)
performance::check_model(anova_rom)
# tam effect for general ANOVA general 
eta_squared(anova_rom, partial = TRUE)
print(tabla_rom)
write.csv(tabla_rom, file.path(output_dir, "Resultados_ANOVA_ROM.csv"), row.names = FALSE)

# ---- GRAPHS ----
#Graphs CC_Flex_Accel
p_cc_flex_accel <- ggplot(data, aes(x = Condition, y = CC_Flex_Accel, fill = Condition)) +
  geom_boxplot() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16), axis.text.x = element_text(size = 10)) +
  labs(title = "CI Flexion Acceleration", y = "CI", x = "Condition") +
  geom_signif(
    y_position = max(data$CC_Flex_Accel, na.rm = TRUE) + 0.05,
    tip_length = 0.01,
    textsize = 5
  )
guardar_plot("Boxplot_CI_Flex_Accel.png", p_cc_flex_accel)

# Graphs CC_Flex_Decel
p_cc_flex_decel <- ggplot(data, aes(x = Condition, y = CC_Flex_Decel, fill = Condition)) +
  geom_boxplot() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16), axis.text.x = element_text(size = 10)) +
  labs(title = "CI Flexion Deceleration", y = "CI", x = "Condition") +
  geom_signif(
    y_position = max(data$CC_Flex_Decel, na.rm = TRUE) + 0.05,
    tip_length = 0.01,
    textsize = 5
  )
guardar_plot("Boxplot_CI_Flex_Decel.png", p_cc_flex_decel)

# Graphs CC_Ext_Accel
p_cc_ext_accel <- ggplot(data, aes(x = Condition, y = CC_Ext_Accel, fill = Condition)) +
  geom_boxplot() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16), axis.text.x = element_text(size = 10)) +
  labs(title = "CI Extension Acceleration", y = "CI", x = "Condition") +
  geom_signif(
    y_position = max(data$CC_Ext_Accel, na.rm = TRUE) + 0.05,
    tip_length = 0.01,
    textsize = 5
  )
guardar_plot("Boxplot_CI_Ext_Accel.png", p_cc_ext_accel)

# Graphs CC_Ext_Decel
p_cc_ext_decel <- ggplot(data, aes(x = Condition, y = CC_Ext_Decel, fill = Condition)) +
  geom_boxplot() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16), axis.text.x = element_text(size = 10)) +
  labs(title = "CI Extension Deceleration", y = "CI", x = "Condition") +
  geom_signif(
    y_position = max(data$CC_Ext_Decel, na.rm = TRUE) + 0.05,
    tip_length = 0.01,
    textsize = 5
  )
guardar_plot("Boxplot_CI_Ext_Decel.png", p_cc_ext_decel)

# Graphs SynergyBiceps_Flexion
p_biceps_flex <- ggplot(data, aes(x = Condition, y = Synergy_Biceps_Flexion, fill = Condition)) +
  geom_boxplot() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16), axis.text.x = element_text(size = 10)) +
  labs(title = "Synergy Biceps - Flexion", y = "Synergy", x = "Condition") +
  geom_signif(
    y_position = max(data$Synergy_Biceps_Flexion, na.rm = TRUE) + 0.05,
    tip_length = 0.01,
    textsize = 5
  )
guardar_plot("Boxplot_SynergyBiceps_Flexion.png", p_biceps_flex)

# Graphs SynergyBiceps_Extension
p_biceps_ext <- ggplot(data, aes(x = Condition, y = Synergy_Biceps_Extension, fill = Condition)) +
  geom_boxplot() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16), axis.text.x = element_text(size = 10)) +
  labs(title = "Synergy Biceps - Extension", y = "Synergy", x = "Condition") +
  geom_signif(
    y_position = max(data$Synergy_Biceps_Extension, na.rm = TRUE) + 0.05,
    tip_length = 0.01,
    textsize = 5
  )
guardar_plot("Boxplot_SynergyBiceps_Extension.png", p_biceps_ext)

# Graphs SynergyTriceps_Flexion
p_triceps_flex <- ggplot(data, aes(x = Condition, y = Synergy_Triceps_Flexion, fill = Condition)) +
  geom_boxplot() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16), axis.text.x = element_text(size = 10)) +
  labs(title = "Synergy Triceps - Flexion", y = "Synergy", x = "Condition") +
  geom_signif(
    y_position = max(data$Synergy_Triceps_Flexion, na.rm = TRUE) + 0.05,
    tip_length = 0.01,
    textsize = 5
  )
guardar_plot("Boxplot_SynergyTriceps_Flexion.png", p_triceps_flex)

# Graphs SynergyTriceps_Extension
p_triceps_ext <- ggplot(data, aes(x = Condition, y = Synergy_Triceps_Extension, fill = Condition)) +
  geom_boxplot() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16), axis.text.x = element_text(size = 10)) +
  labs(title = "Synergy Triceps - Extension", y = "Synergy", x = "Condition") +
  geom_signif(
    y_position = max(data$Synergy_Triceps_Extension, na.rm = TRUE) + 0.05,
    tip_length = 0.01,
    textsize = 5
  )
guardar_plot("Boxplot_SynergyTriceps_Extension.png", p_triceps_ext)

# Graphs ROM
p_rom <- ggplot(data, aes(x = Condition, y = ROM_TOTAL, fill = Condition)) +
  geom_boxplot() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16), axis.text.x = element_text(size = 10)) +
  labs(title = "Range of motion", y = "ROM", x = "Condition") +
  geom_signif(
    y_position = max(data$ROM_TOTAL, na.rm = TRUE) + 0.05,
    tip_length = 0.01,
    textsize = 5
  )
guardar_plot("Boxplot_ROM.png", p_rom)

