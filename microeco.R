# Practica  
install.packages("microeco")  # Solo hace falta instalarlo una vez
library(microeco)
library(ggplot2)

install.packages("ggh4x")
library(ggh4x)

install.packages("ggpubr")
library(ggpubr)

# Dataset de ejemplo incluido en microeco
data(dataset)
microbioma <- dataset$clone(deep = TRUE)

# Inspeccionar las tablas en RStudio
View(microbioma$otu_table)
View(microbioma$tax_table)
View(microbioma$sample_table)

# Calcular abundancias
microbioma$cal_abund()

# Consultar la tabla de abundancia a nivel de filo
names(microbioma$taxa_abund)  # Muestra los niveles disponibles
microbioma$taxa_abund$Phylum  # Muestra la tabla de abundancia a nivel de reino

composicion <- trans_abund$new(
  dataset = microbioma,
  taxrank = "Phylum",
  ntaxa = 12
)

composicion$plot_bar(
  xtext_keep = FALSE,
  facet = c("Saline", "Group")
)

diversidad <- microbioma$clone(deep = TRUE)

# Diversidad alfa
diversidad$cal_alphadiv()
diversidad$alpha_diversity

alfa <- trans_alpha$new(
  dataset = diversidad,
  group = "Saline"
)

alfa$plot_alpha(
  measure = "Observed",
  add = "jitter"
)

# Diversidad beta
diversidad$cal_betadiv()

beta <- trans_beta$new(
  dataset = diversidad,
  measure = "bray",
  group = "Group"
)

beta$cal_ordination()

beta$plot_ordination(
  plot_color = "Group",
  plot_type = c("point", "ellipse")
)

# Confirmar los nombres de las columnas 
colnames(microbioma$sample_table)
colnames(microbioma$tax_table)

