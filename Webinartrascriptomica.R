# Instalar paquetes una sola vez, si hace falta
install.packages(c("readxl", "ggplot2", "dplyr") )

library(readxl)
library(ggplot2)
library(dplyr)

# Descargar y leer la base de datos
url <- "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE223351&format=file&file=GSE223351%5FMMTV%2DErbb2%5FRawCounts%2Exlsx"
file <- "GSE223351.xlsx"

download.file(url, file, mode = "wb")
data <- read_excel(file)
data <- as.data.frame(data)

# Usar la primera columna como identificador de gen
genes <- make.unique(as.character(data[[1]]))
counts <- data[, -1, drop = FALSE]
rownames(counts) <- genes

# Asegurarse de que las columnas restantes sean conteos numéricos
counts <- as.matrix(counts)
storage.mode(counts) <- "numeric"

# Verificar que hay 6 columnas de muestras
grupos <- c(
  "Control", "Control", "Control",
  "Metastasis", "Metastasis", "Metastasis"
)

stopifnot(ncol(counts) == length(grupos))

# CPM: conteos por millón
sizes <- colSums(counts, na.rm = TRUE)
cpm <- sweep(counts, 2, sizes, FUN = "/") * 1e6

# Transformación log2
cpm_log2 <- log2(cpm + 1)

# Filtrar genes sin variación entre muestras
cpm_filtered <- cpm_log2[
  apply(cpm_log2, 1, var, na.rm = TRUE) > 0,
  ,
  drop = FALSE
]

# Prueba t para cada gen
prueba_t <- function(x) {
  tryCatch(
    t.test(
      x[grupos == "Metastasis"],
      x[grupos == "Control"]
    )$p.value,
    error = function(e) NA_real_
  )
}

p_values <- apply(cpm_filtered, 1, prueba_t)

# Diferencia entre las medias de log2 CPM
# (se interpreta como log2 fold change aproximado)
FoldChange <- function(x) {
  mean(x[grupos == "Metastasis"], na.rm = TRUE) -
    mean(x[grupos == "Control"], na.rm = TRUE)
}

log2FC <- apply(cpm_filtered, 1, FoldChange)

# Tabla de resultados y ajuste por pruebas múltiples
results <- data.frame(
  Gene = rownames(cpm_filtered),
  pvalue = p_values,
  log2FC = log2FC
)

results$Adj_p <- p.adjust(results$pvalue, method = "fdr")

# Clasificación
alpha <- 0.05
umbral_fc <- 1

results <- results %>%
  mutate(
    Clasificacion = case_when(
      Adj_p < alpha & log2FC > umbral_fc  ~ "Upregulated",
      Adj_p < alpha & log2FC < -umbral_fc ~ "Downregulated",
      TRUE                                ~ "Not significant"
    )
  )

# Volcano plot
ggplot(results, aes(x = log2FC, y = -log10(Adj_p), color = Clasificacion)) +
  geom_point(size = 1.5) +
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") +
  geom_vline(xintercept = c(-1, 1), linetype = "dashed") +
  labs(
    title = "Volcano plot",
    x = "log2 fold change",
    y = "-log10(FDR)",
    color = "Clasificación"
  ) +
  theme_minimal()

# Genes significativos y los 20 mejores
significativos <- results %>%
  filter(Clasificacion != "Not significant")

top_genes <- significativos %>%
  arrange(Adj_p) %>%
  head(20)

# Matriz para el heatmap
matriz_top <- cpm_filtered[
  rownames(cpm_filtered) %in% top_genes$Gene,
  ,
  drop = FALSE
]

heatmap_data <- t(scale(t(matriz_top)))

# Opcional: dibujar el heatmap si hay genes significativos
if (nrow(heatmap_data) > 0) {
  heatmap(heatmap_data)
}