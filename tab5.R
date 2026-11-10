############################################################
####### Correlaciones entre shocks CSIM y precios de comercio
#######  Tabla 5
############################################################
# Paper: "Copper Price Booms and Tradable Sector Crowding-Out in
#         Chile (2003–2024)", Martín Latorre.
#
# Este script construye la tabla de correlaciones entre los shocks
# CSIM trimestrales y los índices de precios de exportación (frutas,
# metales, salmón y celulosa) e importación (crudo y diésel) de Chile.
#
# Etapas
# 1. Datos. Carga las series trimestrales. Los precios vienen en
#    primera diferencia logarítmica.
# 2. Correlaciones. Coeficiente de correlación de Pearson con
#    inferencia Newey-West (rezagos automáticos y prewhitening),
#    estimado como la pendiente de una regresión entre las series
#    estandarizadas.
# 3. Tabla LaTeX. Genera la tabla, en dos paneles (exportación e
#    importación), y la guarda en tables/tab5.tex.
#
# Nota sobre el código
# Esta versión es una sistematización y simplificación de un código
# anterior del autor, realizada con la asistencia de Claude
# (Anthropic) para hacerlo replicable. El trabajo incluyó ordenar el
# script por etapas, reunir los parámetros al inicio, unificar el
# formato de la tabla con el resto del paper y documentar cada
# sección. La especificación econométrica y las decisiones de
# modelación son del autor.
############################################################

rm(list = ls())

library(readxl)    # importar bases de datos
library(sandwich)  # errores estándar Newey-West
library(lmtest)    # coeftest()

# ---------------------------------------------------------------
# Parámetros
# ---------------------------------------------------------------

archivo_datos <- "correlation data/export_p.xlsx"
hoja_datos    <- "correlations"
col_shock     <- "ds"          # columna del shock CSIM trimestral en el Excel
dir_tab       <- "tables"
archivo_tex   <- "tab5.tex"

panel_x <- c("frutas", "metales", "salmon", "celulosa")   # Panel A: exportación
panel_m <- c("crudo", "diesel")                            # Panel B: importación
precios <- c(panel_x, panel_m)

etiquetas <- c(frutas = "Fruits", metales = "Metals", salmon = "Salmon",
               celulosa = "Pulp", crudo = "Crude oil", diesel = "Diesel")


############################################################
####### 1. Data
############################################################
# Series trimestrales. La columna 'year' contiene la fecha de cada
# trimestre y se usa solo para reportar el período muestral.
############################################################

datos <- read_excel(archivo_datos, sheet = hoja_datos)

faltantes <- setdiff(c("year", col_shock, precios), names(datos))
if (length(faltantes) > 0) {
  stop("No están en la base: ", paste(faltantes, collapse = ", "))
}

datos$fecha <- as.Date(datos$year)
datos$trim  <- paste0(format(datos$fecha, "%Y"), "Q",
                      (as.integer(format(datos$fecha, "%m")) - 1) %/% 3 + 1)


############################################################
####### 2. Correlaciones con inferencia Newey-West
############################################################
# La pendiente de una regresión entre dos series estandarizadas es su
# correlación de Pearson. Estimarla así permite usar errores estándar
# Newey-West (rezagos automáticos y prewhitening) para la inferencia.
############################################################

nw_cor <- function(x, y) {
  ok <- complete.cases(x, y)
  m  <- lm(scale(y[ok]) ~ scale(x[ok]))
  ct <- coeftest(m, vcov. = NeweyWest(m))
  c(r = ct[2, 1], se = ct[2, 2], p = ct[2, 4], n = sum(ok))
}

estrellas <- function(p) ifelse(p < 0.01, "***",
                                ifelse(p < 0.05, "**",
                                       ifelse(p < 0.10, "*", "")))

shock <- datos[[col_shock]]

res <- data.frame(precio = precios,
                  t(sapply(precios, function(v) nw_cor(shock, datos[[v]]))))
res$sig   <- estrellas(res$p)
res$panel <- ifelse(res$precio %in% panel_x, "Export", "Import")

# Comprobación: r debe coincidir con cor()
r_cor <- sapply(precios, function(v) cor(shock, datos[[v]], use = "complete.obs"))
stopifnot(isTRUE(all.equal(unname(res$r), unname(r_cor))))

print(res, row.names = FALSE, digits = 3)   # revisión en consola


############################################################
####### 3. Tabla LaTeX
############################################################
# Formato común a las tablas del paper (threeparttable, reglas dobles
# con \specialrule, \phantom{-} para alinear los signos y asteriscos
# como superíndice). Filas: índices de precios, en dos paneles.
# Columnas: correlación, error estándar y número de observaciones.
# El período muestral se calcula desde los datos.
############################################################

fila_tex <- function(v) {
  f     <- res[res$precio == v, ]
  signo <- if (f$r < 0) "" else "\\phantom{-}"
  paste0(etiquetas[v], " & $", signo, formatC(f$r, format = "f", digits = 2),
         "^{", f$sig, "}$ & (", formatC(f$se, format = "f", digits = 2),
         ") & ", f$n, " \\\\")
}

ok      <- complete.cases(datos[, c(col_shock, precios)])
periodo <- paste0(min(datos$trim[ok]), "--", max(datos$trim[ok]))

nota <- paste0(
  "\\textit{Notes:} The table reports Pearson correlation coefficients between the quarterly ",
  "CSIM shock series and Chilean export (Panel A) and import (Panel B) price indices, ",
  "expressed in first log-differences. Newey-West standard errors with automatic lag ",
  "selection and prewhitening are reported in parentheses. ",
  "$^{***}$ $p<0.01$, $^{**}$ $p<0.05$, $^{*}$ $p<0.10$. ",
  "Sample: ", periodo, "."
)

tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{Correlations Between CSIM Shocks and Export and Import Prices}",
  "\\label{tab:csim_prices}",
  "\\begin{tabular}{lccc}",
  "\\specialrule{1pt}{0pt}{1.5pt}",
  "\\specialrule{0.4pt}{0pt}{3pt}",
  " & Correlation & Std.\\ error & $n$ \\\\",
  "\\midrule",
  "\\multicolumn{4}{l}{\\textit{Panel A. Export prices}} \\\\",
  sapply(panel_x, fila_tex),
  "\\addlinespace",
  "\\multicolumn{4}{l}{\\textit{Panel B. Import prices}} \\\\",
  sapply(panel_m, fila_tex),
  "\\specialrule{0.4pt}{3pt}{1.5pt}",
  "\\specialrule{1pt}{0pt}{0pt}",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  paste0("\\item ", nota),
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

dir.create(dir_tab, showWarnings = FALSE)
writeLines(unname(tex), file.path(dir_tab, archivo_tex))
cat(tex, sep = "\n")   # para revisarla en la consola