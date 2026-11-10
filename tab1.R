############################################################
####### Correlaciones incondicionales
############################################################
# Paper: "Copper Price Booms and Tradable Sector Crowding-Out in
#         Chile (2003–2024)", Martín Latorre.
#
# Este script construye la tabla de correlaciones incondicionales
# entre los factores globales (VIX, DXY e IGREA) y el precio real del
# cobre y variables domésticas (IMACEC, IMACEC no minero y tipo de
# cambio nominal). La tabla motiva la inclusión de los factores
# globales en el SVAR.
#
# Etapas
# 1. Datos. Carga las series mensuales en primera diferencia
#    logarítmica (IGREA en primera diferencia, porque toma valores
#    negativos).
# 2. Agregación trimestral. Suma las diferencias mensuales dentro de
#    cada trimestre, lo que equivale a la variación entre el último
#    mes del trimestre y el último mes del trimestre anterior. Solo se
#    usan trimestres con los tres meses disponibles.
# 3. Correlaciones. Coeficiente de correlación de Pearson con
#    inferencia Newey-West (rezagos automáticos y prewhitening),
#    estimado como la pendiente de una regresión entre las series
#    estandarizadas.
# 4. Tabla LaTeX. Genera la tabla y la guarda en
#    tables/correlations.tex.
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

archivo_datos <- "correlation data/correlation.xlsx"
hoja_datos    <- "Correlations log_difs"
dir_tab       <- "tables"
archivo_tex   <- "tab1.tex"

# Filas (factores globales) y columnas (precio del cobre y variables
# domésticas) de la tabla, con el nombre de la columna en el Excel
filas    <- c("VIX", "DXY", "IGREA")
columnas <- c("copper real price", "IMACEC", "IMACEC-NM", "USD/CLP")
vars     <- c(filas, columnas)

# Etiquetas en la tabla LaTeX
etiquetas_filas <- c("VIX" = "VIX", "DXY" = "DXY", "IGREA" = "IGREA")
etiquetas_cols  <- c("copper real price" = "Real copper price",
                     "IMACEC"            = "IMACEC",
                     "IMACEC-NM"         = "IMACEC-NM",
                     "USD/CLP"           = "NER (USD/CLP)")


############################################################
####### 1. Data
############################################################
# Series mensuales en primera diferencia logarítmica. La columna
# 'year' contiene la fecha de cada mes.
############################################################

correlation <- read_excel(archivo_datos, sheet = hoja_datos)

faltantes <- setdiff(c("year", vars), names(correlation))
if (length(faltantes) > 0) {
  stop("No están en la base: ", paste(faltantes, collapse = ", "))
}


############################################################
####### 2. Agregación trimestral
############################################################
# Como las series están en primeras diferencias, la suma de los tres
# meses de un trimestre es la variación trimestral. Si falta algún
# mes, el trimestre queda como NA.
############################################################

correlation$fecha <- as.Date(correlation$year)
correlation$trim  <- paste0(format(correlation$fecha, "%Y"), "Q",
                            (as.integer(format(correlation$fecha, "%m")) - 1) %/% 3 + 1)

trimestres <- unique(correlation$trim[order(correlation$fecha)])

suma_trim <- function(v) if (sum(!is.na(v)) == 3) sum(v) else NA_real_

corr_q <- data.frame(trim = trimestres, stringsAsFactors = FALSE)
for (v in vars) {
  corr_q[[v]] <- as.numeric(tapply(correlation[[v]],
                                   factor(correlation$trim, levels = trimestres),
                                   suma_trim))
}


############################################################
####### 3. Correlaciones con inferencia Newey-West
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

res <- expand.grid(fila = filas, columna = columnas, stringsAsFactors = FALSE)
est <- t(mapply(function(f, c) nw_cor(corr_q[[f]], corr_q[[c]]),
                res$fila, res$columna))
res <- cbind(res, est)

# Comprobación: r debe coincidir con cor()
r_cor <- mapply(function(f, c)
  cor(corr_q[[f]], corr_q[[c]], use = "complete.obs"),
  res$fila, res$columna)
stopifnot(isTRUE(all.equal(unname(res$r), unname(r_cor))))

estrellas <- function(p) ifelse(p < 0.01, "***",
                                ifelse(p < 0.05, "**",
                                       ifelse(p < 0.10, "*", "")))

# Revisión en consola: correlaciones con asteriscos y errores estándar
tabla <- matrix(paste0(formatC(res$r, format = "f", digits = 2), estrellas(res$p)),
                nrow = length(filas), dimnames = list(filas, columnas))
tabla_se <- matrix(formatC(res$se, format = "f", digits = 2),
                   nrow = length(filas), dimnames = list(filas, columnas))
print(tabla, quote = FALSE)
print(tabla_se, quote = FALSE)


############################################################
####### 4. Tabla LaTeX
############################################################
# Formato común a las tablas del paper (threeparttable, reglas dobles
# con \specialrule, \phantom{-} para alinear los signos y asteriscos
# como superíndice). El período muestral y el número de observaciones
# se calculan desde los datos.
############################################################

# Celdas en modo matemático
celda_tex <- function(r, p) {
  signo <- ifelse(r < 0, "", "\\phantom{-}")
  paste0("$", signo, formatC(r, format = "f", digits = 2),
         "^{", estrellas(p), "}$")
}
res$celda_tex <- celda_tex(res$r, res$p)

cuerpo <- unname(sapply(filas, function(f) {
  sub    <- res[res$fila == f, ]
  celdas <- sub$celda_tex[match(columnas, sub$columna)]
  paste0(etiquetas_filas[f], " & ", paste(celdas, collapse = " & "), " \\\\")
}))

ok      <- complete.cases(corr_q[, vars])
periodo <- paste0(min(corr_q$trim[ok]), "--", max(corr_q$trim[ok]))
n_txt   <- if (min(res$n) == max(res$n)) max(res$n) else
  paste0(min(res$n), "--", max(res$n))

nota <- paste0(
  "\\textit{Notes:} The table reports Pearson correlation coefficients between quarterly series. ",
  "All variables are expressed in first log-differences, except IGREA, which is expressed in ",
  "first differences because it takes negative values. Quarterly series are obtained by summing ",
  "monthly differences within each quarter; only quarters with three monthly observations are used. ",
  "IMACEC and IMACEC-NM (non-mining IMACEC) ",
  "are seasonally adjusted. NER denotes the nominal exchange rate (Chilean pesos per U.S. dollar). ",
  "Significance levels are based on Newey-West standard errors with automatic lag selection and ",
  "prewhitening. $^{***}$ $p<0.01$, $^{**}$ $p<0.05$, $^{*}$ $p<0.10$. ",
  "Sample: ", periodo, " ($n = ", n_txt, "$)."
)

tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{Unconditional Correlations: Global Factors, Copper Price, and Domestic Variables}",
  "\\label{tab:correlations}",
  "\\begin{tabular}{lcccc}",
  "\\specialrule{1pt}{0pt}{1.5pt}",
  "\\specialrule{0.4pt}{0pt}{3pt}",
  paste0(" & ", paste(etiquetas_cols[columnas], collapse = " & "), " \\\\"),
  "\\midrule",
  cuerpo,
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
writeLines(tex, file.path(dir_tab, archivo_tex))
cat(tex, sep = "\n")   # para revisarla en la consola
