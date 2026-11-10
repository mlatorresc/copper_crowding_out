############################################################
####### IV-LP: efectos de un shock de precio del cobre
############################################################
# Paper: "Copper Price Booms and Tradable Sector Crowding-Out in
#         Chile (2003–2024)", Martín Latorre.
#
# Este script estima la segunda parte de la estrategia empírica
# (modelo IV-LP). Toma como insumo los shocks CSIM que se estiman
# en el script del SVAR.
#
# Etapas
# 1. Datos. Carga las series trimestrales, las une en una sola base
#    y deja la muestra común a todas las series. Los shocks CSIM
#    mensuales ya vienen agregados a trimestres sumando los tres
#    meses de cada trimestre (ecuación 5).
# 2. Ajuste estacional. X-13ARIMA-SEATS solo para las variables de
#    actividad chilenas. El resto de las series no se ajusta.
# 3. Instrumento. Estima la primera etapa (ecuación 6) para evaluar
#    la relevancia del instrumento.
# 4. IRF. Estima la segunda etapa (ecuación 7) y las IRF
#    (ecuación 8) para cada variable dependiente, con un horizonte de
#    12 trimestres e intervalos al 90%. Los controles son consumo de
#    gobierno, inversión, TCR y VIX, con 4 rezagos. Cuando la
#    variable dependiente es uno de los controles, ese control se
#    excluye. Las figuras se guardan en la carpeta images.
#
# Variables dependientes
# - Actividad: PIB, PIB minero, PIB NMT, PIB NT y PIB sectoriales
#   (nominal deflactado por IPC)
# - Demanda interna: absorción, inversión, consumo de hogares y de
#   gobierno
# - Precios: TCR, tipo de cambio nominal, IPC
# - Sector externo: exportaciones totales, importaciones y balanza
#   de pagos
# - Mercado laboral: empleo y salarios, totales y sectoriales
# - Variables monetarias y financieras: agregados monetarios y tasas
#   de interés
#
# Nota sobre el código
# Esta versión es una sistematización y simplificación de un código
# anterior del autor, realizada con la asistencia de Claude
# (Anthropic) para hacerlo replicable. 
############################################################

rm(list = ls())

library(urca)
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)
library(plm)
library(readxl)      # importar bases de datos
library(dplyr)       # manejo de datos
library(countrycode) # transformar nombres de países a códigos
library(stats)       # tests de correlación
library(astsa)
library(openxlsx)
library(writexl)
library(lpirfs)      # IV-LP
library(seasonal)    # X-13ARIMA-SEATS
library(sandwich)    # errores estándar Newey-West (primera etapa)
library(magick)      # quitar el canal alfa de los PNG


############################################################
####### 1. Data
############################################################
# Carga las series trimestrales que se usan en el IV-LP y las une
# en una sola base (dt) usando la columna 'year' (trimestre).
#
# - p:   precio real del cobre (regresor endógeno)
# - csim: shocks CSIM agregados a trimestres, estimados en el SVAR
#        (instrumento)
# - Las demás bases contienen las variables dependientes y los
#   controles: PIB por demanda y por origen, comercio exterior,
#   precios, tipo de cambio, agregados monetarios, tasas, empleo,
#   salarios, balanza de pagos y precios de activos.
#
# Después de unir las bases, se eliminan los trimestres con datos
# faltantes en cualquier variable (la muestra es el período común a
# todas las series) y se ordena la base cronológicamente.
# Las series que tienen otra cobertura (salarios sectoriales, spread,
# BTU-10 y tasa onshore) se agregan como submuestras en la sección 4, para no
# cambiar la muestra principal.
############################################################

p <- read_excel("ivlp data/price_cu_q.xlsx", sheet = "p")
csim <- read_excel("ivlp data/csim_shocks.xlsx", sheet = "csim")
exp <- read_excel("ivlp data/exp_nom.xlsx", sheet = "exp")
pib_d <- read_excel("ivlp data/pib_d.xlsx", sheet = "pib_d")
pib_o <- read_excel("ivlp data/pib_o_nom.xlsx", sheet = "pib_o")
tcr <- read_excel("ivlp data/tcr.xlsx", sheet = "tcr")
m <- read_excel("ivlp data/m.xlsx", sheet = "m")
tpm <- read_excel("ivlp data/tpm.xlsx", sheet = "tpm")
vix <- read_excel("ivlp data/vix.xlsx", sheet = "vix")
usd <- read_excel("ivlp data/usd.xlsx", sheet = "usd")
ipc <- read_excel("ivlp data/ipc.xlsx", sheet = "ipc")
ocup <- read_excel("ivlp data/ocup.xlsx", sheet = "ocup")
ocup_rama <- read_excel("ivlp data/ocup_rama.xlsx", sheet = "ocup_rama")
wages <- read_excel("ivlp data/wages.xlsx", sheet = "irr")
cc_cf <- read_excel("ivlp data/cc_cf.xlsx", sheet = "cc_cf")
tasa_cap <- read_excel("ivlp data/tasa_cap.xlsx", sheet = "tasa_cap")
ipsa <- read_excel("ivlp data/ipsa.xlsx", sheet = "ipsa")
ipv <- read_excel("ivlp data/ipv.xlsx", sheet = "ipv")

dt <- merge(p, csim, by = "year", all = TRUE)
dt <- merge(dt, exp, by = "year", all = TRUE)
dt <- merge(dt, pib_d, by = "year", all = TRUE)
dt <- merge(dt, pib_o, by = "year", all = TRUE)
dt <- merge(dt, tcr, by = "year", all = TRUE)
dt <- merge(dt, m, by = "year", all = TRUE)
dt <- merge(dt, tpm, by = "year", all = TRUE)
dt <- merge(dt, vix, by = "year", all = TRUE)
dt <- merge(dt, usd, by = "year", all = TRUE)
dt <- merge(dt, ipc, by = "year", all = TRUE)
dt <- merge(dt, ocup, by = "year", all = TRUE)
dt <- merge(dt, ocup_rama, by = "year", all = TRUE)
dt <- merge(dt, wages, by = "year", all = TRUE)
dt <- merge(dt, cc_cf, by = "year", all = TRUE)
dt <- merge(dt, tasa_cap, by = "year", all = TRUE)
dt <- merge(dt, ipsa, by = "year", all = TRUE)
dt <- merge(dt, ipv, by = "year", all = TRUE)

dt <- na.omit(dt)
dt <- dt %>% arrange(year)


############################################################
####### 2. Ajuste estacional
############################################################
# X-13ARIMA-SEATS (módulo X-11), aplicado solo a las variables de
# actividad chilenas (PIB, componentes de la demanda, PIB sectoriales,
# comercio exterior y empleo). El resto de las series no se ajusta.
#
# Todas las variables tienen su versión *_sa. Las series que no se
# ajustan tienen *_sa idéntica a la serie original.
############################################################

# Clasificación de variables -------------------------------------

vars_actividad <- c(
  # PIB y demanda interna
  "pib", "c_h", "c_g", "i", "abs",
  # PIB sectoriales
  "agro", "min", "pesca", "manuf", "ut", "cons", "com", "trans",
  "serv_f", "serv_com", "serv_viv", "serv_p",
  # Agregados sectoriales
  "t_nc", "nt",
  # Comercio exterior
  "exp_tot", "imp",
  # Empleo
  "ocup", "ocup_min", "ocup_t_nm", "ocup_nt"
)

vars_sin_ajuste <- c(
  # Precio del cobre e instrumento
  "p", "csim",
  # Precios, tipo de cambio y salarios
  "tcr", "usd", "ipc", "irr",
  # Agregados monetarios y tasas
  "m1", "m2", "m3", "tpm", "tasa_cap_us",
  # Variables financieras
  "vix", "ipsa", "ipv",
  # Balanza de pagos (% del PIB)
  "cc", "bc", "cf"
)

faltantes <- setdiff(c(vars_actividad, vars_sin_ajuste), names(dt))
if (length(faltantes) > 0) {
  stop("No están en dt: ", paste(faltantes, collapse = ", "))
}

# Fecha de inicio para los objetos ts ------------------------------
# Acepta 'year' como Date/POSIXct o como texto tipo "2002Q1", "2002-Q1",
# "2002 Q1". Si el formato es otro, define inicio <- c(año, trimestre).

inicio_ts <- function(x) {
  x1 <- x[1]
  if (inherits(x1, c("Date", "POSIXct", "POSIXt"))) {
    anio <- as.numeric(format(x1, "%Y"))
    trim <- (as.numeric(format(x1, "%m")) - 1) %/% 3 + 1
    return(c(anio, trim))
  }
  s <- toupper(as.character(x1))
  if (grepl("^[0-9]{4}[- ]?Q[1-4]$", s)) {
    return(c(as.numeric(substr(s, 1, 4)), as.numeric(substr(s, nchar(s), nchar(s)))))
  }
  stop("Formato de 'year' no reconocido (", x1, "). Define inicio <- c(año, trimestre).")
}

# Ajuste ----------------------------------------------------------

sa_x13 <- function(x, nombre, inicio) {
  y <- ts(x, frequency = 4, start = inicio)
  m <- tryCatch(
    seasonal::seas(y, x11 = ""),
    error = function(e) stop("X-13 falló en '", nombre, "': ",
                             conditionMessage(e), call. = FALSE)
  )
  as.numeric(seasonal::final(m))
}

inicio <- inicio_ts(dt$year)

for (v in vars_actividad) {
  dt[[paste0(v, "_sa")]] <- sa_x13(dt[[v]], v, inicio)
}

for (v in vars_sin_ajuste) {
  dt[[paste0(v, "_sa")]] <- dt[[v]]
}


############################################################
####### 3. Instrumento: primera etapa
############################################################
# Primera etapa (ecuación 6). Para cada variable dependiente y^j,
# regresión de p_t sobre el shock CSIM trimestral, con tendencia
# lineal y 4 rezagos de y^j, de los controles (consumo de gobierno,
# inversión, VIX y TCR) y de p_t. Un control se excluye solo cuando
# es la variable dependiente.
# Errores estándar Newey-West (HAC) con prewhitening, ancho de banda
# automático y ajuste por grados de libertad.
# - F robusto: (beta / s.e. HAC)^2
# - F OLS: F convencional bajo homocedasticidad
#
# Los resultados se imprimen en consola y se exportan como tabla
# LaTeX a tables/tab4.tex (sin nota; la nota se agrega a mano).
############################################################

dir_tab     <- "tables"
archivo_tex <- "tab4.tex"

# Controles comunes a las secciones 3 y 4 --------------------------
# Nombre en la regresión = columna en la base
controles_base <- c(c_g = "c_g_sa", inv = "i_sa", vix = "vix_sa",
                    tcr = "tcr_sa", p = "p_sa")

# Primera etapa -----------------------------------------------------

primera_etapa <- function(y, datos, excluir = character(0), lags = 4) {
  ctrl <- controles_base[!names(controles_base) %in% excluir]
  vars_rez <- c(y = y, ctrl)
  
  X <- data.frame(p     = datos$p_sa,
                  csim  = datos$csim_sa,
                  trend = seq_len(nrow(datos)))
  for (nm in names(vars_rez)) {
    for (l in seq_len(lags)) {
      X[[paste0(nm, "_l", l)]] <- dplyr::lag(datos[[vars_rez[[nm]]]], l)
    }
  }
  X <- na.omit(X)
  filas_usadas <- as.integer(rownames(X))
  
  m1 <- lm(p ~ ., data = X)
  m0 <- lm(p ~ . - csim, data = X)
  V  <- sandwich::NeweyWest(m1, prewhite = TRUE, adjust = TRUE)
  
  b  <- unname(coef(m1)["csim"])
  se <- sqrt(V["csim", "csim"])
  t  <- b / se
  
  data.frame(
    beta     = b,
    se_nw    = se,
    p_val    = 2 * pt(-abs(t), df = df.residual(m1)),
    F_robust = t^2,
    F_ols    = anova(m0, m1)$F[2],
    n        = nobs(m1),
    desde    = min(filas_usadas),
    hasta    = max(filas_usadas)
  )
}

fs_spec <- tribble(
  ~variable,                 ~y,        ~excluir,
  "GDP",                     "pib_sa",  NA,
  "Mining GDP",              "min_sa",  NA,
  "Non-mining tradable GDP", "t_nc_sa", NA,
  "Non-tradable GDP",        "nt_sa",   NA,
  "Domestic absorption",     "abs_sa",  NA,
  "Investment",              "i_sa",    "inv",
  "Household consumption",   "c_h_sa",  NA,
  "Government consumption",  "c_g_sa",  "c_g",
  "Real exchange rate",      "tcr_sa",  "tcr",
  "Nominal exchange rate",   "usd_sa",  NA,
  "Consumer price index",    "ipc_sa",  NA
)

fs_p_lag <- bind_rows(lapply(seq_len(nrow(fs_spec)), function(k) {
  excl <- if (is.na(fs_spec$excluir[k])) character(0) else fs_spec$excluir[k]
  cbind(variable = fs_spec$variable[k],
        primera_etapa(fs_spec$y[k], dt, excl))
}))

# Revisión en consola
print(fs_p_lag %>%
        select(variable, beta, se_nw, F_robust, F_ols, n) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))),
      row.names = FALSE)

# Tabla LaTeX ---------------------------------------------------------
# Formato común a las tablas del paper (threeparttable, reglas dobles
# con \specialrule, \phantom{-} para alinear los signos y asteriscos
# como superíndice).

estrellas <- function(p) ifelse(p < 0.01, "***",
                                ifelse(p < 0.05, "**",
                                       ifelse(p < 0.10, "*", "")))

f3 <- function(x) formatC(x, format = "f", digits = 3)

# Trimestre en formato AAAAQn, a partir de la columna 'year'
etiqueta_trim <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXt"))) {
    paste0(format(x, "%Y"), "Q", (as.integer(format(x, "%m")) - 1) %/% 3 + 1)
  } else {
    toupper(gsub("[- ]", "", as.character(x)))
  }
}

periodo <- paste0(etiqueta_trim(dt$year[min(fs_p_lag$desde)]), "--",
                  etiqueta_trim(dt$year[max(fs_p_lag$hasta)]))
cat("Período de la primera etapa (para la nota):", periodo, "\n")

cuerpo <- sapply(seq_len(nrow(fs_p_lag)), function(k) {
  f     <- fs_p_lag[k, ]
  signo <- if (f$beta < 0) "" else "\\phantom{-}"
  paste0(f$variable, " & $", signo, f3(f$beta), "^{", estrellas(f$p_val), "}$",
         " & ", f3(f$se_nw), " & ", f3(f$F_robust), " & ", f3(f$F_ols),
         " & ", f$n, " \\\\")
})

tex <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\begin{threeparttable}",
  "\\caption{First-Stage Estimates: Instrument Relevance}",
  "\\label{tab:first_stage}",
  "\\begin{tabular}{lccccc}",
  "\\specialrule{1pt}{0pt}{1.5pt}",
  "\\specialrule{0.4pt}{0pt}{3pt}",
  "Dependent variable ($y^j$) & $\\beta$ & s.e. & F (robust) & F (OLS) & N \\\\",
  "\\midrule",
  cuerpo,
  "\\specialrule{0.4pt}{3pt}{1.5pt}",
  "\\specialrule{1pt}{0pt}{0pt}",
  "\\end{tabular}",
  "\\end{threeparttable}",
  "\\end{table}"
)

dir.create(dir_tab, showWarnings = FALSE)
writeLines(unname(tex), file.path(dir_tab, archivo_tex))
cat(tex, sep = "\n")   # para revisarla en la consola

############################################################
####### 4. Estimation
############################################################
# Estima las IRF del modelo IV-LP (ecuaciones 6–8) para cada
# variable dependiente y^j con lp_lin_iv (lpirfs) y guarda cada
# figura en la carpeta images como ivlp_irf_<grupo>_<código>.png.
#
# Especificación estándar
# - endog_data: variable dependiente y^j, con 4 rezagos.
# - shock:      precio real del cobre, p_t (regresor endógeno).
# - instrum:    shock CSIM trimestral (csim), estimado en el SVAR.
# - exog_data:  controles con 4 rezagos. Son consumo de gobierno,
#               inversión, VIX, TCR y el propio p_t. Un control se
#               excluye solo cuando es la variable dependiente.
# - Estimación por 2SLS, con tendencia lineal y errores estándar
#   Newey-West.
# - Horizonte de 12 trimestres e intervalos de confianza al 90%
#   (confint = 1.65).
#
# La especificación de cada variable está en la tabla 'spec'. La única
# excepción al estándar son los salarios sectoriales, que solo están
# disponibles desde 2017 (2 rezagos, horizonte de 4 trimestres y sin
# tendencia).
#
# Las IRF de variables en logaritmos son elasticidades. Las de
# balanza de pagos (% del PIB) y tasas de interés son
# semielasticidades.
#
# Grupos (prefijo del nombre de archivo)
# - gdp:      PIB total, minero, NMT y NT (nominal deflactado por IPC)
# - abs:    absorción interna y sus componentes
# - sec:    PIB sectoriales
# - trade:  exportaciones totales e importaciones
# - prices: TCR, tipo de cambio nominal e IPC
# - labor:  empleo y salarios
# - bop:    balanza de pagos
# - money:  agregados monetarios
# - rates:  tasas de interés
# - other:  IPSA y precio de la vivienda
############################################################

# Parámetros de las figuras ----------------------------------------
# Las figuras se guardan al tamaño aproximado en que se verán en el
# paper, para que el texto no quede pequeño al escalarlas. Si cambia
# el ancho con que se insertan en LaTeX, ajusta estos valores.
# Después de guardar cada PNG se le quita el canal alfa (transparencia)
# con magick. La figura se ve igual, pero LaTeX la procesa mucho más
# rápido al compilar.

dir_fig    <- "images"
fig_ancho  <- 5.5   # pulgadas
fig_alto   <- 4.6   # pulgadas
fig_dpi    <- 200   # resolución del PNG
tam_titulo <- 14    # tamaño de los títulos de los ejes
tam_texto  <- 13    # tamaño de los números de los ejes

dir.create(dir_fig, showWarnings = FALSE)

ylab_el    <- "Elasticity"
ylab_pp    <- "Base points"
ylab_ppgdp <- "Percentage points of GDP"

# Bases de datos ----------------------------------------------------
# 'dt' es la base principal. Las demás son submuestras para series
# con otra cobertura, que se agregan aparte para no cambiar la
# muestra principal:
# - Salarios sectoriales (desde 2017), spread Chile–EE. UU. (desde
#   2004), BTU-10 en UF y tasa onshore en USD (desde 2007). No se
#   desestacionalizan (son salarios y tasas de interés).

submuestra <- function(archivo, hoja, ajustar = character(0)) {
  extra <- read_excel(file.path("ivlp data", archivo), sheet = hoja)
  d <- merge(dt, extra, by = "year", all = TRUE)
  d <- na.omit(d)
  d <- d %>% arrange(year)
  if (length(ajustar) > 0) {
    ini <- inicio_ts(d$year)
    for (v in ajustar) d[[paste0(v, "_sa")]] <- sa_x13(d[[v]], v, ini)
  }
  d
}

bases <- list(
  dt       = dt,
  salarios = submuestra("wages_rama.xlsx", "wages_rama"),
  spread   = submuestra("spread.xlsx", "spread"),
  btu      = submuestra("btu.xlsx", "btu"),
  onshore  = submuestra("onshore.xlsx", "onshore")
)

# Especificación por variable ----------------------------------------
# grupo:   prefijo del archivo (ver lista en el preámbulo)
# codigo:  identificador de la variable dentro del grupo
#          (archivo: ivlp_irf_<grupo>_<codigo>.png)
# y:       columna de la variable dependiente
# base:    base de datos (elemento de 'bases')
# excluir: control que se excluye por ser la variable dependiente
# lags:    rezagos de y^j y de los controles
# hor:     horizonte de la IRF (trimestres)
# trend:   1 = tendencia lineal, 0 = sin tendencia
# ylab:    etiqueta del eje y

spec <- tribble(
  ~grupo,   ~codigo,         ~y,             ~base,      ~excluir, ~lags, ~hor, ~trend, ~ylab,
  # PIB y sectores transable y no transable
  "gdp",    "total",         "pib_sa",       "dt",       NA,       4,     12,   1,      ylab_el,
  "gdp",    "mining",        "min_sa",       "dt",       NA,       4,     12,   1,      ylab_el,
  "gdp",    "nmt",           "t_nc_sa",      "dt",       NA,       4,     12,   1,      ylab_el,
  "gdp",    "nt",            "nt_sa",        "dt",       NA,       4,     12,   1,      ylab_el,
  # Absorción interna
  "abs",    "total",         "abs_sa",       "dt",       NA,       4,     12,   1,      ylab_el,
  "abs",    "investment",    "i_sa",         "dt",       "inv",    4,     12,   1,      ylab_el,
  "abs",    "cons_hh",       "c_h_sa",       "dt",       NA,       4,     12,   1,      ylab_el,
  "abs",    "cons_gov",      "c_g_sa",       "dt",       "c_g",    4,     12,   1,      ylab_el,
  # PIB sectoriales (minería está en el grupo gdp)
  "sec",    "agriculture",   "agro_sa",      "dt",       NA,       4,     12,   1,      ylab_el,
  "sec",    "fishing",       "pesca_sa",     "dt",       NA,       4,     12,   1,      ylab_el,
  "sec",    "manufacturing", "manuf_sa",     "dt",       NA,       4,     12,   1,      ylab_el,
  "sec",    "utilities",     "ut_sa",        "dt",       NA,       4,     12,   1,      ylab_el,
  "sec",    "construction",  "cons_sa",      "dt",       NA,       4,     12,   1,      ylab_el,
  "sec",    "commerce",      "com_sa",       "dt",       NA,       4,     12,   1,      ylab_el,
  "sec",    "transport",     "trans_sa",     "dt",       NA,       4,     12,   1,      ylab_el,
  "sec",    "fin_serv",      "serv_f_sa",    "dt",       NA,       4,     12,   1,      ylab_el,
  "sec",    "comm_serv",     "serv_com_sa",  "dt",       NA,       4,     12,   1,      ylab_el,
  "sec",    "housing_serv",  "serv_viv_sa",  "dt",       NA,       4,     12,   1,      ylab_el,
  "sec",    "personal_serv", "serv_p_sa",    "dt",       NA,       4,     12,   1,      ylab_el,
  # Exportaciones e importaciones
  "trade",  "exp_total",     "exp_tot_sa",   "dt",       NA,       4,     12,   1,      ylab_el,
  "trade",  "imports",       "imp_sa",       "dt",       NA,       4,     12,   1,      ylab_el,
  # Precios
  "prices", "rer",           "tcr_sa",       "dt",       "tcr",    4,     12,   1,      ylab_el,
  "prices", "ner",           "usd_sa",       "dt",       NA,       4,     12,   1,      ylab_el,
  "prices", "cpi",           "ipc_sa",       "dt",       NA,       4,     12,   1,      ylab_el,
  # Mercado laboral
  "labor",  "emp",           "ocup_sa",      "dt",       NA,       4,     12,   1,      ylab_el,
  "labor",  "emp_mining",    "ocup_min_sa",  "dt",       NA,       4,     12,   1,      ylab_el,
  "labor",  "emp_nmt",       "ocup_t_nm_sa", "dt",       NA,       4,     12,   1,      ylab_el,
  "labor",  "emp_nt",        "ocup_nt_sa",   "dt",       NA,       4,     12,   1,      ylab_el,
  "labor",  "wage",          "irr_sa",       "dt",       NA,       4,     12,   1,      ylab_el,
  "labor",  "wage_mining",   "wage_min",     "salarios", NA,       2,     4,    0,      ylab_el,
  "labor",  "wage_nmt",      "wage_tnm",     "salarios", NA,       2,     4,    0,      ylab_el,
  "labor",  "wage_nt",       "wage_nt",      "salarios", NA,       2,     4,    0,      ylab_el,
  # Balanza de pagos (% del PIB)
  "bop",    "ca",            "cc_sa",        "dt",       NA,       4,     12,   1,      ylab_ppgdp,
  "bop",    "tb",            "bc_sa",        "dt",       NA,       4,     12,   1,      ylab_ppgdp,
  "bop",    "fa",            "cf_sa",        "dt",       NA,       4,     12,   1,      ylab_ppgdp,
  # Agregados monetarios
  "money",  "m1",            "m1_sa",        "dt",       NA,       4,     12,   1,      ylab_el,
  "money",  "m2",            "m2_sa",        "dt",       NA,       4,     12,   1,      ylab_el,
  "money",  "m3",            "m3_sa",        "dt",       NA,       4,     12,   1,      ylab_el,
  # Tasas de interés
  "rates",  "mpr",           "tpm_sa",       "dt",       NA,       4,     12,   1,      ylab_pp,
  "rates",  "spread",        "spread",       "spread",   NA,       4,     12,   1,      ylab_pp,
  "rates",  "btu10",         "btu",          "btu",      NA,       4,     12,   1,      ylab_pp,
  "rates",  "onshore",       "onshore",      "onshore",  NA,       4,     12,   1,      ylab_pp,
  # Otros
  "other",  "ipsa",          "ipsa_sa",      "dt",       NA,       4,     12,   1,      ylab_el,
  "other",  "house_prices",  "ipv_sa",       "dt",       NA,       4,     12,   1,      ylab_el
) %>%
  mutate(id = paste(grupo, codigo, sep = "_"))

# Funciones -----------------------------------------------------------

estimar_irf <- function(y, datos, excluir = character(0),
                        lags = 4, hor = 12, trend = 1) {
  ctrl <- controles_base[!names(controles_base) %in% excluir]
  lp_lin_iv(
    endog_data     = data.frame(y = datos[[y]]),
    exog_data      = as.data.frame(lapply(ctrl, function(v) datos[[v]])),
    shock          = data.frame(p = datos$p_sa),
    instrum        = data.frame(csim = datos$csim_sa),
    use_twosls     = TRUE,
    lags_endog_lin = lags,
    lags_exog      = lags,
    hor            = hor,
    confint        = 1.65,
    use_nw         = TRUE,
    trend          = trend
  )
}

graficar_irf <- function(modelo, ylab, id) {
  g <- plot_lin(modelo)[[1]] +
    labs(x = "Quarters after shock", y = ylab, title = "") +
    theme_minimal() +
    theme(
      axis.title       = element_text(size = tam_titulo),
      axis.text        = element_text(size = tam_texto),
      panel.grid.minor = element_blank()
    )
  archivo <- file.path(dir_fig, paste0("ivlp_irf_", id, ".png"))
  ggsave(
    filename = archivo,
    plot     = g,
    width    = fig_ancho,
    height   = fig_alto,
    units    = "in",
    dpi      = fig_dpi,
    bg       = "white"
  )
  # Quita el canal alfa (deja el PNG en RGB sobre fondo blanco)
  img <- image_read(archivo)
  img <- image_background(img, "white", flatten = TRUE)
  img <- image_convert(img, type = "TrueColor")
  image_write(img, archivo, format = "png")
  g
}

# Estimación y figuras ------------------------------------------------

faltantes <- spec$y[!mapply(function(y, b) y %in% names(bases[[b]]),
                            spec$y, spec$base)]
if (length(faltantes) > 0) {
  stop("Variables no encontradas: ", paste(faltantes, collapse = ", "))
}

resultados <- list()
graficos   <- list()

for (k in seq_len(nrow(spec))) {
  s <- spec[k, ]
  excl <- if (is.na(s$excluir)) character(0) else s$excluir
  modelo <- estimar_irf(s$y, bases[[s$base]], excl, s$lags, s$hor, s$trend)
  resultados[[s$id]] <- modelo
  graficos[[s$id]]   <- graficar_irf(modelo, s$ylab, s$id)
}

# Para ver una figura en particular, por ejemplo graficos$gdp_nmt
