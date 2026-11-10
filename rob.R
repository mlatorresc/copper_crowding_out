############################################################
####### 5. Robustness
############################################################
# Ejercicios de robustez para el grupo gdp (PIB total, minero, NMT y
# NT, nominales deflactados por IPC). Se corre después de la sección 4,
# con 'spec', 'bases', 'controles_base' y 'resultados' en memoria.
#
# - 5.1 Inferencia tF: inferencia robusta a instrumento débil (Lee
#   et al., 2022). Ajusta el valor crítico del estadístico t según el
#   F robusto de primera etapa en cada horizonte. Intervalos al 95%.
# - 5.2 Instrumento alternativo: shocks CSIM de un SVAR con el índice
#   de producción industrial mundial (WIP) de Baumeister y Hamilton
#   en vez de la IGREA.
# - 5.3 Dummies de crisis y shocks sectoriales: crisis financiera
#   global (2008Q4), COVID (2020Q2) y crisis del virus ISA en la
#   salmonicultura (2007Q3–2010Q4, una sola dummy).
# - 5.4 Controles mínimos: sin consumo de gobierno, inversión, TCR
#   ni VIX. Se mantienen los rezagos de y^j y del precio del cobre.
#
# Salvo el cambio indicado en cada ejercicio, la especificación es la
# de la sección 4. Las figuras se guardan como
# rob_ivlp_irf_gdp_<código>_<ejercicio>.png, con <ejercicio> igual a
# tf, wip, crisis o minctrl.
#
# Nota sobre el código
# Esta versión es una sistematización y simplificación de un código
# anterior del autor, realizada con la asistencia de Claude
# (Anthropic) para hacerlo replicable. 
############################################################


rm(list = ls())

library(tidyverse)   # manejo de datos (dplyr, purrr)
library(ggplot2)     # figuras
library(readxl)      # importar bases de datos
library(writexl)     # exportar resultados
library(seasonal)    # X-13ARIMA-SEATS
library(lpirfs)      # IV-LP
library(magick)      # quitar canal alfa de los PNG
library(sandwich)    # Newey-West (robustez)
library(ivDiag)      # tF de Lee, McCrary, Moreira y Porter (2022) (robustez)


############################################################
####### 1. Data
############################################################
# Carga todas las series trimestrales y las une en una sola base (dt)
# usando la columna 'year'. Después se eliminan los trimestres con
# datos faltantes en cualquier variable y se ordena la base
# cronológicamente.
#
# El instrumento de la especificación principal son los shocks CSIM del
# SVAR con IGREA (csim_shocks.xlsx). Los shocks del SVAR con WIP se usan
# en la sección 5.2.
############################################################

dir_datos <- "ivlp data"

leer <- function(archivo, hoja) read_excel(file.path(dir_datos, archivo), sheet = hoja)

archivos <- tribble(
  ~archivo,               ~hoja,
  # Especificación principal
  "price_cu_q.xlsx",      "p",
  "csim_shocks.xlsx",     "csim",
  "exp_nom.xlsx",         "exp",
  "pib_d.xlsx",           "pib_d",
  "pib_o_nom.xlsx",       "pib_o",
  "tcr.xlsx",             "tcr",
  "m.xlsx",               "m",
  "tpm.xlsx",             "tpm",
  "vix.xlsx",             "vix",
  "usd.xlsx",             "usd",
  "ipc.xlsx",             "ipc",
  "ocup.xlsx",            "ocup",
  "ocup_rama.xlsx",       "ocup_rama",
  "wages.xlsx",           "irr",
  "cc_cf.xlsx",           "cc_cf",
  "tasa_cap.xlsx",        "tasa_cap",
  "ipsa.xlsx",            "ipsa",
  "ipv.xlsx",             "ipv",
  # Robustez
  "rob_pib_o_real.xlsx",  "pib_o_real",
  "rob_pib_d_real.xlsx",  "pib_d_real",
  "rob_pib_d_nom.xlsx",   "pib_d_nom"
)

dt <- map2(archivos$archivo, archivos$hoja, leer) %>%
  reduce(merge, by = "year", all = TRUE) %>%
  na.omit() %>%
  arrange(year)


############################################################
####### 2. Ajuste estacional
############################################################
# X-13ARIMA-SEATS (módulo X-11), aplicado solo a las variables de
# actividad chilenas. El resto de las series no se ajusta.
#
# Todas las variables tienen su versión *_sa. Las series que no se
# ajustan tienen *_sa idéntica a la serie original.
############################################################

vars_actividad <- c(
  # PIB y demanda interna
  "pib", "c_h", "c_g", "i", "abs",
  # PIB sectoriales
  "agro", "min", "pesca", "manuf", "ut", "cons", "com", "trans",
  "serv_f", "serv_com", "serv_viv", "serv_p",
  # Agregados sectoriales
  "t_nc", "nt",
  # Comercio exterior
  "exp_nomin", "exp_min", "exp_tot", "imp",
  # Empleo
  "ocup", "ocup_min", "ocup_t_nm", "ocup_nt",
  # Robustez: PIB por origen, real
  "pib_o_real", "pib_min_o_real", "pib_t_nm_o_real", "pib_nt_o_real",
  # Robustez: PIB y exportaciones por demanda, real
  "pib_d_real", "exp_d_real", "exp_min_d_real", "exp_nomin_d_real",
  # Robustez: PIB y exportaciones por demanda, nominal
  "pib_d_nom", "exp_d_nom", "exp_min_d_nom", "exp_nomin_d_nom"
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
if (length(faltantes) > 0) stop("No están en dt: ", paste(faltantes, collapse = ", "))

# Fecha de inicio para los objetos ts.
# Acepta 'year' como Date/POSIXct o como texto tipo "2002Q1", "2002-Q1",
# "2002 Q1".
inicio_ts <- function(x) {
  x1 <- x[1]
  if (inherits(x1, c("Date", "POSIXct", "POSIXt"))) {
    return(c(as.numeric(format(x1, "%Y")),
             (as.numeric(format(x1, "%m")) - 1) %/% 3 + 1))
  }
  s <- toupper(as.character(x1))
  if (grepl("^[0-9]{4}[- ]?Q[1-4]$", s)) {
    return(c(as.numeric(substr(s, 1, 4)), as.numeric(substr(s, nchar(s), nchar(s)))))
  }
  stop("Formato de 'year' no reconocido (", x1, "). Define inicio <- c(año, trimestre).")
}

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

for (v in vars_actividad) dt[[paste0(v, "_sa")]] <- sa_x13(dt[[v]], v, inicio)
dt[paste0(vars_sin_ajuste, "_sa")] <- dt[vars_sin_ajuste]


############################################################
####### 3. Controles
############################################################
# Controles del IV-LP (entran con rezagos). El nombre de cada elemento
# es el que se usa en la columna 'excluir' de 'spec'.
############################################################

controles_base <- c(
  c_g = "c_g_sa",   # consumo de gobierno
  inv = "i_sa",     # inversión
  tcr = "tcr_sa",   # tipo de cambio real
  vix = "vix_sa",   # VIX
  p   = "p_sa"      # precio real del cobre
)


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
# - exog_data:  controles con 4 rezagos (sección 3). Un control se
#               excluye solo cuando es la variable dependiente.
# - Estimación por 2SLS, con tendencia lineal y errores estándar
#   Newey-West.
# - Horizonte de 12 trimestres e intervalos de confianza al 90%
#   (confint = 1.65).
#
# La única excepción al estándar son los salarios sectoriales, que solo
# están disponibles desde 2017 (2 rezagos, horizonte de 4 trimestres y
# sin tendencia).
#
# Las IRF de variables en logaritmos son elasticidades. Las de
# balanza de pagos (% del PIB) y tasas de interés son
# semielasticidades.
#
# Grupos (prefijo del nombre de archivo)
# - gdp:    PIB total, minero, NMT y NT (nominal deflactado por IPC)
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
# Tamaño aproximado al que se ven en el paper. Si cambia el ancho con
# que se insertan en LaTeX, ajusta estos valores.

dir_fig    <- "images"
fig_ancho  <- 5.5   # pulgadas
fig_alto   <- 4.6   # pulgadas
fig_dpi    <- 200
tam_titulo <- 14    # títulos de los ejes
tam_texto  <- 13    # números de los ejes
z_ci       <- 1.65  # confint (90%)

dir.create(dir_fig, showWarnings = FALSE)

ylab_el    <- "Elasticity"
ylab_pp    <- "Base points"
ylab_ppgdp <- "Percentage points of GDP"

# Bases de datos ----------------------------------------------------
# 'dt' es la base principal. Las demás son submuestras para series con
# otra cobertura, que se agregan aparte para no cambiar la muestra
# principal: salarios sectoriales (desde 2017), spread Chile–EE. UU.
# (desde 2004), BTU-10 en UF y tasa onshore en USD (desde 2007). No se
# desestacionalizan.

submuestra <- function(archivo, hoja, ajustar = character(0)) {
  d <- merge(dt, leer(archivo, hoja), by = "year", all = TRUE) %>%
    na.omit() %>%
    arrange(year)
  if (length(ajustar) > 0) {
    ini <- inicio_ts(d$year)
    for (v in ajustar) d[[paste0(v, "_sa")]] <- sa_x13(d[[v]], v, ini)
  }
  d
}

bases <- list(
  dt       = dt,
  salarios = submuestra("wages_rama.xlsx", "wages_rama"),
  spread   = submuestra("spread.xlsx",     "spread"),
  btu      = submuestra("btu.xlsx",        "btu"),
  onshore  = submuestra("onshore.xlsx",    "onshore")
)

# Especificación por variable ----------------------------------------
# grupo:   prefijo del archivo
# codigo:  identificador de la variable dentro del grupo
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

excluidos <- function(s) if (is.na(s$excluir)) character(0) else s$excluir

estimar_irf <- function(s, datos) {
  ctrl <- controles_base[!names(controles_base) %in% excluidos(s)]
  lp_lin_iv(
    endog_data     = data.frame(y = datos[[s$y]]),
    exog_data      = as.data.frame(lapply(ctrl, function(v) datos[[v]])),
    shock          = data.frame(p = datos$p_sa),
    instrum        = data.frame(csim = datos$csim_sa),
    use_twosls     = TRUE,
    lags_endog_lin = s$lags,
    lags_exog      = s$lags,
    hor            = s$hor,
    confint        = z_ci,
    use_nw         = TRUE,
    trend          = s$trend
  )
}

tema_irf <- theme_minimal() +
  theme(
    axis.title       = element_text(size = tam_titulo),
    axis.text        = element_text(size = tam_texto),
    panel.grid.minor = element_blank()
  )

# Guarda el PNG y le quita el canal alfa (RGB sobre fondo blanco)
guardar_png <- function(g, nombre) {
  archivo <- file.path(dir_fig, paste0(nombre, ".png"))
  ggsave(archivo, plot = g, width = fig_ancho, height = fig_alto,
         units = "in", dpi = fig_dpi, bg = "white")
  image_read(archivo) %>%
    image_background("white", flatten = TRUE) %>%
    image_convert(type = "TrueColor") %>%
    image_write(archivo, format = "png")
}

# Archivo: <prefijo>ivlp_irf_<id><sufijo>.png
graficar_irf <- function(modelo, ylab, id, prefijo = "", sufijo = "") {
  g <- plot_lin(modelo)[[1]] +
    labs(x = "Quarters after shock", y = ylab, title = "") +
    tema_irf
  guardar_png(g, paste0(prefijo, "ivlp_irf_", id, sufijo))
  g
}

# Estima y grafica todas las filas de una spec. En robustez se usa
# prefijo = "rob_" y un sufijo que identifica el ejercicio (p. ej. "_wip").
estimar_spec <- function(spec_sub, bases, prefijo = "", sufijo = "") {
  faltan <- spec_sub$y[!map2_lgl(spec_sub$y, spec_sub$base, ~ .x %in% names(bases[[.y]]))]
  if (length(faltan) > 0) stop("Variables no encontradas: ", paste(faltan, collapse = ", "))
  
  res <- list()
  for (k in seq_len(nrow(spec_sub))) {
    s   <- spec_sub[k, ]
    mod <- estimar_irf(s, bases[[s$base]])
    graficar_irf(mod, s$ylab, s$id, prefijo, sufijo)
    res[[s$id]] <- mod
  }
  res
}

# Estimación principal ------------------------------------------------

resultados <- estimar_spec(spec, bases)

# Para ver una figura en particular, por ejemplo plot_lin(resultados$gdp_nmt)


############################################################
####### 5. Robustness
############################################################

spec_gdp <- filter(spec, grupo == "gdp")


############################################################
####### 5.1 Inferencia robusta a instrumento débil (tF)
############################################################
# Grupo gdp (PIB total, minero, NMT y NT, nominales deflactados por IPC).
#
# - coef y s.e. salen de los resultados de lp_lin_iv. El s.e. se
#   recupera de la banda, (irf_lin_up - irf_lin_mean) / z_ci.
# - lp_lin_iv no reporta la primera etapa. El F robusto se calcula
#   aparte en cada horizonte, con la misma especificación y en la
#   muestra de ese horizonte (se pierden las últimas h observaciones).
# - ivDiag::tF() usa valores críticos al 5%, así que los intervalos tF
#   son del 95%.
# - Con hor = 12, lp_lin_iv entrega h = 0, ..., 11.
#
# Chequeo: el F en h = 0 debe coincidir con la tabla de primera etapa
# del Anexo. Si no coincide, ajusta nw_lag_fs().
############################################################

nw_lag_fs <- function(h) h + 1       # rezago Newey-West de la primera etapa

lag_k <- function(x, k) c(rep(NA, k), head(x, -k))

# F robusto de primera etapa para la fila 's' de spec en el horizonte h
f_robusto <- function(s, datos, h) {
  ctrl <- controles_base[!names(controles_base) %in% excluidos(s)]
  n    <- nrow(datos)
  rez  <- c(list(datos[[s$y]]), lapply(ctrl, function(v) datos[[v]]))
  
  d <- data.frame(p = datos$p_sa, z = datos$csim_sa)
  if (s$trend == 1) d$trend <- seq_len(n)
  for (j in seq_along(rez)) {
    for (k in 1:s$lags) d[[paste0("x", j, "_l", k)]] <- lag_k(rez[[j]], k)
  }
  d <- na.omit(d[seq_len(n - h), ])
  
  m <- lm(p ~ ., data = d)
  V <- NeweyWest(m, lag = nw_lag_fs(h), prewhite = FALSE, adjust = FALSE)
  c(F = unname(coef(m)["z"]^2 / V["z", "z"]), N = nrow(d))
}

# tF en todos los horizontes de una variable
tf_variable <- function(s, mod, datos) {
  b  <- as.numeric(mod$irf_lin_mean)
  se <- (as.numeric(mod$irf_lin_up) - b) / z_ci
  h  <- seq_along(b) - 1
  fs <- sapply(h, function(hh) f_robusto(s, datos, hh))
  cF <- mapply(function(bi, si, Fi) unname(ivDiag::tF(coef = bi, se = si, Fstat = Fi)["cF"]),
               b, se, fs["F", ])
  tibble(variable = s$id, h = h, N = fs["N", ], F_rob = fs["F", ],
         coef = b, se = se, cF = cF)
}

# Tabla tF para una spec, sus resultados y sus bases
tabla_tf <- function(spec_sub, res, bases) {
  detalle <- map_dfr(seq_len(nrow(spec_sub)), function(k) {
    s <- spec_sub[k, ]
    tf_variable(s, res[[s$id]], bases[[s$base]])
  }) %>%
    mutate(
      t        = coef / se,
      sig_std  = abs(t) > 1.96,      # 5% convencional
      sig_tF   = abs(t) > cF,        # 5% con ajuste tF
      ci_tF_lo = coef - cF * se,
      ci_tF_hi = coef + cF * se
    )
  resumen <- detalle %>%
    group_by(variable) %>%
    summarise(
      F_h0         = F_rob[h == 0],
      F_min        = min(F_rob),
      n_sig_std    = sum(sig_std),
      n_sig_tF     = sum(sig_tF),
      h_pierde_sig = paste(h[sig_std & !sig_tF], collapse = ", ")
    )
  list(detalle = detalle, resumen = resumen)
}

# Figura tF: coeficiente, banda estándar del 90% y banda tF del 95%.
# Archivo: rob_ivlp_irf_<id><sufijo>.png
graficar_tf <- function(detalle, spec_sub, sufijo = "_tf") {
  for (k in seq_len(nrow(spec_sub))) {
    s <- spec_sub[k, ]
    d <- filter(detalle, variable == s$id)
    g <- ggplot(d, aes(x = h)) +
      geom_ribbon(aes(ymin = ci_tF_lo, ymax = ci_tF_hi, fill = "tF 95%")) +
      geom_ribbon(aes(ymin = coef - z_ci * se, ymax = coef + z_ci * se,
                      fill = "Standard 90%")) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_line(aes(y = coef), linewidth = 0.8) +
      scale_fill_manual(values = c("tF 95%" = "grey85", "Standard 90%" = "grey60"),
                        breaks = c("Standard 90%", "tF 95%"), name = NULL) +
      labs(x = "Quarters after shock", y = s$ylab) +
      tema_irf +
      theme(legend.position = "bottom", legend.text = element_text(size = tam_texto))
    guardar_png(g, paste0("rob_ivlp_irf_", s$id, sufijo))
  }
}

tf_main <- tabla_tf(spec_gdp, resultados, bases)
print(tf_main$resumen)
graficar_tf(tf_main$detalle, spec_gdp, sufijo = "_tf")


############################################################
####### 5.2 Instrumento alternativo: SVAR con WIP
############################################################
# Reestima el IV-LP del grupo gdp (PIB total, minero, NMT y NT) usando
# como instrumento los shocks CSIM del SVAR con WIP en vez de IGREA.
# El resto de la especificación no cambia.
# Figuras: rob_ivlp_irf_gdp_<código>_wip.png
############################################################

dt_wip <- dt %>%
  select(-csim, -csim_sa) %>%
  merge(leer("csim_wip_shocks.xlsx", "csim"), by = "year") %>%
  arrange(year) %>%
  mutate(csim_sa = csim)

range(dt_wip$year)   # compara con range(dt$year)

bases_wip <- modifyList(bases, list(dt = dt_wip))

resultados_wip <- estimar_spec(spec_gdp, bases_wip, prefijo = "rob_", sufijo = "_wip")


############################################################
####### 5.3 Dummies de crisis y shocks sectoriales
############################################################
# Reestima el IV-LP del grupo gdp agregando tres dummies contemporáneas.
# - d_gfc:   crisis financiera global (2008Q4)
# - d_covid: COVID (2020Q2)
# - d_isa:   crisis del virus ISA en la salmonicultura (2007Q3–2010Q4),
#            una sola dummy igual a 1 en todos los trimestres del episodio
# Entran en ambas etapas vía contemp_data. El resto de la especificación
# no cambia.
# Figuras: rob_ivlp_irf_gdp_<código>_crisis.png
############################################################

# Secuencia de trimestres "AAAAQn" entre dos fechas
seq_q <- function(desde, hasta) {
  a <- as.numeric(substr(desde, 1, 4)); q <- as.numeric(substr(desde, 6, 6))
  out <- character(0)
  repeat {
    s <- paste0(a, "Q", q); out <- c(out, s)
    if (s == hasta) break
    q <- q + 1; if (q > 4) { q <- 1; a <- a + 1 }
  }
  out
}

# Convierte 'year' a texto "AAAAQn"
trimestre <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXt"))) {
    paste0(format(x, "%Y"), "Q", (as.numeric(format(x, "%m")) - 1) %/% 3 + 1)
  } else {
    gsub("[- ]", "", toupper(as.character(x)))
  }
}

dummies <- list(
  d_gfc   = "2008Q4",
  d_covid = "2020Q2",
  d_isa   = seq_q("2007Q3", "2010Q4")
)

dt_crisis <- dt
for (d in names(dummies)) {
  dt_crisis[[d]] <- as.numeric(trimestre(dt_crisis$year) %in% dummies[[d]])
}
# Cada dummy debe marcar exactamente sus trimestres (1, 1 y 14)
stopifnot(colSums(dt_crisis[names(dummies)]) == lengths(dummies))

estimar_irf_crisis <- function(s, datos) {
  ctrl <- controles_base[!names(controles_base) %in% excluidos(s)]
  lp_lin_iv(
    endog_data     = data.frame(y = datos[[s$y]]),
    exog_data      = as.data.frame(lapply(ctrl, function(v) datos[[v]])),
    contemp_data   = datos[names(dummies)],
    shock          = data.frame(p = datos$p_sa),
    instrum        = data.frame(csim = datos$csim_sa),
    use_twosls     = TRUE,
    lags_endog_lin = s$lags,
    lags_exog      = s$lags,
    hor            = s$hor,
    confint        = z_ci,
    use_nw         = TRUE,
    trend          = s$trend
  )
}

resultados_crisis <- list()
for (k in seq_len(nrow(spec_gdp))) {
  s   <- spec_gdp[k, ]
  mod <- estimar_irf_crisis(s, dt_crisis)
  graficar_irf(mod, s$ylab, s$id, prefijo = "rob_", sufijo = "_crisis")
  resultados_crisis[[s$id]] <- mod
}

############################################################
####### 5.4 Controles mínimos
############################################################
# Reestima el IV-LP del grupo gdp sin los controles macroeconómicos
# (consumo de gobierno, inversión, TCR y VIX). Se mantienen los
# rezagos de y^j y del precio del cobre, y la tendencia.
# Figuras: rob_ivlp_irf_gdp_<código>_minctrl.png
############################################################

controles_min <- controles_base["p"]

estimar_irf_ctrl <- function(s, datos, ctrl) {
  lp_lin_iv(
    endog_data     = data.frame(y = datos[[s$y]]),
    exog_data      = as.data.frame(lapply(ctrl, function(v) datos[[v]])),
    shock          = data.frame(p = datos$p_sa),
    instrum        = data.frame(csim = datos$csim_sa),
    use_twosls     = TRUE,
    lags_endog_lin = s$lags,
    lags_exog      = s$lags,
    hor            = s$hor,
    confint        = z_ci,
    use_nw         = TRUE,
    trend          = s$trend
  )
}

resultados_minctrl <- list()
for (k in seq_len(nrow(spec_gdp))) {
  s   <- spec_gdp[k, ]
  mod <- estimar_irf_ctrl(s, bases[[s$base]], controles_min)
  graficar_irf(mod, s$ylab, s$id, prefijo = "rob_", sufijo = "_minctrl")
  resultados_minctrl[[s$id]] <- mod
}
