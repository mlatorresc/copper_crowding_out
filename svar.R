############################################################
####### SVAR: identificación de shocks CSIM
############################################################
# Paper: "Copper Price Booms and Tradable Sector Crowding-Out in
#         Chile (2003–2024)", Martín Latorre.
#
# Este script estima la primera parte de la estrategia empírica
# (modelo SVAR, ecuaciones 1–4). Identifica los shocks CSIM
# (copper-specific international market shocks), que luego se usan
# como instrumento en el script del IV-LP.
#
# Etapas
# 1. Datos. Carga las series mensuales y las une en una sola base.
# 2. VAR en forma reducida. Variables en primera diferencia
#    logarítmica, con constante y rezagos elegidos por AIC. El orden
#    de las variables (q, igrea, vix, dxy, p) define la identificación
#    recursiva (Cholesky).
# 3. IRF. Respuesta del precio del cobre a cada shock estructural,
#    con un horizonte de 36 meses e intervalos al 90% por bootstrap.
# 4. Shocks estructurales. Recupera los shocks a partir de los
#    residuos del VAR y la descomposición de Cholesky, y los exporta
#    a csim_shocks.xlsx. El shock CSIM corresponde a la columna p.
# 5. FEVD. Descomposición de varianza del error de predicción del
#    precio del cobre, con la misma identificación.
#
# Las figuras se guardan en la carpeta images.
#
# Nota sobre el código
# Esta versión es una sistematización y simplificación de un código
# anterior del autor, realizada con la asistencia de Claude
# (Anthropic) para hacerlo replicable. 
############################################################

rm(list = ls())

library(urca)
library(vars)        # VAR, irf y fevd
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

set.seed(1234)       # para que los intervalos por bootstrap sean replicables

# ---------------------------------------------------------------
# Parámetros
# ---------------------------------------------------------------

# Especificación del VAR y de las IRF
vars_svar   <- c("q", "igrea", "vix", "dxy", "p")  # orden = identificación
lag_max     <- 24     # rezago máximo para la selección por AIC
n_ahead     <- 36     # horizonte de IRF y FEVD (meses)
boot_runs   <- 10000    # repeticiones del bootstrap
ci_nivel    <- 0.9    # nivel de los intervalos de confianza

# Figuras
dir_fig        <- "images"
fig_ancho      <- 3.2   # IRF, pulgadas
fig_alto       <- 2.4
fevd_ancho     <- 6.5   # FEVD (lleva leyenda), pulgadas
fevd_alto      <- 4.0
fig_dpi        <- 300
tam_titulo     <- 11    # títulos de los ejes
tam_texto      <- 10    # números de los ejes
tam_leyenda    <- 10    # leyenda de la FEVD

dir.create(dir_fig, showWarnings = FALSE)

guardar_fig <- function(g, nombre, ancho = fig_ancho, alto = fig_alto) {
  ggsave(
    filename = file.path(dir_fig, paste0(nombre, ".png")),
    plot     = g,
    width    = ancho,
    height   = alto,
    units    = "in",
    dpi      = fig_dpi,
    bg       = "white"
  )
}

# Nombres de los shocks: código para archivos y etiqueta para la FEVD
shocks <- tribble(
  ~var,    ~codigo,        ~etiqueta,
  "q",     "chile_supply", "Chilean supply shock",
  "igrea", "igrea",        "IGREA shock",
  "vix",   "vix",          "VIX shock",
  "dxy",   "dxy",          "DXY shock",
  "p",     "csim",         "CSIM (p) shock"
)


############################################################
####### 1. Data
############################################################
# Series mensuales, en primera diferencia logarítmica (ver la tabla
# de fuentes en el Anexo del paper). La base se restringe a los meses
# con datos en todas las series cargadas.
############################################################

p <- read_excel("svar data/precio_cu.xlsx", sheet = "precio_cu")     # precio nominal del cobre LME (spot), Cochilco
igrea <- read_excel("svar data/igrea.xlsx", sheet = "Kilian Index")  # Index of Global Real Economic Activity, Kilian (2009)
wip <- read_excel("svar data/wip.xlsx", sheet = "wip")               # World Industrial Production (WIP), 1958M1–2026M8
q <- read_excel("svar data/prod_chile.xlsx", sheet = "prod")         # producción de cobre de Chile, BCCh
vix <- read_excel("svar data/vix.xlsx", sheet = "vix")               # VIX, BCCh
dxy <- read_excel("svar data/dxy.xlsx", sheet = "dxy")               # Dollar Index (DXY), Yahoo Finance
r_fed <- read_excel("svar data/r_fed.xlsx", sheet = "r_fed")         # tasa de interés de la FED, BCCh

dt <- merge(p, igrea, by = "year", all = TRUE)
dt <- merge(dt, wip, by = "year", all = TRUE)
dt <- merge(dt, q, by = "year", all = TRUE)
dt <- merge(dt, vix, by = "year", all = TRUE)
dt <- merge(dt, dxy, by = "year", all = TRUE)
dt <- merge(dt, r_fed, by = "year", all = TRUE)

dt <- na.omit(dt)
dt <- dt %>% arrange(year)


############################################################
####### 2. VAR en forma reducida
############################################################
# VAR con constante y número de rezagos elegido por AIC (máximo 24).
# La identificación es recursiva (Cholesky) con el orden de
# 'vars_svar' (ecuación 3).
############################################################

var <- VAR(
  na.omit(dt[, vars_svar]),
  type    = "const",
  season  = NULL,
  exogen  = NULL,
  lag.max = lag_max,
  ic      = "AIC"
)

cat("Rezagos seleccionados por AIC:", var$p, "\n")


############################################################
####### 3. IRF: respuesta del precio del cobre
############################################################
# Respuesta del precio del cobre a cada shock estructural, con
# intervalos al 90% por bootstrap. Cada figura se guarda como
# images/svar_irf_<código>.png.
############################################################

irf_list <- map(
  shocks$var,
  ~ irf(
    var,
    impulse  = .x,
    response = "p",
    n.ahead  = n_ahead,
    runs     = boot_runs,
    ci       = ci_nivel
  )
)
names(irf_list) <- shocks$var

df_irf <- map_dfr(shocks$var, function(s) {
  irf_obj <- irf_list[[s]]
  tibble(
    horizon = 0:(length(irf_obj$irf[[1]]) - 1),
    irf     = irf_obj$irf[[1]],
    lower   = irf_obj$Lower[[1]],
    upper   = irf_obj$Upper[[1]],
    shock   = s
  )
})

graficar_irf_svar <- function(s) {
  ggplot(filter(df_irf, shock == s), aes(x = horizon, y = irf)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
    geom_line(linewidth = 0.8) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = "Months after shock",
      y = "% change of copper price growth rate",
      title = ""
    ) +
    theme_minimal() +
    theme(
      axis.title       = element_text(size = tam_titulo),
      axis.text        = element_text(size = tam_texto),
      panel.grid.minor = element_blank()
    )
}

graficos_irf <- list()
for (k in seq_len(nrow(shocks))) {
  g <- graficar_irf_svar(shocks$var[k])
  guardar_fig(g, paste0("svar_irf_", shocks$codigo[k]))
  graficos_irf[[shocks$codigo[k]]] <- g
}

# Para ver una figura en particular, por ejemplo graficos_irf$csim


############################################################
####### 4. Shocks estructurales
############################################################
# Los shocks estructurales se obtienen de los residuos del VAR en
# forma reducida (u) y del factor de Cholesky (P) de su matriz de
# covarianzas, como eps = u P^(-1). Por construcción tienen matriz
# de covarianzas identidad.
#
# Se exportan a csim_shocks.xlsx, con una columna por shock y la
# fecha de cada observación. El shock CSIM es la columna p. Las
# primeras var$p observaciones de la muestra se pierden por los
# rezagos del VAR.
############################################################

u       <- resid(var)
Sigma_u <- crossprod(u) / nrow(u)
P       <- t(chol(Sigma_u))
eps     <- u %*% solve(P)

print(round(cov(eps), 3))  # comprobación: debe ser cercana a la identidad

eps_df <- data.frame(
  year = dt$year[(var$p + 1):nrow(dt)],
  as.data.frame(eps)
)

write_xlsx(eps_df, "csim_shocks.xlsx")
write_xlsx(dt, "dt.xlsx")


############################################################
####### 5. FEVD: descomposición de varianza del precio del cobre
############################################################
# Usa la misma identificación recursiva que las IRF. Reporta la
# participación de cada shock en la varianza del error de predicción
# del precio del cobre (en %) para horizontes de 1 a 36 meses.
############################################################

fevd_var <- fevd(var, n.ahead = n_ahead)

fevd_p <- as.data.frame(fevd_var$p * 100) %>%
  mutate(horizon = row_number()) %>%
  relocate(horizon)

# Tabla resumen en horizontes seleccionados
horizontes <- c(1, 3, 6, 12, 24, 36)

tabla_fevd_p <- fevd_p %>%
  filter(horizon %in% horizontes) %>%
  mutate(across(-horizon, ~ round(.x, 1)))

print(tabla_fevd_p)

# Figura (áreas apiladas)
fevd_labels <- setNames(shocks$etiqueta, shocks$var)

df_fevd_plot <- fevd_p %>%
  pivot_longer(-horizon, names_to = "shock", values_to = "share") %>%
  mutate(shock = factor(shock, levels = vars_svar,
                        labels = fevd_labels[vars_svar]))

plot_fevd <- ggplot(df_fevd_plot, aes(x = horizon, y = share, fill = shock)) +
  geom_area(color = "white", linewidth = 0.2) +
  scale_fill_grey(start = 0.85, end = 0.2) +
  scale_y_continuous(limits = c(0, 100.01), expand = c(0, 0)) +
  labs(
    x = "Months after shock",
    y = "% of copper price forecast error variance",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title       = element_text(size = tam_titulo),
    axis.text        = element_text(size = tam_texto),
    legend.text      = element_text(size = tam_leyenda),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )

guardar_fig(plot_fevd, "svar_fevd", ancho = fevd_ancho, alto = fevd_alto)



############################################################
####### 6. Robustez: SVAR con WIP en vez de IGREA
############################################################
# Mismo SVAR de la sección 2, reemplazando la IGREA por el índice de
# producción industrial mundial (WIP) como medida de actividad
# global. Se mantiene el orden recursivo (q, wip, vix, dxy, p), la
# constante y la selección de rezagos por AIC.
#
# Los shocks estructurales se recuperan igual que en la sección 4 y
# se exportan a csim_wip_shocks.xlsx. El shock CSIM es la columna p.
############################################################

vars_svar_wip <- c("q", "wip", "vix", "dxy", "p")  # orden = identificación

var_wip <- VAR(
  na.omit(dt[, vars_svar_wip]),
  type    = "const",
  season  = NULL,
  exogen  = NULL,
  lag.max = lag_max,
  ic      = "AIC"
)

cat("Rezagos seleccionados por AIC (WIP):", var_wip$p, "\n")

u_wip       <- resid(var_wip)
Sigma_u_wip <- crossprod(u_wip) / nrow(u_wip)
P_wip       <- t(chol(Sigma_u_wip))
eps_wip     <- u_wip %*% solve(P_wip)

print(round(cov(eps_wip), 3))  # comprobación: debe ser cercana a la identidad

eps_wip_df <- data.frame(
  year = dt$year[(var_wip$p + 1):nrow(dt)],
  as.data.frame(eps_wip)
)

write_xlsx(eps_wip_df, "csim_wip_shocks.xlsx")