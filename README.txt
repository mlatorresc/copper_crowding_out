Copper Price Booms and Tradable Sector Crowding-Out in Chile (2003-2024)
Martín Latorre, Energy Center, FCFM, Universidad de Chile

Replication code for the paper.


SCRIPTS

svar.R   Estimates the monthly SVAR and identifies the CSIM shocks. Produces
         the IRFs and FEVD of the copper price and exports csim_shocks.xlsx.
         Also estimates the WIP version and exports csim_wip_shocks.xlsx.
tab1.R   Table 1. Correlations between global factors, the copper price and
         domestic variables.
ivlp.R   First stage (Table 4) and IRFs of the IV-LP model.
tab5.R   Table 5. Correlations between CSIM shocks and export and import
         prices.
rob.R    Robustness exercises (tF inference, WIP instrument, crisis dummies
         and minimal controls).


RUN ORDER

1. Open copper_crowding_out.Rproj.
2. Run svar.R.
3. Aggregate the shocks to quarterly frequency and save them in the
   ivlp data folder.
4. Run tab1.R, ivlp.R, tab5.R and rob.R.


FOLDERS AND FILES

svar data/             Monthly data for the SVAR.
ivlp data/             Quarterly data for the IV-LP and robustness checks.
correlation data/      Data for Tables 1 and 5.
tables/                LaTeX tables produced by the scripts.
images/                Figures produced by the scripts.
csim_shocks.xlsx       Shocks from the SVAR with IGREA (output of svar.R).
csim_wip_shocks.xlsx   Shocks from the SVAR with WIP (output of svar.R).

Data sources are listed in the paper's Appendix.


R PACKAGES

tidyverse, readxl, writexl, openxlsx, vars, urca, mFilter, tseries,
TSstudio, forecast, plm, countrycode, astsa, lpirfs, seasonal, sandwich,
lmtest, magick, ivDiag


CONTACT

martin.latorre@centroenergia.cl
