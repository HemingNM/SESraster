## R CMD check results

── R CMD check results ──────────────────────────────────────────────────────────────── SESraster 0.7.1 ────
Duration: 1m 4.7s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

Maintainer: ‘Neander Marcel Heming <neanderh@yahoo.com.br>’

## Maintainer comments
Main changes are listed below:

# SESraster 0.7.1

* bug fixes
  - bug fix on SESraster() that was creating only a single temporary raster for 
  FUN metrics results. Now it creates "n = aleats" temporary rasters.

* function changes
  - SESraster() now also returns p values for the upper and lower tails (i.e. "p_lower", "p_upper")

