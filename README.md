# oceanography_scripts
oceanography code

## secchi disk plot ##
Plot of [Secchi disk](https://en.wikipedia.org/wiki/Secchi_disk) data from [NOAA National Centers for Environmental Information](https://www.ncei.noaa.gov/data/oceans/woa/WOD/DATA_SUBSETS/)

![WOD13_secchi_forel.png](https://github.com/wrf/oceanography_scripts/blob/master/images/WOD13_secchi_forel.png)

## mbari CTD plot ##
Plot of the CTD from [MBARI](https://www.mbari.org/products/data-repository/) ROVs

`Rscript ../mbari_ctd_plotter.R mbari_dive_d420_ctd.txt`

![mbari_dive_d420_ctd.png](https://github.com/wrf/oceanography_scripts/blob/master/images/mbari_dive_d420_ctd.png)

## phanerozoic oxygen ##
Plot of Phanerozoic oxygen level, based on various models ( [Bergman 2004 COPSE](https://doi.org/10.2475/ajs.304.5.397) and [Berner 2006 GEOCARBSULF](https://doi.org/10.1016/j.gca.2005.11.032) )

![o2_models_phanerozoic_v1.png](https://github.com/wrf/oceanography_scripts/blob/master/images/o2_models_phanerozoic_v1.png)

## ts_diagram_example ##
Plot of Temperature-Salinity diagram from WOCE Station P17N in the North Pacific on 1-June-1993

`Rscript ts_diagram_example.R`

![woce_p17n_t-s_diagram_v1.png](https://github.com/wrf/oceanography_scripts/blob/master/images/woce_p17n_t-s_diagram_v1.png)
