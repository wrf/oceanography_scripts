# oceanography_scripts
oceanography code

## WOD OSD bottle data ##
Data for OSD (Ocean Station Data) were downloaded from [NOAA WOD](https://www.ncei.noaa.gov/access/world-ocean-database-select/dbsearch.html), as of March 2021, containing data for 3069708 casts/stations for 28987253 total stops.

This is in a completely useless .csv format that is NOT a table, broken down into 13 files (`ocldb1616358314.25649.OSD.csv.gz` up to `ocldb1616358314.25649.OSD13.csv.gz`. Here, a python parser converts it into a giant table. The process took 16 minutes on my laptop and requires 22Gb RAM.

`compile_wod_csv_to_real_table.py -c *.csv.gz > ocldb1616358314.25649.OSD_all.all_vars.tab`

The table headers are listed below, and most are self explanatory. `stop` refers to the bottle order in a single cast, with 0 being the first. Note that the variable names mostly keep format of the OSD data, including the 10 character limit (e.g. `Temperatur`).

`cast_id	cruise_id	orig_station_id	orig_cruise_id	latitude	longitude	year	month	day	country	country_acc_number	stop	depth	CFC11	DeltaC13	DeltaC14	Nitrate	pH	CFC12	Chlorophyl	Alkalinity	Pressure	Argon	Temperatur	CFC113	tCO2	Silicate	Oxygen	Salinity	Oxy18	Tritium	Neon	DeltaHe3	Phosphate	pCO2	Helium	Ammonia`

Units should be:

```
Depth         m
Pressure      dbar
Temperatur    degrees C
Salinity      PSS
Oxygen        umol/kg
Phosphate     umol/kg
Nitrate       umol/kg
Silicate      umol/kg
Ammonia       umol/l
Chlorophyl    ug/l
tCO2          mM
DeltaC14      per mille
DeltaC13      per mille
Oxy18         per mille
Alkalinity    meq/l
CFC11         pmol/kg
CFC12         pmol/kg
CFC113        pmol/kg
Helium        nmol/kg
DeltaHe3      percent
Tritium       TU
Neon          nmol/kg
Argon         nmol/kg
```

Loading the entire table into R then requires 14Gb RAM.

```
wod_data_file = "~/project/WOD_select/ocldb1616358314.25649.OSD_all.all_vars.tab"
wod_data = read.table(wod_data_file, header=TRUE, sep="\t")
wod_summary = summary(wod_data)
wod_summary
```

![WOD_OSD_samples_per_year.png](https://github.com/wrf/oceanography_scripts/blob/master/images/WOD_OSD_samples_per_year.png)

![WOD_OSD_max_depth_by_year.png](https://github.com/wrf/oceanography_scripts/blob/master/images/WOD_OSD_max_depth_by_year.png)

Some basic filtering can be applied to simplify the dataset. To take only the first, or shallowest bottle, set `stop==0`. This would be looking at surface values of nearly all measurements, of a total of 3069708 observations.

`first_stop_only = filter(wod_data, stop == 0)`

Taking all shallow water measurements is a larger set, since there are many 10m or 20m samples, this leaves 12134025 samples.

`surface_data_only = filter(wod_data, abs(depth) < 50)`

In general, the data are messy and need substantial post-processing. For example, most casts have temperature. However, this column contains a few negative values (below -2, which would be the temperature of [brine-excluded polar water](https://nsidc.org/cryosphere/seaice/index.html)), and values between 50 and 100, which are likely Fahrenheit, instead of [hydrothermal vents](https://oceanservice.noaa.gov/facts/vents.html).

```
> table( round(wod_data[["Temperatur"]]) )

   -100     -60     -48     -44     -39     -34     -33     -22     -16     -15     -12     -11     -10      -8 
      7       1       1       1       2       1       1       1       1       1       4       2       1       1 
     -7      -5      -4      -3      -2      -1       0       1       2       3       4       5       6       7 
      1       2       4      23  258491  718538 1000463 1075674 1522455 1735250 1978899 1625205 1600309 1521408 
      8       9      10      11      12      13      14      15      16      17      18      19      20      21 
1386334 1192817 1029452  914095  899285  972681  996685  848622  844351  759436  741297  599957  539063  455230 
     22      23      24      25      26      27      28      29      30      31      32      33      34      35 
 443248  374590  369995  329949  332909  319404  300566  214482   64114    5055     948     337     149      86 
     36      37      38      39      40      42      43      45      47      48      49      50      51      52 
     19      21      90       8       5       4       1       1       3       2       4       5       7       2 
     53      55      56      57      58      59      60      61      62      63      64      65      66      67 
      2       2      14       9       8       1       2       1       9       1       7       1       8       2 
     68      70      72      74      76      77      78      79      80      81      82      83      87      88 
      5       6       4       3       9       1       9       1       7       2      11       2       2       2 
     90      93      94      96      98      99     100     105     107     110     116     117     118     127 
      1       1       1       1       1       1      12       2       1       2       1       2       1       1 
    131     138     266     270     311    1000 
      1       1       1       1       1       1 
```

![WOD_OSD_samples_by_temp.png](https://github.com/wrf/oceanography_scripts/blob/master/images/WOD_OSD_samples_by_temp.png)

This code makes a map of global surface nitrate. The highest values appear to be due to river inputs. The southern ocean is also noticeably darker than much of the rest of the world, as a well known [HNCL region](https://en.wikipedia.org/wiki/High-nutrient,_low-chlorophyll_regions).

```
library(ggplot2)
library(dplyr)
worldpolygons = map_data("world")
first_stop_w_nitrate = filter(wod_data, stop==0, !is.na(Nitrate))
wnit_gg = ggplot(worldpolygons) +
    coord_cartesian(expand = c(0,0)) +
    labs(x=NULL, y=NULL) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position=c(0.75,0.75)  ) +
    geom_polygon( aes(x=long, y = lat, group = group), fill="#aaaaaa", colour="#ffffff") +
    scale_colour_gradient(low = "#e7e1ef", high = "#8e1236", trans="log10", na.value="#f7f4f9" ) +
    geom_point(data=first_stop_w_nitrate, aes( x=longitude, y=latitude, colour=Nitrate), size=0.5 )
ggsave(file="~/git/oceanography_scripts/images/WOD_OSD_surface_nitrate.png", wnit_gg, device="png", width=12, height=6, dpi=90)
```

![WOD_OSD_surface_nitrate.png](https://github.com/wrf/oceanography_scripts/blob/master/images/WOD_OSD_surface_nitrate.png)

## secchi disk plot ##
Plot of [Secchi disk](https://en.wikipedia.org/wiki/Secchi_disk) data from [NOAA National Centers for Environmental Information](https://www.ncei.noaa.gov/data/oceans/woa/WOD/DATA_SUBSETS/). This was in the format of a .csv file containing 463875 casts, and required little post-processing.

![WOD13_secchi_forel.png](https://github.com/wrf/oceanography_scripts/blob/master/images/WOD13_secchi_forel.png)

## mbari CTD plot ##
Plot of the CTD from [MBARI](https://www.mbari.org/products/data-repository/) ROVs

`Rscript ../mbari_ctd_plotter.R mbari_dive_d420_ctd.txt`

![mbari_dive_d420_ctd.png](https://github.com/wrf/oceanography_scripts/blob/master/images/mbari_dive_d420_ctd.png)

## phanerozoic oxygen ##
Plot of Phanerozoic oxygen level, based on various models ( [Bergman 2004 COPSE](https://doi.org/10.2475/ajs.304.5.397) and [Berner 2006 GEOCARBSULF](https://doi.org/10.1016/j.gca.2005.11.032) ). These data were hacked out of the paper, though an updated version of the model code is [here](https://github.com/sjdaines/COPSE) by [Lenton 2018](https://doi.org/10.1016/j.earscirev.2017.12.004).

![o2_models_phanerozoic_v1.png](https://github.com/wrf/oceanography_scripts/blob/master/images/o2_models_phanerozoic_v1.png)

## ts_diagram_example ##
Plot of Temperature-Salinity diagram from [WOCE](https://en.wikipedia.org/wiki/World_Ocean_Circulation_Experiment) Station P17N in the North Pacific on 1-June-1993

`Rscript ts_diagram_example.R`

![woce_p17n_t-s_diagram_v1.png](https://github.com/wrf/oceanography_scripts/blob/master/images/woce_p17n_t-s_diagram_v1.png)
