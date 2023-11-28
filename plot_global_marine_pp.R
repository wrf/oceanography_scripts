# plot global marine primary productivity
#
# data from 
# Kulk 2020 Primary Production, an Index of Climate Change in the Ocean: Satellite-Based Estimates over Two Decades. Remote Sens. 2020, 12(5), 826.
# https://www.mdpi.com/2072-4292/12/5/826
# https://data.ceda.ac.uk/neodc/bicep/data/marine_primary_production/v4.2/monthly/2020

library(raster)
library(ggplot2)

gppm_file = "~/git/BICEP_NCEO_PP_ESA-OC-L3S-MERGED-1M_MONTHLY_9km_mapped_202001-fv4.2.nc"
gppm = raster(gppm_file)

rowvals = function(lat1, lat2, nrow){
  lat1_row = round((lat1+90)/180*nrow)
  lat2_row = round((lat2+90)/180*nrow)
  return( c(lat1_row, lat2_row) )
}

rowvals(-10,30,2160)

getValuesBlock(gppm,960,481)

str(gppm)
plot( gppm$Phytoplankton.Primary.Production, xlim=c(-10,30), ylim=c(30,50), frame.plot=FALSE )

matrix(data = c(1,2,3,4), nrow = 2, byrow = TRUE) / 2
















#