# plot Secchi disk depth from WOD13
# created by WRF 2021-03-22
# data from NOAA National Centers for Environmental Information
# https://www.ncei.noaa.gov/data/oceans/woa/WOD/DATA_SUBSETS/
# data fields are:

secchi_headers = c("accession","cruise","cast_number",
                   "latitude", "longitude", "year", "month", "day", "time",
                   "disk_depth", "water_color")

secchidata_file = "~/git/oceanography_scripts/data/WOD13_secchi_forel.csv"
secchidata = read.csv(secchidata_file, header=FALSE, col.names=secchi_headers)

# filter only records with depth
has_secchi_depth = !is.na(secchidata[["disk_depth"]])
filt_secchidata = secchidata[has_secchi_depth,]
secchi_depth = filt_secchidata[["disk_depth"]]
secchi_depth_rounded = round(secchi_depth) + 1

# show full map of the world
latrange = c(-90,90)
longrange = c(-180,180)

# color scale goes from green (shallow, 0m) to blue (deep, 50m), to dark blue (anything deeper)
water_color_palette = c(colorRampPalette(c("#38ff6b33","#0cb1ff33","#1100cd33"),alpha=TRUE)(50), colorRampPalette(c("#1100cd33","#0b007caa"),alpha=TRUE)(350) )

# generate plot
png(file = "~/git/oceanography_scripts/images/WOD13_secchi_forel.png", width=1200, height=700)
worldmap = map('world', xlim=longrange, ylim=latrange, fill=FALSE, col="#898989" )
points(filt_secchidata[["V5"]], filt_secchidata[["V4"]], pch=16, cex=3, col=water_color_palette[secchi_depth_rounded] )
text(0,-77, "WOD 2013 Secchi Disk Depth", pos=4, cex=3)
rect( seq(30,80,10), rep(-92,6), seq(40,90,10), rep(-82,6), col=water_color_palette[c(1,11,21,31,41,51)] )
text(30,-87, "0m", cex=2, pos=2)
text(90,-87, ">50m", cex=2, pos=4)
dev.off()








#