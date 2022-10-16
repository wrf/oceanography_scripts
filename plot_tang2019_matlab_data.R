# plot Tang2019 data on diazotroph abundance
# data from
# https://doi.pangaea.de/10.1594/PANGAEA.905108
# for paper
# Tang and Cassar (2019) Data‐Driven Modeling of the Distribution of Diazotrophs in the Global Ocean
# https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2019GL084376
#

library(R.matlab)
library(maps)
library(ggplot2)

# array of 16200 x 16 x 12
# possibly lat/long x 16 variables x 12 months
diaz_predictor_file = "~/project/oceanography/tang2019_diazotroph/predictors_diazotrophs.mat"
diaz_predictor = readMat(diaz_predictor_file)
summary(diaz_predictor)

diaz_basin_file = "~/project/oceanography/tang2019_diazotroph/basin_separation.mat"
diaz_basin = readMat(diaz_basin_file)
summary(diaz_basin)

diaz_rf_model_file = "~/project/oceanography/tang2019_diazotroph/Diazotrophs_RF_models.mat"
diaz_rf_model = readMat(diaz_rf_model_file)
summary(diaz_rf_model)

diaz_monthly_file = "~/project/oceanography/tang2019_diazotroph/Diazotrophs_RF_global_monthly.mat"
diaz_monthly = readMat(diaz_monthly_file)
summary(diaz_monthly)
# appears to be array of 90*180*12, for lat-long-month

image( t(diaz_monthly$Richelia.RF.global.monthly[,,1]) )

rx1 = rep(seq(-180,178,2), rep(90,180) )
rx2 = rep(seq(-178,180,2), rep(90,180) )
ry1 = rep(seq(90,-88,-2),180)
ry2 = rep(seq(88,-90,-2),180)

month1 = diaz_monthly$Richelia.RF.global.monthly[,,1]
month1_col = c(round(log10(month1))+1)
is_ocean = !is.na(month1_col)
table(is_ocean)

colorset = colorRampPalette(c("#99008800","#990000ff"), alpha=TRUE)(8)
plot(1:8,1:8,pch=15,cex=5,col=colorset)

diaz_rx1 = rx1[is_ocean]
diaz_rx2 = rx2[is_ocean]
diaz_ry1 = ry1[is_ocean]
diaz_ry2 = ry2[is_ocean]
diaz_color_cat = colorset[month1_col[is_ocean]]
diaz_by_month = data.frame(diaz_rx1, diaz_ry1, diaz_rx2, diaz_ry2, diaz_color_cat)
head(diaz_by_month)

worldpolygons = map_data("world")

n2gg = ggplot(worldpolygons, aes(long,lat, group=group)) +
   # coord_cartesian(xlim=c(-180,180), ylim=c(-80, 80) ) +
    coord_fixed(xlim = c(-180, 180),  ylim = c(-80, 80), ratio = 1.3) +
#    scale_x_continuous(expand = c(0,0), limits=c(-180,185)) +
 #   scale_y_continuous(expand = c(0,0), limits=c(-80, 80))+
    labs(title="Global Richelia - January",
         subtitle="from Tang and Cassar (2019)") +
    geom_polygon( data=worldpolygons, aes(x=long, y = lat, group = group), 
                  fill="#767676", colour="#ffffff" ) +
    annotate( geom="rect", xmin=diaz_rx1, xmax=diaz_rx2, ymin=diaz_ry1, ymax=diaz_ry2, 
              fill=diaz_color_cat )
ggsave("~/project/oceanography/tang2019_diazotroph/Diazotrophs_RF_global_monthly.01.pdf", n2gg, device="pdf", width=13, height=7)


pdf(file="~/project/oceanography/tang2019_diazotroph/Diazotrophs_RF_global_monthly.01.pdf", width=13, height=7)
worldmap = map('world', xlim=c(-180,180), ylim=c(-90,90), fill=FALSE, col="#898989", mar=c(1,1,1,1))
plot(0,0,type='n', xlim=c(-180,180), ylim=c(-90,90))

rect(seq(-180,178,2),seq(90,-88,-2),seq(-178,180,2),seq(88,-90,-2),col=colorset[month1_col], border=FALSE)
rect(rx1, ry1,
     rx2, ry2,
     col=colorset[month1_col], border=FALSE)
dev.off()
