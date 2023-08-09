# summarize geotrace metadata
# from
# https://geotraces.webodv.awi.de/IDP2021
#

library(ggplot2)
library(dplyr)


# try importing directly
#geotrace_meta_file = "~/project/oceanography/geotraces/IDP2021_GEOTRACES_IDP2021_Seawater_Discrete_Sample_Data_v1_metadata.txt"
# fails
# raw file contains intermittent comments, cannot use skip or comment.char
#geotrace_meta_file = "~/project/oceanography/geotraces/IDP2021_GEOTRACES_IDP2021_Seawater_v1_metadata_clean.txt"
#geotrace_meta = read.table(geotrace_meta_file, head=TRUE, sep="\t")

#summary(geotrace_meta)

#geotrace_tm_file = "~/project/oceanography/geotraces/GEOTRACES_IDP2021_Seawater_Discrete_Sample_Data_v1_ra6YszDy-tracemetal-w-isotopes.clean.txt"
#geotrace_tm_file = "~/project/oceanography/geotraces/GEOTRACES_IDP2021_Seawater_Discrete_Sample_v1_WUEpD6wK.clean.txt"
geotrace_tm_file = "~/git/envbiogeo-mgap/datasets/GEOTRACES_IDP2021_Seawater_Discrete_Sample_Data_v1_nfYzzsKg.clean.txt"
geotrace_tm = read.table(geotrace_tm_file, header=TRUE, sep="\t")

geotrace_pt_file = "~/git/envbiogeo-mgap/datasets/GEOTRACES_IDP2021_Seawater_Discrete_Sample_Data_v1_0PxszZoW.clean.txt"
geotrace_pt = read.table(geotrace_pt_file, header=TRUE, sep="\t")
str(geotrace_pt)

str(geotrace_tm)
glimpse(geotrace_tm)

dim(geotrace_tm)
nrow(geotrace_tm)
ncol(geotrace_tm)

table( geotrace_tm$Chief.Scientist )

trace_years = sapply( strsplit( as.character(geotrace_tm$yyyy.mm.ddThh.mm.ss.sss) , "-", fixed=TRUE), "[", 1)
table(trace_years)
trace_month = sapply( strsplit( as.character(geotrace_tm$yyyy.mm.ddThh.mm.ss.sss) , "-", fixed=TRUE), "[", 2)
table(trace_month)
trace_years = gsub("^(\\d+)-.*","\\1", as.character(geotrace_tm$yyyy.mm.ddThh.mm.ss.sss) )
table(trace_years)


pdf(file="~/git/envbiogeo-mgap/images/IDP2021_num_of_samples_by_year.pdf", width=8, height=4)
par(mar=c(3,4.5,1,1))
barplot( table( trace_years ), ylab="Number of samples", col="#476cbe", cex.lab=1.4, cex.axis=1.3, cex.names=0.9 )
dev.off()

sum(table( trace_years ))

as.character(geotrace_tm$yyyy.mm.ddThh.mm.ss.sss)

head( names(geotrace_tm) )
names(geotrace_tm)
sort( table( geotrace_tm$Chief.Scientist ), decreasing = TRUE)

count_nas = function(x){
  length(x) - sum(is.na(x))
}

apply(geotrace_tm, 2, count_nas)
apply(geotrace_pt, 2, count_nas)


table( geotrace_tm$Al_D_CONC_BOTTLE_nmol.kg )
table( round(geotrace_tm$Al_D_CONC_BOTTLE_nmol.kg , digits=0) )
hist( geotrace_tm$Al_D_CONC_BOTTLE_nmol.kg )
mean( geotrace_tm$Al_D_CONC_BOTTLE_nmol.kg )
mean( geotrace_tm$Al_D_CONC_BOTTLE_nmol.kg, na.rm = TRUE )
mean( na.omit(geotrace_tm$Al_D_CONC_BOTTLE_nmol.kg ) )
median( geotrace_tm$Al_D_CONC_BOTTLE_nmol.kg, na.rm=TRUE )
hist( na.omit(geotrace_tm$Al_D_CONC_BOTTLE_nmol.kg ) )


hist( geotrace_tm$CTDTMP_T_VALUE_SENSOR_deg.C )

sum( is.na(geotrace_tm$Al_D_CONC_BOTTLE_nmol.kg) )

range( geotrace_tm$Longitude_degrees_east )
range( geotrace_tm$Latitude_degrees_north )

geotrace_tm$CTDSAL_D_CONC_SENSOR_pss.78

plot(1:25, 1:25, pch=1:25)

plot(geotrace_tm$CTDSAL_D_CONC_SENSOR_pss.78, geotrace_tm$SALINITY_D_CONC_BOTTLE )

plot(geotrace_tm$CTDSAL_D_CONC_SENSOR_pss.78, geotrace_tm$SALINITY_D_CONC_BOTTLE, 
     xlim=c(20,40), ylim=c(20,40), frame.plot=FALSE,
     pch=16, col="#db6a0044", cex=2,
     xlab="CTD Salinity (0/00)", ylab="Bottle Salinity (0/00)", cex.lab=1.4 )

salt_conc = c( geotrace_tm$CTDSAL_D_CONC_SENSOR_pss.78 , geotrace_tm$SALINITY_D_CONC_BOTTLE )
salt_method = c( rep("CTD", length(geotrace_tm$CTDSAL_D_CONC_SENSOR_pss.78) ),
            rep("bottle", length(geotrace_tm$SALINITY_D_CONC_BOTTLE) ) )
salt_ctd_vs_bot = data.frame( salt_conc, salt_method )

salt_diff = geotrace_tm$CTDSAL_D_CONC_SENSOR_pss.78 - geotrace_tm$SALINITY_D_CONC_BOTTLE
hist(salt_diff, breaks=10000, xlim=c(-0.03,0.03))
t.test( x=salt_diff )
t.test(x=geotrace_tm$CTDSAL_D_CONC_SENSOR_pss.78, y=geotrace_tm$SALINITY_D_CONC_BOTTLE,
       paired=TRUE)
wilcox.test(x=salt_diff)

t.test( formula = salt_conc ~ salt_method,
        data = salt_ctd_vs_bot,
        var.equal = FALSE )
t.test( formula = salt_conc ~ salt_method,
                         data = salt_ctd_vs_bot,
                         var.equal = TRUE )

sg = ggplot(geotrace_tm, aes(x=CTDSAL_D_CONC_SENSOR_pss.78, y=SALINITY_D_CONC_BOTTLE)) +
  scale_x_continuous( limits=c(20,40) ) +
  scale_y_continuous( limits=c(20,40) ) +
  geom_point( color="#db6a00", alpha=0.25, size=4)
sg

plot( geotrace_tm$CTDTMP_T_VALUE_SENSOR_deg.C, geotrace_tm$DEPTH_m, type="l",
      xlim=c(-5,35), ylim=c(6000,0) )

tg = ggplot(geotrace_tm, aes(x=CTDTMP_T_VALUE_SENSOR_deg.C , y=DEPTH_m )) +
  scale_y_reverse() +
  geom_point( color="#bc99ab" , alpha=0.05, size=4)
tg

tgl = ggplot(geotrace_tm, aes(x=CTDTMP_T_VALUE_SENSOR_deg.C , y=DEPTH_m , group=Cast.Identifier)) +
  scale_y_reverse() +
  geom_line( color="#77223a" , alpha=0.05, size=1)
tgl

oxygl = ggplot(geotrace_tm, aes(x=CTDOXY_D_CONC_SENSOR_umol.kg , y=DEPTH_m , group=Cast.Identifier)) +
  scale_y_reverse( limits=c(6000,0)) +
  geom_line( color="#1b5a99" , alpha=0.05, size=1)
oxygl

filter(geotrace_tm, CTDOXY_D_CONC_SENSOR_umol.kg > 350)

# plot aluminium vs depth
aldg = ggplot(geotrace_tm, aes(x=Al_D_CONC_BOTTLE_nmol.kg , y=DEPTH_m ) ) +
  scale_y_reverse(limits=c(6000,0)) +
  geom_point( color="#8888aa" , alpha=0.3, size=4) +
  geom_point( data=filter(geotrace_tm, Al_D_CONC_BOTTLE_nmol.kg > 100), color="#ee0000", alpha=0.3, size=4)
aldg
ggsave(file="~/project/oceanography/geotraces/geotraces_2021_aluminium_depth_w_red.pdf", aldg, device="pdf", width=7, height=6)
ggsave(file="~/project/oceanography/geotraces/geotraces_2021_aluminium_depth_w_red.png", aldg, device="png", width=7, height=6,  dpi=90)

# try grouping by concentration value
aldg = geotrace_tm %>%
  mutate(concgrp = (Al_D_CONC_BOTTLE_nmol.kg > 100) ) %>%
  ggplot( aes(x=Al_D_CONC_BOTTLE_nmol.kg , y=DEPTH_m , color=concgrp ) ) +
  scale_y_reverse(limits=c(6000,0)) +
  scale_color_manual( values=c("#8888aa","#ee0000" ) ) +
  theme(legend.position = "none") +
  geom_point( alpha=0.3, size=4)
aldg

# try as lines
aldg = ggplot(geotrace_tm, aes(x=Al_D_CONC_BOTTLE_nmol.kg , y=DEPTH_m, group=Cast.Identifier ) ) +
  scale_y_reverse(limits=c(6000,0)) +
  geom_line( color="#8888aa" , alpha=0.1, size=1)
aldg

summary( filter(tm_short, Al_D_CONC_BOTTLE_nmol.kg > 100) )

longitude_adj = geotrace_tm$Longitude_degrees_east
longitude_adj[longitude_adj > 180] = longitude_adj[longitude_adj > 180] - 360
tm_short = cbind( select(geotrace_tm, DEPTH_m , Latitude_degrees_north , Al_D_CONC_BOTTLE_nmol.kg ), longitude_adj)

worldpolygons = map_data("world")

gt_gg = ggplot(worldpolygons) +
  coord_cartesian(expand = c(0,0)) +
  labs(x=NULL, y=NULL) +
  # theme(axis.text = element_blank(),
  #       axis.ticks = element_blank(),
  #       legend.position=c(0.75,0.75)  ) +
  theme_bw() +
  geom_polygon( aes(x=long, y = lat, group = group), fill="#cdcdcd", colour="#ffffff") +
  #scale_colour_gradient(low = "#e7e1ef", high = "#8e1236", trans="log10", na.value="#f7f4f9" ) +
  geom_point(data=tm_short, aes( x=longitude_adj, y=Latitude_degrees_north ), color="#8888aa", alpha=0.2, size=2 ) +
  geom_point(data=filter(tm_short, Al_D_CONC_BOTTLE_nmol.kg > 100), aes( x=longitude_adj, y=Latitude_degrees_north ), color="#ee0000", alpha=0.3, size=2 )

gt_gg
ggsave(file="~/project/oceanography/geotraces/geotraces_2021_aluminium_map_w_red.pdf", gt_gg, device="pdf", width=12, height=6)
ggsave(file="~/project/oceanography/geotraces/geotraces_2021_aluminium_map_w_red.png", gt_gg, device="png", width=12, height=6, dpi=90)

tm_short = cbind( select(geotrace_tm, DEPTH_m , Latitude_degrees_north , CTDOXY_D_CONC_SENSOR_umol.kg ), longitude_adj)

ggplot(worldpolygons) +
  coord_cartesian(expand = c(0,0)) +
  labs(x=NULL, y=NULL) +
  theme_bw() +
  geom_polygon( aes(x=long, y = lat, group = group), fill="#cdcdcd", colour="#ffffff") +
  geom_point(data=tm_short, aes( x=longitude_adj, y=Latitude_degrees_north ), color="#8888aa", alpha=0.2, size=2 ) +
  geom_point(data=filter(tm_short, CTDOXY_D_CONC_SENSOR_umol.kg > 350), aes( x=longitude_adj, y=Latitude_degrees_north ), color="#0000ee", alpha=0.3, size=2 )




#