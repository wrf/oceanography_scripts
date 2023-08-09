# summarize_WOD_data.R

library(ggplot2)
library(dplyr)

#wod_data_file = "~/git/oceanography_scripts/datasets/WOD_select/ocldb1616358314.25649.OSD_all.all_vars.100k.tab"
wod_data_file = "~/git/oceanography_scripts/datasets/WOD_select/ocldb1616358314.25649.OSD_all.all_vars.tab"
wod_data = read.table(wod_data_file, header=TRUE, sep="\t")
wod_summary = summary(wod_data)
wod_summary

head(wod_data)

wod_data[["CFC11"]]
hist(pmin(wod_data[["Salinity"]],59), breaks=30)

temp_table = table( round(wod_data[["Temperatur"]]) )
temp_colors = colorRampPalette(c("#e0f3f8", "#213685", "#dddd9f", "#850026"),alpha=TRUE)(65)

surface_data_only = filter(wod_data, abs(depth) < 50)
surface_temp_table = table( round(all_surface_data[["Temperatur"]]) )

saltcolors = c( colorRampPalette(c("#fde0dd","#49006a"),alpha=TRUE)(20) )

first_stop_only = filter(wod_data, stop == 0)
dim(first_stop_only)

wod_data[["Temperatur"]]
hist(wod_data[["Temperatur"]], breaks=20)
as.numeric(wod_data[["Temperatur"]])

data_years = table(wod_data[["year"]])

data_depths = data.frame(table(round(wod_data[["depth"]])))
head(data_depths)

pdf(file="~/git/oceanography_scripts/images/WOD_OSD_samples_per_year.pdf", width=8, height=4)
par(mar=c(2.5,4.5,2,1))
plot(data_years, type="h", axes=FALSE, frame.plot=FALSE, ylab="Samples", col="#0570b0")
axis(1, at=seq(1775,2025,25))
axis(2, at=seq(0,1000000,250000))
text(1770,900000,paste("Total samples:", sum(data_years)), cex=1.1, pos=4)
dev.off()

pdf(file="~/git/oceanography_scripts/images/WOD_OSD_samples_by_temp.pdf", width=4, height=4)
par(mar=c(4.5,4.5,1,1))
plot(temp_table, type="h", xlim=c(-5,39), axes=FALSE, frame.plot=FALSE, xlab="Temperature (C)", ylab="Samples", col=temp_colors, lwd=5)
axis(1, at=seq(-5,40,5), cex.axis=0.9)
axis(2, at=seq(0,2000000,1000000), labels=c("0", "1M", "2M"), las=1)
dev.off()

# dt_gg = wod_data %>% filter(Temperatur > -3, Temperatur < 40) %>%
# ggplot( aes(x=Temperatur, y=depth)) +
#     scale_y_reverse(expand = c(0,0), limits=c(6000,0) ) +
#     scale_x_continuous(expand = c(0,0), limits=c(-3, 40) ) +
#     scale_colour_gradient(low="#21368511", high="#85002611") +
#     geom_point(aes(colour=Temperatur), size=2, show.legend = FALSE)
# 
# ggsave(file="~/git/oceanography_scripts/images/WOD_OSD_depth_v_temp.png", dt_gg, device="png", width=8, height=8, dpi=90)




year_extremes = wod_data %>%
    group_by(year) %>%
    summarise(year_min = min(abs(depth)),
              year_max = max(abs(depth)))
head(year_extremes)

pdf(file="~/git/oceanography_scripts/images/WOD_OSD_max_depth_by_year.pdf", width=8, height=4)
par(mar=c(1,4.5,3,1))
plot(year_extremes$year, -1*year_extremes$year_max, type="h", axes=FALSE, frame.plot=FALSE, ylab="Max depth of that year (m)", col="#052060")
axis(3, at=seq(1775,2025,25))
axis(2, at=seq(0,-10000,-2500), labels=c("0", "2500", "5000", "7500", "10000"))
dev.off()

ggplot(year_extremes, aes(x=year, y=year_min, xend=year, yend=year_max )) + 
    scale_y_reverse(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0) ) +
    labs(x=NULL, y="Max depth of that year (m)" ) +
    scale_colour_gradient(low = "#65a0e0", high = "#052040") +
    geom_segment(aes(colour=year_max), show.legend = FALSE, size=1.5)

worldpolygons = map_data("world")

first_stop_w_nitrate = filter(wod_data, stop==0, !is.na(Nitrate))
hist(log10(first_stop_w_nitrate$Nitrate))

wnit_gg = ggplot(worldpolygons) +
    coord_cartesian(expand = c(0,0)) +
    labs(x=NULL, y=NULL) +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position=c(0.75,0.75)  ) +
    geom_polygon( aes(x=long, y = lat, group = group), fill="#aaaaaa", colour="#ffffff") +
    scale_colour_gradient(low = "#e7e1ef", high = "#8e1236", trans="log10", na.value="#f7f4f9" ) +
    geom_point(data=first_stop_w_nitrate, aes( x=longitude, y=latitude, colour=Nitrate), size=0.5 )
ggsave(file="~/git/oceanography_scripts/images/WOD_OSD_surface_nitrate.pdf", wnit_gg, device="pdf", width=12, height=6)
ggsave(file="~/git/oceanography_scripts/images/WOD_OSD_surface_nitrate.png", wnit_gg, device="png", width=12, height=6, dpi=90)






#