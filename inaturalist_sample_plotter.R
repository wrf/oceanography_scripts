# make static maps of iNaturalist csv data
# created by WRF 2023-06-19

library(maps)
library(ggplot2)


sample_data_file = "~/git/oceanography_scripts/data/observations-337199.csv"
sample_data = read.csv(sample_data_file)
head(sample_data)

table(sample_data$coordinates_obscured)

# is_in_thailand = sample_data$time_zone=="Hanoi"
is_cots = sample_data$scientific_name=="Acanthaster"
#table(is_in_thailand)
sample_data.thai = sample_data[is_in_thailand]
sample_data.cots = sample_data[is_cots]

TL_cots_lat = c(7.580)
TL_cots_lon = c(98.522)

table(sample_data$observed_on)

worldpolygons = map_data("world")

ggplot(worldpolygons) +
  coord_cartesian(xlim = c(70,130), ylim = c(-5,25), expand = c(0,0)) +
  labs(x=NULL, y=NULL) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position=c(0.75,0.75)  ) +
  geom_polygon( aes(x=long, y = lat, group = group), fill="#aaaaaa", colour="#ffffff") +
  annotate("rect", xmin = 98, xmax = 103, ymin = 8, ymax = 15, fill=NA, colour="black", size=2 ) +
  annotate("point", x = TL_cots_lon, y = TL_cots_lat, fill="#f5d900", colour="black", shape=21, size=5 )
ggsave("~/project/crown_of_thorns_starfish/indo_pacific_map.png", device = "png", width = 8, height = 4, units = "in", dpi = 90)

ifelse(sample_data$coordinates_obscured, sample_data$private_latitude, sample_data$latitude)
ifelse(sample_data$coordinates_obscured, sample_data$private_longitude, sample_data$longitude)
sample_data$coordinates_obscured

ggplot(worldpolygons) +
  coord_cartesian(xlim = c(95,105), ylim = c(5,15), expand = c(0,0)) +
  labs(x=NULL, y=NULL) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position=c(0.75,0.75)  ) +
  geom_polygon( aes(x=long, y = lat, group = group), fill="#aaaaaa", colour="#ffffff")

#














#