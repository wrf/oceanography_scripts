# historical fisheries replots
#
# cod fish data from NOAA
# Mayo et al, The 2008 Assessment of the Gulf of Maine Atlantic Cod (Gadus morhua) Stock
# https://apps-nefsc.fisheries.noaa.gov/rcb/publications/center-reference-documents.html
# and 
# Alexander et al 2009 Gulf of Maine cod in 1861: Historical analysis of fishery logbooks, with ecosystem implications
# https://doi.org/10.1111/j.1467-2979.2009.00334.x
#
# created by WRF 2022-10-22

library(ggplot2)

nmsf_data_file = "~/git/oceanography_scripts/data/mayo_2008_USA_codfish_landings_table1.tab"
nmsf_data = read.table(nmsf_data_file, header = TRUE, sep = "\t")

a2009_data_file = "~/git/oceanography_scripts/data/alexander_2009_fig4_codfish_landings.txt"
a2009_data = read.table(a2009_data_file, header = TRUE, sep = "\t")

gp = ggplot(data=nmsf_data , aes(x=year, y=total) ) +
  theme(legend.position="none",
        panel.background = element_rect(fill = "#e4ecfb"),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18),
        plot.title = element_text(size=23) ) +
  labs(x=NULL, y="Landings (tonnes)",
       title="Historical landings of cod (Gadus morhua) in North America",
       caption = "Data from: Alexander et al (2009) Gulf of Maine cod in 1861: Historical analysis of fishery logbooks, with ecosystem implications. Fish and Fisheries\nMSY: Maximum Sustainable Yield; SSB: Spawning Stock Biomass") +
  scale_x_continuous(limits = c(1848,2010), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,85000), expand = c(0,0) ) +
  geom_hline(yintercept = 16600, color="#991d00", size=3, alpha=0.8) + 
  geom_hline(yintercept = 82830, color="#170c25", size=3, alpha=0.8) +
  annotate(geom="text", x=1860, y=22000, label="MSY", color="#991d00", size=6) +
  annotate(geom="text", x=2000, y=78000, label="SSB", color="#170c25", size=6) +
  geom_point( color="#004dac", size=6, alpha=0.8) +
  geom_segment( data =a2009_data, aes(x=year, xend=year, y=landings_t_min, yend=landings_t_max), color = "#112994", alpha=0.5, size=3) +
  geom_point( data =a2009_data, aes(x = year, y = landings_t_min), color = "#112994", alpha=0.7, size=6) +
  geom_point( data =a2009_data, aes(x = year, y = landings_t_max), color = "#112994", alpha=0.7, size=6)
gp

ggsave(filename = "~/git/oceanography_scripts/images/historical_codfish_landings_from_alexander2009.pdf", plot = gp, device ="pdf" , width = 12, height = 7, units = "in")
ggsave(filename = "~/git/oceanography_scripts/images/historical_codfish_landings_from_alexander2009.png", plot = gp, device ="png" , width = 12, height = 7, units = "in", dpi = 90)








#