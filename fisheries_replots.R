# historical fisheries replots
#
# created by WRF 2022-10-22
# last modified 2023-06-14

library(ggplot2)


# cod fish data from NOAA
# Mayo et al, The 2008 Assessment of the Gulf of Maine Atlantic Cod (Gadus morhua) Stock
# https://apps-nefsc.fisheries.noaa.gov/rcb/publications/center-reference-documents.html
# and 
# Alexander et al 2009 Gulf of Maine cod in 1861: Historical analysis of fishery logbooks, with ecosystem implications
# https://doi.org/10.1111/j.1467-2979.2009.00334.x

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
  geom_hline(yintercept = 16600, color="#991d00", size=2, alpha=0.8, linetype="dashed") + 
  geom_hline(yintercept = 82830, color="#170c25", size=2, alpha=0.8, linetype="dashed") +
  annotate(geom="text", x=1860, y=22000, label="MSY", color="#991d00", size=6) +
  annotate(geom="text", x=2000, y=78000, label="SSB", color="#170c25", size=6) +
  geom_point( color="#004dac", size=6, alpha=0.8) +
  geom_segment( data =a2009_data, aes(x=year, xend=year, y=landings_t_min, yend=landings_t_max), color = "#112994", alpha=0.5, size=3) +
  geom_point( data =a2009_data, aes(x = year, y = landings_t_min), color = "#112994", alpha=0.7, size=6) +
  geom_point( data =a2009_data, aes(x = year, y = landings_t_max), color = "#112994", alpha=0.7, size=6)
gp

ggsave(filename = "~/git/oceanography_scripts/images/historical_codfish_landings_from_alexander2009.pdf", plot = gp, device ="pdf" , width = 12, height = 7, units = "in")
ggsave(filename = "~/git/oceanography_scripts/images/historical_codfish_landings_from_alexander2009.png", plot = gp, device ="png" , width = 12, height = 7, units = "in", dpi = 90)

# Mediterranean grouper data from
# Coll et al (2004) Spear fishing in the Balearic Islands (west central Mediterranean): 
# species affected and catch evolution during the period 1975-2001. 
# Fisheries Research 70: 97–111.

spfish_table2_file = "~/git/oceanography_scripts/data/coll_2004_table2.tab"
spfish_table2 = read.table(spfish_table2_file, header = TRUE, sep = "\t")

gp = ggplot( data=spfish_table2 , aes(x=Year, y=MWF_kg) ) +
  theme(legend.position="none",
        panel.background = element_rect(fill = "#e4ecfb"),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18),
        plot.title = element_text(size=23) ) +
  labs(x=NULL, y="Mean weight of fish (kg)",
       title="Spearfishing catches from 1975 to 2000",
       subtitle="for the first ten classifieds of each sport spearfishing competition in the Balearic Islands",
       caption = "Data from: Coll et al (2004) Spear fishing in the Balearic Islands (west central Mediterranean): \nspecies affected and catch evolution during the period 1975-2001. \nFisheries Research 70: 97-111.") +
  scale_x_continuous(limits = c(1974,2001), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,1.5), expand = c(0,0) ) +
  geom_point( color="#000a5a", size=6, alpha=0.8)
gp
ggsave(filename = "~/git/oceanography_scripts/images/spearfish_catch_tab2_from_coll2004.pdf", plot = gp, device ="pdf" , width = 8, height = 5, units = "in")
ggsave(filename = "~/git/oceanography_scripts/images/spearfish_catch_tab2_from_coll2004.png", plot = gp, device ="png" , width = 8, height = 5, units = "in", dpi = 90)

spfish_table4_file = "~/git/oceanography_scripts/data/coll_2004_table4.tab"
spfish_table4 = read.table(spfish_table4_file, header = TRUE, sep = "\t")
gp = ggplot( data=spfish_table4 , aes(x=Year, y=Mean_weight) ) +
  theme(legend.position="none",
        panel.background = element_rect(fill = "#e4ecfb"),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18),
        plot.title = element_text(size=23) ) +
  labs(x=NULL, y="Mean weight of fish (kg)",
       title=expression( italic("E. marginatus")~"catches from 1975 to 2000"),
       subtitle="for the first ten classifieds of each sport spearfishing competition in the Balearic Islands",
       caption = "Data from: Coll et al (2004) Spear fishing in the Balearic Islands (west central Mediterranean): \nspecies affected and catch evolution during the period 1975-2001. \nFisheries Research 70: 97-111.") +
  scale_x_continuous(limits = c(1974,2001), expand = c(0,0)) +
  scale_y_continuous(limits = c(0,30), expand = c(0,0) ) +
  geom_point( color="#000a5a", size=6, alpha=0.8) +
  geom_errorbar( aes(x=Year, ymin=Minimum_weight, ymax=Maximum_weight), color="#000a5a", size=1, alpha=0.3)
gp
ggsave(filename = "~/git/oceanography_scripts/images/spearfish_catch_tab4_from_coll2004.pdf", plot = gp, device ="pdf" , width = 8, height = 5, units = "in")
ggsave(filename = "~/git/oceanography_scripts/images/spearfish_catch_tab4_from_coll2004.png", plot = gp, device ="png" , width = 8, height = 5, units = "in", dpi = 90)

#