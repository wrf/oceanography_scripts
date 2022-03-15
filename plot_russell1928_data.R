# replotting data from Russell 1928
# The Vertical Distribution of Marine Macroplankton. VI. Further Observations on Diurnal Changes
#
# https://doi.org/10.1017/S0025315400055545
#
# created by WRF 2022-03-15

# data copied from tables at the end of Russell 1928
vm_data = read.table("~/git/oceanography_scripts/data/russell1928_vertical_migration_data.tab", header=TRUE, sep="\t")

has_enough_counts = as.logical(colSums(vm_data[,6:57]) > 20)
#has_enough_counts
num_columns_w_enough = sum(has_enough_counts)
# 42 columns have more than 20 counts

# convert day-hour-clock time to minutes
clock_hour = as.integer(sapply( strsplit( as.character(vm_data$time) , ":" , fixed=TRUE), "[", 1))
clock_minute = as.integer(sapply( strsplit( as.character(vm_data$time) , ":" , fixed=TRUE), "[", 2))
exp_time = (vm_data$day - 17)*24*60 + 60*clock_hour + clock_minute

# fix many outdated species names
species = names(vm_data)[6:57]
species[1] = "Corymorpha nutans" # from Steenstrupia_rubra
species[3] = "Amphinema dinema" # from Stomatoca_dinema # typo of Stomotoca
species[4] = "Neoturris pileata" # from Turris_pileata , genus name in use for molluscs
species[9] = "Eutira gracilis" # from Saphenia_gracilis
species[24] = "Anchialina agilis" # from Anchialus_agilis

# check for errors in counts
#rowSums(vm_data[,6:57])
#vm_data$total_reported
total_count_error = vm_data$total_reported - rowSums(vm_data[,6:57])
# there are plenty
# > total_count_error
#  [1]     0     0     0     0     0     0     0     0     0     0     0     0     0 -1000     0     0     0
# [18]     0     0     0     0     0     0     0     0     0     0     5    -4    -5     0     0  -300  -900
# [35]   -18  -109     0     0     0  -110 -1000     0     0     0     0     0     0  -180     0     0     0
# [52]    50 -2010     1

# blue-yellow-blue color palette
daytime_cols = colorRampPalette( c("#081d5855","#feb24c55","#54278f55"))(24)
#plot(1:24, 1:24, col=daytime_cols, pch=16, cex=3)
#plot(exp_time,  vm_data$depth, col=daytime_cols[clock_hour+1], pch=16, ylim=c(50,0))


# plot a trace for each species
pdf("~/git/oceanography_scripts/images/russell1928_vertical_migration_data.6pages.pdf", width=8, height=11)
par(mfrow=c(7,1), mar=c(2.5,4.5,1,1))
for (i in (6:57)[has_enough_counts] ){
  plot(exp_time, vm_data$depth, xlim=c(900,3360), ylim=c(55,-4), frame.plot=FALSE, axes=FALSE,
       main="", xlab="", ylab="Depth (m)",
       pch=ifelse(vm_data[,i]>0,16,1), 
       col=ifelse(vm_data[,i]>0,daytime_cols[clock_hour+1],"#00000033"), 
       cex=log10( vm_data[,i]+0.1)+2 ) 
  text(2100, 50, gsub("_"," ",species[i-5]), cex=1.2, font=3)
  axis(2)
  axis(1, at=seq(840,3300,240), labels = c("14:00", "18:00", "22:00", "02:00", "06:00", "10:00", "14:00", "18:00", "22:00", "02:00", "06:00") )
}
dev.off()

example_species = c(7,9,15,18,21,23)

#