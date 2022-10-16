# convert Wang 2019 data into a table
# and plot some datasets as maps
# created by WRF 2021-03-29
#
# The files modern.mat and preindustrial.mat contain the 3d fields of N2
# fixation inferred from the inverse model using the modern-day and
# preindustrial estimate of atmospheric N deposition (see Wang et al. 2019 for more details).
#
# The files contain the following variables:
#   
#msk           land-sea mask, where wet = 1, dry = 0
#MSK           structure that contains basin masks
#Nfix          3D N2 fixation filed [mmol m^-3 yr^-1]          (input)
#Nriv          Riverine input [mmol N m^-2 yr^-1]              (input)
#Ndep          Atmospheric N depostion [mmol N m^-2 yr^-1].    (input)
#wcD           water-column denitrification [mmol N m^-2 yr^-1](loss)
#sdD           benthic denitrification [mmol N m^-2 yr^-1]     (loss)
#dVt           grid box volumes [m^3].
#dAt           horizontal grid box areas [m^2]
#iwet          index to wet grid boxes
#xt            longitude [deg]
#yt            latitude  [deg]
#zt            depth     [m]

# EXAMPLE CALCULATIONS:
# 1) To make a color map of the water-column integrated N2 fixation rate:
# pcolor(xt,yt,(Nfix(:,:,1).*dVt(:,:,1)+Nfix(:,:,2))./dAt(:,:,1)); % mmolN m^-2 yr^-1
# 2) To get globally integrated N2 fixation rate in TgN/year
# Total_fixation = sum(dVt(iwet).*Nfix(iwet)*1e-15*14);
# where 14 is atomic weight of N, and 1e-15 is for unit converstion
# 3) To get the Atlantic basin integrated N2 fixation rate in TgN/year:
# Atl_fixation = sum(MSK.ATL(iwet).*dVt(iwet).*Nfix(iwet)*1e-15*14);
# 4) To check that the global N budget is balanced:
# dNdt = 14*1e-15*sum(dVt(iwet).*(Nfix(iwet)+Nriv(iwet)+Ndep(iwet)-wcD(iwet)-sdD(iwet))); %TgN/year

library(R.matlab)
library(maps)

###################################################
# process nitrogen model data from Wang 2019 Nature

# data is formatted with longitude from 0-360, instead of -180 to +180
# so remake boxes in order to match other data plots
rx1 = rep(c(seq(0,178,2),seq(-180,-2,2)),rep(90,180))
rx2 = rep(c(seq(2,180,2),seq(-178,0,2)),rep(90,180))
ry1 = rep(seq(-90,88,2),180)
ry2 = rep(seq(-88,90,2),180)

wang2019_column_names = c("rate", "rate_bin", "color", "x1", "y1", "x2", "y2", "category")

# colorsets:
# nfix_color_palette, 16 colors, includes dark gray, then light blue to dark blue
nfix_color_palette = c("#331d00aa", colorRampPalette(c("#152e9608","#152e9611","#152e9633","#152e9666","#4262e5cc"),alpha=TRUE)(15) )

#plot(1:16,1:16,pch=15,cex=5, col=nfix_color_palette)
# dnit_color_palette, 20 colors, light pink to red
dnit_color_palette = c("#331d00aa", rep("#f84f1808",2) , colorRampPalette(c("#f84f1822","#ea3c07ee"), alpha=TRUE)(8), rep("#ea3c07ee",9) )

# process modern data
moderndata_file = "~/project/oceanography/wang2019/modern.mat"
moderndata = readMat(moderndata_file)

# set up volume and area
mod_dVt  = moderndata[["dVt"]]
mod_dAt  = moderndata[["dAt"]]

# get modern integrated nitrogen fixation rate
mod_Nfix = moderndata[["Nfix"]]
modern_int_n2_fix = mod_Nfix[,,1] * mod_dVt[,,1] + mod_Nfix[,,2] / mod_dAt[,,1]
modern_int_n2_fix_has_score = !is.na(c(modern_int_n2_fix)) & c(modern_int_n2_fix)!=0
filt_modern_int_n2_fix = modern_int_n2_fix[modern_int_n2_fix_has_score]
table(modern_int_n2_fix_has_score)
log_modern_int_n2_fix = pmax( floor( log10(modern_int_n2_fix[modern_int_n2_fix_has_score]) ), 2 )
log_modern_int_n2_fix[ filt_modern_int_n2_fix<0 ] = 1 # assign the 4 NAs to 1
#table(log_modern_int_n2_fix)
modern_nfix_df = data.frame(filt_modern_int_n2_fix, log_modern_int_n2_fix,
                            nfix_color_palette[log_modern_int_n2_fix],
                            rx1[modern_int_n2_fix_has_score], ry1[modern_int_n2_fix_has_score], 
                            rx2[modern_int_n2_fix_has_score], ry2[modern_int_n2_fix_has_score],
                            rep("modnfix", length(log_modern_int_n2_fix)),
                            stringsAsFactors = FALSE)
colnames(modern_nfix_df) = wang2019_column_names

pdf(file="~/project/oceanography/wang2019/modern.map_integrated_N_fix.pdf", width=13, height=7)
worldmap = map('world', xlim=c(-180,180), ylim=c(-90,90), fill=FALSE, col="#898989", mar=c(1,1,1,1))
rect(rx1[modern_int_n2_fix_has_score], ry1[modern_int_n2_fix_has_score], 
     rx2[modern_int_n2_fix_has_score], ry2[modern_int_n2_fix_has_score], 
     col=nfix_color_palette[log_modern_int_n2_fix], border=FALSE)
dev.off()


mod_Nriv = moderndata[["Nriv"]]
mod_Ndep = moderndata[["Ndep"]]
mod_Nriv[is.na(mod_Nriv)]=0 # turn all NAs into 0 to sum by depth
mod_Ndep[is.na(mod_Ndep)]=0
modern_N_river = mod_Nriv[,,1] * mod_dAt[,,1] + mod_Ndep[,,1] * mod_dAt[,,1]
modern_N_river_has_score = ( modern_N_river > 0) # ignore zero
filt_modern_N_river = modern_N_river[modern_N_river_has_score]
log_modern_N_river = pmax( floor( log10(modern_N_river[modern_N_river_has_score]) ), 2 )
#table(log_modern_N_river)

modern_nriv_df = data.frame(filt_modern_N_river, log_modern_N_river,
                            nfix_color_palette[log_modern_N_river],
                            rx1[modern_N_river_has_score], ry1[modern_N_river_has_score], 
                            rx2[modern_N_river_has_score], ry2[modern_N_river_has_score],
                            rep("modnriv", length(log_modern_N_river)),
                            stringsAsFactors = FALSE)
colnames(modern_nriv_df) = wang2019_column_names

pdf(file="~/project/oceanography/wang2019/modern.map_river_N_input.pdf", width=13, height=7)
worldmap = map('world', xlim=c(-180,180), ylim=c(-90,90), fill=FALSE, col="#898989", mar=c(1,1,1,1))
rect(rx1[modern_N_river_has_score], ry1[modern_N_river_has_score], 
     rx2[modern_N_river_has_score], ry2[modern_N_river_has_score], 
     col=nfix_color_palette[log_modern_N_river], border=FALSE)
dev.off()


# get water column denitrification
modern_wcD = moderndata[["wcD"]]
modern_wcD_has_score = !is.na(c(moderndata[["wcD"]][,,1]))
modern_wcD[is.na(modern_wcD)] = 0 # turn all NAs into 0 to sum by depth
mod_int_wcD = apply( (modern_wcD * mod_dVt / mod_dAt), MARGIN=c(1,2), FUN=sum) # sum across depth, apparently in units of m2
mod_int_wcD_is_neg = which( mod_int_wcD[modern_wcD_has_score] < 0 ) # could assign negative rates to separate color
col_modern_wcD = pmax( ceiling( log2( pmax(mod_int_wcD[modern_wcD_has_score],1e-100) ) ), 0)+2
col_modern_wcD[mod_int_wcD_is_neg] = 1 # this would assign negative rates to color 1, dark gray
#table(col_modern_wcD)
modern_wcD_df = data.frame(mod_int_wcD[modern_wcD_has_score], col_modern_wcD,
                           dnit_color_palette[col_modern_wcD],
                           rx1[modern_wcD_has_score], ry1[modern_wcD_has_score], 
                           rx2[modern_wcD_has_score], ry2[modern_wcD_has_score],
                           rep("modwcd", length(col_modern_wcD)),
                           stringsAsFactors = FALSE)
colnames(modern_wcD_df) = wang2019_column_names

pdf(file="~/project/oceanography/wang2019/modern.map_water_col_denit.pdf", width=13, height=7)
worldmap = map('world', xlim=c(-180,180), ylim=c(-90,90), fill=FALSE, col="#898989", mar=c(1,1,1,1))
rect(rx1[modern_wcD_has_score], ry1[modern_wcD_has_score], 
     rx2[modern_wcD_has_score], ry2[modern_wcD_has_score], 
     col=dnit_color_palette[col_modern_wcD], border=FALSE)
dev.off()


# get benthic denitrification
modern_sdD = moderndata[["sdD"]]
modern_sdD_has_score = !is.na(c(moderndata[["sdD"]][,,1]))
modern_sdD[is.na(modern_sdD)]=0 # turn all NAs into 0 to sum by depth
mod_int_sdD = apply( (modern_sdD * mod_dVt / mod_dAt), MARGIN=c(1,2), FUN=sum)
mod_int_sdD_is_neg = which( mod_int_sdD[modern_sdD_has_score] < 0 )
col_modern_sdD = pmax( ceiling( log2( pmax(mod_int_sdD[modern_sdD_has_score],1e-100) ) ), 0)+2
col_modern_sdD[mod_int_sdD_is_neg] = 1 # this would assign negative rates to color 1, dark gray
#table(col_modern_sdD)
modern_sdD_df = data.frame(mod_int_sdD[modern_sdD_has_score], col_modern_sdD,
                           dnit_color_palette[col_modern_sdD],
                           rx1[modern_sdD_has_score], ry1[modern_sdD_has_score], 
                           rx2[modern_sdD_has_score], ry2[modern_sdD_has_score],
                           rep("modsdd", length(col_modern_sdD)),
                           stringsAsFactors = FALSE)
colnames(modern_sdD_df) = wang2019_column_names




###
###
###
# get preindustrial N2 fixation

preindustdata_file = "~/project/oceanography/wang2019/preindustrial.mat"
preindustdata = readMat(preindustdata_file)

summary(preindustdata)

pre_Nfix = preindustdata[["Nfix"]]
pre_dVt  = preindustdata[["dVt"]]
pre_dAt  = preindustdata[["dAt"]]

# get preindust integrated nitrogen fixation rate
pre_Nfix = preindustdata[["Nfix"]]
preindust_int_n2_fix = pre_Nfix[,,1] * pre_dVt[,,1] + pre_Nfix[,,2] / pre_dAt[,,1]
preindust_int_n2_fix_has_score = !is.na(c(preindust_int_n2_fix)) & c(preindust_int_n2_fix)!=0
filt_preindust_int_n2_fix = preindust_int_n2_fix[preindust_int_n2_fix_has_score]
table(preindust_int_n2_fix_has_score)
log_preindust_int_n2_fix = pmax( floor( log10(preindust_int_n2_fix[preindust_int_n2_fix_has_score]) ), 2 )
log_preindust_int_n2_fix[ filt_preindust_int_n2_fix<0 ] = 1 # assign the 4 NAs to 1
#table(log_preindust_int_n2_fix)
preindust_nfix_df = data.frame(filt_preindust_int_n2_fix, log_preindust_int_n2_fix,
                               nfix_color_palette[log_preindust_int_n2_fix],
                               rx1[preindust_int_n2_fix_has_score], ry1[preindust_int_n2_fix_has_score], 
                               rx2[preindust_int_n2_fix_has_score], ry2[preindust_int_n2_fix_has_score],
                               rep("prenfix", length(log_preindust_int_n2_fix)),
                               stringsAsFactors = FALSE)
colnames(preindust_nfix_df) = wang2019_column_names

pdf(file="~/project/oceanography/wang2019/preindust.map_integrated_N_fix.pdf", width=13, height=7)
worldmap = map('world', xlim=c(-180,180), ylim=c(-90,90), fill=FALSE, col="#898989", mar=c(1,1,1,1))
rect(rx1[preindust_int_n2_fix_has_score], ry1[preindust_int_n2_fix_has_score], 
     rx2[preindust_int_n2_fix_has_score], ry2[preindust_int_n2_fix_has_score], 
     col=nfix_color_palette[log_preindust_int_n2_fix], border=FALSE)
dev.off()


###
###
###

# combine tables into one data frame
combined_modern_df = rbind(modern_nfix_df,
                           modern_nriv_df,
                           modern_wcD_df,
                           modern_sdD_df )
#
table( combined_modern_df[["category"]] )
# write table of modern data
moderndata_outfile = "~/git/marina/datasets/wang2019_combined_data.tab"
write.table(combined_modern_df, file=moderndata_outfile, sep="\t", row.names = FALSE)

#
dim(combined_modern_df)


npp_data_file = "~/git/marina/datasets/wang2019/npp_90x180.mat"
npp_data = readMat(npp_data_file)
npp_raw = npp_data$npp
npp_has_data = !is.na(npp_raw)
npp_col = pmax(round(log(npp_raw[npp_has_data]+0.01)),1)
npp_col = 1
npp_colors = colorRampPalette(c("#e5f5e000","#41ab5dcc"),alpha=TRUE)(8)

npgg = ggplot(worldpolygons, aes(long,lat, group=group)) +
        # coord_cartesian(xlim=c(-180,180), ylim=c(-80, 80) ) +
        coord_fixed(xlim = c(-180, 180),  ylim = c(-80, 80), ratio = 1.3) +
        #    scale_x_continuous(expand = c(0,0), limits=c(-180,185)) +
        #   scale_y_continuous(expand = c(0,0), limits=c(-80, 80))+
        labs(title="Global NPP",
             subtitle="from Wang (2019)") +
        geom_polygon( data=worldpolygons, aes(x=long, y = lat, group = group), 
                      fill="#767676", colour="#ffffff" ) +
        annotate( geom="rect", xmin=rx1[npp_has_data], xmax=rx2[npp_has_data], ymin=ry1[npp_has_data], ymax=ry2[npp_has_data], 
                  fill=npp_colors[npp_col] )
ggsave("~/git/marina/datasets/wang2019/npp_90x180.pdf", npgg, device="pdf", width=13, height=7)

#