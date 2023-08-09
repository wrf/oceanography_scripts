#
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

moderndata_file = "~/project/oceanography/wang2019/modern.mat"
moderndata = readMat(moderndata_file)

summary(moderndata)

xt = moderndata[["xt"]]
yt = moderndata[["yt"]]

Nfix = moderndata[["Nfix"]]
dVt  = moderndata[["dVt"]]
dAt  = moderndata[["dAt"]]

modern_int_n2_fix = Nfix[,,1] * dVt[,,1] + Nfix[,,2] / dAt[,,1]
modern_int_n2_fix_has_score = !is.na(c(modern_int_n2_fix))
dim(modern_int_n2_fix)

xbox_pos = rep(c(seq(181,359,2),seq(1,179,2)),rep(90,180))[modern_int_n2_fix_has_score]
ybox_pos = rep(seq(-89,89,2),180)[modern_int_n2_fix_has_score]
log_modern_int_n2_fix = floor( log10(modern_int_n2_fix[modern_int_n2_fix_has_score] + 0.1 ) ) + 3
log_modern_int_n2_fix[is.na(log_modern_int_n2_fix)] = 1

nfix_colors = c("#771d00aa", colorRampPalette(c("#152e9608","#152e9611","#152e9622","#152e9633","#152e9644","#4262e5cc"),alpha=TRUE)(15) )

pdf(file="~/project/oceanography/wang2019/modern.plot_integrated_N_fix.pdf", width=13, height=7)
par(mar=c(1,1,2,1))
plot(xbox_pos, ybox_pos, pch=15, cex=1.0, col=nfix_colors[log_modern_int_n2_fix], 
     frame.plot=FALSE, axes=FALSE, xlab="", ylab="", main="Modern depth-integrated N2-fixation rate\nfrom Wang 2019")
dev.off()

pdf(file="~/project/oceanography/wang2019/modern.image_integrated_N_fix.pdf", width=12, height=7)
image( t(modern_int_n2_fix), col=colorRampPalette(c("#152e9611","#152e96aa"),alpha=TRUE)(16), 
       axes=FALSE, xlab="Modern depth-integrated N2-fixation rate\nfrom Wang 2019" )
dev.off()

mod_Nriv = moderndata[["Nriv"]]
mod_Ndep = moderndata[["Ndep"]]

mod_Nriv[is.na(mod_Nriv)]=0 # turn all NAs into 0 to sum by depth
#modern_N_river = apply( (mod_Nriv * mod_dVt ), MARGIN=c(1,2), FUN=sum)
modern_N_river = mod_Nriv[,,1] * mod_dVt[,,1] + mod_Ndep[,,1] * mod_dVt[,,1]
modern_N_river
image( t(modern_N_river) )
modern_N_river_has_score = (!is.na(modern_N_river) & modern_N_river > 0) # ignore zero
table(modern_N_river_has_score)
image( t(modern_N_river_has_score) )
filt_modern_N_river = modern_N_river[modern_N_river_has_score]
log_modern_N_river = pmax( floor( log10(filt_modern_N_river[modern_N_river_has_score]) ), 2 )
hist(log_modern_N_river)

pdf(file="~/project/oceanography/wang2019/modern.map_integrated_N_fix.pdf", width=13, height=7)
worldmap = map('world', xlim=c(-180,180), ylim=c(-90,90), fill=FALSE, col="#898989", mar=c(1,1,1,1))
#plot(0,0,type='n', xlim=c(-180,180),ylim=c(-90,90), frame.plot=FALSE, axes=FALSE, xlab="", ylab="",
#     main="Pre-industrial depth-integrated N2-fixation rate\nfrom Wang 2019" )
rect(rx1[modern_int_n2_fix_has_score], ry1[modern_int_n2_fix_has_score], 
     rx2[modern_int_n2_fix_has_score], ry2[modern_int_n2_fix_has_score], 
     col=nfix_color_palette[log_modern_int_n2_fix], border=FALSE)
rect(rx1[modern_N_river_has_score], ry1[modern_N_river_has_score], 
     rx2[modern_N_river_has_score], ry2[modern_N_river_has_score], 
     col=nfix_color_palette[log_modern_N_river], border=FALSE)
dev.off()


###
###
###

preindustdata_file = "~/project/oceanography/wang2019/preindustrial.mat"
preindustdata = readMat(preindustdata_file)

summary(preindustdata)

Nfix = preindustdata[["Nfix"]]
dVt  = preindustdata[["dVt"]]
dAt  = preindustdata[["dAt"]]

x = c(1,2,5,3,6,0,NaN,3,2,NaN,0,0,0,10)
x[!is.na(x) & x!=0]
x[is.na(x)]

preindust_int_n2_fix = Nfix[,,1] * dVt[,,1] + Nfix[,,2] / dAt[,,1]
preindust_int_n2_fix_has_score = !is.na(c(preindust_int_n2_fix))
dim(preindust_int_n2_fix)

xbox_pos = rep(c(seq(1,179,2),seq(-179,-1,2)),rep(90,180))[preindust_int_n2_fix_has_score]
ybox_pos = rep(seq(-89,89,2),180)[preindust_int_n2_fix_has_score]
log_preindust_int_n2_fix = floor( log10(preindust_int_n2_fix[preindust_int_n2_fix_has_score] + 0.1 ) ) + 3
log_preindust_int_n2_fix[is.na(log_preindust_int_n2_fix)] = 1
nfix_colors = c("#331d00aa", colorRampPalette(c("#152e9608","#152e9611","#152e9622","#152e9633","#152e9644","#4262e5cc"),alpha=TRUE)(15) )


pdf(file="~/project/oceanography/wang2019/preindustrial.plot_integrated_N_fix.pdf", width=13, height=7)
par(mar=c(1,1,2,1))
plot(xbox_pos, ybox_pos, pch=15, cex=1.0, col=nfix_colors[log_preindust_int_n2_fix], 
     frame.plot=FALSE, axes=FALSE, xlab="", ylab="", main="Pre-industrial depth-integrated N2-fixation rate\nfrom Wang 2019")
dev.off()
#

rx1 = rep(c(seq(0,178,2),seq(-180,-2,2)),rep(90,180))
rx2 = rep(c(seq(2,180,2),seq(-178,0,2)),rep(90,180))
ry1 = rep(seq(-90,88,2),180)
ry2 = rep(seq(-88,90,2),180)

pdf(file="~/project/oceanography/wang2019/preindustrial.rect_integrated_N_fix.pdf", width=13, height=7)
par(mar=c(1,1,2,1))
plot(0,0,type='n', xlim=c(-180,180),ylim=c(-90,90), frame.plot=FALSE, axes=FALSE, xlab="", ylab="",
     main="Pre-industrial depth-integrated N2-fixation rate\nfrom Wang 2019" )
rect(rx1[preindust_int_n2_fix_has_score], ry1[preindust_int_n2_fix_has_score], 
     rx2[preindust_int_n2_fix_has_score], ry2[preindust_int_n2_fix_has_score], 
     col=nfix_colors[log_preindust_int_n2_fix], border=FALSE)
dev.off()

# water column denitrification
preindust_wcD = preindustdata[["wcD"]]
preindust_wcD[is.na(preindust_wcD)]=0 # turn all NAs into 0 to sum by depth
int_wcD = apply( (preindust_wcD * dVt / dAt), MARGIN=c(1,2), FUN=sum)
int_wcD[int_wcD==0] = -99
preindust_int_wcD_has_score = !is.na(c(preindustdata[["wcD"]][,,1]))
col_preindust_int_wcD = ceiling( (int_wcD[preindust_int_wcD_has_score]/50) +2 )
#table(col_preindust_int_wcD)
surfdenit_colors = c( rep("#f8b51800",3) , colorRampPalette(c("#f8b51822","#eaa607cc"), alpha=TRUE)(7), rep("#e7a407ee",9) )

# benthic denitrification
preindust_sdD = preindustdata[["sdD"]]
preindust_sdD[is.na(preindust_sdD)]=0 # turn all NAs into 0 to sum by depth
int_sdD = apply( (preindust_sdD * dVt / dAt), MARGIN=c(1,2), FUN=sum)
int_sdD[int_sdD==0] = -99
preindust_int_sdD_has_score = !is.na(c(preindustdata[["sdD"]][,,1]))
col_preindust_int_sdD = ceiling( (int_sdD[preindust_int_sdD_has_score]/50) +2 )
#table(col_preindust_int_sdD)
benthdenit_colors = c( "#f84f1800" ,  colorRampPalette(c("#f84f1822","#ea3c07ee"), alpha=TRUE)(17) )

denit_colors = c( rep("#f84f1800",3) , colorRampPalette(c("#f84f1822","#ea3c07ee"), alpha=TRUE)(8), rep("#ea3c07ee",9) )

plot(0,0,type='n', xlim=c(-180,180),ylim=c(-90,90), frame.plot=FALSE, axes=FALSE, xlab="", ylab="",
     main="Pre-industrial benthic denitrification rate\nfrom Wang 2019" )
rect(rx1[preindust_int_wcD_has_score], ry1[preindust_int_wcD_has_score], 
     rx2[preindust_int_wcD_has_score], ry2[preindust_int_wcD_has_score], 
     col=surfdenit_colors[col_preindust_int_wcD], border=FALSE)
rect(rx1[preindust_int_sdD_has_score], ry1[preindust_int_sdD_has_score], 
     rx2[preindust_int_sdD_has_score], ry2[preindust_int_sdD_has_score], 
     col=benthdenit_colors[col_preindust_int_sdD], border=FALSE)

# add plot onto map
pdf(file="~/project/oceanography/wang2019/preindustrial.map_integrated_N_fix.pdf", width=13, height=7)
worldmap = map('world', xlim=c(-180,180), ylim=c(-90,90), fill=FALSE, col="#898989", mar=c(1,1,1,1))
rect(rx1[preindust_int_n2_fix_has_score], ry1[preindust_int_n2_fix_has_score], 
     rx2[preindust_int_n2_fix_has_score], ry2[preindust_int_n2_fix_has_score], 
     col=nfix_colors[log_preindust_int_n2_fix], border=FALSE)
rect(rx1[preindust_int_wcD_has_score], ry1[preindust_int_wcD_has_score], 
     rx2[preindust_int_wcD_has_score], ry2[preindust_int_wcD_has_score], 
     col=denit_colors[col_preindust_int_wcD], border=FALSE)
rect(rx1[preindust_int_sdD_has_score], ry1[preindust_int_sdD_has_score], 
     rx2[preindust_int_sdD_has_score], ry2[preindust_int_sdD_has_score], 
     col=denit_colors[col_preindust_int_sdD], border=FALSE)

dev.off()

