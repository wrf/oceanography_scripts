#

meta_lca_dohrmann = c(-937, -787)
meta_lca_dosreis = c(-833,-649)
meta_lca_erwin = c(-784,-747)
meta_lca_lartillot = c(-860,-710)
meta_lca_mids = c(-850, -750, -780, -795)

fungi_lca = c(-750, -550)
asco_lca = c(-570, -455)
asco_lca_mids = c(-515)
basidio_lca = c(-540, -410)
basidio_lca_mids = c(-475)

#rhodo_lca_yang = c(-817, -1047) # florideo + bangio
rhodo_lca_yang = c(-879, -681) # crown florideo
rhodo_lca_parfrey = c(-915, -630)
rhodo_lca_berney = c(-929, -600)
#rhodo_lca_mids = c(-943, -765, -780)
rhodo_lca_mids = c(-781, -765, -780)


chloro_lca = c(-469, -420)
chloro_lca_morris = c(-515,-470)
chloro_lca_magallon = c(-480, -471)
# chloro_lca_clarke = c(-815, -568) @ w 1042 mya basal max
chloro_lca_clarke = c(-660, -489)
chloro_lca_mids = c(-500, -475, -505)

# crown group
#phaeo_lca_silber = c(-162, -98)
#phaeo_lca_mids = c(-128)
# laminariales
phaeo_lca_silber = c(-110, -59)
phaeo_lca_mids = c(-84)


textsize = 1.4
maxy = 6

meta_body = c(-570)
chloro_body = c(-474)
rhodo_body = c(-580)
phaeo_body = c(-13)



pdf(file="~/git/oceanography_scripts/multicellularity/complex_multicellularity_plot.pdf", width=10, height=7)
par(mar=c(4.5,1,1,0.2))

plot(0,0,type='n',xlim=c(-1100,0),ylim=c(0,maxy),axes=FALSE,frame.plot=FALSE, xlab="Time (Ma)", ylab="", cex.lab=1.3)
axis(1,at=seq(-1000,0,100),labels=rev(seq(0,1000,100)),cex.axis=1.4)

#snowball earths
rect(-717, 0, -659, maxy, col="#8e8eae99", border=FALSE)
rect(-646, 0, -635, maxy, col="#8e8eae99", border=FALSE)

# draw geological periods
period_letters = c("Tonian","Cr","Ed","C","O","S","D","C","P","T","J","K","Pe","N")
period_starts =  c(-1000,-720,-635,-541,-485,-443,-419,-358,-298,-252,-201,-145,-66,-23)
period_ends = c(-720,-635,-541,-485,-443,-419,-358,-298,-252,-201,-145,-66,-23,0)
period_color = c("#c6c6c6", "#63a9c3", "#d6ab24", "#6da26e", "#28a46e", "#a5e3e5", "#d48f16", "#4fa092", "#d9491c", "#be58d0", "#4d94de", "#26bf3f", "#dba426", "#baab09")
rect(period_starts,-0.2,period_ends,0, border="#FFFFFF", col=period_color)
text( (period_starts+period_ends)/2, -0.1, period_letters)
text( -517, -0.1, "-", cex=1.1) # add dash for Cambrian

legend(-250,maxy, legend=c("Mean MC probability","Earliest fossils"), pch=c(5,17), pt.cex=c(1.1,1.4), pt.lwd=2, cex=1.1)


#rect(meta_lca[1],   0.80*maxy, meta_lca[2],   0.94*maxy, col="#6816b599", border=FALSE)
text(-800, 0.98*maxy, "Animals", col="#6816b5", cex=textsize)
rect(meta_lca_dohrmann[1],   0.80*maxy, meta_lca_dohrmann[2],   0.83*maxy, col="#6816b599", border=FALSE)
text(-937, 0.81*maxy, "Dohrmann 2017 MC", pos=2)
rect(meta_lca_dosreis[1],   0.84*maxy, meta_lca_dosreis[2],   0.87*maxy, col="#6816b599", border=FALSE)
text(-833, 0.855*maxy, "Dos Reis 2015 MC", pos=2)
rect(meta_lca_erwin[1],   0.88*maxy, meta_lca_erwin[2],   0.91*maxy, col="#6816b599", border=FALSE)
text(-784, 0.895*maxy, "Erwin 2011 MC", pos=2)
rect(meta_lca_lartillot[1],   0.92*maxy, meta_lca_lartillot[2],   0.95*maxy, col="#6816b599", border=FALSE)
text(-860, 0.935*maxy, "Lartillot 2009 MC", pos=2)
#text(-800, 0.98*maxy, "Animals", col="#000000", cex=textsize)
points(meta_lca_mids, c(0.815,0.855,0.895,0.935)*maxy, pch=5, col="#6816b5")

points(meta_body, rep(0.89*maxy,length(meta_body)), cex=2, pch=17, col="#6816b5")
text(meta_body+10, 0.89*maxy, "Animal fossils", pos=4)
points(-652, 0.935*maxy, cex=2, pch=15, col="#6816b5")
text(-645, 0.935*maxy, "Sponge biomarkers", pos=4)


#rect(fungi_lca[1],  0.56*maxy, fungi_lca[2],  0.70*maxy, col="#38a1a299", border=FALSE)
text(-440, 0.79*maxy, "Ascomycota (Fungi)", col="#389192", cex=textsize)
rect(asco_lca[1],  0.68*maxy, asco_lca[2],  0.71*maxy, col="#389192dd", border=FALSE)
text(asco_lca[2], 0.695*maxy, "Chang 2015 MC", pos=4)
points(asco_lca_mids, c(0.695)*maxy, pch=5, col="#387172", lwd=1.2)
points(-400, 0.74*maxy, cex=2, pch=17, col="#389192")
text(-390, 0.74*maxy, "Ascomycota fossils", pos=4)

text(-430, 0.63*maxy, "Basidiomycota (Fungi)", col="#4075b2", cex=textsize)
rect(basidio_lca[1],  0.52*maxy, basidio_lca[2],  0.55*maxy, col="#4075b2dd", border=FALSE)
text(basidio_lca[2], 0.535*maxy, "Chang 2015 MC", pos=4)
points(basidio_lca_mids, c(0.535)*maxy, pch=5, col="#406592", lwd=1.2)
points(-330, 0.58*maxy, cex=2, pch=17, col="#4075b2")
text(-320, 0.58*maxy, "Basidiomycota fossils", pos=4)


# 0.49 as max
text(-820, 0.51*maxy, "Florideophytes\n(red algae)", col="#bc3c0a", cex=textsize)
rect(rhodo_lca_yang[1],  0.30*maxy, rhodo_lca_yang[2],  0.33*maxy, col="#bc3c0a99", border=FALSE)
text(rhodo_lca_yang[1], 0.315*maxy, "Yang 2016 MC", pos=2)
rect(rhodo_lca_parfrey[1],  0.34*maxy, rhodo_lca_parfrey[2],  0.37*maxy, col="#bc3c0a99", border=FALSE)
text(rhodo_lca_parfrey[1], 0.355*maxy, "Parfrey 2011 MC", pos=2)
rect(rhodo_lca_berney[1],  0.38*maxy, rhodo_lca_berney[2],  0.41*maxy, col="#bc3c0a99", border=FALSE)
text(rhodo_lca_berney[1], 0.395*maxy, "Berney 2006 MC", pos=2)
points(rhodo_body, rep(0.43*maxy,length(rhodo_body)), cex=2, pch=17, col="#bc3c0a")
text(rhodo_body[1]-10, 0.43*maxy, "Florideophyceae fossils", pos=2)
points(rhodo_lca_mids, c(0.315,0.355,0.395)*maxy, pch=5, col="#bc3c0a")

text(-440, 0.32*maxy, "Embryophytes (land plants)", col="#18d025", cex=textsize)
rect(chloro_lca_morris[1], 0.14*maxy, chloro_lca_morris[2], 0.17*maxy, col="#18d02599", border=FALSE)
text(chloro_lca_morris[2], 0.155*maxy, "Morris 2018 MC", pos=4)
rect(chloro_lca_magallon[1], 0.18*maxy, chloro_lca_magallon[2], 0.21*maxy, col="#18d02599", border=FALSE)
text(chloro_lca_magallon[2], 0.195*maxy, "Magallon 2013 MC", pos=4)
rect(chloro_lca_clarke[1], 0.22*maxy, chloro_lca_clarke[2], 0.25*maxy, col="#18d02599", border=FALSE)
text(chloro_lca_clarke[2], 0.23*maxy, "Clarke 2011 MC", pos=4)
points(chloro_lca_mids, c(0.155,0.195,0.235)*maxy, pch=5, col="#149025")
points(chloro_body, rep(0.27*maxy,length(chloro_body)), cex=2, pch=17, col="#18d025")
text(chloro_body[1]+10, 0.27*maxy, "Plant fossils", pos=4)

text(-110, 0.15*maxy, "Laminarialeans\n(brown algae)", col="#8d8f0d", cex=textsize)
rect(phaeo_lca_silber[1],  0.02*maxy, phaeo_lca_silber[2],  0.05*maxy, col="#8d8f0d99", border=FALSE)
text(phaeo_lca_silber[1], 0.0325*maxy, "Silberfeld 2010 MC", pos=2)
points(phaeo_body, 0.07*maxy, cex=2, pch=17, col="#8d8f0d")
text(phaeo_body-10, 0.07*maxy, "Laminariales fossils", pos=2)
points(phaeo_lca_mids, c(0.035)*maxy, pch=5, col="#6d6f0d", lwd=1.2)
#points(-680,0.1*maxy,cex=2,pch=8,lwd=2,col="#63a9c3")

dev.off()









#