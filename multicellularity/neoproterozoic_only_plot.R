


meta_lca_mids = c(-850, -750, -780, -795)

rhodo_lca_mids = c(-783, -765, -780)


#par(mfrow=c(3,1))
#par(mfrow=c(1,1))
maxy = 10

pdf(file="~/git/oceanography_scripts/multicellularity/neoproterozoic_plot.pdf", width=8, height=7)
#png(file="~/git/oceanography_scripts/multicellularity/neoproterozoic_plot.png", width=800, height=700)

par(mar=c(4.5,5,1,0.5))


plot(0,0,type='n',xlim=c(-1000,-500),ylim=c(0,maxy),axes=FALSE,frame.plot=FALSE, xlab="Time (Ma)", ylab="", cex.lab=1.3)
axis(1,at=seq(-1000,-500,100),labels=rev(seq(500,1000,100)),cex.axis=1.4)

#snowball earths
rect(-717, 0, -659, maxy, col="#8e8eae99", border=FALSE)
rect(-646, 0, -635, maxy, col="#8e8eae99", border=FALSE)


### draw geological periods
period_letters = c("Tonian","Cr","Ed","C","O","S","D","C","P","T","J","K","Pe","N")
period_starts =  c(-1000,-720,-635,-541,-485,-443,-419,-358,-298,-252,-201,-145,-66,-23)
period_ends = c(-720,-635,-541,-485,-443,-419,-358,-298,-252,-201,-145,-66,-23,0)
period_color = c("#c6c6c6", "#63a9c3", "#d6ab24", "#6da26e", "#28a46e", "#a5e3e5", "#d48f16", "#4fa092", "#d9491c", "#be58d0", "#4d94de", "#26bf3f", "#dba426", "#baab09")
rect(period_starts[1:4],-0.38,period_ends[1:4],-0.1, border="#FFFFFF", col=period_color)
text( (period_starts[1:4]+period_ends[1:4])/2, -0.225, period_letters[1:4])
text( -516, -0.225, "-", cex=1.1) # add dash for Cambrian

### DRAW DIVERSITY CURVE

cohendata = read.table("~/git/oceanography_scripts/multicellularity/Cohen_Macdonald_2015_Mya_vs_WAD.tab", header=TRUE, sep="\t")

cohenspline = smooth.spline(cohendata[,1], cohendata[,2], df=36)
head(cohendata)

#axis(2, at=c(0,1,2), labels=c(0,10,20))
#lines(-cohendata[,1],cohendata[,2]/10, lwd=3)
#text(-1000,2,"Within-assemblage Diversity (Cohen and MacDonald 2015)", cex=1.2, pos=4)
#mtext("WAD", side=2, at=1, line=2.5, cex=1.4)
#text(-1020,2.5,"C",pos=4,cex=1.5)
#mtext("D",side=2,las=1,cex=2, at=2.5,line=2.8)

lines(c(-1000,-500),c(6.9,6.9),lwd=0.3)
#lines(c(-1000,-500),c(7.6,7.6),lwd=0.3)
#lines(c(-1000,-500),c(8.2,8.2),lwd=0.3)

axis(2, at=c(6.9,7.6,8.3), labels=c(0,10,20))
lines(-cohendata[,1],cohendata[,2]/(20/1.4)+6.9, lwd=1, col="#94681b88")
text(-1005,7.7,"LOESS fitting", cex=1, pos=4, col="#94681b88")
mtext("WAD", side=2, at=7.6, line=2.5, cex=1.4)
#text(-1020,2.5,"C",pos=4,cex=1.5)
mtext("B",side=2,las=1,cex=2, at=8.6,line=3.2)

cohenpoints = read.table("~/git/oceanography_scripts/multicellularity/Cohen_Macdonald_2015_fig5b_points.txt", header=TRUE, sep="\t")

low_wad = cohenpoints[,2] < 21
points(-cohenpoints[,1][low_wad], cohenpoints[,2][low_wad]/(20/1.4)+6.9, pch=16, col="#e7790e88", cex=2)

### ADD CHROMIUM DATA

crfractdata = read.table("~/git/oceanography_scripts/multicellularity/chromium_fractions_2018.tab", header=TRUE, sep="\t")

axis(2, at=c(3.9, 4.3, 4.7, 5.1, 5.5, 5.9), labels=c("",0,"",1,"",2), cex.lab=0.9, cex.axis=0.9 )
#mtext("d53 Cr", side=2, at=4.5, line=2.5, cex=1.4)
mtext(expression(paste(delta^53,"Cr")), side=2, at=5, line=2.5, cex=1.4)
lines(c(-1000,-500),c(4.3,4.3),lwd=0.3)

cr_age = crfractdata[,2]
cr_ratio = crfractdata[,4]
low_cr_values = cr_ratio < 3.1

rocktypes = c("carbonate","shale","clastic sediment","iron formation","Siltstone","Limestone","chert","Dolostone","oolitic ironstones")
rockcolors = c("#d0903e88","#76767688","#76769688","#b7131388","#a6767688","#d0903e88","#76967688","#d0903e88","#b7131388")
rockcodes = match(crfractdata[["rock.type"]], rocktypes)
points(-cr_age[low_cr_values], cr_ratio[low_cr_values]/(2/1.6)+4.3, pch=16, cex=2, col=rockcolors[low_cr_values][rockcodes])
#unique(crfractdata[["rock.type"]])
legend(-940,6.5,legend=c("Carbonates","Shales","Ironstones"), pch=16, cex=1.1, col=c("#d0903e","#767676","#b71313"))
#text(-1020,7,"B",pos=4,cex=1.5)
mtext("C",side=2,las=1,cex=2, at=6.5,line=3.2)

modern_values = range(crfractdata[cr_age < 10,4]) / (2/1.6)+4.3
rect(-490,modern_values[1],-480,modern_values[2], col="#c51b8a", border=FALSE)
text(-475, 5.5, "Modern\nrange", cex=1.1, pos=2, col="#c51b8a")


### DRAW MOLECULAR CLOCK

rect(-937, 0.95*maxy, -649, 1.0*maxy, col="#6816b599", border=FALSE)
points(meta_lca_mids, rep(c(9.75),4), pch=5, col="#6816b5",cex=1.2, lwd=2)
#text(-980, 1.0*maxy, "Crown-group animal MC", col="#6816b5", cex=1.1, pos=4)
text(-937, 0.975*maxy, "Metazoans", cex=1.1, pos=4)
rect(-929, 8.75, -600, 9.250, col="#bc3c0a99", border=FALSE)
points(rhodo_lca_mids, rep(c(9.0),3), pch=5, col="#bc3c0a",cex=1.2, lwd=2)
#text(-980, 0.90*maxy, "Crown-group Florideophytes MC", col="#bc3c0a", cex=1.1, pos=4)
text(-929, 0.9*maxy, "Florideophytes", cex=1.1, pos=4)
#text(-1020,10,"A",pos=4,cex=1.5)
mtext("A",side=2,las=1,cex=2, at=10.1,line=3.2)
points(-570, 0.975*maxy, cex=2, pch=17, col="#6816b5")
text(-563, 0.975*maxy, "Fossils", pos=4)

points(-580, 0.9*maxy, cex=2, pch=17, col="#bc3c0a")
text(-573, 0.9*maxy, "Fossils", pos=4)


### DRAW S/H ratios
mtext("D",side=2,las=1,cex=2, at=3.6,line=3.3)
brocksdata = read.table("~/git/oceanography_scripts/multicellularity/brocks2017_ext_table_2.tab", header=TRUE, sep="\t")
has_sh = !is.na(brocksdata[["S.H"]])
brocks_age = brocksdata[,5][has_sh]
brocks_sh = brocksdata[,11][has_sh]
axis(2, at=c(2.05,2.3,2.55,2.8,3.05,3.3,3.55), labels=c(0,"",1,"",2,"",3))
mtext("S/H", side=2, at=2.8, line=2.5, cex=1.4)
lines(c(-1000,-500),c(2.05,2.05),lwd=0.3)
points(-brocks_age,brocks_sh/2+2.05, pch=15, cex=2, col="#7f1e8299")
legend(-940,3.5,legend=c("Sterane/hopane ratio"), pch=15, pt.cex=1.2, cex=1.1, col=c("#7f1e82") )

mtext("E",side=2,las=1,cex=2, at=1.5,line=3.2)
has_steranes = (!is.na(brocksdata[["steranes"]])) & (!brocksdata[["steranes"]]=="0_0_0")

c27_conc = as.numeric(unlist(lapply(strsplit(as.character(brocksdata[["steranes"]])[has_steranes],"_"), function(x) x[1])))
c28_conc = as.numeric(unlist(lapply(strsplit(as.character(brocksdata[["steranes"]])[has_steranes],"_"), function(x) x[2])))
c29_conc = as.numeric(unlist(lapply(strsplit(as.character(brocksdata[["steranes"]])[has_steranes],"_"), function(x) x[3])))

axis(2, at=c(0,0.7,1.4), labels=c(0,0.5,1))
lines(c(-1000,-500),c(0.0,0.0),lwd=0.3)
points(-brocks_age[has_steranes],c27_conc/(100/1.4), pch=18, cex=2, col="#cd0f3299")
points(-brocks_age[has_steranes],c28_conc/(100/1.4), pch=18, cex=2, col="#470fcd99")
points(-brocks_age[has_steranes],c29_conc/(100/1.4), pch=18, cex=2, col="#0fcd4b99")

brockscambdata = read.table("~/git/oceanography_scripts/multicellularity/brocks2017_supp_tab1_cambrian_only.tab", header=TRUE, sep="\t")
has_sh = !is.na(brockscambdata[["S_H"]])
brocks_age = brockscambdata[["appx_Ma"]][has_sh]
brocks_sh = brockscambdata[["S_H"]][has_sh]
points(-brocks_age,brocks_sh/2+2.05, pch=15, cex=2, col="#7f1e8299")

has_steranes = !is.na(brockscambdata[["C27"]])
points(-brocks_age[has_steranes],brockscambdata[["C27"]][has_steranes]/(100/1.4), pch=18, cex=2, col="#cd0f3299")
points(-brocks_age[has_steranes],brockscambdata[["C28"]][has_steranes]/(100/1.4), pch=18, cex=2, col="#470fcd99")
points(-brocks_age[has_steranes],brockscambdata[["C29"]][has_steranes]/(100/1.4), pch=18, cex=2, col="#0fcd4b99")

legend(-940,1.7,legend=c("C27","C28","C29"), pch=18, pt.cex=1.2, cex=1.1, col=c("#cd0f32","#470fcd","#0fcd4b"))


dev.off()

#