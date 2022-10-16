# plot oxygen models from various models for the phanerozoic
# created 2018-04-24

copse_baseline = read.table("~/git/oceanography_scripts/data/bergman_2006_COPSE_baseline_run1.txt", header=TRUE, sep="\t")
copse_run9 = read.table("~/git/oceanography_scripts/data/bergman_2006_COPSE_run9.txt", header=TRUE, sep="\t")
#bergman2004 = read.table("~/git/oceanography_scripts/data/bergman_2004_o2_plot.txt", header=TRUE, sep="\t")
geocarbsulf_standard = read.table("~/git/oceanography_scripts/data/berner_2006_GEOCARBSULF_standard.txt", header=TRUE, sep="\t")
geocarbsulf_upper = read.table("~/git/oceanography_scripts/data/berner_2006_GEOCARBSULF_upper.txt", header=TRUE, sep="\t")
geocarbsulf_lower = read.table("~/git/oceanography_scripts/data/berner_2006_GEOCARBSULF_lower.txt", header=TRUE, sep="\t")


modernO2 = 20

pdf("~/git/oceanography_scripts/images/o2_models_phanerozoic_v2.pdf", width=8, height=7)
par(mar=c(4.5,4.5,2,1))
plot(0,0,type='n',xlim=c(-600,0),ylim=c(0,40),axes=FALSE,frame.plot=FALSE, xlab="Time (Ma)", ylab="% atmospheric oxygen", cex.lab=1.3, main="Phanerozoic Oxygen Record")
axis(1,at=seq(-600,0,50),labels=rev(seq(0,600,50)),cex.axis=1.4)
axis(2,at=seq(0,40,5),labels=seq(0,40,5),cex.axis=1.4)


# draw geological periods
georange= c(-1.5,-0.3)
period_letters = c("Ed","C","O","S","D","C","P","T","J","K","Pe","N")
period_starts =  c(-635,-541,-485,-443,-419,-358,-298,-252,-201,-145,-66,-23)
period_ends = c(-541,-485,-443,-419,-358,-298,-252,-201,-145,-66,-23,0)
period_color = c("#d6ab24", "#6da26e", "#28a46e", "#a5e3e5", "#d48f16", "#4fa092", "#d9491c", "#be58d0", "#4d94de", "#26bf3f", "#dba426", "#baab09")
rect(period_starts, georange[1], period_ends, georange[2], border="#FFFFFF", col=period_color)
text( (period_starts+period_ends)/2, mean(georange), period_letters)
text( -516, mean(georange), "-", cex=1.1) # add dash for Cambrian

lines(-1*geocarbsulf_standard[,1],geocarbsulf_standard[,2], lwd=5, col="#ec7014cc")
polygon(c(-1*geocarbsulf_upper[,1],rev(-1*geocarbsulf_lower[,1])), c(geocarbsulf_upper[,2],rev(geocarbsulf_lower[,2])), col="#ec701433", border=FALSE)

#lines(-1*bergman2004[,1],bergman2004[,2], lwd=5, col="#31a354cc")

lines(-1*copse_baseline[,1],copse_baseline[,2]*modernO2, lwd=5, col="#225ea8cc")
lines(-1*copse_run9[,1],copse_run9[,2]*modernO2, lwd=5, col="#41b6c4cc")

polygon(c(-1*copse_baseline[,1],rev(-1*copse_run9[,1])), c(copse_baseline[,2],rev(copse_run9[,2]))*modernO2, col="#41b6c433", border=FALSE)

#"COPSE Standard", 
#"#31a354", 

legenditems = c("COPSE model", "GEOCARBSULF")
legendcols = c("#41b6c4", "#ec7014")
legend(-600,40, legend=legenditems, lwd=4, col=legendcols, cex=1.3)

dev.off()