# plot mbari annotations for depth distributions

#file1 = "~/git/oceanography_scripts/species_samples/samples/tomopteris_annotations.txt"
file1 = "~/git/oceanography_scripts/species_samples/pantachogon_haeckeli_annotations.txt"

#alllines = readLines(file1)
#alllines = alllines[-2]
alldat = read.table(file1, header=TRUE, skip=2, sep="\t")

depth = alldat[,3]
nd = as.numeric(as.character(depth[depth!="None"]))
# for some reason must be converted to character then numeric vector
pd = nd[nd>0]
pd[pd>4500] = 4500
#max(rd)

#file2 = "~/git/oceanography_scripts/species_samples/samples/poeobius_annotations.txt"
file2 = "~/git/oceanography_scripts/species_samples/vampyrocrossota_annotations.txt"
alldat2 = read.table(file2, header=TRUE, skip=2, sep="\t")
depth2 = alldat2[,3]
nd2 = as.numeric(as.character(depth2[depth2!="None"]))
pd2 = nd2[nd2>0]
pd2[pd2>4500] = 4500

#file3 = "~/git/oceanography_scripts/species_samples/samples/flota_annotations.txt"
file3 = "~/git/oceanography_scripts/species_samples/flota_annotation_2016.tab"
alldat3 = read.table(file3, header=TRUE, skip=2, sep="\t")
depth3 = alldat3[,3]
nd3 = as.numeric(as.character(depth3[depth3!="None"]))
pd3 = nd3[nd3>0]
pd3[pd3>4500] = 4500

outfilename = "~/git/oceanography_scripts/species_samples/figure-1-v3.pdf"

setEPS()
#postscript("~/git/oceanography_scripts/species_samples/figure-1-v3.eps",  height=3.5, width=8)
pdf(file=outfilename, height=3.5, width=8)
bins = seq(0,4500,100)
cexsize=1.2
layout(matrix(c(1,2,3),1,3,byrow=TRUE),width=c(1.1,1,1))
#par(mfrow=c(1,3), mar=c(4.1,4.5,4,2))
par(mar=c(4.1,4.5,4,1))
h = hist(pd, breaks=bins, xlab="Depth (m)", ylim=c(0,1300), axes=FALSE, ylab="Number of observations", font.main=3, main=file1, cex=cexsize, cex.axis=cexsize, cex.lab=cexsize, cex.main=1.4)
axis(1,at=c(0,1000,2000,3000,4000), labels=c(0,1000,2000,3000,4000), cex.axis=cexsize)
axis(2,at=seq(0,1200,300), cex.axis=cexsize)
par(mar=c(4.1,2,4,1))
h = hist(pd2, breaks=bins, xlab="Depth (m)", ylim=c(0,30000), axes=FALSE, ylab="", font.main=3, main=file2, cex=cexsize, cex.axis=cexsize, cex.lab=cexsize, cex.main=1.4)
axis(1,at=pretty(c(0,4000)), cex.axis=cexsize)
axis(2,at=seq(0,30000,7500), cex.axis=cexsize)
h = hist(pd3, breaks=bins, xlab="Depth (m)", ylab="", font.main=3, plot = FALSE,
         main="Flota flabelligera",
         cex=cexsize, cex.axis=cexsize, cex.lab=cexsize, cex.main=1.4)
plot(h$counts, h$mids, type = "n", ylim = c(4500,0) , frame.plot = FALSE)
segments(0, h$mids, h$counts, h$mids )


dev.off()

#par(mfrow=c(1,1))
