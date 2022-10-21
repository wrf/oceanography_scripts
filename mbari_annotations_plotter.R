# plot MBARI VARS annotation frequency for depth distributions
#
# used for paper
# Francis et al 2016 Bioluminescence spectra from three deep-sea polychaete worms. Marine Biology
# https://link.springer.com/article/10.1007/s00227-016-3028-2
#

file1 = "/home/user/samples/tomopteris_annotations.txt"

#alllines = readLines(file1)
#alllines = alllines[-2]
alldat = read.table(file1, header=TRUE, skip=2, sep="\t")

depth = alldat[,3]
nd = as.numeric(as.character(depth[depth!="None"]))
# for some reason must be converted to character then numeric vector
pd = nd[nd>0]
pd[pd>4500] = 4500
#max(rd)

file2 = "/home/user/samples/poeobius_annotations.txt"
alldat2 = read.table(file2, header=TRUE, skip=2, sep="\t")
depth2 = alldat2[,3]
nd2 = as.numeric(as.character(depth2[depth2!="None"]))
pd2 = nd2[nd2>0]
pd2[pd2>4500] = 4500

file3 = "/home/user/samples/flota_annotations.txt"
alldat3 = read.table(file3, header=TRUE, skip=2, sep="\t")
depth3 = alldat3[,3]
nd3 = as.numeric(as.character(depth3[depth3!="None"]))
pd3 = nd3[nd3>0]
pd3[pd3>4500] = 4500

bins = seq(0,4500,100)
cexsize=1.2
#par(mfrow=c(1,1))
par(mfrow=c(1,3), mar=c(4.1,3,4,1))
h = hist(pd, breaks=bins, xlab="Depth", ylab="", main="Tomopteris Annotations", cex=cexsize, cex.axis=cexsize, cex.lab=cexsize, cex.main=1.5)
# possibly add axes=TRUE, plot=TRUE

h = hist(pd2, breaks=bins, xlab="Depth", ylab="", main="Poeobius Annotations", cex=cexsize, cex.axis=cexsize, cex.lab=cexsize, cex.main=1.5)

h = hist(pd3, breaks=bins, xlab="Depth", ylab="", main="Flota Annotations", cex=cexsize, cex.axis=cexsize, cex.lab=cexsize, cex.main=1.5)


#dev.off()

