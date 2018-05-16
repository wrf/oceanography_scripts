# display MBARI CTD data

args = commandArgs(trailingOnly=TRUE)
inputfile = args[1]
outfilename = gsub("([\\w/]+)\\....$","\\1.pdf",inputfile,perl=TRUE)

alldat = read.table(inputfile, header=TRUE, sep=',')

pdf(file=outfilename, height = 8, width = 8)


#ctddata = alldat[1:690,]
ctddata = alldat

depth = ctddata[,"depth"]
temp = ctddata[,"temper"]
salt = ctddata[,"salin"]
o2 = ctddata[,"oxyg"]
conv_o2 = o2*44.6
beamtrans = ctddata[,"light"]
beamtrans[beamtrans=NA]=75

templims = seq(0,20,2)
yr = c(3600,0)
o2lims = seq(0,8,1)
conv_o2lims=seq(0,300,20)
saltlims = seq(24,38,1)
beamlims = seq(0,100,10)

par(mar=c(6,3.5,6,2))

# set default colors to white text with black background
fgcol = "white"
par(col=fgcol, col.lab=fgcol, col.sub=fgcol, bg="black")
linewid=2

actcol="yellow"
plot(beamtrans,depth,xlim=range(beamlims),ylim=yr, type="l", col=actcol, xlab="", ylab="", frame.plot=FALSE, axes=FALSE, lwd=linewid)
par(las=2)
axis(2, ylim=yr, line=-1, col=fgcol, col.axis=fgcol, hadj=1)
par(las=0)
mtext("Depth (m)",side=2, line=2, col=fgcol)
axis(1, xlim=range(beamlims), at=beamlims, line=0, col=actcol, col.axis=actcol)
mtext("Beam Transmission %",side=1, line=2, col=actcol)
par(new=TRUE)
actcol="green"
#plot(o2,depth,xlim=range(o2lims),ylim=yr, type='l', xlab="", ylab="", col=actcol, frame.plot=FALSE, axes=FALSE, lwd=linewid)
#axis(1, xlim=range(o2lims), at=o2lims, line=3, col=actcol, col.axis=actcol)
#mtext("Oxygen (mL/L)",side=1, line=5, col=actcol)
plot(conv_o2,depth,xlim=range(conv_o2lims),ylim=yr, type='l', xlab="", ylab="", col=actcol, frame.plot=FALSE, axes=FALSE, lwd=linewid)
axis(1, xlim=range(conv_o2lims), at=conv_o2lims, line=3, col=actcol, col.axis=actcol)
yl = expression(paste("Oxygen (", mu, "M)"))
mtext(yl,side=1, line=5, col=actcol)
par(new=TRUE)
actcol="#4488FF"
plot(temp,depth,xlim=range(templims),ylim=yr, type='l', xlab="", ylab="", col=actcol, frame.plot=FALSE, axes=FALSE, lwd=linewid)
axis(3, xlim=range(templims), at=templims, line=3, col=actcol, col.axis=actcol)
mtext("Temperature (C)",side=3, line=5, col=actcol)
par(new=TRUE)
actcol="#FF0000"
plot(salt,depth,xlim=range(saltlims),ylim=yr, type='l', xlab="", ylab="", col=actcol, frame.plot=FALSE, axes=FALSE, lwd=linewid)
axis(3, xlim=range(saltlims), at=saltlims, line=0, col=actcol, col.axis=actcol)
mtext("Salinity (ppt)",side=3, line=2, col=actcol)

# undo color settings
par(col="black", col.lab="black", col.sub="black", bg="white")

dev.off()
