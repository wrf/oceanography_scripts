# plot oxygen from multiple CTD csv files

filepath = "~/git/oceanography_scripts/data/"

filelist = dir(filepath,pattern="*.csv")

pdf(file="~/git/oceanography_scripts/data/july_2018_o2_plot.pdf", width=7, height=7)
par(col="white", col.axis="white", col.lab="white", col.sub="white", bg="black", mar=c(4.5,4.5,2,2))
plot(0,0,xlim=c(0,7), ylim=c(3500,0),type="n",xlab="Oxygen (mL/L)",ylab="Depth (m)",col.axis="white", axes=FALSE, cex.axis=1.3, cex.lab=1.4)
axis(1, col="white", col.ticks="white", cex.axis=1.4)
axis(2, col="white", col.ticks="white", cex.axis=1.4)
colset = c("#e41a1cbb", "#377eb8bb", "#4daf4abb", "#984ea3bb", "#ff7f00bb", "#ffff33bb", "#a65628bb", "#f781bfbb", "#999999bb")

colcounter = 1
for (csvfile in filelist){
	ctddata = read.table(paste(filepath,csvfile,sep=""), header=TRUE, sep=",")
	depth = ctddata[,"depth"]
	temp = ctddata[,"temper"]
	salt = ctddata[,"salin"]
	o2 = ctddata[,"oxyg"]
	conv_o2 = o2*44.6
	lines(o2, depth, col=colset[colcounter], lwd=4)
	colcounter = colcounter+1
}
legend(3.5,2000, col=colset, legend=filelist, text.col="white", lwd=5, cex=1.3, bty="n")

dev.off()