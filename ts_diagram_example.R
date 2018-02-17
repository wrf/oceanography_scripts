# example TS diagram    created 2017-01-18

infile = "~/git/oceanography_scripts/data/wocedata.tsv"

wocedata = read.table(infile, sep="\t", header=TRUE)

depth = wocedata[,1]
temp = wocedata[,2]
salinity = wocedata[,4]

roundedtemp = round(temp)
depthtocol = rev(colorRampPalette(c("#FF8800","#0000BB"),alpha=0.9)(15))

roundedsalt = round(salinity,digits=1)
salttocol = rev(colorRampPalette(c("#FF0000","#00FF00"),alpha=0.9)(16))
salttoindex = 10*(roundedsalt-33.2)+1


pdf(file="~/git/oceanography_scripts/images/woce_p17n_t-s_diagram_v1.pdf",width=6,height=6)

#png(file="~/git/oceanography_scripts/images/woce_p17n_t-s_diagram_v1.png",width=500,height=500)

par(mar=c(4.5,4.5,1,1))
plot(salinity,temp, type='l', xlim=c(33.0,35.0), ylim=c(0,16), xlab="Salinity", ylab="Temp (C)", cex.lab=1.5, cex.axis=1.2, lwd=2)
points(salinity, temp, pch=22, bg=depthtocol[roundedtemp])
text(34.3,16,"WOCE Station P17N", cex=1.5)
text(34.3,15,"37.5 N 135.0 W", cex=1.5)
text(34.3,14,"1-June-1993", cex=1.5)

keypoints = c(1,5,9,10,13,17,20,22,35)
text(salinity[keypoints]+0.03, temp[keypoints], paste(round(depth[keypoints]),"m",sep=""), pos=4)

text(33.2,13,"Surface", cex=1.3)
text(33.5,8,"Central", cex=1.3)
text(34.6,5,"Intermediate", cex=1.3)
text(34.5,1,"Deep", cex=1.3)

dev.off()

pdf(file="~/git/oceanography_scripts/images/woce_p17n_temp-salinity-depth_v1.pdf",width=10,height=5)
par(mar=c(4.5,4.5,3,1), mfrow=c(1,2))
plot(temp,-depth, type='l', xlim=c(0,16), ylim=c(-5000,0), xlab="Temp (C)", ylab="Depth (m)", frame.plot=TRUE, axes=FALSE, cex.lab=1.5, cex.axis=1.2, lwd=2)
points(temp,-depth, pch=24, bg=depthtocol[roundedtemp])
axis(1, cex.axis=1.2)
axis(2,at=pretty(c(-5000,0),6),labels=rev(pretty(c(5000,0),6)), cex.axis=1.2)
plot(salinity,-depth, type='l', xlim=c(33.0,35.0), ylim=c(-5000,0), xlab="Salinity", ylab="Depth (m)", frame.plot=TRUE, axes=FALSE, cex.lab=1.5, cex.axis=1.2, lwd=2)
points(salinity,-depth, pch=23, bg=salttocol[salttoindex])
axis(1, cex.axis=1.2)
axis(2,at=pretty(c(-5000,0),6),labels=rev(pretty(c(5000,0),6)), cex.axis=1.2 )
mtext("WOCE Station P17N", line=1,at=32.5, cex=1.5)
dev.off()