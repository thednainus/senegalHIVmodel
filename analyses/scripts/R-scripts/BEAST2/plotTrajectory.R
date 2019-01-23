# Plot BEAST trajectory

csvFile <- "~/Box Sync/BEAST2/BEAST_test_trajectoryOnly_byIgor/trajSenegalComplexModel.csv"
pngFile <- paste(csvFile,".png",sep='')

traj <- read.csv(csvFile)
x <- traj[,1]

png(pngFile)

ymax <- max(traj[,2],traj[,3],traj[,4],traj[,5])
plot(x,traj[,2], 'l', ylim=c(0,ymax*1.20), col='blue', xlab='time',ylab='Population')
lines(x,traj[,3],  col='red')
lines(x,traj[,4],  col='green')
lines(x,traj[,5],  col='cyan')

legend("topleft",
       inset=.05,
       cex = 0.8,
       title="Legend",
       c("gpm","gpf","msm","src"),
       horiz=TRUE,
       lty=c(1,1),
       lwd=c(2,2),
       col=c("blue","red","green","cyan"),
       bg="grey96")


dev.off()
