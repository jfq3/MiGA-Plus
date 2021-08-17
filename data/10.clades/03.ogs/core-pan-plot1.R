core_pan<-"/Users/himajamedarametla/Desktop/On-Campus-parttime/MiGA-Plus/data/10.clades/03.ogs/miga-project.core-pan.tsv"
library(ggplot2)
library(maps)

# Core
plot_core_pan <- function(core_pan){
  a <- read.table(core_pan, sep="\t", header=TRUE)
  ggplot()+scale_x_continuous(breaks = seq(0,max(a$genomes)*1.05,2))+
  scale_y_continuous(breaks = seq(0,max(a$pan_q3)*1.05,500),limits = c(0,max(a$pan_q3)*1.05))+
  labs(x= "Genomes", y = "Orthologous Groups",colour="legends")+
  geom_polygon(aes(x=c(a$genomes, rev(a$genomes)),y=c(a$core_q1, rev(a$core_q3)),colour="coregenome"),fill="cadetblue1")+
  geom_line(aes(x=a$genomes,y=a$core_avg),colour="cadetblue",linetype = "dashed")+
  geom_line(aes(x=a$genomes,y=a$core_q2,colour="median"),colour="cadetblue",linetype = "solid")+
  # Pan
  geom_polygon(aes(x=c(a$genomes, rev(a$genomes)),y=c(a$pan_q1, rev(a$pan_q3)),colour="pangenome"),fill="rosybrown2")+
  geom_line(aes(x=a$genomes,y=a$pan_avg),colour="rosybrown",linetype = "dashed")+
  geom_line(aes(x=a$genomes,y=a$pan_q2),colour="rosybrown",linetype = "solid")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour = "black"),legend.position = c(0.2, 0.9))
}

plot_core_pan(core_pan)

legend("topleft",
       legend=c("pangenome","core genome","Inter-Quartile","Median","Average"),
       pch=c(16,16,15,NA,NA),lty=c(NA,NA,NA,1,2), pt.cex=c(1,1,2,NA,NA),
       col=c(rgb(96,11,64,max=255), rgb(0,121,166,max=255),
             rgb(0.5,0.5,0.5,166/255), rep(rgb(0.5,0.5,0.5),2)), bty="n")

#cluster of k-median
#

