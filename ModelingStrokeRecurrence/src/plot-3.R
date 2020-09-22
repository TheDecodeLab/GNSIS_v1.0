tiff("Figure3.tiff", units="in", width=8, height=8, res=300)
p3<-ggplot(melt(featuresImp.avgScore[c('FeaturesAbbr','Overall.Average')]),
           aes(y=FeaturesAbbr,x=value))+                             # order as per main feaImp plot
  geom_bar(stat = "identity",position = 'dodge',fill=axiscolor,alpha=.8)+
  geom_text(aes(label=round(value,1)),
            hjust=0,size=2.5,position = position_stack(vjust = 0)           # to arrange text inside the bars
            )+
  theme_bw()+
  theme(axis.text = element_blank(),
        #axis.text.x = element_text(face="bold",color="black",angle =0,size=8),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y.left  = element_blank(),
        axis.title.y.right = element_text(face="bold",color="black",angle =0,size=11),#margin=margin(0,0,20,0)), #t, r, b, l)),
        axis.title.x = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.spacing.x = unit(1.65, "lines"),
        plot.margin = unit(c(4.6,1,3.6,-.1), "cm")
  )+
  #scale_fill_manual(values=c("red"))+
  scale_x_continuous(name="Overall average score \n\n (F)",limits=c(0,101),expand = c(0, 0))+
  coord_cartesian(clip = "off")
p3<-p3+
  geom_text(data=annonate.df(p3,"(","F",")"),
            mapping = aes(x = -Inf, y = -Inf, label = fig.labels, fontface = 2),
            family="Helvetica",color="black",size=4,
            hjust=-25,vjust=2
  )
p4<-cowplot::plot_grid(g1,p3,rel_widths = c(4, .8))
p4<-ggdraw(p4)+
  draw_label("BASELINE ",x=.15,y=.96,size=10,angle=0,color="firebrick",fontface = "bold")+
  draw_label("~ MEDICAL HISTORY ",x=.26,y=.96,size=10,angle=0,color="plum3",fontface = "bold")+
  draw_label("~ MEDICATION HISTORY ",x=0.42,y=.96,size=10,angle=0,color="orange3",fontface = "bold")+
  draw_label("~ MEDICATION AT DISCHARGE ",x=0.62,y=.96,size=10,angle=0,color="slategray4",fontface = "bold")+
  draw_label("~ FAMILY HISTORY",x=0.79,y=.96,size=10,angle=0,color="aquamarine3",fontface = "bold")+
  draw_label("(F)",x=.9,y=.015,size = 11,angle = 0,color = "black",fontface = "bold")+
  draw_label("Overall average score",x=.91,y=.125,color = "black",fontface = "bold",size = 10)
rect <- grid::rectGrob(
  x = unit(1, "in"),
  y = unit(1, "npc") - unit(1, "in"),
  width = unit(10.25, "in"),
  height = unit(.25, "in"),
  hjust = 0.1,
  vjust = -1.87,
  #gp = gpar(fill = "white",alpha=.25)
  gp=grid::gpar(col="black",#lty="dashed",
                fill = "white",alpha=.1,lwd=1)
)
p4<-p4+draw_grob(rect)
p4
dev.off()
p4<-p4+
  geom_text(data=annonate.df(p4,"(","F",")"),
            mapping = aes(x = -Inf, y = -Inf, label = fig.labels, fontface = 2),
            family="Helvetica",color="black",size=4,
            hjust=-71,vjust=-.8
              )
p4
dev.off()
