tiff("Figure1.tiff", units="in", width=9, height=10, res=300)
p1<-meltedResultsCompiled%>%
          filter(Sampling=='As-Is')%>% 
          filter(variable %in% c("AUROC_Train",                              
                                 "AUROC",
                                 "Precision",
                                 "Accuracy",
                                 "Sensitivity",
                                 "Specificity")
                                 )%>%
 ggplot(aes(y=Window,x=value))+
  labs(title="Summary of the Binary Classification",y="Observation windows")+
  geom_quasirandom(aes(color=Features),size = 4.5,shape=18,groupOnX = T,dodge.width = 2,
                   varwidth=T,alpha=0.9)+
  scale_color_manual(name="Feature Selection Approach:",
                     #labels=c("Combination","Data Driven","Expert\nSelection","Full Set"),
                     labels=c("Set 4","Set 3","Set 2","Set 1"),
                     values = c("green","wheat3", "royalblue3", "deeppink3"))+
  facet_grid(Classifier~variable,scales = "free")+
  coord_cartesian(clip = "off")+
  theme_bw()+
  theme(plot.title = element_text(color="black", size=16, face="bold",hjust = .5),
        axis.text.x = element_text(face="bold",color="black",size=10,angle=0),
        axis.text.y.left = element_text(face="bold",color="black",size=9),
        axis.title = element_blank(),
        #axis.title.y.left = element_text(face="bold",size=13,color="black"),
        strip.text.x = element_text(size=10,face="bold",color="navy",angle=0),
        strip.text.y = element_text(size=9,face="bold",color="navy"),
        legend.title = element_text(face="bold",color="black"),
        legend.position = "top",
        legend.text = element_text(color="navy",size=9,face="bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black",size=.5,linetype = "dashed"),
        panel.spacing.x = unit(1.65, "lines")
  )
p1<-p1+geom_text(data=annonate.df(p1,"(",c(LETTERS[1:26], paste0("A",LETTERS[1:10])),")"),
             mapping = aes(x = -Inf, y = Inf, label = fig.labels, fontface = 2),
             family="Helvetica",color="black",size=3.25,
             hjust=1,
             vjust=.8
)  
p1
dev.off()
