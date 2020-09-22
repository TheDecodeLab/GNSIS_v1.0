tiff("Figure2.tiff", units="in", width=10, height=10, res=300)
p3<-meltedResultsCompiled%>%
        filter(Window=="3-Year")%>%
        filter(variable %in% c("AUROC_Train",
                                               "AUROC",
                                               "Precision",
                                               "Accuracy",
                                               "Sensitivity",
                                               "Specificity"
                                               ))%>%
  ggplot(aes(x=Classifier,y=value),fill=variable)+
  geom_quasirandom(aes(color=Features),size = 4.5,shape=18,groupOnX = F,dodge.width = 2,
                   varwidth=T,alpha=0.9)+
  scale_color_manual(name="Feature Selection Approach:",
                     #labels=c("Combination","Data Driven","Expert\nSelection","Full Set"),
                     labels=c("Set 4","Set 3","Set 2","Set 1"),
                     values = c("green","wheat3", "royalblue3", "deeppink3"))+
  coord_flip()+
  facet_grid(Sampling~variable,
             scales="free_x"
             )+
  #ggtitle("Classifiers performance in 3-Year window vs Sampling")+
  theme_bw()+
  theme(plot.title = element_text(color="black", size=16, face="bold",hjust = .5),
        axis.text.x = element_text(face="bold",color="black",size=9,angle=0),
        axis.text.y.left = element_text(face="bold",color="black",size=9),
        axis.title = element_blank(),
        strip.text = element_text(size=10,face="bold",color="navy"),
        legend.title = element_text(face="bold",color="black"),
        legend.position = "top",
        legend.text = element_text(color="navy",size=9,face="bold"),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black",size=.5,linetype = "dashed")
        #panel.spacing.x=unit(0.5, "lines")
  )
#annonate each panel
p3<-p3+geom_text(data=annonate.df(p3,"(",LETTERS,")"),
             mapping = aes(x = Inf, y = -Inf, label = fig.labels, fontface = 2),
             family="Helvetica",color="black",size=4,
             hjust=-0.1,vjust=1.2
)  
p3
dev.off()
