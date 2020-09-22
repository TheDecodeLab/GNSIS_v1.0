tiff("Figure4.tiff", units="in", width=10, height=6, res=300)
f5<-aucthresholds %>% 
  filter(window %in% c("1-Year")) %>%
  filter(sampling=="As-Is" & feaSel=="Data Driven" | 
           sampling=="Up sampling(1:2 Cases to Controls)" & feaSel=="Data Driven" |
           sampling=="Up sampling(1:1 Cases to Controls)" & feaSel=="Data Driven")%>%
  ggplot(aes(x=fpr,y=tpr))+
  geom_point(aes(#shape=feaSel,
                 color=classifier,fill=classifier),size=.8,alpha=.75,
             position = "jitter")+
  facet_grid(window~sampling)+
  scale_color_manual(name="Classifier",
                     values=c("yellow2","lightblue","darkgreen","darkblue","orange3","deeppink4"))+
  scale_fill_manual(name="Classifier",
                    values=c("yellow2","lightblue","darkgreen","darkblue","orange3","deeppink4"))+
  scale_shape_manual(name="Feature\nSelection\nApproach",
                     labels=c("Full Set","Expert Selection","Data Driven","Combination"),
                     values=c(8,23,22,21))+
  #ggtitle("Area under the ROC curve of observation window:4 years")+
  xlab("False Positive Rate")+
  ylab("True Positive Rate")+
  theme_minimal()+
  theme(plot.title = element_text(color="black", size=16, face="bold",hjust = .5),
        axis.text = element_text(face="bold",color="black",angle =0,size=8),
        axis.title = element_text(face="bold",color="black",angle =0,size=12),
        strip.text  = element_blank(),
        #strip.text.x =  element_text(size=8,face="bold",color="black",vjust = 10),
        legend.title = element_text(face="bold",color="black"),
        legend.position = "top",
        legend.text = element_text(color="navy",size=10,face="bold"),
        legend.box.background = element_rect(colour = "black",size=.5,linetype = "dashed")
        )+
  guides(shape=guide_legend(ncol=2,nrow=2,override.aes = list(size = 3),byrow=TRUE))+
  guides(color=guide_legend(ncol=3,nrow=2,override.aes = list(size = 3),byrow=TRUE))+
  geom_abline(intercept = 0,slope = 1,linetype=2,color="darkgrey",size=.5)
f5<-f5+geom_text(data=annonate.df(f5,"",c(
  "(A) As-Is",
  "(B) Up sampling(1:2 Cases to Controls)",
  "(C) Up sampling(1:1 Cases to Controls)"
),
                                  ""),
                 mapping = aes(x = -Inf, y = Inf, label = fig.labels, fontface = 2),
                 family="Helvetica",color="black",size=3,
                 hjust=0,vjust=1
)  
f5
dev.off()
