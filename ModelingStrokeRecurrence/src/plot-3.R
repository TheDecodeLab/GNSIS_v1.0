tiff("Figure3.tiff", units="in", width=8, height=8, res=300)
axiscolor<-c(rep("firebrick",14),
             rep("plum3",18),
             rep("orange3",7), #7 for features non abbr
             rep("slategray4",12),
             rep("aquamarine3",2)
)
g1<-meltedFeaturesCompiled%>%
  mutate_all(~replace(., is.na(.), 0))%>%
  #filter(Sampling=='As-Is') %>%
  ggplot(aes(y=FeaturesAbbr,x=`Trained Classifiers`,color=varCategory))+
  geom_tile(aes(fill = `Normalized Score`),colour = "white") + 
  geom_point(aes(color=factor(FeatureSelMethod)),size = 2,shape=18,alpha=10,
              position = position_jitter(h=0,w=.2))+
  scale_color_manual(#labels=c("Combination","Data Driven","Expert\nSelection","Full Set"),
                     labels=c("Set 4","Set 3","Set 2","Set 1"),
                     values = c("green","wheat3", "royalblue3", "deeppink3"))+
  ggtitle("")+
  labs(colour="Feature Selection\nApproach")+
  facet_grid(~Window,scales="free")+
  scale_fill_gradient(name="Normalized Feature\nImportance Score",low = "white",high = "steelblue")+
  theme_classic()+
  theme(plot.title = element_text(color="black", size=16, face="bold",hjust = .5,
                                  margin=margin(0,0,28,0)), #t, r, b, l
        axis.text.x = element_text(face="bold",color="black",margin = margin(b = 10),
                                   angle =90,hjust=1,vjust = .5,size=9),
        axis.text.y = element_text(face="bold",size=9,color=axiscolor),
        axis.title = element_blank(),
        #axis.line.y.left = element_blank(),
        strip.text = element_text(size=12,face="bold",color="black"),
        legend.title = element_text(face="bold",color="black"),
        legend.position = "top",
        legend.text = element_text(color="black",size=9,face="bold")
  )+
  guides(colour=guide_legend(ncol=2,nrow=2,override.aes = list(size = 4),byrow=TRUE))+
  #guides(shape=guide_legend(ncol=2,nrow=2,override.aes = list(size = 3),byrow=TRUE))+
  #guides(color=guide_legend(ncol=3,nrow=2,override.aes = list(size = 3),byrow=TRUE))+
  coord_cartesian(clip="off")
g1<-g1+
  #theme(#strip.background = element_blank(),
  #      panel.spacing.x=unit(1, "lines"))+
  geom_text(data=annonate.df(g1,"(",LETTERS,")"),
            mapping = aes(x = -Inf, y = -Inf, label = fig.labels, fontface = 2),
            family="Helvetica",color="black",size=4,
            hjust=-1.5,vjust=12
  ) 
library(cowplot)
# Composite score for Feature importance variables
featuresImp.avgScore<-featuresCompiled%>%
  filter(Sampling=="As-Is")%>%
  filter(Features!="PAST_HEMORRHAGIC_STROKE_AT_INDEX" & Features!="PAST_ISCHEMIC_STROKE_AT_INDEX")%>%
  select(-c("Window","Sampling","FeatureSelMethod","Features","varCategory"))%>%
  group_by(FeaturesAbbr)%>%
  summarize_all(list(mean),na.rm=T)%>% # compute mean for each variable by classifier
  mutate(Overall.Average=rowMeans(.[,2:7],na.rm = T)) # compute total mean of each variable
featuresImp.sdScore<-featuresCompiled%>%
  select(-c("Window","Sampling","FeatureSelMethod","Features","varCategory"))%>%
  group_by(FeaturesAbbr)%>%
  summarize_all(list(sd),na.rm=T) # compute mean for each variable by classifier
featuresImp.sdScore$row_std <- apply(featuresImp.sdScore[,-1], 1, sd,na.rm=T)
write.xlsx(featuresImp.avgScore,"feaImpavg.xlsx",row.names=F,quote=FALSE)
featuresImp.avgScore$FeaturesAbbr<-factor(featuresImp.avgScore$FeaturesAbbr,levels = c(
  "Age",
  "Body mass index",
  "Creatinine",
  "Current smoker",
  "Diastolic blood pressure",
  "High-density lipoprotein",
  "Hemoglobin A1C",
  "Hemoglobin",
  "Last outpatient visit",
  "Low-density lipoprotein",
  "Male",
  "Platelet",
  "Systolic blood pressure",
  "White blood cell",
  "Atrial fibrillation or flutter",
  "Atrial fibrillation",
  "Atrial flutter",
  "Chronic liver disease (mild)",
  "Chronic liver disease (mod/severe)",
  "Diabetes",
  "Dyslipidemia",
  "Heart failure",
  "Hypercoagulable",
  "Hypertension",
  "Kidney diseases",
  "Liver diseases",
  "Lung diseases",
  "Myocardial infarction",
  "Neoplasm",
  "Patent foramen ovale",
  "Peripheral vascular disease",
  "Rheumatic diseases",
  "Anti-hypertensives",
  "Aspirin",
  "Clopidogrel",
  "Coumadin/Warfarin",
  "Dipyridamole",
  "Oral anticoagulants",
  "Statins",
  "ACE inhibitors",
  "Angiotensin receptor blockers",
  "Apixaban/Rivaroxaban",
  "Aspirin discharge",
  "Beta blockers",
  "Calcium channel blockers",
  "Clopidogrel discharge",
  "Coumadin/Warfarin discharge",
  "Dabigatran",
  "Dipyridamole discharge",
  "Diuretics",
  "Statins discharge",
  "Heart disorder",
  "Stroke"
  ))
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
