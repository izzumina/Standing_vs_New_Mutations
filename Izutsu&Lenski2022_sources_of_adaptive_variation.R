library(ggplot2)
library(reshape2)
library(tidyverse)
library(grid)
library(ggpubr)
library(cowplot)
library(dplyr)
library(stringr)


######Fig2_grand_mean_fitness.tiff######
averages<- read.csv("All_averages216.csv",header=T)

averages_Clone_T0<-subset(averages,Treatment=="Clone" & Time=="0")
averages_Population_T0<-subset(averages,Treatment=="Population" & Time=="0")
averages_Mixed_clones_T0<-subset(averages,Treatment=="Mixed-Clones" & Time=="0")
averages_Mixed_populations_T0<-subset(averages,Treatment=="Mixed-Populations" & Time=="0")

averages_Clone_T75<-subset(averages,Treatment=="Clone"& Time=="75")
averages_Population_T75<-subset(averages,Treatment=="Population"& Time=="75")
averages_Mixed_clones_T75<-subset(averages,Treatment=="Mixed-Clones"& Time=="75")
averages_Mixed_populations_T75<-subset(averages,Treatment=="Mixed-Populations"& Time=="75")

averages_Clone_T300<-subset(averages,Treatment=="Clone"& Time=="300")
averages_Population_T300<-subset(averages,Treatment=="Population"& Time=="300")
averages_Mixed_clones_T300<-subset(averages,Treatment=="Mixed-Clones"& Time=="300")
averages_Mixed_populations_T300<-subset(averages,Treatment=="Mixed-Populations"& Time=="300")


t300clone<-t.test(averages_Clone_T300$meanLnFitness)
t300pop<-t.test(averages_Population_T300$meanLnFitness)
t300mixedclones<-t.test(averages_Mixed_clones_T300$meanLnFitness)
t300mixedpop<-t.test(averages_Mixed_populations_T300$meanLnFitness)

t75clone<-t.test(averages_Clone_T75$meanLnFitness)
t75pop<-t.test(averages_Population_T75$meanLnFitness)
t75mixedclones<-t.test(averages_Mixed_clones_T75$meanLnFitness)
t75mixedpop<-t.test(averages_Mixed_populations_T75$meanLnFitness)

t0clone<-t.test(averages_Clone_T0$meanLnFitness)
t0pop<-t.test(averages_Population_T0$meanLnFitness)
t0mixedclones<-t.test(averages_Mixed_clones_T0$meanLnFitness)
t0mixedpop<-t.test(averages_Mixed_populations_T0$meanLnFitness)


data<-data.frame(Treatment=as.factor(c("Single-Clone","Single-Clone","Single-Clone","Single-Population","Single-Population","Single-Population","Mixed-Clones","Mixed-Clones","Mixed-Clones","Mixed-Populations","Mixed-Populations","Mixed-Populations")),
                 Generation=as.integer(c(0,500,2000,0,500,2000,0,500,2000,0,500,2000)),
                 meanLnFitness=c(t0clone$estimate,t75clone$estimate,t300clone$estimate,
                                 t0pop$estimate,t75pop$estimate,t300pop$estimate,
                                 t0mixedclones$estimate,t75mixedclones$estimate,t300mixedclones$estimate,
                                 t0mixedpop$estimate,t75mixedpop$estimate,t300mixedpop$estimate),
                 IC95low=c(t0clone$conf.int[1],t75clone$conf.int[1],t300clone$conf.int[1],
                           t0pop$conf.int[1],t75pop$conf.int[1],t300pop$conf.int[1],
                           t0mixedclones$conf.int[1],t75mixedclones$conf.int[1],t300mixedclones$conf.int[1],
                           t0mixedpop$conf.int[1],t75mixedpop$conf.int[1],t300mixedpop$conf.int[1]),
                 IC95up=c(t0clone$conf.int[2],t75clone$conf.int[2],t300clone$conf.int[2],
                          t0pop$conf.int[2],t75pop$conf.int[2],t300pop$conf.int[2],
                          t0mixedclones$conf.int[2],t75mixedclones$conf.int[2],t300mixedclones$conf.int[2],
                          t0mixedpop$conf.int[2],t75mixedpop$conf.int[2],t300mixedpop$conf.int[2]),
                 SD=c(sd(averages_Clone_T0$meanLnFitness),sd(averages_Clone_T75$meanLnFitness),sd(averages_Clone_T300$meanLnFitness),
                      sd(averages_Population_T0$meanLnFitness),sd(averages_Population_T75$meanLnFitness),sd(averages_Population_T300$meanLnFitness),
                      sd(averages_Mixed_clones_T0$meanLnFitness),sd(averages_Mixed_clones_T75$meanLnFitness),sd(averages_Mixed_clones_T300$meanLnFitness),
                      sd(averages_Mixed_populations_T0$meanLnFitness),sd(averages_Mixed_populations_T75$meanLnFitness),sd(averages_Mixed_populations_T300$meanLnFitness)
                 )
)

ggplot(data, aes(x = (Generation + 60*c(-1,-1,-1,0,0,0,1,1,1,2,2,2)), y = meanLnFitness))+ #, shape = as.factor(Treatment)
  geom_line(aes(linetype=as.factor(Treatment)),size=1.1,show.legend = FALSE) +
  theme_classic()+
  scale_linetype_manual(values=c("longdash","dotted","solid","dashed"))+
  geom_errorbar(data = data, aes(ymin=IC95low, ymax=IC95up, width = 4),size=0.5) +
  geom_point(aes(size= as.factor(Treatment),color=as.factor(Treatment),fill=as.factor(Treatment),shape=as.factor(Treatment)),stroke=2) + #,"black" for 1.png,shape = as.factor(Treatment),fill=as.factor(Treatment)
  scale_fill_manual(values  =  c("white","grey50","white","white"),name="Treatment",limits = c("Single-Clone","Single-Population","Mixed-Clones","Mixed-Populations"),labels = c("Single Clone","Single Population","Mixed Clones","Mixed Populations"))+
  scale_shape_manual(values =  c(21, 24, 18,15),name="Treatment",limits = c("Single-Clone","Single-Population","Mixed-Clones","Mixed-Populations"),labels = c("Single Clone","Single Population","Mixed Clones","Mixed Populations"))+ 
  scale_size_manual(values =  c(5,3.6,7,5),name="Treatment",limits = c("Single-Clone","Single-Population","Mixed-Clones","Mixed-Populations"),labels = c("Single Clone","Single Population","Mixed Clones","Mixed Populations"))+
  scale_color_manual(values  =  c("black","gray50","gray50","black"),limits = c("Single-Clone","Single-Population","Mixed-Clones","Mixed-Populations"))+##values  =  c("black","gray40","gray40","black")
  scale_y_continuous(expand=c(-0.20,0.05),limits=c(-0.21,0.07),breaks=seq(-0.20,0.05,0.05)) + 
  labs(y="ln(Relative fitness)",x="Generation")+
  guides(fill = "none" ,color="none",
         shape = guide_legend(override.aes = list(fill = c("white","gray50","white","white"),color=c("black","gray50","grey50","black"))))+
  theme(legend.position = c(0.65, 0.35))+ 
  theme(axis.title.x = element_text(size = 18),axis.title.y = element_text(size = 18),axis.text.x=element_text(size=16,colour = "black"),axis.text.y=element_text(size=16,colour = "black"))+
  theme(legend.title = element_text(size=18),legend.text = element_text(size=16)) 
ggsave('Fig2_grand_mean_fitness.tiff', plot = last_plot(),dpi=300, width = 20, height = 19, units = "cm")
######END######Fig2_grand_mean_fitness.tiff######

######ANOVAs Table S1######
averages<- read.csv("All_averages216.csv",header=T)
averages$Time<-as.factor(averages$Time) ##Anova ignores parameters that has value=0. To make sure it takes Time=0 into account, make Time a factor.

averages_T0<-subset(averages,Time=="0")
averages_T75<-subset(averages,Time=="75")
averages_T300<-subset(averages,Time=="300")
sum_T0<-summary(aov(meanLnFitness~Treatment,data=averages_T0))
sum_T75<-summary(aov(meanLnFitness~Treatment,data=averages_T75))
sum_T300<-summary(aov(meanLnFitness~Treatment,data=averages_T300))
######END######ANOVAs Table S1######

######FigS1_72means.tiff######

# Custom labeller for the facets
averages.labeller <- as_labeller(function(labels) {
  lapply(labels, function(label) {
    # Input string format will be like "Mixed.Population"
    # Output string format will be like "Mixed Populations"
    
    # Indices that treatment1 and treatment2 will show up in the input string
    t1.idx <- 1
    t2.idx <- 2
    parts <- str_split(as.character(label), "\\.")[[1]]
    #paste0(c("Single-Clone","Mixed-Clones","Single-Population","Mixed-Populations"))
    paste0(parts[t2.idx], " ", parts[t1.idx], ifelse(parts[t2.idx] == "Mixed", "s", ""))
  })
})

# A custom version of position_dodge that will let us dodge by color,
# even when the group aesthetic is not set to color.
# This mystical code goes far into the depths of ggplot and was not easy to get working
# It is unfortunate that there didn't seem to be a better way to get these results
position_custom_aesthetic_dodge <- function(aesthetic, width = NULL) {
  ggproto(NULL, PositionCustomAestheticDodge, width=width,
          aesthetic=aesthetic, required_aes = c("x", "y", aesthetic))
}

PositionCustomAestheticDodge <- ggproto("PositionCustomAestheticDodge", PositionDodge,
                                        # Override compute_panel function which computes coordinates based on dodging via groups
                                        # This override tricks it by setting another aesthetic in the group column,
                                        # then restoring groups after the positions have been computed
                                        compute_panel = function(self, data, params, scales) {
                                          group.old <- if ("group" %in% colnames(data)) data$group else NULL
                                          data$group <- unlist(lapply(data[self$aesthetic], as.integer))
                                          result <- ggproto_parent(PositionDodge, self)$compute_panel(data, params, scales)
                                          result$group <- group.old
                                          result
                                        }
)

# Actual position object to use in ggplot, dodges based on color instead of group
points.position <- position_custom_aesthetic_dodge("colour", width=0.6)

averages<- read.csv("All_averages216.csv",header=T)
averages$Treatment<-factor(averages$Treatment,levels = c("Clone","Population","Mixed-Clones","Mixed-Populations"))
averages$Generation<-factor(averages$Generation,levels=c("0","500","2000"))
averages$Line<-factor(averages$Line,levels=c("m1","p2","m4","m5","p5","m6"))

anno <- data.frame(lab=c("SC","SP","MC","MP"),Treatment=as.factor(c("Clone","Population","Mixed-Clones","Mixed-Populations")),
                   meanLnFitnessMixed=0.18,Generation=0.6)

averages %>%##Go to the later script to see excluding outliers averages
  ggplot(aes(x=Generation,y=meanLnFitnessMixed))+facet_wrap(~Treatment,ncol=2)+
  geom_hline(yintercept = 0.2,linetype="dashed",color="grey60")+
  geom_hline(yintercept = 0.0,linetype="dashed",color="grey60")+
  geom_hline(yintercept = -0.2,linetype="dashed",color="grey60")+
  geom_hline(yintercept = -0.4,linetype="dashed",color="grey60")+
  
  geom_line(aes(color = Line, group = interaction(Rep, Line)),
            position = points.position,
            alpha = 0.3,lwd=3
  ) +
  geom_point(aes(color = Line), 
             position = points.position,
             alpha=0.3, size = 6 , shape=16  
  ) +
  
  scale_x_discrete(labels=c("0","500","2000"))+
  theme_classic()+
  theme(panel.background = element_blank(), panel.border = element_rect(color = 'black', fill = NA, size = 0.5)) +
  theme(strip.background = element_blank(),strip.text.x = element_blank())+
  coord_cartesian(ylim=c(-0.4,0.2))+
  labs(y="ln(Relative fitness)",x="Generation")+
  guides(fill="none")+ ##remove extra
  scale_color_discrete(name="Founder of SC and SP",labels=c("Ara–1","Ara+2","Ara–4","Ara–5","Ara+5","Ara–6"))+ ##
  theme(axis.text.x=element_text(size=19,colour = "black"),axis.text.y=element_text(size=19,colour = "black"),axis.title=element_text(size=22,colour = "black"))+
  theme(legend.text = element_text(size = 19),legend.title = element_text(size=19))+
  geom_text(data = anno, aes(label = lab),hjust='left',vjust=1,size=9)#labeller=averages.labeller,fontface = "bold"
ggsave('FigS1_72means.tiff', plot = last_plot(), dpi=500, width = 34, height = 25, units = "cm")
######END######FigS1_72means.tiff######

############Marker trajectory#########
######FigS2_Gen2000.tiff and Fig3_Gen500.tiff######

x1<- read.csv("Colony_count_Gen0_2000.csv",header=T)
MC <- subset(x1, Treatment=="Mixed-Clones")
MCx <- subset(MC, T==5)#-0.2424125
MCx <- subset(MC, T==7)#-0.8248866
MCx <- subset(MC, T==9)#0.5324507

mean(MCx$log2ratio,na.rm = T)

MP <- subset(x1, Treatment=="Mixed-Populations")
MPx <- subset(MP, T==5)#-7.026503
MPx <- subset(MP, T==7)#-7.004177
MPx <- subset(MP, T==9)#-6.877256
MPx <- subset(MP, T==11)#-6.478956

mean(MPx$log2ratio,na.rm = T)

Mixed <- subset(x1, Treatment=="Mixed-Clones" | Treatment=="Mixed-Populations")
Mixed$Line3<-as.factor(Mixed$Line3)

Mixed$log2ratio<-as.numeric(Mixed$log2ratio)


colorscale <- c("black","slateblue4","mediumblue","dodgerblue","thistle","gray",
                "mediumorchid","hotpink",
                "firebrick2","brown4","indianred3","darkorange","gold",
                "limegreen","aquamarine","darkgreen","darkcyan","yellow4")#olivedrab1
anno <- data.frame(labs=c("MC","MP"),Treatment=c("Mixed-Clones","Mixed-Populations"),
                   Generation=1900,log2ratio=5,Line3='1')

ggplot(Mixed[!is.na(Mixed$log2ratio),], aes(x=Generation, y=log2ratio, col=factor(Line3)))+
  geom_line(size=1)+facet_wrap(~ Treatment, ncol = 1)+
  scale_y_continuous(breaks=c(5,0,-5)) + #expand=c(0,3)
  scale_x_continuous(limits=c(0,2000),breaks=seq(0,2000,500)) + 
  coord_cartesian(ylim=c(-8,8))+
  theme_classic(base_line_size = 0.01)+
  theme(strip.background = element_blank(),strip.text.x = element_blank())+
  theme(panel.background = element_rect(fill="transparent",colour="black"))+
  labs(x="Generation",y=expression("log"[2]*"(Marker ratio)"))+
  theme(axis.title.x = element_text(size=18),axis.title.y = element_text(size=18))+
  scale_color_manual(values=colorscale)+theme(legend.position = 'none')+
  geom_text(data = anno, aes(label = labs),hjust='left',size=7,nudge_x=-6,nudge_y=2.5)+ #,fontface = "bold"
  theme(axis.text.x=element_text(size=16,colour = "black"),axis.ticks =element_line(size=0.3,colour = "black"),axis.text.y=element_text(size=16,colour = "black"))
ggsave('FigS2_Gen2000.tiff', plot = last_plot(), ,dpi=1000, width = 20, height = 20, units = "cm")

anno <- data.frame(labs=c("MC","MP"),Treatment=c("Mixed-Clones","Mixed-Populations"),
                   Generation=0,log2ratio=5,Line3='1')
ggplot(Mixed[!is.na(Mixed$log2ratio),], aes(x=Generation, y=log2ratio, col=factor(Line3)))+
  geom_line(size=1)+facet_wrap(~ Treatment, ncol = 1)+
  scale_color_manual(values=colorscale)+
  coord_cartesian(ylim=c(-8,8),xlim=c(0,500))+
  scale_y_continuous(breaks=c(5,0,-5)) +
  theme_classic(base_line_size = 0.01)+
  theme(strip.background = element_blank(),strip.text.x = element_blank())+
  #theme_bw()+
  theme(panel.background = element_rect(fill="transparent",colour="black",size=0.01))+
  labs(x="Generation",y=expression("log"[2]*"(Marker ratio)"))+
  theme(axis.title.x = element_text(size=18),axis.title.y = element_text(size=18))+
  theme(legend.position = 'none')+
  geom_text(data = anno, aes(label = labs),hjust='left',size=7,nudge_x=-6,nudge_y=2.5)+ #,fontface = "bold"
  theme(axis.text.x=element_text(size=16,colour = "black"),axis.ticks =element_line(size=0.3,colour = "black"),axis.text.y=element_text(size=16,colour = "black"))
ggsave('Fig3_Gen500.tiff', plot = last_plot(), ,dpi=300, width = 20, height = 20, units = "cm")

######END######FigS2_Gen2000.tiff and Fig3_Gen500.tiff######

######variance component, Table S2, and Fig4_founders_againstMI.tiff######

x<- read.csv("Competition_againstMI_T0.csv",header=T)

xPMI<-subset(x,Treatment2=="Population") 
sum_aovPMI<-summary(aov(LnFitness~Line,data=xPMI))
##Table S2 top
sum_aovPMI

MSamongPMI<-as.numeric(as.character(sum_aovPMI[[1]][[3]][[1]])) 
MSwithinPMI<-as.numeric(as.character(sum_aovPMI[[1]][[3]][[2]])) 
#variance component Sokal & Rohlf 		p214~
(MSamongPMI-MSwithinPMI)/18 #0.005244159

# Tukey test
aovPMI<-aov(LnFitness~Line,data=xPMI)
library(agricolae)
HSD<-HSD.test(aovPMI, trt = 'Line')

tukeyPMI <- data.frame(tukey= c("a", "ab", "bc", "bc", "c", "d"))

########END founder Population against MI#### 
########founder Clone against MI########
xCMI<-subset(x,Treatment2=="Clone") 
sum_aovCMI<-summary(aov(LnFitness~Line,data=xCMI))
##Table S2 bottom
sum_aovCMI

MSamongCMI<-  as.numeric(as.character(sum_aovCMI[[1]][[3]][[1]])) 
MSwithinCMI<-  as.numeric(as.character(sum_aovCMI[[1]][[3]][[2]])) 
#variance compotent Sokal & Rohlf 		p214~
(MSamongCMI-MSwithinCMI)/18 #0.0009322637

# Tukey test ##TukeyHSD and HSD.test gave the same results
aovCMI<-aov(LnFitness~Line,data=xCMI)
library(agricolae)
HSD<-HSD.test(aovCMI, trt = 'Line')

tukeyCMI <- data.frame(tukey= c("a", "ab", "b", "b", "b", "b"))

#########END founder Clone against MI#### 

x<- read.csv("Competition_againstMI_T0.csv",header=T)

xPMI<-subset(x,Treatment2=="Population") 
sum_aovPMI<-summary(aov(LnFitness~Line,data=xPMI))
clPMI18<-qt(0.025,17,lower.tail = F)*sqrt(sum_aovPMI[[1]][[3]][[2]]/18)#

xCMI<-subset(x,Treatment2=="Clone")
sum_aovCMI<-summary(aov(LnFitness~Line,data=xCMI))
clCMI18<-qt(0.025,17,lower.tail = F)*sqrt(sum_aovCMI[[1]][[3]][[2]]/18)#

x$Treatment2<-factor(x$Treatment2,levels=unique(x$Treatment2))
C1MI<-subset(x,Treatment2=="Clone"& Line=="m1") 
P1MI<-subset(x,Treatment2=="Population"& Line=="m1") 
C2MI<-subset(x,Treatment2=="Clone"& Line=="p2") 
P2MI<-subset(x,Treatment2=="Population"& Line=="p2") 
C4MI<-subset(x,Treatment2=="Clone"& Line=="m4") ##n=17
P4MI<-subset(x,Treatment2=="Population"& Line=="m4") 
C5MI<-subset(x,Treatment2=="Clone"& Line=="m5") 
P5MI<-subset(x,Treatment2=="Population"& Line=="m5") ##n=17
C5pMI<-subset(x,Treatment2=="Clone"& Line=="p5") 
P5pMI<-subset(x,Treatment2=="Population"& Line=="p5") 
C6MI<-subset(x,Treatment2=="Clone"& Line=="m6") 
P6MI<-subset(x,Treatment2=="Population"& Line=="m6") 


PMI <- data.frame(tukey= c("a", "ab", "bc", "bc", "c", "d"),
                Line=factor(c("m6","m1","m5","m4","p2","p5"),levels=c("m6","m1","m5","m4","p2","p5")),
                #LnFitness=c(-0.1,-0.02,-0.02,-0.1,-0.05,0),
                meanLnFitness=c(mean(P6MI$LnFitness),mean(P1MI$LnFitness),mean(P5MI$LnFitness,na.rm = T),
                                mean(P4MI$LnFitness),mean(P2MI$LnFitness),mean(P5pMI$LnFitness)),
                CI95low=c(mean(P6MI$LnFitness)-clPMI18,mean(P1MI$LnFitness)-clPMI18,mean(P5MI$LnFitness,na.rm = T)-clPMI18,
                          mean(P4MI$LnFitness)-clPMI18,mean(P2MI$LnFitness)-clPMI18,mean(P5pMI$LnFitness)-clPMI18),
                CI95up=c(mean(P6MI$LnFitness)+clPMI18,mean(P1MI$LnFitness)+clPMI18,mean(P5MI$LnFitness,na.rm = T)+clPMI18,
                         mean(P4MI$LnFitness)+clPMI18,mean(P2MI$LnFitness)+clPMI18,mean(P5pMI$LnFitness)+clPMI18))
#tukeyPMI <- data.frame(tukey= c("a", "ab", "bc", "bc", "c", "d"))
tukeyPMI <- data.frame(tukey= c("a", "ab", "bc", "bc", "c", "d"),
                     Line=factor(c("m6","m1","m5","m4","p2","p5"),levels=c("m6","m1","m5","m4","p2","p5")),
                     meanLnFitness=0.04+PMI$CI95up)


Population<-
  ggplot(PMI, aes(x=Line, y=meanLnFitness))+
  geom_point(aes(fill=Line),shape = 16,size=5,color = "black",show.legend = FALSE)+
  geom_errorbar(aes(ymin=CI95up,ymax=CI95low), size=1.1,width=0.3)+
  scale_x_discrete(labels=c("Ara–6","Ara–1","Ara–5","Ara–4","Ara+2","Ara+5"))+
  geom_text(data=tukeyPMI,aes(label = tukey),size=6)+
  coord_cartesian(ylim=c(-0.35,0.05))+
  scale_y_continuous(breaks=c(-0.3,-0.2,-0.1,0)) +
  labs(y="ln(Relative fitness)",x="")+
  theme_classic()+
  theme(axis.text.y=element_text(size=18,colour = "black"),axis.text.x=element_text(size=18,colour = "black"),axis.ticks =element_line(colour = "black"),axis.title=element_text(size=18,colour = "black"))

CMI <- data.frame(Line=factor(c("m4","m6","p5","p2","m5","m1"),
                            levels=c("m4","m6","p5","p2","m5","m1")),
                meanLnFitness=c(mean(C4MI$LnFitness),mean(C6MI$LnFitness),mean(C5pMI$LnFitness),
                                mean(C2MI$LnFitness),mean(C5MI$LnFitness,na.rm = T),mean(C1MI$LnFitness)),
                CI95low=c(mean(C4MI$LnFitness)-clCMI18,mean(C6MI$LnFitness)-clCMI18,mean(C5pMI$LnFitness)-clCMI18,
                          mean(C2MI$LnFitness)-clCMI18,mean(C5MI$LnFitness,na.rm = T)-clCMI18,mean(C1MI$LnFitness)-clCMI18),
                CI95up=c(mean(C4MI$LnFitness)+clCMI18,mean(C6MI$LnFitness)+clCMI18,mean(C5pMI$LnFitness)+clCMI18,
                         mean(C2MI$LnFitness)+clCMI18,mean(C5MI$LnFitness,na.rm = T)+clCMI18,mean(C1MI$LnFitness)+clCMI18))
#tukeyCMI <- data.frame(tukey= c("a", "ab", "b", "b", "b", "b"))
tukeyCMI <- data.frame(tukey= c("a", "ab", "b", "b", "b", "b"),
                     Line=factor(c("m4","m6","p5","p2","m5","m1"),
                                 levels=c("m4","m6","p5","p2","m5","m1")),
                     meanLnFitness=0.03+CMI$CI95up)

Clone<-
  ggplot(CMI, aes(x=Line, y=meanLnFitness))+
  geom_point(aes(fill=Line),shape = 16,size=5,color = "black",show.legend = FALSE)+
  geom_errorbar(aes(ymin=CI95up,ymax=CI95low), size=1.1,width=0.3)+
  scale_x_discrete(labels=c("Ara–4","Ara–6","Ara+5","Ara+2","Ara–5","Ara–1"))+
  geom_text(data=tukeyCMI,aes(label = tukey),size=6)+
  coord_cartesian(ylim=c(-0.3,0.0))+
  scale_y_continuous(breaks=c(-0.3,-0.2,-0.1,0)) +
  labs(y="ln(Relative fitness)",x="")+
  theme_classic()+
  theme(axis.text.y=element_text(size=18,colour = "black"),axis.text.x=element_text(size=18,colour = "black"),axis.ticks =element_line(colour = "black"),axis.title=element_text(size=18,colour = "black"))
plot_grid(Population,Clone,ncol = 1,labels = c("A","B"),label_size = 35,label_fontface ="plain",label_x = 0.13, rel_heights = c(3,3))
ggsave('Fig4_founders_againstMI.tiff', plot = last_plot(), ,dpi=300, width = 20, height =30, units = "cm")
######END######Fig4_founders_againstMI.tiff######

######variance component, Table S3 and FigS3_founders_againstREL.tiff######
y<- read.csv("Competition_againstREL_T0.csv",header=T)
y$Line<-factor(y$Line,levels=unique(y$Line))
yPREL<-subset(y,Treatment2=="Population") 

sum_aovPREL<-summary(aov(LnFitness~Line,data=yPREL))##Table S3 top

MSamongPREL<-  as.numeric(as.character(sum_aovPREL[[1]][[3]][[1]])) #
MSwithinPREL<-  as.numeric(as.character(sum_aovPREL[[1]][[3]][[2]])) #

#variance component Sokal & Rohlf 		p214~
(MSamongPREL-MSwithinPREL)/3 #0.02741141

yCREL<-subset(y,Treatment2=="Clone") 

sum_aovCREL<-summary(aov(LnFitness~Line,data=yCREL))##Table S3 bottom

MSamongCREL<-  as.numeric(as.character(sum_aovCREL[[1]][[3]][[1]])) #
MSwithinCREL<-  as.numeric(as.character(sum_aovCREL[[1]][[3]][[2]])) #

#variance compotent Sokal & Rohlf 		p214~
(MSamongCREL-MSwithinCREL)/3 #0.02690798

# Tukey test
aovPREL<-aov(LnFitness~Line,data=yPREL)

library(agricolae)
HSD.test(aovPREL, trt = 'Line')

tukeyPREL <- data.frame(tukey= c("a", "a", "a", "a", "a", "b"))
########END founder Population#### 
########founder Clone########

# Tukey test 
aovCREL<-aov(LnFitness~Line,data=yCREL)

library(agricolae)
HSD.test(aovCREL, trt = 'Line')

tukeyCREL <- data.frame(tukey= c("a", "ab", "bc", "bc", "cd", "d"))
#########END Clone tukey######

y<- read.csv("Competition_againstREL_T0.csv",header=T)

yPREL<-subset(y,Treatment2=="Population") 
sum_aovPREL<-summary(aov(LnFitness~Line,data=yPREL))
clPREL3<-qt(0.025,2,lower.tail = F)*sqrt(sum_aovPREL[[1]][[3]][[2]]/3)#

yCREL<-subset(y,Treatment2=="Clone")
sum_aovCREL<-summary(aov(LnFitness~Line,data=yCREL))
clCREL3<-qt(0.025,2,lower.tail = F)*sqrt(sum_aovCREL[[1]][[3]][[2]]/3)#

y$Line<-factor(y$Line,levels=unique(y$Line))
C1REL<-subset(y,Treatment2=="Clone"& Line=="m1") 
P1REL<-subset(y,Treatment2=="Population"& Line=="m1") 
C2REL<-subset(y,Treatment2=="Clone"& Line=="p2") 
P2REL<-subset(y,Treatment2=="Population"& Line=="p2") 
C4REL<-subset(y,Treatment2=="Clone"& Line=="m4") 
P4REL<-subset(y,Treatment2=="Population"& Line=="m4") 
C5REL<-subset(y,Treatment2=="Clone"& Line=="m5") 
P5REL<-subset(y,Treatment2=="Population"& Line=="m5") 
C5pREL<-subset(y,Treatment2=="Clone"& Line=="p5") 
P5pREL<-subset(y,Treatment2=="Population"& Line=="p5") 
C6REL<-subset(y,Treatment2=="Clone"& Line=="m6") 
P6REL<-subset(y,Treatment2=="Population"& Line=="m6") 


PREL <- data.frame(Line=factor(c("m6","m5","p2","m1","m4","p5"),levels=c("m6","m5","p2","m1","m4","p5")),
                   meanLnFitness=c(mean(P6REL$LnFitness),mean(P5REL$LnFitness,na.rm = T),mean(P2REL$LnFitness),
                                   mean(P1REL$LnFitness),mean(P4REL$LnFitness),mean(P5pREL$LnFitness)),
                   CI95low=c(mean(P6REL$LnFitness)-clPREL3,mean(P5REL$LnFitness,na.rm = T)-clPREL3,
                             mean(P2REL$LnFitness)-clPREL3,mean(P1REL$LnFitness)-clPREL3,
                             mean(P4REL$LnFitness)-clPREL3,mean(P5pREL$LnFitness)-clPREL3),
                   CI95up=c(mean(P6REL$LnFitness)+clPREL3,mean(P5REL$LnFitness,na.rm = T)+clPREL3,
                            mean(P2REL$LnFitness)+clPREL3,mean(P1REL$LnFitness)+clPREL3,
                            mean(P4REL$LnFitness)+clPREL3,mean(P5pREL$LnFitness)+clPREL3))
tukeyPREL2 <- data.frame(tukey= tukeyPREL,
                     Line=factor(c("m6","m5","p2","m1","m4","p5"),levels=c("m6","m5","p2","m1","m4","p5")),
                     meanLnFitness=0.1+PREL$CI95up)

Population<-
  ggplot(PREL, aes(x=Line, y=meanLnFitness))+
  geom_point(aes(fill=Line),shape = 16,size=5,color = "black",show.legend = FALSE)+
  geom_errorbar(aes(ymin=CI95up,ymax=CI95low), size=1.1,width=0.3)+
  scale_x_discrete(labels=c("Ara–6","Ara–5","Ara+2","Ara–1","Ara–4","Ara+5"))+
  geom_text(data=tukeyPREL2,aes(label = tukey),size=6)+
  coord_cartesian(ylim=c(0,1.10))+
  scale_y_continuous(breaks=c(0.00,0.25,0.5,0.75,1.00)) +
  labs(y="ln(Relative fitness)",x="")+
  theme_classic()+
  theme(axis.text.y=element_text(size=18,colour = "black"),axis.text.x=element_text(size=18,colour = "black"),axis.ticks =element_line(colour = "black"),axis.title=element_text(size=18,colour = "black"))

CREL <- data.frame(Line=factor(c("m6","p2","m5","m4","m1","p5"),
                               levels=c("m6","p2","m5","m4","m1","p5")),
                   meanLnFitness=c(mean(C6REL$LnFitness),mean(C2REL$LnFitness),
                                   mean(C5REL$LnFitness,na.rm = T),mean(C4REL$LnFitness),
                                   mean(C1REL$LnFitness),mean(C5pREL$LnFitness)),
                   CI95low=c(mean(C6REL$LnFitness)-clCREL3,mean(C2REL$LnFitness)-clCREL3,
                             mean(C5REL$LnFitness,na.rm = T)-clCREL3,mean(C4REL$LnFitness)-clCREL3,
                             mean(C1REL$LnFitness)-clCREL3,mean(C5pREL$LnFitness)-clCREL3),
                   CI95up=c(mean(C6REL$LnFitness)+clCREL3,mean(C2REL$LnFitness)+clCREL3,
                            mean(C5REL$LnFitness,na.rm = T)+clCREL3,mean(C4REL$LnFitness)+clCREL3,
                            mean(C1REL$LnFitness)+clCREL3,mean(C5pREL$LnFitness)+clCREL3))

tukeyCREL2 <- data.frame(tukey= tukeyCREL,
                     Line=factor(c("m6","p2","m5","m4","m1","p5"),
                                 levels=c("m6","p2","m5","m4","m1","p5")),
                     meanLnFitness=0.09+CREL$CI95up)

Clone<-
  ggplot(CREL, aes(x=Line, y=meanLnFitness))+
  geom_point(aes(fill=Line),shape = 16,size=5,color = "black",show.legend = FALSE)+
  geom_errorbar(aes(ymin=CI95up,ymax=CI95low), size=1.1,width=0.3)+
  scale_x_discrete(labels=c("Ara–6","Ara+2","Ara–5","Ara–4","Ara–1","Ara+5"))+
  geom_text(data=tukeyCREL2,aes(label = tukey),size=6)+
  coord_cartesian(ylim=c(0,1))+
  scale_y_continuous(breaks=c(0.00,0.25,0.5,0.75,1.00)) +
  labs(y="ln(Relative fitness)",x="")+
  theme_classic()+
  theme(axis.text.y=element_text(size=18,colour = "black"),axis.text.x=element_text(size=18,colour = "black"),axis.ticks =element_line(colour = "black"),axis.title=element_text(size=18,colour = "black"))

Clone
plot_grid(Population,Clone,ncol = 1,labels = c("A","B"),label_size = 35,label_fontface ="plain",label_x = 0.13, rel_heights = c(3,3))
ggsave('FigS3_founders_againstREL.tiff', plot = last_plot(), ,dpi=300, width = 20, height =30, units = "cm")
######END######variance component and FigS3_founders_againstREL.tiff######