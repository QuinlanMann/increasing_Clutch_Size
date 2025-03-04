#Tables and visualization
kbl(
  cbind(
    (rbind(
      (tidy(Tot.nb2.1)%>%
         mutate(term=recode(term, 
                            `FEM`="Female group size",
                            `(Intercept)`="Intercept"))),
      (tidy(Inc.nb2.zi.1)%>%
         mutate(term=recode(term, 
                            `FEM`="Female group size",
                            `(Intercept)`="Intercept"))),
      (tidy(Hatc.p.1)%>%
         mutate(term=recode(term, 
                            `FEM`="Female group size",
                            `(Intercept)`="Intercept"))),
      (tidy(Fled.p.1)%>%
         mutate(term=recode(term, 
                            `FEM`="Female group size",
                            `(Intercept)`="Intercept"))))),
    (rbind(
      (tidy(Tot.nb2.2)%>%
         mutate(term=recode(term, 
                            `FEM`="Female group size",
                            `(Intercept)`="Intercept"))),
      (tidy(Inc.nb2.zi.2)%>%
         mutate(term=recode(term, 
                            `FEM`="Female group size",
                            `(Intercept)`="Intercept"))),
      (tidy(Hatc.p.2)%>%
         mutate(term=recode(term, 
                            `FEM`="Female group size",
                            `(Intercept)`="Intercept"))),
      (tidy(Fled.p.2)%>%
         mutate(term=recode(term, 
                            `FEM`="Female group size",
                            `(Intercept)`="Intercept")))))
  )[-c(1,2,9:12)],
  col.names = c("grp", "Term", "Estimate", "Std. Err.", "z value", "p value","Estimate", "Std. Err.", "z value", "p value"), 
  caption="summary of probability models shown in log(odds) scale",
  booktabs = T)%>%
  pack_rows(index = c("Eggs laid"= 6,
                      "incubation" = 6, 
                      "hatching" = 6, 
                      "fledging" = 6))%>%
  kable_styling(latex_options = "HOLD_position")


grid.arrange(
  plot(ggpredict(Tot.nb2.1, 
                 terms ="FEM"))+
    geom_jitter(data=ANI2, aes(FEM, TOT_EGGS), 
                color="black",
                fill="black",
                stroke=0,
                width=0.075, 
                height=0.25, 
                pch=21, 
                size=3)+
    coord_cartesian(xlim = c(1,10))+
    scale_y_continuous(breaks=c(0, 30, 60, 90))+
    scale_x_continuous(breaks=c(2,4,6,8,10))+
    labs(x="Female group size", y="Eggs laid", title="a")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      axis.title=element_text(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)), 
  plot(ggeffect(Tot.nb2.2, 
                terms ="FEM", 
                offset=0))+
    geom_jitter(data=ANI2, aes(FEM, TOT_EGGS/FEM), 
                color="black",
                fill="black",
                stroke=0, 
                width=0.075, 
                height=0.25, 
                pch=21, 
                size=3)+
    coord_cartesian(xlim = c(1,10))+
    scale_y_continuous(breaks=c(0, 10, 20, 30))+
    scale_x_continuous(breaks=c(2,4,6,8,10))+
    labs(x="Female group size", y="Eggs laid per capita", title="b")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.title=element_text(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)),
  plot(ggpredict(Inc.nb2.zi.1, 
                 terms ="FEM"))+
    geom_jitter(data=ANI2, aes(FEM, EGGS_UNBURIED), 
                color="black",
                fill="black",
                stroke=0,
                width=0.075, 
                height=0.25, 
                pch=21, 
                size=3)+
    coord_cartesian(xlim = c(1,10))+
    scale_y_continuous(breaks=c(0, 10, 20, 30))+
    scale_x_continuous(breaks=c(2,4,6,8,10))+
    labs(x="Female group size", y="Eggs incubated", title="c")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      axis.title=element_text(color="black"),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)),
  plot(ggeffect(Inc.nb2.zi.2, 
                terms ="FEM", 
                offset=0))+
    geom_jitter(data=ANI2, aes(FEM, EGGS_UNBURIED/FEM), 
                color="black",
                fill="black",
                stroke=0,
                width=0.075, 
                height=0.25, 
                pch=21, 
                size=3)+
    coord_cartesian(xlim = c(1,10))+
    scale_y_continuous(breaks=c(0, 5, 10, 15, 20, 25))+
    scale_x_continuous(breaks=c(2,4,6,8,10))+
    labs(x="Female group size", y="Eggs incubated per capita", title="d")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.title=element_text(color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)),
  plot(ggpredict(Hatc.p.1, 
                 terms ="FEM"))+
    geom_jitter(data=ANI2, aes(FEM, HATCHED), 
                color="black",
                fill="black",
                stroke=0,
                width=0.075, 
                height=0.25, 
                pch=21, 
                size=3)+
    coord_cartesian(xlim = c(1,10))+
    scale_y_continuous(breaks=c(0, 3, 6, 9, 12))+
    scale_x_continuous(breaks=c(2,4,6,8,10))+
    labs(x="Female group size", y="Eggs hatched", title="e")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.title=element_text(color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)),
  plot(ggeffect(Hatc.p.2, 
                terms ="FEM", 
                offset=0))+
    geom_jitter(data=ANI2, aes(FEM, HATCHED/FEM), 
                color="black",
                fill="black",
                stroke=0,
                width=0.075, 
                height=0.25, 
                pch=21, 
                size=3)+
    coord_cartesian(xlim = c(1,10))+
    scale_y_continuous(breaks=c(0, 2, 4, 6, 8, 10))+
    scale_x_continuous(breaks=c(2,4,6,8,10))+
    labs(x="Female group size", y="Eggs hatched per capita", title="f")+
    theme(
      axis.title=element_text(color="black"),
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)),
  plot(ggpredict(Fled.p.1, 
                 terms ="FEM"))+
    geom_jitter(data=ANI2, aes(FEM, FLEDGED), 
                color="black",
                fill="black",
                stroke=0,
                width=0.075, 
                height=0.25, 
                pch=21, 
                size=3)+  
    coord_cartesian(xlim = c(1,10))+
    scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    scale_x_continuous(breaks=c(2,4,6,8,10))+
    labs(x="Female group size", y="Chicks fledged", title="g")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title=element_text(color="black"),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)),
  plot(ggeffect(Fled.p.2, 
                terms ="FEM", 
                offset=0))+
    geom_jitter(data=ANI2, aes(FEM, FLEDGED/FEM), 
                color="black",
                fill="black",
                stroke=0,
                width=0.075, 
                height=0.25, 
                pch=21, 
                size=3)+
    coord_cartesian(xlim = c(1,10))+
    scale_y_continuous(breaks = c(0,2,4,6,8,10))+
    scale_x_continuous(breaks=c(2,4,6,8,10))+
    labs(x="Female group size", y="Chicks fledged per capita", title="h")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.title=element_text(color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)),
  ncol=2)

kbl(rbind(
  (tidy(EpEg.b.1)[-c(1:2)]%>%
     mutate(term=recode(term, 
                        `FEM`="Female group size",
                        `(Intercept)`="Intercept", 
                        `ANI2.NESTS`="Nesting attempts/group/season", 
                        `ANI2.SITELC`="Study Site"))), 
  (tidy(HpEg.b.1)[-c(1:2)]%>%
     mutate(term=recode(term, 
                        `FEM`="Female group size",
                        `(Intercept)`="Intercept", 
                        `ANI2.NESTS`="Nesting attempts/group/season", 
                        `ANI2.SITELC`="Study Site"))), 
  (tidy(FpHa.b.1)[-c(1:2)]%>%
     mutate(term=recode(term, 
                        `FEM`="Female group size",
                        `(Intercept)`="Intercept", 
                        `ANI2.NESTS`="Nesting attempts/group/season", 
                        `ANI2.SITELC`="Study Site")))), 
  col.names = c("grp", "Term", "Estimate", "Std. Err.", "z value", "p value"), 
  caption="summary of probability models shown in log(odds) scale",
  booktabs = T)%>%
  pack_rows(index = c("Pr(incubation)" = 6, 
                      "Pr(hatching)" = 6, 
                      "Pr(fledging)" = 6))%>%
  kable_styling(latex_options = "HOLD_position")


grid.arrange(
  plot(ggpredict(EpEg.b.1, 
                 terms ="FEM"))+
    geom_jitter(data=ANI_EpEg, aes(FEM, ANI2.EpEg), 
                color="black",
                fill="black",
                stroke=0,
                width=0.075,
                pch=21, 
                size=3)+  
    coord_cartesian(ylim=c(0,1), xlim = c(1,10))+
    scale_y_continuous()+
    scale_x_continuous(breaks=c(2,4,6,8,10))+
    labs(x="Female group size", y="Proportion incubated", title="a")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.title=element_text(color="black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)),
  plot(ggpredict(HpEg.b.1, 
                 terms ="FEM"))+
    geom_jitter(data=ANI_HpEg, aes(FEM, ANI2.HpEg), 
                color="black",
                fill="black",
                stroke=0,
                width=0.075,
                pch=21, 
                size=3)+
    coord_cartesian(ylim=c(0,1), xlim = c(1,10))+
    scale_y_continuous()+
    scale_x_continuous(breaks=c(2,4,6,8,10))+
    labs(x="Female group size", y="Poportion hatching", title="b")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.title=element_text(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)),
  plot(ggpredict(FpHa.b.1, 
                 terms ="FEM"))+
    geom_jitter(data=ANI_FpHa, aes(FEM, ANI2.FpHa), 
                color="black",
                fill="black",
                stroke=0,
                width=0.075, 
                pch=21, 
                size=3)+
    coord_cartesian(ylim=c(0,1), xlim = c(1,10))+
    scale_y_continuous()+
    scale_x_continuous(breaks=c(2,4,6,8,10))+
    labs(x="Female group size", y="Proportion fledging", title="c")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      axis.title=element_text(color="black"),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)),
  ncol=1)

#skew model visulization and table
grid.arrange(
  plot(ggpredict(skew_mod5, 
                 terms ="SPAN"))+
    geom_jitter(data=SKEW_DATA, aes(SPAN, FpHa), 
                color="black",
                fill="black",
                stroke=0,
                width=0.075,
                pch=21, 
                size=3)+
    scale_y_continuous(breaks=c(0.25, 0.5, 0.75, 1))+
    scale_x_continuous(breaks=c(0, 2, 4, 6, 8, 10))+
    labs(x="Hatching span (days)", y="proportion fledging", title="a")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.line.x = element_line(color="black"),
      axis.title=element_text(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)), 
  plot(ggpredict(skew_mod5, 
                 terms ="SKEW"))+
    geom_jitter(data=SKEW_DATA, aes(SKEW, FpHa), 
                color="black",
                fill="black",
                stroke=0,
                width=0.05,
                pch=21, 
                size=3)+
    scale_y_continuous(breaks=c(0.25, 0.5, 0.75, 1))+
    labs(x="Skew index", y="Proportion fledging", title="b")+
    theme(
      text = element_text(family="Arial", size = 12, color="black"),
      axis.text.x = element_text(size=12, color = "black"),
      axis.text.y = element_text(size=12, color="black"),
      axis.ticks = element_line(color="black"),
      axis.title=element_text(color="black"),
      axis.line.x = element_line(color="black"),
      axis.line.y = element_line(color="black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "white", fill=NA, size=1)), 
  ncol=1)

tidy(skew_mod5)[-c(1,2)]
#End of code

ggplot(ANI2, aes(FEM))+
  geom_histogram(center=0, bins=10, color="black", fill="gray")+
  theme(
    text = element_text(family="Arial", size = 12, color="black"),
    axis.text.x = element_text(size=12, color = "black"),
    axis.text.y = element_text(size=12, color="black"),
    axis.ticks = element_line(color="black"),
    axis.title=element_text(color="black"),
    axis.line.x = element_line(color="black"),
    axis.line.y = element_line(color="black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "white", fill=NA, size=1))
