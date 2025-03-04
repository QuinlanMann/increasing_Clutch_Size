library(tidyverse)
library(glmmTMB)
library(gridExtra)
library(sjPlot)
library(broom.mixed)
library(bbmle)
library(performance)
library(see)
library(ggeffects)
library(kableExtra)
library(factoextra)
library(ggthemes)
library(reshape2)
library(cowplot)

#data import and cleanup, ensuring that all columns are appropriate types
ANI<-(read.csv("C:/Users/quinl/Desktop/ANI/NEST_SUMMARY_ANIS.csv")	%>% 
        mutate(NULL, 
               ADULTS=as.numeric(ADULTS), 
               EGGS_BURIED=as.numeric(EGGS_BURIED),
               EGGS_BURIED=as.numeric(EGGS_BURIED), 
               EGGS_UNBURIED=as.numeric(EGGS_UNBURIED),
               HATCHED=as.numeric(HATCHED), 
               FLEDGED=as.numeric(FLEDGED),
               TOT_EGGS=as.numeric(TOT_EGGS),
               LOCATION=as.factor(LOCATION),
               YEAR=as.factor(YEAR), 
               SITE=as.factor(SITE)	
        ))

ANI$EpEg<-(ANI$EGGS_UNBURIED/ANI$TOT_EGGS)	
ANI$HpEg<-(ANI$HATCHED/ANI$EGGS_UNBURIED)	
ANI$FpHa<-(ANI$FLEDGED/ANI$HATCHED)

#removes the missing values
ANI<- ANI[!(is.na(ANI$ADULTS)), ] 
ANI<- ANI[! (ANI$ADULTS==1), ] #removes the record of 1 adult from the dataset
ANI<-ANI %>% #removes the unknown groups
  group_by(SITE) %>%
  filter(n() > 12)

#settign the theme for ggplots
theme_set(ggthemes::theme_few())

#PCA
#bringing in the "best first date" data and morphometrics
PCA_DAT<-read.csv("C:/Users/quinl/Desktop/ANI/PCA_DATA.csv")

#PCA results, and inserting PC1 back to the main dataset
PCA_DAT$PC1<-prcomp(PCA_DAT[11:13], scale = TRUE)[["x"]][,1]
theme_set(theme_few())

#PCA values for all PCs
pca_dat <- prcomp(PCA_DAT[11:13], scale = TRUE)

#looking at the R^2 of lienar models
summary(lm(pca_dat[["x"]][,1]~as.factor(PCA_DAT$DATE)))
summary(lm(PCA_DAT$DATE~PCA_DAT$TARSUS))
summary(lm(PCA_DAT$CULMEN~PCA_DAT$DATE))
summary(lm(PCA_DAT$MASS~PCA_DAT$DATE))


#Setup for Jackknife Leave one out Cross validation
library(sampling)

N=length(PCA_DAT$TARSUS)

jack.tars<-numeric(N)
jack.int1<-numeric(N)
jack.rsq1<-numeric(N)
predictions.tars<-NULL
jack.pc1<-numeric(N)
jack.int2<-numeric(N)
jack.rsq2<-numeric(N)
predictions.pc1<-NULL
jack.culm<-numeric(N)
jack.int3<-numeric(N)
jack.rsq3<-numeric(N)
predictions.culm<-NULL

for (i in 1:N) {
  
  validation<-PCA_DAT[i,]
  training<-PCA_DAT[-i,]
  
  model<-lm(DATE~TARSUS, data=training)
  jack.tars[i]<-coef(model)[2] 
  jack.int1[i]<-coef(model)[1]
  jack.rsq1[i]<-summary(model)$adj.r.squared
  predictions.tars[i]<-predict(model, newdata=validation)
  
  model<-lm(DATE~PC1, data=training)
  jack.pc1[i]<-coef(model)[2] 
  jack.int2[i]<-coef(model)[1]
  jack.rsq2[i]<-summary(model)$adj.r.squared
  predictions.pc1[i]<-predict(model, newdata=validation)
  
  model<-lm(DATE~CULMEN, data=training)
  jack.culm[i]<-coef(model)[2] 
  jack.int3[i]<-coef(model)[1]
  jack.rsq3[i]<-summary(model)$adj.r.squared
  predictions.culm[i]<-predict(model, newdata=validation)
  
}

#plotting the error associated with Leave one out cross-validation (LOOCV)
pc1.error<-(predictions.pc1-PCA_DAT$DATE)

#Generating Skew and Span Dataset from PCA
#bias corrected measures
bias_pc1<-(N*coef(lm(PCA_DAT$DATE~PCA_DAT$PC1))[2]) - (N - 1)*mean(jack.pc1)
bias_int2<-(N*coef(lm(PCA_DAT$DATE~PCA_DAT$PC1))[1]) - (N - 1)*mean(jack.int2)

#bringing in the unknown aged chicks
UNK_DATE<-read.csv("C:/Users/quinl/Desktop/ANI/METRICS_UNK_AGE.csv")
names(UNK_DATE) <- toupper(names(UNK_DATE))
UNK_DATE$TARSUS <- as.double(UNK_DATE$TARSUS)
UNK_DATE$MASS <- as.double(UNK_DATE$MASS)
UNK_DATE<-na.omit(UNK_DATE)
UNK_DATE$AGE<-NA

#center for scaling for PC1
UNK_DATE$TARSUS_s<-(UNK_DATE$TARSUS-mean(PCA_DAT$TARSUS))/sd(PCA_DAT$TARSUS)
UNK_DATE$CULMEN_s<-(UNK_DATE$CULMEN-mean(PCA_DAT$CULMEN))/sd(PCA_DAT$CULMEN)
UNK_DATE$MASS_s<-(UNK_DATE$MASS-mean(PCA_DAT$MASS))/sd(PCA_DAT$MASS)

#multiplying by the calculated rotations from the PCA of known data to get the PC1 value
UNK_DATE$PC1<-
  (UNK_DATE$TARSUS_s*pca_dat[["rotation"]][1])+
  (UNK_DATE$CULMEN_s*pca_dat[["rotation"]][2])+
  (UNK_DATE$MASS_s*pca_dat[["rotation"]][3])


#function to generate ages
#x is the data you want to extrapolate dates
#z is the variable you want regressed against date to generate the linear model
INTER_AGE_j<-function(coeff, data, intercept){
  w = coeff*data + intercept
  return(w)
}

#use the bias corrected estimates from the JACKKNIFE dataset
#UNK_DATE$AGE_TCONT_j <- round(INTER_AGE_j(bias_tars, UNK_DATE$TARSUS, bias_int1))                   
UNK_DATE$AGE_PCONT_j <- round(INTER_AGE_j(bias_pc1, UNK_DATE$PC1, bias_int2))
#UNK_DATE$AGE_CCONT_j <- round(INTER_AGE_j(bias_culm, UNK_DATE$CULMEN, bias_int3))
UNK_DATE<-UNK_DATE[-c(7:11)]

#trying to get data from extrapolated stuff
#reordering the dataframe so that within 
UNK_DATE<-UNK_DATE[with(UNK_DATE, order(TERRITORY,DATE, CHICK)), ]
#removing the YEAR variable to assign "Year territory" name and get hatching date
UNK_DATE$DATE <- as.Date(UNK_DATE$DATE, format = "%Y-%m-%d")
#subtracting days age from hatching date
UNK_DATE$HATCH <- UNK_DATE$DATE-(UNK_DATE$AGE_PCONT_j-1)

#giving "year territory" name
UNK_DATE$YEAR <- format(UNK_DATE$HATCH, format = "%Y")
UNK_DATE$GRP <- paste(UNK_DATE$YEAR, UNK_DATE$TERRITORY)

library(data.table)

succ<-(read.csv("C:/Users/quinl/Desktop/ANI/SUCCESSFUL_NESTS.csv")	%>% 
         mutate(NULL, 
                ADULTS=as.numeric(ADULTS), 
                EGGS_BURIED=as.numeric(EGGS_BURIED),
                EGGS_BURIED=as.numeric(EGGS_BURIED), 
                EGGS_UNBURIED=as.numeric(EGGS_UNBURIED),
                HATCHED=as.numeric(HATCHED), 
                FLEDGED=as.numeric(FLEDGED),
                TOT_EGGS=as.numeric(TOT_EGGS),
                LOCATION=as.factor(LOCATION),
                YEAR=as.factor(YEAR), 
                SITE=as.factor(SITE)	
         ))

#new dataset just of entries with accurate chickdates
suc<-subset(succ, SPAN>0)

#female group size instead of #adults
suc$FEM<-floor(suc$ADULTS/2)

suc$GRP<-paste(suc$YEAR, suc$LOCATION)

#from DOI : 10.1098/rspb.2018.1452
#ti = time since beginning of span
#tm = time at the middle of the span
#p = proportion at that time

skew2<-function(ti, tm, p){
  skew<-((ti-tm)/(tm))*p
  print(skew)
}

suc$SPAN<-as.numeric(suc$SPAN)
suc$skew1<-skew2(1, (suc$SPAN/2), (suc$NEW.CHCKS_DAY1/suc$HATCHED))  
suc$skew2<-skew2(2, (suc$SPAN/2), (suc$NEW.CHCKS_DAY2/suc$HATCHED))  
suc$skew3<-skew2(3, (suc$SPAN/2), (suc$NEW.CHCKS_DAY3/suc$HATCHED))  
suc$skew4<-skew2(4, (suc$SPAN/2), (suc$NEW.CHCKS_DAY4/suc$HATCHED))  
suc$skew5<-skew2(5, (suc$SPAN/2), (suc$NEW.CHCKS_DAY5/suc$HATCHED))  
suc$skew6<-skew2(6, (suc$SPAN/2), (suc$NEW.CHCKS_DAY6/suc$HATCHED))  
suc$skew7<-skew2(7, (suc$SPAN/2), (suc$NEW.CHCKS_DAY7/suc$HATCHED))  

#sums all the values to calculate skew metric
suc$SKEW<-apply(suc[23:29], 1, sum, na.rm=T)

#return to work with Unknown chick ages
#removes duplicate measures of each chick in the group and keeps the first instance
SKEW_1<-setDT(UNK_DATE)[ , .SD[(!duplicated(CHICK))], by = GRP]

SKEW_1<-SKEW_1%>%
  group_by(GRP)%>%
  summarise(DATE_DIFF=as.numeric(HATCH-min(HATCH))+1,
            SPAN=max(DATE_DIFF))
SKEW_1<-SKEW_1%>%
  group_by(GRP, DATE_DIFF, SPAN)%>%
  summarise(n=n())

#reshaping data to be able to calculate the skew values
SKEW_1<-reshape2::dcast(SKEW_1, GRP+SPAN~DATE_DIFF)

SKEW_SUCC<-succ[1:11]

SKEW_SUCC$GRP <- paste(SKEW_SUCC$YEAR, SKEW_SUCC$LOCATION)

#merging data on group success with unknown hatching data
SKEW_1<-merge(SKEW_SUCC, SKEW_1, by="GRP")

SKEW_1$skew1<-skew2(1, (SKEW_1$SPAN/2), (SKEW_1$`1`/SKEW_1$HATCHED))
SKEW_1$skew2<-skew2(2, (SKEW_1$SPAN/2), (SKEW_1$`2`/SKEW_1$HATCHED))
SKEW_1$skew3<-skew2(3, (SKEW_1$SPAN/2), (SKEW_1$`3`/SKEW_1$HATCHED))
SKEW_1$skew4<-skew2(4, (SKEW_1$SPAN/2), (SKEW_1$`4`/SKEW_1$HATCHED))
SKEW_1$skew5<-skew2(5, (SKEW_1$SPAN/2), (SKEW_1$`5`/SKEW_1$HATCHED))
SKEW_1$skew6<-skew2(6, (SKEW_1$SPAN/2), (SKEW_1$`6`/SKEW_1$HATCHED))
SKEW_1$skew7<-skew2(7, (SKEW_1$SPAN/2), (SKEW_1$`7`/SKEW_1$HATCHED))
SKEW_1$skew8<-skew2(8, (SKEW_1$SPAN/2), (SKEW_1$`8`/SKEW_1$HATCHED))
SKEW_1$skew10<-skew2(10, (SKEW_1$SPAN/2), (SKEW_1$`10`/SKEW_1$HATCHED))

SKEW_1$SKEW<-apply(SKEW_1[23:31], 1, sum, na.rm=T)

#creating empty columns so that I can merge the known and unknown datasets together without issue
suc$NEW.CHCKS_DAY8<-NA
suc$NEW.CHCKS_DAY10<-NA

#merging known and unknown aged chick datasets together
SKEW_DATA<-rbind(SKEW_1[,c("GRP", "YEAR", "LOCATION", "SITE", "ADULTS", "HATCHED", "FLEDGED", 
                           "1", "2", "3", "4",  "5", "6", "7", "8", "10", "SPAN", "SKEW")], 
                 setnames((suc[,c("GRP", "YEAR", "LOCATION", "SITE", "ADULTS", "HATCHED", "FLEDGED", "NEW.CHCKS_DAY1", 
                                  "NEW.CHCKS_DAY2", "NEW.CHCKS_DAY3", "NEW.CHCKS_DAY4", "NEW.CHCKS_DAY5", 
                                  "NEW.CHCKS_DAY6", "NEW.CHCKS_DAY7", "NEW.CHCKS_DAY8", "NEW.CHCKS_DAY10", "SPAN", "SKEW")]), 
                          names((SKEW_1[,c("GRP", "YEAR", "LOCATION", "SITE", "ADULTS", "HATCHED", "FLEDGED", 
                                          "1", "2", "3", "4",  "5", "6", "7", "8", "10", "SPAN", "SKEW")])))
)

SKEW_DATA<-SKEW_DATA%>%distinct(GRP, .keep_all=T)
SKEW_DATA$FpHa<-SKEW_DATA$FLEDGED/SKEW_DATA$HATCHED
SKEW_DATA$FEM<-floor(SKEW_DATA$ADULTS/2)

ANI$GRP<-paste(ANI$YEAR, ANI$LOCATION)
SKEW_1<-merge(SKEW_DATA, ANI, by="GRP")

SKEW_1$true_false<-ifelse(SKEW_1$HATCHED.x==SKEW_1$HATCHED.y, "true", "false")
skew_new<-subset(SKEW_1, true_false=="true")

#Models----

success_mod1<-glmmTMB(cbind(FLEDGED, HATCHED-FLEDGED) ~ 
                        EGGS_UNBURIED+ADULTS+as.factor(NEST_ATTMPT)+I((TOT_EGGS-EGGS_UNBURIED)/floor(ADULTS/2))+SITE+(1|SITE:LOCATION)+(1|YEAR),
                      data = ANI, 
                      family=binomial(link="logit"))

success_mod2<-glmmTMB(cbind(HATCHED, EGGS_UNBURIED-HATCHED) ~ 
                        EGGS_UNBURIED+ADULTS+as.factor(NEST_ATTMPT)+I((TOT_EGGS-EGGS_UNBURIED)/floor(ADULTS/2))+SITE+(1|SITE:LOCATION)+(1|YEAR),
                      data = ANI, 
                      family=binomial(link="logit"))

success_mod3<-glmmTMB(cbind(FLEDGED.x, HATCHED.x-FLEDGED.x) ~ EGGS_UNBURIED+SPAN*SKEW+FEM+HATCHED.x+FEM:HATCHED.x+(1|LOCATION.x)+(1|YEAR.x), 
                      data=skew_new, 
                      family = binomial(link = "logit"))
Span_mod<-glmmTMB(SPAN ~ EGGS_UNBURIED+FEM+as.factor(NEST_ATTMPT)+SITE.x+(1|SITE.x:LOCATION.x)+(1|YEAR.x),
                  data = skew_new, 
                  family = poisson(link="log"))
Skew_mod<-glmmTMB(SKEW ~ EGGS_UNBURIED+FEM+as.factor(NEST_ATTMPT)+SITE.x+(1|SITE.x:LOCATION.x)+(1|YEAR.x),
                  data = skew_new)

check_model(success_mod1)
check_model(success_mod2)
check_model(success_mod3)
check_model(Span_mod)
check_model(Skew_mod)

summary(success_mod1)
summary(success_mod2)
summary(success_mod3)
summary(Span_mod)
summary(Skew_mod)

#Visualization----
plot(ggpredict(success_mod1, 
               terms ="EGGS_UNBURIED"))+
  geom_jitter(data=ANI, aes(EGGS_UNBURIED, (FLEDGED/HATCHED)), 
              color="black",
              fill="black",
              stroke=0,
              width=0, 
              height=0, 
              pch=21, 
              size=3)+
  coord_cartesian(xlim = c(4,16))+
  labs(x="Clutch size", y="Fledging Success\n(fledged/hatched)", title="")+
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
    panel.border = element_rect(colour = "white", fill=NA, size=1))

plot(ggpredict(success_mod2, 
               terms ="EGGS_UNBURIED"))+
  geom_jitter(data=ANI, aes(EGGS_UNBURIED, HpEg), 
              color="black",
              fill="black",
              stroke=0,
              width=0.15, 
              height=0.01, 
              pch=21, 
              size=3)+
  coord_cartesian(xlim = c(2,24))+
  labs(x="Clutch size", y="Hatching Success\n(hatched/incubated)", title="")+
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
    panel.border = element_rect(colour = "white", fill=NA, size=1))

plot(ggpredict(success_mod3, 
               terms ="EGGS_UNBURIED"))+
  geom_jitter(data=skew_new, aes(EGGS_UNBURIED, (FLEDGED.x/HATCHED.x)), 
              color="black",
              fill="black",
              stroke=0,
              width=0.15, 
              height=0.01, 
              pch=21, 
              size=3)+
  coord_cartesian(xlim = c(4,16))+
  labs(x="Clutch size", y="Hatching Success\n(hatched/incubated)", title="")+
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
    panel.border = element_rect(colour = "white", fill=NA, size=1))


plot(ggpredict(Span_mod, 
               terms ="EGGS_UNBURIED"))+
  geom_jitter(data=skew_new, aes(EGGS_UNBURIED, SPAN), 
              color="black",
              fill="black",
              stroke=0,
              width=0.15, 
              height=0.15, 
              pch=21, 
              size=3)+
  coord_cartesian(xlim = c(4,16))+
  labs(x="Clutch size", y="Hatching Span", title="")+
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
    panel.border = element_rect(colour = "white", fill=NA, size=1))

plot(ggpredict(Skew_mod, 
               terms ="EGGS_UNBURIED"))+
  geom_jitter(data=skew_new, aes(EGGS_UNBURIED, SKEW, fill=SITE.x), 
              color="black",
              fill="black",
              stroke=0,
              width=0.15, 
              height=0.01, 
              pch=21, 
              size=3)+
  #coord_cartesian(xlim = c(4,16))
  labs(x="Clutch size", y="Hatching Skew", title="")+
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
    panel.border = element_rect(colour = "white", fill=NA, size=1))
