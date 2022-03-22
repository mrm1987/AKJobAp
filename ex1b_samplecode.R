#PREAMBLE----------------------------------------------------------------------
library("multcomp")
library("Rmisc")
library("tidyverse")
library("lme4")
library("lmerTest")
library("MuMIn")
library("optimx")
library("sjstats")
library("lsr")

#Gather Data-------------------------------------------------------------------

#! SET CORRECT WORKING DIRECTORY !#

setwd("/home/matt/Desktop/Wheel/Barely/")

read.csv("barely1.csv")%>%
  mutate(Participant=as.factor(Participant)) -> final.data

#Modify Data-------------------------------------------------------------------

#Finding to-be-removed Participants

final.data%>%
  mutate(Participant=as.factor(Participant))%>%
  select(matches("win"))%>%
  mutate(Participant=as.factor(1:54))%>%
  gather("Cond", "Rating", 1:6) %>%
  filter(Rating!=11)%>% 
  count(Participant)%>%  
  filter(n>2)%>% 
  select(contains("Participant")) -> error.list

#Remove Participants

final.data%>%
  mutate(Participant=as.factor(Participant))%>%
  select(-Q70)%>%
  anti_join(., error.list, by="Participant")%>%
  anti_join(., double.list, by="Participant")%>%
  gather("Info", "Rating", luc_3_cen_bef_YEL:almo_18_cen_win_YEL)%>%
  separate(Info, c("Word", "Wheel", "Loc", "WinSec", "Color"))%>%
  mutate(Word=as.factor(Word))%>% 
  filter(WinSec!="win")-> mod.data.removed

#No Remove Participants

final.data%>%
  mutate(Participant=as.factor(Participant))%>%
  select(-Q70)%>%
  gather("Info", "Rating", luc_3_cen_bef_YEL:almo_18_cen_win_YEL)%>%
  separate(Info, c("Word", "Wheel", "Loc", "WinSec", "Color"))%>%
  filter(WinSec!="win")-> mod.data

#Between Subjects (Because of Order Effects [see later analyses])
mod.data.removed %>%
  mutate(Word=as.factor(Word))%>% 
  filter(Order!="ALB" | Word!="luc")%>%
  filter(Order!="ALB" | Word!="bare")%>%
  filter(Order!="BAL" | Word!="almo")%>%
  filter(Order!="BAL" | Word!="luc")%>%
  filter(Order!="LBA" | Word!="bare")%>%
  filter(Order!="LBA" | Word!="almo") -> between.mod.data.removed

#Means and Errors--------------------------------------------------------------------

mod.data.removed%>%
  filter(Wheel!=3 | Loc!="cen")%>%
  mutate(Wheel = as.factor(Wheel)) %>%
  summarySEwithin(., measurevar="Rating", withinvars=c("Word", "WinSec", "Wheel"), idvar="Participant",
                  na.rm=TRUE, conf.interval=.95) -> se

#Assessment Differences-----------------------------------------------------------------

#Omnibus test of WORD

mod.data.removed %>%
  summarySEwithin(., measurevar="Rating", withinvars=c("Word"), idvar="Participant",
                  na.rm=TRUE, conf.interval=.95) -> means

mod.data.removed %>%
  lmer(Rating ~ Word + (1|Participant), ., REML=TRUE) -> gen.model

mod.data.removed %>%
  lmer(Rating ~1 + (1|Participant), ., REML=TRUE) -> gen.null.model

anova(gen.model, gen.null.model) #Overall effect of WORD on participant's ratings

#Direct comparison between BARELY and ALMOST

mod.data.removed %>%
  filter(Word!="luc")%>%
  lmer(Rating ~ Word + (1|Participant), ., REML=TRUE) -> model1

mod.data.removed %>%
  filter(Word!="luc")%>%
  lmer(Rating ~1 + (1|Participant), ., REML=TRUE) -> null.model1

anova(model1, null.model1) #No significant diff

#Direct comparison between LUCKY and ALMOST

mod.data.removed %>%
  filter(Word!="bare")%>%
  lmer(Rating ~ Word + (1|Participant), ., REML=TRUE) -> model2

mod.data.removed %>%
  filter(Word!="bare")%>%
  lmer(Rating ~1 + (1|Participant), ., REML=TRUE) -> null.model2

anova(model2, null.model2) #LUCKY ratings > ALMOST ratings

#Direct comparison between LUCKY and BARELY

mod.data.removed %>%
  filter(Word!="almo")%>%
  lmer(Rating ~ Word + (1|Participant), ., REML=TRUE) -> model3

mod.data.removed %>%
  filter(Word!="almo")%>%
  lmer(Rating ~1 + (1|Participant), ., REML=TRUE) -> null.model3

anova(model3, null.model3) #LUCKY ratings > BARELY ratings

#Correlation Analysis-----------------------------------------------------------------

#Correlation between ALMOST and BARELY

mod.data.removed %>%
  filter(Word!="luc")%>%
  spread(Word, Rating)%>%
  lmer(bare ~ almo + (1|Participant), ., REML=TRUE) -> septest.model1

summary(septest.model1) 
r.squaredGLMM(septest.model1) #Measure of goodness-of-fit for mixed-effect models 

#Correlation between BARELY and LUCKY

mod.data.removed %>%
  filter(Word!="almo")%>%
  spread(Word, Rating)%>%
  lmer(luc ~ bare + (1|Participant), ., REML=TRUE) -> septest.model2

summary(septest.model2)
r.squaredGLMM(septest.model2)

#Correlation between ALMOST and LUCKY

mod.data.removed %>%
  filter(Word!="bare")%>%
  spread(Word, Rating)%>%
  lmer(luc ~ almo + (1|Participant), ., REML=TRUE) -> septest.model3

summary(septest.model3)
r.squaredGLMM(septest.model3)

#Correlation Graphs-------------------------------------------------------------------

#LUCKY and BARELY

mod.data.removed %>%
  filter(Word!="almo")%>%
  spread(Word, Rating)%>%
  ggplot(aes(x=bare, y=luc)) +
  geom_jitter(width = 0.2, height = 0.2) +
  stat_summary(fun.data = mean_cl_normal) +
  geom_abline(intercept = 4.76, slope = .34, color="blue", size=1.2) +
  scale_y_continuous(name="Lucky Rating",limits = c(1, 10), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  scale_x_continuous(name="Barely Won Rating",limits = c(1, 10), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  theme(axis.text = element_text(size=22), plot.title = element_text(size=24, face = "bold", hjust = 0.5),
        axis.title = element_text(size=24), panel.background = element_blank(),
        axis.ticks.x=element_blank(), axis.title.x = element_text(size = 24),
        axis.text.x = element_text(size = 20, colour = "black"), axis.title.y = element_text(size = 24),
        axis.text.y = element_text(colour = "black", size=20), legend.position="none")

#LUCKY and ALMOST 

mod.data.removed %>%
  filter(Word!="bare")%>%
  spread(Word, Rating)%>%
  ggplot(aes(x=almo, y=luc)) +
  geom_jitter(width = 0.2, height = 0.2) +
  stat_summary(fun.data = mean_cl_normal) +
  geom_abline(intercept = 4.73, slope = .33, color="blue", size=1.2) +
  scale_y_continuous(name="Lucky Rating",limits = c(1, 10), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  scale_x_continuous(name="Almost Lost Rating",limits = c(1, 10), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  theme(axis.text = element_text(size=22), plot.title = element_text(size=24, face = "bold", hjust = 0.5),
        axis.title = element_text(size=24), panel.background = element_blank(),
        axis.ticks.x=element_blank(), axis.title.x = element_text(size = 24),
        axis.text.x = element_text(size = 20, colour = "black"), axis.title.y = element_text(size = 24),
        axis.text.y = element_text(colour = "black", size=20), legend.position="none")

#ALMOST and BARELY  

mod.data.removed %>%
  filter(Word!="luc")%>%
  spread(Word, Rating)%>%
  ggplot(aes(x=almo, y=bare)) +
  geom_jitter(width = 0.2, height = 0.2) +
  stat_summary(fun.data = mean_cl_normal) +
  geom_abline(intercept = .80, slope = .74, color="blue", size=1.2) +
  scale_y_continuous(name="Barely Won Rating",limits = c(1, 10), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  scale_x_continuous(name="Almost Lost Rating",limits = c(1, 10), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  theme(axis.text = element_text(size=22), plot.title = element_text(size=24, face = "bold", hjust = 0.5),
        axis.title = element_text(size=24), panel.background = element_blank(),
        axis.ticks.x=element_blank(), axis.title.x = element_text(size = 24),
        axis.text.x = element_text(size = 20, colour = "black"), axis.title.y = element_text(size = 24),
        axis.text.y = element_text(colour = "black", size=20), legend.position="none")

#ANOVA Order Effects w/Contrasts (Replication Question)--------------------------------------

#Creation of orthogonal contrastrasts

c(2,0) ->c1
c(-1,1)->c2
c(-1,-1)->c3
rbind(c2,c1,c3) -> cHelmert

cHelmert->contrasts(mod.data.removed$Order)

#Effect size measure accounting for within-subject variation

mod.data.removed %>%
  filter(Loc=="cen")%>%
  summarySEwithin(., measurevar="Rating", withinvars=c("Order"), idvar="Participant",
                  na.rm=TRUE, conf.interval=.95)

#Repeated-measure ANOVA 

mod.data.removed %>%
  filter(Loc=="cen")%>%
  group_by(Participant, Order, Word, Wheel, WinSec)%>%
  aov(Rating~Order * Wheel * Word * WinSec+ Error(Participant/(Wheel*Word*WinSec)), data = .) -> aov.order

summary(aov.order, split=list(Order=list("BALv2"=1, "ALBvLBA"=2))) #The BARELY-ALMOST-LUCKY order received lower ratings than other two orderings

#Order Graph---------------------------------------------------------------------------------------

#Graph Labels
lab2 <- c("ALB" = "Amost-Lucky-Barely", "BAL" = "Barely-Almost-Lucky", "LBA" = "Lucky-Barely-Almost")

#Means for graph

mod.data.removed %>%
  filter(Loc=="cen")%>%
  group_by(Order, Word, Wheel)%>%
  summarise(Rating=mean(Rating)) -> means

#Standard-error bar values

mod.data.removed%>%
  filter(Loc=="cen")%>%
  group_by(Participant, Word, Wheel, Order)%>%
  summarize(avg=mean(Rating)) %>% 
  summarySEwithin(., measurevar="avg", withinvars=c("Word", "Wheel"), betweenvars = c("Order"), idvar="Participant",
                  na.rm=TRUE, conf.interval=.95)%>%
  mutate(avg=means$Rating) -> se2

#Graph

mod.data.removed %>%
  filter(Loc=="cen")%>%
  group_by(Word, Wheel, Order)%>%
  summarise(Rating=mean(Rating))%>%
  ggplot(aes(x=reorder(Wheel, Rating), y=Rating, group=Word)) +
  geom_line(linetype=1, size=1) +
  geom_point(aes(shape=Word), size=4, color="black", fill="white", stroke=1)+
  scale_shape_manual(labels = c("Almost lost", "Barely won", "Lucky"), values = c(0,1,2)) +
  geom_errorbar(width=.2, aes(ymin=Rating-se2$se, ymax=Rating + se2$se), size = .4) +
  scale_color_manual(values=c("red", "blue", "purple"), breaks=c("almo", "luc", "bare"), labels=c("Almost lost", "Lucky", "Barely won")) +
  scale_y_continuous(limits = c(.5, 10), breaks = c(1,2,3,4,5,6,7,8,9,10), name = "Mean Ratings") +
  scale_x_discrete(labels = c("3-Sector", "18-Sector")) + 
  facet_grid(~Order, labeller = as_labeller(lab2), switch = "x") +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=20), panel.background = element_blank(), 
        axis.title.x=element_blank(), legend.text = element_text(size=16), axis.line.x = element_line(size=1),
        axis.text.x = element_text(size = 13, colour = "black"), axis.title.y = element_text(size = 20),legend.title = element_blank(),
        axis.text.y = element_text(colour = "black", size=18), legend.position = c(.2, .9), strip.text = element_text(size=14))

#Wheel Analysis (Between Subjects) ---------------------------------------------------------

#Creation of contrasts

contr.helmert(3)->crHelmert
crHelmert->contrasts(between.mod.data.removed$Word)

#Repeated-measures ANOVA

between.mod.data.removed%>%
  filter(Loc=="cen")%>% 
  mutate(Wheel = as.factor(Wheel), WinSec = as.factor(WinSec))%>% 
  aov(Rating~ Wheel * Word * WinSec + Error(Participant/(Wheel*WinSec)), data = .) -> between.wheel.aov

summary(between.wheel.aov, split=list(Word=list( "Clo vs Almo"=1, "Luc vs AlmoClo"=2))) #Significant effect of WHEEL (18-sector had higher ratings)

#3-sector vs 18-sector Wheel Graphs---------------------------------------------------------------

#Standard-error bars

between.mod.data.removed%>%
  filter(Loc=="cen")%>%
  mutate(Wheel = as.factor(Wheel))%>%
  summarySEwithin(., measurevar="Rating", withinvars=c("Wheel"), betweenvars = ("Word"), idvar="Participant",
                  na.rm=TRUE, conf.interval=.95) -> se4

#Graph

between.mod.data.removed%>%
  filter(Loc=="cen")%>%
  mutate(Wheel = as.factor(Wheel))%>% 
  group_by(Wheel, Word) %>%
  summarise(avg=round(mean(Rating), 2))%>%
  ggplot(aes(x=reorder(Wheel, avg), y=avg, group=Word)) +
  geom_line(linetype=1, size=1) +
  geom_errorbar(width=.2, aes(ymin=avg-se4$se, ymax=avg + se4$se), size = .4) +
  scale_shape_manual(labels = c("Almost lost", "Barely won", "Lucky"), values = c(0,1,2)) +
  geom_point(aes(shape=Word), size=4, color="black", fill="white", stroke=1)+
  scale_y_continuous(limits = c(1, 10), breaks = c(1,2,3,4,5,6,7,8,9,10), name = "Mean Ratings") +
  scale_x_discrete(labels = c("3-Sector", "18-Sector")) + 
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=20), panel.background = element_blank(), 
        axis.title.x=element_blank(), legend.text = element_text(size=16), axis.line.x = element_line(size=1),
        axis.text.x = element_text(size = 16, colour = "black"), axis.title.y = element_text(size = 20),legend.title = element_blank(),
        axis.text.y = element_text(colour = "black", size=18), legend.position = c(.2, .9), strip.text = element_text(size=20))

#Decisiveness (i.e., WINSEC) Analysis (Between Subjects) --------------------------------------------------------------------

#Contrast creation

contr.helmert(3)->crHelmert
crHelmert->contrasts(between.mod.data.removed$Word)

#Repeated-measures ANOVA

between.mod.data.removed%>%
  filter(Loc!="cen")%>%
  mutate(Wheel = as.factor(Wheel), WinSec = as.factor(WinSec))%>% 
  aov(Rating ~ WinSec * Word + Error(Participant/(WinSec)), data = .) -> between.decisive.aov

summary(between.decisive.aov, split=list(Word=list( "Clo vs Almo"=1, "Luc vs AlmoClo"=2))) #No effect WINSEC

#Decisiveness Graph------------------------------------------------------------------------

#Graph labels

lab <- c("3" = "3-Sector")

#Standard-error bars

between.mod.data.removed%>%
  filter(Wheel!=3 | Loc!="cen")%>%
  filter(Wheel!=18)%>%
  mutate(Wheel = as.factor(Wheel))%>%
  summarySEwithin(., measurevar="Rating", withinvars=c("WinSec"), betweenvars = ("Word"), idvar="Participant",
                  na.rm=TRUE, conf.interval=.95) -> se3

#Graph

between.mod.data.removed%>%
  filter(Wheel!=3 | Loc!="cen")%>%
  filter(Wheel!=18)%>%
  mutate(Wheel = as.factor(Wheel))%>% 
  group_by(Word, Wheel, WinSec)%>%
  summarise(avg=round(mean(Rating), 2))%>%
  ggplot(aes(x=reorder(WinSec, avg), y=avg, group=Word)) +
  geom_line(linetype=1, size=1) +
  geom_errorbar(width=.2, aes(ymin=avg-se3$se, ymax=avg + se3$se), size = .4) +
  scale_shape_manual(labels = c("Almost lost", "Barely won", "Lucky"), values = c(0,1,2)) +
  geom_point(aes(shape=Word), size=4, color="black", fill="white", stroke=1)+
  scale_color_manual(values=c("red", "blue", "purple"), breaks=c("almo", "luc", "bare"), labels=c("Almost", "Lucky", "Barely")) +
  scale_y_continuous(limits = c(1, 10), breaks = c(1,2,3,4,5,6,7,8,9,10), name = "Mean Ratings") +
  scale_x_discrete(labels = c("Departed Lose", "Approaching Lose")) + 
  facet_grid(~fct_reorder(Wheel, desc(avg)), labeller = as_labeller(lab), switch = "x") +
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size=20), panel.background = element_blank(), 
        axis.title.x=element_blank(), legend.text = element_text(size=16), axis.line.x = element_line(size=1),
        axis.text.x = element_text(size = 16, colour = "black"), axis.title.y = element_text(size = 20),legend.title = element_blank(),
        axis.text.y = element_text(colour = "black", size=18), legend.position = c(.2, .2), strip.text = element_text(size=20))

