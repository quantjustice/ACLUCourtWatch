
# Charlotte McClintock
# ACLU Court Watch
# Analysis Script

# ..................................................................................

library(tidyverse)
library(margins)
library(xtable)
library(RColorBrewer)
library(scales)
library(table1)
library(stargazer)

load("ACLUCourtWatch.Rdata")

aclublues <- brewer.pal(9, "Blues")
blueyellow <- c("#003f5c", "#374c80", "#7a5195", "#bc5090", "#ef5675",
                "#ff764a", "#ffa600")

cw$DefRep <- ifelse(cw$DefRepByCounsel=="1", "Counsel", "No Counsel")
cw$DefRep <- ifelse(is.na(cw$DefRepByCounsel), "Unknown", cw$DefRep)
cw$DefRep <- factor(cw$DefRep, levels=c("Counsel", "Unknown", "No Counsel"))


# cw <- left_join(cw, cw_charges, by=c("RespondentID","Charges"))

cw$Poverty.Related <- ifelse(cw$ChargesCategorized=="Poverty Related/Petty", 1, 0)

cw$JudgeSet <- ifelse(cw$Bondsetbycourt %in% 
                        c("Cash or Surety (C/S)", "Cash Only"), 
                      "Judge Set Cash Bail", "Judge Set PR")
cw$ProsAsk <- ifelse(cw$ProsecutionBondRequest %in% 
                       c("Cash or Surety (C/S)", "Cash Only"), 
                     "Prosecution Sought Cash Bail", "Prosecution Sought PR")

cw$JudgeProsAgree <- with(cw, ifelse(CashBailSet=="1"&ProsCashBail=="1"|CashBailSet=="0"&ProsCashBail=="0", 1, 0))

cw$ChargesCategorized <- ifelse(is.na(cw$ChargesCategorized), "Unknown", cw$ChargesCategorized)

cw$DefGender <- with(cw, ifelse(!DefGender %in% c("Man", "Woman"), NA, as.character(DefGender)))

table(cw$DefGender)


# ..................................................................................

# (4) Counsel

# What percent of people are represented by counsel at bond setting?

# if all of these are bond setting cases, 85% 
prop.table(table(cw$DefRepByCounsel))

# by court
prop.table(table(cw$DefRep, cw$WhichCourt), 2)

pdf("counsel-court.pdf", width=6.5, height=4)
ggplot(subset(cw, !is.na(WhichCourt)), aes(WhichCourt, fill=factor(DefRep))) +
  geom_bar(position="fill") + scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values=c(aclublues[3], "grey", aclublues[7])) +
  labs(title="Presence of Counsel by Court",
       subtitle="Colorado ACLU Court Watch Project (September - November 2019)",
       y="Percent of Cases",
       x="Court",
       fill="Counsel",
       caption="Data from the ACLU of Colorado")
dev.off()

# by race

prop.table(table(cw$DefRep, cw$DefRace), 2)

pdf("counsel-race.pdf", width=6.5, height=4)
ggplot(cw, aes(DefRace, fill=factor(DefRep))) +
  geom_bar(position="fill") + scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values=c(aclublues[3], "grey", aclublues[7])) +
  labs(title="Presence of Counsel by Race of Defendant",
       subtitle="Colorado ACLU Court Watch Project (September - November 2019)",
       y="Percent of Cases",
       x="Race of Defendant",
       fill="Counsel",
       caption="Data from the ACLU of Colorado")
dev.off()

# What percent of people who plead at bond setting are represented by counsel?

prop.table(table(subset(cw, Result=="Guilty plea")$DefRepByCounsel)) # 76.9%

# Are there apparent differences in bond setting practices (money bond/PR bond/amount 
# of bond) dependent on presence of counsel?

xtable(100*prop.table(table(cw$JudgeSet, cw$DefRepByCounsel), margin = 2))
xtable(100*prop.table(table(cw$ProsAsk, cw$DefRepByCounsel), margin = 2))

# does the presence of counsel vary by charge? # yes
pdf("counsel-charge.pdf", width=8, height=5)
ggplot(cw, aes(ChargesCategorized, fill=factor(DefRep))) + 
  geom_bar(position="fill") + coord_flip() + scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values=c(aclublues[3], "grey", aclublues[7])) +
  labs(title="Presence of Counsel by Category of Charge",
       subtitle="Colorado ACLU Court Watch Project (September - November 2019)",
       y="Percent of Cases",
       x="Charge Category",
       fill="Counsel",
       caption="Data from the ACLU of Colorado")
dev.off()


# model of prosecution bond request
ask.mod1 <- glm(ProsCashBail ~ DefRep, 
               family="binomial", data=cw)
ask.mod2 <- glm(ProsCashBail ~ DefRep + DefRace + DefGender, 
                family="binomial", data=cw)
ask.mod <- glm(ProsCashBail ~ DefRep + DefRace + DefGender + Assault.Violent.DV +
             Drug.Related + Poverty.Related, 
           family="binomial", data=cw)
summary(ask.mod)
margins(ask.mod)
stargazer(ask.mod1, ask.mod2, ask.mod)
margins(ask.mod)


# model of judge bond set
set.mod <- glm(CashBailSet ~DefRep + DefRace + DefGender + Assault.Violent.DV + 
             Drug.Related + Poverty.Related + ProsCashBail, 
           family="binomial", data=cw)
summary(set.mod)

set.mod1<- glm(CashBailSet ~ DefRep + DefRace + DefGender + Assault.Violent.DV + 
                 Drug.Related + Poverty.Related, 
               family="binomial", data=cw)
summary(set.mod1)



stargazer(set.mod1, set.mod)
xtable(data.frame(summary(margins(set.mod))))

# ..................................................................................

# (6) What percent of the time does the judge follow the prosecutor's bond request?

table(cw$JudgeSet, cw$ProsAsk)
sum(diag(table(cw$JudgeSet, cw$ProsAsk)))/nrow(cw) # agreed in 85% of cases

xtable(round(100*prop.table(table(cw$JudgeSet, cw$ProsAsk), margin = 2),1))

# what is this mitigated by?
with(subset(cw, DefRep=="Counsel"), prop.table(table(JudgeSet, ProsAsk), 2))
with(subset(cw, DefRep=="No Counsel"), prop.table(table(JudgeSet, ProsAsk), 2))
# numbers aren't big enough here to say anything about this difference

cw.mod <- subset(cw, !DefGender=="Gender non-conforming")
cw.mod$DefGender <- droplevels(as.factor(cw.mod$DefGender))
  
agree.mod <- glm(JudgeProsAgree ~ DefRep + DefRace + DefGender + Assault.Violent.DV + 
             Drug.Related + Poverty.Related, 
           family="binomial", data=cw.mod)
summary(agree.mod)
margins(agree.mod)
print(xtable(data.frame(summary(margins(agree.mod)))), include.rownames=FALSE)

# ..................................................................................

# (8) What percent of defendants are experiencing homelessness 
#and how does that play out in all the above analyses?
str_detect(string=names(cw), "homeless")
# only measure of homelessness is in defense bond arguments
sum(cw$DefBondArg.Defendantishomeless) # 42 individuals identified as homeless

ggplot(cw, aes(WhichCourt)) + geom_bar() + 
  facet_wrap(~DefBondArg.Defendantishomeless, scales="free") 

homeless <- subset(cw, DefBondArg.Defendantishomeless=="1")


arb <- subset(cw, DefBondArg.Defendantishomeless=="1") %>% group_by(ChargesCategorized) %>% count()

pdf("homeless-charge.pdf", width=8, height=4)
ggplot(arb, aes(reorder(ChargesCategorized,n), n)) + 
  stat_summary(geom="bar", fun.y="sum", fill=aclublues[8]) + 
  coord_flip() +
  labs(title="Homeless Individuals by Charge Category", 
       subtitle="Homeless as identified by defense bond argument (n=42)", 
       x="", 
       y="Number of Individuals", 
       caption="Data from the ACLU of Colorado") + 
  theme(plot.margin = unit(c(0.5, 2,0.5,0.2), "cm"))
dev.off()

table(subset(cw, DefBondArg.Defendantishomeless=="1")$ChargesCategorized)

table(homeless$DefRace)

# ..................................................................................

# 5. Sentencing

# only sentences for guilty pleas
table(cw$Result)
guiltyplea <- subset(cw, Result=="Guilty plea")

# graph sentences
arb <- data.frame(count=apply(guiltyplea[,36:40], 2, sum))
arb$sentence <- c("Jail Time", "Useful Public Service", "Restitution", "Fine", "Program Participation")

pdf("sentence.pdf", width=7, height=4)
ggplot(arb, aes(reorder(sentence,count), count)) + 
  stat_summary(geom="bar", fun.y="sum", fill=aclublues[8]) + 
  coord_flip() +
  labs(title="Sentences Assigned after Guilty Plea", 
       subtitle="among 262 individuals who entered guilty pleas, September-November 2019",
       x="", 
       y="Number of Individuals", 
       caption="Data from the ACLU of Colorado") + 
  theme(plot.margin = unit(c(0.5, 2,0.5,0.2), "cm")) + 
  geom_text(aes(x=sentence, y=count, label=count), hjust=-0.2) + ylim(0,210)
dev.off()

# model jail time
guiltyplea$DefGender <- droplevels(guiltyplea$DefGender)
jail.mod <- glm(Sentence.Jailtime ~ DefRep + DefRace + DefGender + Assault.Violent.DV + 
      Drug.Related + Poverty.Related, data=guiltyplea)
summary(jail.mod)
margins(jail.mod)
stargazer(jail.mod)

jailtime <- subset(guiltyplea, Sentence.Jailtime=="1")

# compare race distributions
racedem <- cw %>% group_by(DefRace) %>% count()
jailracedem <- jailtime %>% group_by(DefRace) %>% count()
gpracedem <- guiltyplea %>% group_by(DefRace) %>% count()

racedem$group <- "All Individuals in Court"
jailracedem$group <- "Individuals Receiving Jail Time"
gpracedem$group <- "Individuals Pleading Guilty"

racecomp <- full_join(racedem, jailracedem, by=names(racedem))
racecomp <- full_join(racecomp, gpracedem, by=names(racedem))

pdf("race-comp.pdf", width=8.3, height=5.3)
ggplot(racecomp, aes(x=group, y=n, fill=DefRace)) + 
  stat_summary(fun.y = "sum", geom="bar", position="fill") +
  scale_fill_manual(values=c(aclublues[c(3,5,6,8,9)], "grey")) + 
  scale_y_continuous(labels=scales::percent) +
  labs(title="Race Demographics of Guilty Pleas & Individuals Receiving Jail Time",
       subtitle="Colorado ACLU Court Watch Project (September - November 2019)",
       y="Percent of Cases",
       x="",
       fill="Race of Defendant",
       caption="Data from the ACLU of Colorado") + 
  theme(plot.margin = unit(c(0.5, 0.5,0.5,1), "cm")) 
dev.off()

# compare charge distributions

allcharges <- cw %>% group_by(ChargesCategorized) %>% count()
jailcharges <- jailtime %>% group_by(ChargesCategorized) %>% count()
gpcharges <- guiltyplea %>% group_by(ChargesCategorized) %>% count()


allcharges$group <- "All Individuals in Court"
jailcharges$group <- "Individuals Receiving Jail Time"
gpcharges$group <- "Individuals Pleading Guilty"

chargecomp <- full_join(allcharges, jailcharges, by=names(allcharges))
chargecomp <- full_join(chargecomp, gpcharges, by=names(allcharges))

chargecomp$ChargesCategorized <- fct_relevel(chargecomp$ChargesCategorized, "Unknown")

pdf("charge-comp.pdf", width=9, height=4.3)
ggplot(chargecomp, aes(x=group, y=n, fill=ChargesCategorized)) + 
  stat_summary(fun.y = "sum", geom="bar", position="fill", alpha=0.9) +
  scale_fill_manual(values=c("grey", blueyellow))+ 
  scale_y_continuous(labels=scales::percent) +
  labs(title="Charge Categories of Guilty Pleas & Individuals Receiving Jail Time",
       subtitle="Colorado ACLU Court Watch Project (September - November 2019)",
       y="Percent of Cases",
       x="",
       fill="Charge Category",
       caption="Data from the ACLU of Colorado") + 
  theme(plot.margin = unit(c(0.5, 0.5,0.5,1), "cm")) 
dev.off()

table(guiltyplea$WhichCourt)
table(jailtime$WhichCourt)
table(cw$WhichCourt)

pdf("plea-which.pdf", width=6, height=4)
arb <- data.frame(table(guiltyplea$WhichCourt)/table(cw$WhichCourt))
ggplot(arb, aes(Var1, Freq)) + stat_summary(geom="bar", fun.y="sum", fill=aclublues[8]) + 
  scale_y_continuous(labels=scales::percent, limits = c(0, 0.5)) +
  labs(title="Percent of Individuals Pleading Guilty by Court",
       subtitle="Colorado ACLU Court Watch Project (September - November 2019)",
       y="Percent of Cases",
       x="",
       fill="Court",
       caption="Data from the ACLU of Colorado")
dev.off()

# ..................................................................................

# defense bond arguments
defbondarg <- data.frame(count=apply(cw[,53:61], 2, sum))
defbondarg$why <- c("Defendant is homeless", "Defendant is unemployed", 
                    "Defendant can't afford bond", 
                "Defendant needs release (school/work/family)", 
                "Defendant has strong ties", 
                "Defendant has no/minimal criminal history", 
                "Defendant has low risk assessment", 
                "Charge is non-violent", "Case evidence is weak")

pdf("def-bond-arg.pdf", width=9, height=5)
ggplot(defbondarg, aes(reorder(why,count), count)) + 
  stat_summary(geom="bar", fun.y="sum", fill=aclublues[8]) + 
  coord_flip() + scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(title="Defense Bond Arguments by Frequency",
       subtitle="Colorado ACLU Court Watch Project (September - November 2019)",
       y="Number of Cases",
       x="",
       caption="Data from the ACLU of Colorado")
dev.off()

# prosecution bond arguments
prosbondarg <- data.frame(count=apply(cw[,73:78], 2, sum))
prosbondarg$why <- c("Defendant has history of missing court",
                     "Defendant has extensive criminal history", 
                     "Defendant is a flight risk", 
                     "Charge is violent", 
                     'Risk assessment score is high', 
                     "Evidence is strong")

pdf("pros-bond-arg.pdf", width=9, height=5)
ggplot(prosbondarg, aes(reorder(why,count), count)) + 
  stat_summary(geom="bar", fun.y="sum", fill=aclublues[8]) + 
  coord_flip() + scale_x_discrete(labels = function(x) str_wrap(x, width = 20))+ 
  labs(title="Prosecution Bond Arguments by Frequency",
       subtitle="Colorado ACLU Court Watch Project (September - November 2019)",
       y="Number of Cases",
       x="",
       caption="Data from the ACLU of Colorado")
dev.off()

pros.arg.mod <- glm(CashBailSet ~ ProsBondArg.ThedefendanthasahistoryofmissingcourthaspriorFTAs + 
      ProsBondArg.Thedefendanthasextensivecriminalhistory + 
      ProsBondArg.Thedefendantisaflightrisk +
      ProsBondArg.Thechargeisviolent + 
      ProsBondArg.Theriskassessmentscoreishigh + 
      ProsBondArg.Theevidenceinthecaseisstrong + 
      Assault.Violent.DV + Drug.Related + Poverty.Related, family="binomial", data=cw)
summary(pros.arg.mod)


# ...........................................................................................

# defense release conditions
apply(cw[,45:51], 2, sum)
defrelcond <- data.frame(count=apply(cw[,45:51], 2, sum))
defrelcond$condition <- c("GPS", "Protection order", "Sobriety", 
                          "No guns", "Supervision", "Enhanced intensive supervision", 
                          "Admin only supervision")
defrelcond$who <- "Defense"

# prosecution release conditions
apply(cw[,65:71], 2, sum)
prosrelcond <- data.frame(count=apply(cw[,65:71], 2, sum))
prosrelcond$condition <- c("GPS", "Protection order", "Sobriety", 
                          "No guns", "Supervision", "Enhanced intensive supervision", 
                          "Admin only supervision")
prosrelcond$who <- "Prosecution"


# court set release conditions
apply(cw[,83:89], 2, sum)
courtrelcond <- data.frame(count=apply(cw[,83:89], 2, sum))
courtrelcond$condition <- c("GPS", "Protection order", "Sobriety", 
                          "No guns", "Supervision", "Enhanced intensive supervision", 
                          "Admin only supervision")
courtrelcond$who <- "Court"

relcond <- full_join(defrelcond, prosrelcond, by=names(defrelcond))
relcond <- full_join(relcond, courtrelcond, by=names(defrelcond))

pdf("counsel-bondcond.pdf", width=9, height=4)
ggplot(subset(relcond, !who=="Court"), aes(x=who, y=count, fill=condition)) + 
  stat_summary(fun.y = "sum", geom="bar", position="dodge", alpha=0.9) +
  scale_fill_manual(values=c(blueyellow)) +
  labs(title="Counsel Requested Bond Conditions by Frequency",
       subtitle="Colorado ACLU Court Watch Project (September - November 2019)",
       y="Number of Cases",
       x="",
       fill="Condition",
       caption="Data from the ACLU of Colorado")
dev.off()

pdf("court-bondcond.pdf", width=7, height=5)
ggplot(subset(relcond, who=="Court"), aes(x=reorder(condition, count), y=count, fill=condition)) + 
  stat_summary(fun.y = "sum", geom="bar", position="dodge", alpha=0.9) + 
  coord_flip() +
  scale_fill_manual(values=c(blueyellow)) + guides(fill=F) + 
  labs(title="Court Bond Conditions by Frequency",
       subtitle="Colorado ACLU Court Watch Project (September - November 2019)",
       y="Number of Cases",
       x="",
       caption="Data from the ACLU of Colorado") + 
  theme(plot.margin = unit(c(0.5, 1.5 ,0.5,0.2), "cm")) + 
  geom_text(aes(x=reorder(condition, count), y=count, label=count), hjust=-0.2) + 
  ylim(0,565) + scale_x_discrete(labels = function(x) str_wrap(x, width = 20))
dev.off()

# how often do defense, prosecution, and judges agree?
# how is this related to charge type?


cond_charge <- cw %>% group_by(ChargesCategorized) %>% summarise_at(c(83:89), sum)
names(cond_charge)[2:8] <- c("GPS", "Protection order", "Sobriety", 
                        "No guns", "Supervision", "Enhanced intensive supervision", 
                        "Admin only supervision")
cond_charge <- gather(cond_charge, 2:8, key="condition", value="count")
cond_charge$ChargesCategorized <- fct_rev(fct_relevel(cond_charge$ChargesCategorized, "Unknown"))

pdf("charge-bondcond.pdf", width=7.5, height=8)
ggplot(cond_charge, aes(x=condition, y=count, fill=condition)) + 
  stat_summary(fun.y="sum", geom="bar") + 
  facet_wrap(~ChargesCategorized, scales="free_y", ncol=2)  + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + guides(fill=F) +
  scale_fill_manual(values=c(blueyellow, "grey"))+ 
  labs(title="Court Bond Conditions by Charge Type & Frequency",
       subtitle="Colorado ACLU Court Watch Project (September - November 2019)",
       y="Number of Cases",
       x="",
       caption="Data from the ACLU of Colorado")
dev.off()

# <------------- Sam's Additions ------------------>

# Descriptive Statistics for an Appendix


table1data <-
  cw %>% 
  select(
    WhichCourt,
    DefRace,
    DefGender,
    DefEstimatedAge,
    DefRepByCounsel,
    ChargesCategorized,
    Assault.Violent.DV,
    Drug.Related,
    ProsCashBail,
    CashBailSet,
    #  CourtWatchername,
    #  Judgeslastname,
    CaseResolved,
    Result
    #  DefenseBondRequest,
    #  ProsecutionBondRequest,
    #  Bondsetbycourt
  ) %>%
  mutate(
    # WhichCourt = fct_explicit_na(WhichCourt),
    Drug.Related = fct_explicit_na( fct_recode(as.factor(Drug.Related), "Yes" = "1", "No" = "0" )) ,
    Assault.Violent.DV = fct_explicit_na( fct_recode(as.factor(Assault.Violent.DV), "Yes" = "1", "No" = "0" )) ,
    DefRepByCounsel = fct_explicit_na( fct_recode(as.factor(DefRepByCounsel), "Yes" = "1", "No" = "0" )) ,
    CashBailSet = fct_explicit_na( fct_recode(as.factor(CashBailSet), "Yes" = "1", "No" = "0" )) ,
    ProsCashBail = fct_explicit_na( fct_recode(as.factor(ProsCashBail), "Yes" = "1", "No" = "0" ) ),
    CaseResolved =  fct_explicit_na(   fct_recode(as.factor(CaseResolved), "Yes" = "1", "No" = "0" )),
    Result = fct_explicit_na(Result),
    ChargesCategorized = fct_explicit_na(ChargesCategorized)
  ) %>%
  filter(is.na(WhichCourt) == FALSE)

label(table1data$DefRace) <- "Defendant Race"
label(table1data$DefGender) <- "Defendant Gender"
label(table1data$DefEstimatedAge) <- "Defendant Estimated Age"
label(table1data$DefRepByCounsel) <- "Defendant Represented by Counsel"
label(table1data$ChargesCategorized) <- "Primary Charge"
label(table1data$Assault.Violent.DV) <- "Violent Crime, Assault, or DV"
label(table1data$Drug.Related) <- "Drug Related"
label(table1data$CashBailSet) <- "Cash Bail Set"
label(table1data$CaseResolved) <- "Case Resolved"
label(table1data$ProsCashBail) <- "Prosecution Requests Cash Bail"


table1(~ DefRace + DefGender + DefEstimatedAge + DefRepByCounsel + ChargesCategorized + Assault.Violent.DV + Drug.Related + ProsCashBail + CashBailSet + CaseResolved + Result | WhichCourt, data = table1data)

######## What Offenses were Charged?

prop.table(table(cw$ChargesCategorized))
# Overall, 35% of offenses were Violent, only 4% were drug related, 28% were Poverty Related or Petty. 8% constituted violation of court restrictions. 



pdf("charges-court.pdf", width=11, height=7)

cw %>%  
  group_by( WhichCourt, ChargesCategorized) %>%
  summarize(count= n()) %>%
  filter(is.na(WhichCourt) == FALSE) %>%
  mutate(ChargesCategorized = ifelse(ChargesCategorized == "Assault/Violent Offense/DV", "Assault/Violent", ChargesCategorized)) %>%
  
  ggplot( aes(x = reorder(WhichCourt, count), y = count, fill = reorder(ChargesCategorized, count))) + 
  geom_bar(stat = "identity", position = "dodge", alpha = .8, color = NA) +
#  ggtitle('Offenses Charged by Court (Counts)') +
  theme(
    plot.title = element_text( face="bold", hjust = 0 ),
    legend.title = element_text(),
   legend.position = "none",
    axis.title.x = element_text(vjust=-2 ),
 #   axis.title.y = element_text(vjust= 2),
   axis.text.y = element_text(angle = 90, hjust = 0.5, face = "bold"),    
    axis.ticks.x = element_blank()
  )+ 
  scale_y_continuous(labels = function(x) paste0(round(x)), expand = c(0, 0 ), limits = c(-1, 450)) +
  geom_text(aes(label= paste0(round(count), " ", ChargesCategorized), y = count + 2), position = position_dodge(width = .9), size = 3, hjust = 0 ) +
  labs(x="", y="", fill = "Charge Type",
       title="Charges by Court",
       subtitle="Colorado ACLU Court Watch Project (September - November 2019)",
       caption="Data from the ACLU of Colorado")  +
  coord_cartesian(clip = 'off') +
  scale_fill_manual(values=c(aclublues)) + 
  coord_flip()
  
dev.off()

########### Use of PR Bonds ######
prop.table(table(cw$Bondsetbycourt))*100 # 72.1487603
# Personal Recognince is at 72% 

pdf("prbonds-court.pdf", width=5, height=4)

cw %>%  
  filter(is.na(Bondsetbycourt) == FALSE) %>%
  group_by(WhichCourt) %>%
  add_tally() %>%
  group_by( WhichCourt, n, Bondsetbycourt) %>%
  summarize(count= n()) %>%
  ungroup() %>%
  mutate(percent = count/n*100) %>%
  spread(Bondsetbycourt, percent, fill = 0) %>%
  group_by(WhichCourt, n) %>% 
  summarize( percent = sum(`Personal Recognizance (PR)`)) %>%
  mutate(pr_count = percent * n/100) %>%
  filter(is.na(WhichCourt) == FALSE) %>%
  
  ggplot( aes(x = reorder(WhichCourt, percent), y = percent) )+ 
  geom_bar(stat = "identity", position = "dodge", alpha = .8, color = "black", fill = aclublues[9]) +
  
  # ggtitle('Use of Personal Recognizance (PR) Bonds, %') +
  theme(
    plot.title = element_text( face="bold", hjust = 0 ),
    legend.title = element_text(),
    #  legend.position = 
    axis.title.x = element_text(vjust=-2),
    axis.title.y = element_text(vjust= 2),
    axis.text.x=element_text(vjust=-3),
    axis.ticks.x = element_blank()
    
  )+ 
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), expand = c(0, 0 ), limits = c(-1, 90)) +
  geom_text(aes(label= paste0(round(percent), "%", " (", pr_count, " Cases)"), y = percent+2), position = position_dodge(width = .9), size = 2  ) +
  geom_text(aes(label= paste0(n, " Cases Set Bond"),  y = 0), vjust = 2, size = 2 ) +
  labs(x="Court", y="Percent of Cases",
       title="PR Bonds Issued by Court",
       subtitle="Colorado ACLU Court Watch Project (Sep - Nov 2019)",
       caption="Data from the ACLU of Colorado")  +
  coord_cartesian(clip = 'off')

dev.off()

### Charge Breakdowns abit 
pdf("prbonds-charges", width=8, height=4)

cw %>%
  filter(is.na(Bondsetbycourt) == FALSE) %>%
  group_by(ChargesCategorized) %>%
  add_tally() %>%
  group_by( ChargesCategorized, n, Bondsetbycourt) %>%
  summarize(count= n()) %>%
  ungroup() %>%
  mutate(percent = count/n*100) %>%
  spread(Bondsetbycourt, percent, fill = 0) %>%
  group_by(ChargesCategorized, n) %>% 
  summarize( percent = sum(`Personal Recognizance (PR)`)) %>%
  mutate(pr_count = percent * n/100) %>%
  filter(is.na(ChargesCategorized) == FALSE) %>%
  
ggplot( aes(x = reorder(ChargesCategorized, percent), y = percent) )+ 
  geom_bar(stat = "identity", position = "dodge", alpha = .8, color = "black", fill = aclublues[9]) +
  
  # ggtitle('Use of Personal Recognizance (PR) Bonds, %') +
  theme(
    plot.title = element_text( face="bold", hjust = 0 ),
    legend.title = element_text(),
    #  legend.position = 
    axis.title.x = element_text(vjust=-2),
    axis.title.y = element_text(vjust= 2),
    axis.text.x=element_text(vjust=-3),
    axis.ticks.x = element_blank()
    
  )+ 
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), expand = c(0, 0 ), limits = c(-1, 90)) +
  geom_text(aes(label= paste0(round(percent), "%", " (", pr_count, " Cases)"), y = percent+2), position = position_dodge(width = .9), size = 2  ) +
  geom_text(aes(label= paste0(n, " Cases Set Bond"),  y = 0), vjust = 2, size = 2 ) +
  labs(x="Court", y="Percent of Cases that Set Bond",
       title="PR Bonds Issued by Charge",
       subtitle="Colorado ACLU Court Watch Project (Sep - Nov 2019)",
       caption="Data from the ACLU of Colorado")  +
  coord_cartesian(clip = 'off')

dev.off()


# Defendant Race intersecting with Pr Bond setting
cw %>%
  filter(is.na(Bondsetbycourt) == FALSE) %>%
  group_by(DefRace) %>%
  add_tally() %>%
  group_by( DefRace, n, Bondsetbycourt) %>%
  summarize(count= n()) %>%
  ungroup() %>%
  mutate(percent = count/n*100) %>%
  spread(Bondsetbycourt, percent, fill = 0) %>%
  group_by(DefRace, n) %>% 
  summarize( percent = sum(`Personal Recognizance (PR)`)) %>%
  mutate(pr_count = percent * n/100) %>%
  filter(is.na(DefRace) == FALSE)


# Can race, gender, & charge type predict bond setting
cw$PRBond <- ifelse(cw$Bondsetbycourt == "Personal Recognizance (PR)", 1, 0)
cw$DefRace  <- as.factor(cw$DefRace)

chisq.test(table(cw$PRBond, cw$DefRace)[,c(1,3:4)]) # p = 0.012
  

race_model <- glm(PRBond ~  DefRace , data = cw, family = binomial() )
summary(race_model)

model1 <- glm(PRBond ~  DefRace + DefGender + Assault.Violent.DV + 
                Drug.Related + Poverty.Related, data = cw, family = binomial() )

 
summary(model1)

xtable(summary(margins(model1)))




# Of those who received PR
pdf("prbonds-race-court.pdf", width=5, height=4)

  ggplot( subset(cw, Bondsetbycourt == "Personal Recognizance (PR)" & !is.na(WhichCourt)), aes(x = WhichCourt, fill = as.factor(DefRace)) )+ 
    geom_bar(position="fill") + scale_y_continuous(labels=scales::percent) +
    scale_fill_manual(values=c(aclublues[c(3,5,6,8,9)], "grey")) + 
    labs(title="Race of Those Released on PR Bonds by Court",
         subtitle="Colorado ACLU Court Watch Project (Sep - Nov 2019)",
         y="Percent of Individuals Released on PR",
         x="Court",
         fill="Race of Defendant",
         caption="Data from the ACLU of Colorado")

dev.off()




pdf("prbonds-charge-court.pdf", width=5, height=4)
