
# Charlotte McClintock
# ACLU Court Watch
# Analysis Script

# ..................................................................................

library(tidyverse)
library(margins)

load("ACLUCourtWatch.Rdata")

# ..................................................................................

# (4) Counsel

# What percent of people are represented by counsel at bond setting?

# if all of these are bond setting cases, 85% 
prop.table(table(cw$DefRepByCounsel))

# What percent of people who plead at bond setting are represented by counsel?

prop.table(table(subset(cw, Result="Guilty plea")$DefRepByCounsel)) # 84%

# Are there apparent differences in bond setting practices (money bond/PR bond/amount 
# of bond) dependent on presence of counsel?

prop.table(table(cw$JudgeSet, cw$DefRepByCounsel), margin = 2)
prop.table(table(cw$ProsAsk, cw$DefRepByCounsel), margin = 2)


# ..................................................................................

# (6) What percent of the time does the judge follow the prosecutor's bond request?

cw$JudgeSet <- ifelse(cw$Bondsetbycourt %in% 
                        c("Cash or Surety (C/S)", "Cash Only"), 
                      "Judge Set Cash Bail", "Judge Set PR")
cw$ProsAsk <- ifelse(cw$ProsecutionBondRequest %in% 
                       c("Cash or Surety (C/S)", "Cash Only"), 
                     "Prosecution Sought Cash Bail", "Prosecution Sought PR")

table(cw$JudgeSet, cw$ProsAsk)
100*prop.table(table(cw$JudgeSet, cw$ProsAsk), margin = 2)

# ..................................................................................

# (8) What percent of defendants are experiencing homelessness 
#and how does that play out in all the above analyses?
str_detect(string=names(cw), "homeless")
# only measure of homelessness is in defense bond arguments
sum(cw$DefBondArg.Defendantishomeless) # 42 individuals identified as homeless

ggplot(cw, aes(WhichCourt)) + geom_bar() + 
  facet_wrap(~DefBondArg.Defendantishomeless, scales="free") 

# ..................................................................................

# defense bond arguments
apply(cw[,53:61], 2, sum)

# prosecution bond arguments
apply(cw[,73:78], 2, sum)

# messing around with bail requests and sets
prosask.mod <- glm(ProsCashBail ~ DefRace + DefGender + DefBondArg.Defendantishomeless, family="binomial", data=cw)
summary(prosask.mod)
margins(prosask.mod)

bondset.mod <- glm(CashBailSet ~ DefRace + DefGender + DefBondArg.Defendantishomeless + 
                     DefRepByCounsel, family="binomial", data=cw)
summary(bondset.mod)

