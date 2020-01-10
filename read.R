
# load libraries
library(readxl)
library(tidyverse)
library(tools)

# read in data
cw <- read_excel("Court Watching Forms.xlsx", skip=2, col_names = F)

# read in names of columns, two separate rows
names1 <- names(read_excel("Court Watching Forms.xlsx", n_max = 1))
names2 <- names(read_excel("Court Watching Forms.xlsx", skip=1, n_max = 1))

# initial name cleaning
names2[which(str_detect(names2, "Response"))] <- names1[which(str_detect(names2, "Response"))]
names2 <- gsub('[[:digit:]]+', '', names2)
names2 <- gsub('\\...', '', names2)
names2[1:9] <- names1[1:9]
names2 <- gsub(' ', "", names2)
names2 <- str_replace_all(names2, "[^[:alnum:]]", "")
names1 <- gsub('[[:digit:]]+', '', names1)
names1 <- gsub('\\...', '', names1)
names1 <- gsub(' ', "", names1)
names1 <- str_replace_all(names1, "[^[:alnum:]]", "")

# manual recoding for some names
names2[13] <- "WhichCourt.specific"
names2[15] <- "WhatDivisionCourtroom.specific"
names2[20:28] <- paste0(rep(c("DefGender", "DefRace"),c(4, 5)), ".", names2[20:28])
names2[29] <- "DefEstimatedAge"
names2[30] <- "DefVetStatus"
names2[31] <- "DefRepByCounsel"
names2[32] <- "CaseStartTime"
names2[33] <- "CaseEndTime"
names2[34] <- "CaseNumbers"
names2[35] <- "Charges"
names2[36] <- "CaseInvDV"
names2[37] <- "CaseResolved"
names2[38] <- "Result"
names2[39] <- "ResultSpecifics"
names2[38] <- "Result"
names2[40:45] <- paste0(rep("Sentence",6), ".", names2[40:45])
names2[46] <- "SentenceDetail"
names2[49:56] <- paste0(rep("DefReleaseConditions",8), ".", names2[49:56])
names2[57:66] <- paste0(rep("DefBondArg",10), ".", names2[57:66])
names2[48] <- "DefenseBondAmount"
names2[68] <- "ProsectionBondAmount"
names2[86] <- "CourtSetBondAmount"
names2[69:76] <- paste0(rep("ProsReleaseConditions",8), ".", names2[69:76])
names2[77:83] <- paste0(rep("ProsBondArg",7), ".", names2[77:83])
names2[87:94] <- paste0(rep("CourtSetReleaseConditions",8), ".", names2[87:94])
names2[95] <- "BondReturnDate"
names2[86] <- "OtherComments"
names2[86] <- "SuggestionsforTwitter"

# use new clean names for data
names(cw) <- names2

# merge race columns into one, same for gender
cw <- cw %>% mutate(DefRace = coalesce(DefRace.White, DefRace.Black, 
                                       DefRace.Hispanic, DefRace.Asian, 
                                       DefRace.Indigenous))
cw <- cw %>% mutate(DefGender = coalesce(DefGender.Man, DefGender.Woman, 
                                         DefGender.Gendernonconforming, DefGender.Otherpleasespecify))

# drop totally empty columns
cw <- select(cw, -c(EmailAddress, FirstName, LastName, CustomData1))

# recode gender
cw$DefGender <- ifelse(is.na(cw$DefGender), "Unknown", cw$DefGender)
cw$DefGender <- fct_recode(cw$DefGender, 
                           "Unknown"="Defendant was not present in court, so I'm not sure, and there was no notation of gender on the docket.", 
                           "Unknown"="I can't ID gender because Def. was not present.  Counsel waived appearance because def. has flu.", 
                           "Unknown"="Not present in court due to another jail fight", 
                           "Woman"="Trans-gendered MTF")

# recode race NAs
cw$DefRace <- ifelse(is.na(cw$DefRace), "Unknown", cw$DefRace)

# recode character columns to binary indicator
bincols <- names(cw)[c(36:40, 45:51, 53:61, 65:71, 73:78, 83:89)]
cw[bincols] <- lapply(cw[bincols], function (x) {ifelse(!is.na(x), 1, 0)}) 
cw[bincols] <- lapply(cw[bincols], function (x) {as.numeric(x)}) 
cw$DefRepByCounsel <- ifelse(cw$DefRepByCounsel=="Yes", 1, 0)
cw$CaseResolved <- ifelse(cw$CaseResolved=="Yes", 1, 0)

# relevel factors
cw <- mutate(cw, 
             DefGender=fct_relevel(DefGender, "Man"), 
             DefRace=fct_relevel(DefRace, "White"))

# create binary indicators for cash bail requests and sets
cw$CashBailSet <- ifelse(cw$Bondsetbycourt %in% c("Cash or Surety (C/S)", "Cash Only"), 1, 0)
cw$ProsCashBail <- ifelse(cw$ProsecutionBondRequest %in% c("Cash or Surety (C/S)", "Cash Only"), 1, 0)

# ....................... exploratory .............................

library(naniar)
vis_miss(cw)

table(cw$WhichCourt) # this is an important break out

table(cw$DefEstimatedAge,cw$WhichCourt) # plot these
ggplot(cw, aes(DefEstimatedAge)) + geom_bar() + facet_wrap(~WhichCourt, scales="free_y")

table(cw$DefRace,cw$WhichCourt) # plot these
ggplot(cw, aes(DefRace)) + geom_bar() + facet_wrap(~WhichCourt, scales="free_y")
ggplot(cw, aes(DefRace)) + geom_bar() + facet_grid(DefGender~WhichCourt, scales="free_y")


table(cw$DefRepByCounsel) # this is important
ggplot(cw, aes(WhichCourt, fill=DefRepByCounsel)) + geom_bar(position="fill")
ggplot(cw, aes(DefRace, fill=DefRepByCounsel)) + geom_bar(position="fill") + facet_wrap(~WhichCourt, scales="free_y")
ggplot(cw, aes(DefRace, fill=DefRepByCounsel)) + geom_bar(position="dodge") + facet_wrap(~WhichCourt, scales="free_y")

cw$Charges # this needs to be extensively cleaned

table(cw$CourtWatchername) # count these

table(cw$CaseResolved) # mostly not resolved
table(cw$CaseResolved, cw$WhichCourt) # varies significantly by court

table(cw$Result) # if resolved, usually resolved by guilty plea
table(cw$Result, cw$WhichCourt) # similar across courts

table(cw$Doesvictimwantdetention)

table(cw$DefenseBondRequest)
table(cw$ProsecutionBondRequest)
table(cw$Bondsetbycourt)

# ideas: 
# if the prosecution asks for PR, does the judge ever set cash?
# how often does the judge agree with the prosecution? with the defense?

# court set release conditions
apply(cw[,83:89], 2, sum)
cor(cw[,83:89])

# prosecutor requested release conditions
apply(cw[,65:71], 2, sum)

# defense requested release conditions
apply(cw[,45:51], 2, sum)

# plot correlation matrices
library(ggcorrplot)
ggcorrplot(cor(cw[,83:89]))
ggcorrplot(cor(cw[,65:71]))
ggcorrplot(cor(cw[,45:51]))
ggcorrplot(cor(cw[,c(45:51, 65:71, 83:89)]))

# defense bond arguments
apply(cw[,53:61], 2, sum)

# prosecution bond arguments
apply(cw[,73:78], 2, sum)

# messing around with bail requests and sets
mod <- glm(ProsCashBail ~ DefRace + DefGender, family="binomial", data=cw)
summary(mod)

mod <- glm(CashBailSet ~ DefRace + DefGender, family="binomial", data=cw)
summary(mod)

cor(cw$CashBailSet, cw$ProsCashBail)
xtabs(cw$ProsCashBail ~ cw$CashBailSet)


# ideas: 
# calculate average time from start to end? by court?


