
# load libraries
library(readxl)
library(tidyverse)
library(tools)

# setwd("/Volumes/GoogleDrive/My Drive/QJL/ACLU Court Watch")
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
names2[96] <- "OtherComments"
names2[97] <- "SuggestionsforTwitter"

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
             DefGender=fct_relevel(DefGender, "Man"), # Makes Man Base factor for analysis in Gender
             DefRace=fct_relevel(DefRace, "White"))   # White is base factor for analysis in Race

# create binary indicators for cash bail requests and sets
cw$CashBailSet <- ifelse(cw$Bondsetbycourt %in% c("Cash or Surety (C/S)", "Cash Only"), 1, 0)
cw$ProsCashBail <- ifelse(cw$ProsecutionBondRequest %in% c("Cash or Surety (C/S)", "Cash Only"), 1, 0)


# ------------ Back to Cleaning -------------- #
#### Cleaning the Offenses 

cw_charges <- 
cw %>%
  select(RespondentID, Charges, CaseInvDV) %>%
  mutate(
    Charges_clean = toupper(Charges),
    ChargesCategorized = 
      case_when(
        CaseInvDV == "Yes" ~  "Assault/Violent Offense/DV",
        grepl("ASSAULT", Charges_clean, fixed = TRUE ) == TRUE  ~ "Assault/Violent Offense/DV",
        grepl("ASSUALT", Charges_clean, fixed = TRUE ) == TRUE  ~ "Assault/Violent Offense/DV",
        grepl("RAPE", Charges_clean, fixed = TRUE ) == TRUE  ~ "Assault/Violent Offense/DV",
        grepl("DRUG", Charges_clean, fixed = TRUE ) == TRUE ~ "Drug Offense",
        grepl("METH", Charges_clean, fixed = TRUE ) == TRUE ~ "Drug Offense",
        grepl("SUBSTANCE", Charges_clean, fixed = TRUE ) == TRUE ~ "Drug Offense",
        grepl("MARIJUANA", Charges_clean, fixed = TRUE ) == TRUE ~ "Drug Offense",
        grepl("DUI", Charges_clean, fixed = TRUE ) == TRUE ~ "DUI",
        grepl("DRIVING UNDER THE INFLUENCE", Charges_clean, fixed = TRUE ) == TRUE ~ "DUI",
        grepl("ALCOHOL", Charges_clean, fixed = TRUE ) == TRUE ~ "Poverty Related/Petty",
        grepl("PETTY", Charges_clean, fixed = TRUE ) == TRUE  ~ "Poverty Related/Petty",
        grepl("SHOPLIFTING", Charges_clean, fixed = TRUE ) == TRUE  ~ "Poverty Related/Petty",
        grepl("THEFT", Charges_clean, fixed = TRUE ) == TRUE  ~ "Poverty Related/Petty",
        grepl("TRESPASS", Charges_clean, fixed = TRUE ) == TRUE  ~ "Poverty Related/Petty",
        grepl("TRESSPASS", Charges_clean, fixed = TRUE ) == TRUE  ~ "Poverty Related/Petty",
        grepl("MISCHIEF", Charges_clean, fixed = TRUE ) == TRUE  ~ "Poverty Related/Petty",
        grepl("DISORDERLY CONDUCT", Charges_clean, fixed = TRUE ) == TRUE  ~ "Poverty Related/Petty",
        grepl("DISTURB", Charges_clean, fixed = TRUE ) == TRUE  ~ "Poverty Related/Petty",
        grepl("PUBLIC", Charges_clean, fixed = TRUE ) == TRUE  ~ "Poverty Related/Petty",
        grepl("PARK VIOLATION", Charges_clean, fixed = TRUE ) == TRUE  ~ "Poverty Related/Petty",
        grepl("FALSE", Charges_clean, fixed = TRUE ) == TRUE  ~ "Poverty Related/Petty",
        grepl("PANHANDLING", Charges_clean, fixed = TRUE ) == TRUE  ~ "Poverty Related/Petty",
        grepl("DESTRUCTION OF", Charges_clean, fixed = TRUE ) == TRUE  ~ "Poverty Related/Petty",
        grepl("CURFEW", Charges_clean, fixed = TRUE ) == TRUE  ~ "Poverty Related/Petty",
        grepl("DUR", Charges_clean, fixed = TRUE ) == TRUE  ~ "Violation of Court Restrictions",
        grepl("LICENSE", Charges_clean, fixed = TRUE ) == TRUE  ~ "Violation of Court Restrictions",
        grepl("LICENCE", Charges_clean, fixed = TRUE ) == TRUE  ~ "Violation of Court Restrictions",
        grepl("VIOLATION OF", Charges_clean, fixed = TRUE ) == TRUE  ~ "Violation of Court Restrictions",
        grepl("VIOLATION OF", Charges_clean, fixed = TRUE ) == TRUE  ~ "Violation of Court Restrictions",
        grepl("PROBATION", Charges_clean, fixed = TRUE ) == TRUE  ~ "Violation of Court Restrictions",
        grepl("COURT ORDER", Charges_clean, fixed = TRUE ) == TRUE  ~ "Violation of Court Restrictions",
        grepl("DRIVING UNDER RESTRAINT", Charges_clean, fixed = TRUE ) == TRUE  ~ "Violation of Court Restrictions",
        grepl("CARELESS", Charges_clean, fixed = TRUE ) == TRUE  ~ "Driving Violations",
        grepl("SPEEDING", Charges_clean, fixed = TRUE ) == TRUE  ~ "Driving Violations",
        grepl("INSURANCE", Charges_clean, fixed = TRUE ) == TRUE  ~ "Driving Violations",
        grepl("NO BRAKES", Charges_clean, fixed = TRUE ) == TRUE  ~ "Driving Violations",
        grepl("PROSTITUTION", Charges_clean, fixed = TRUE ) == TRUE  ~ "Other",
        grepl("PROSITUTION", Charges_clean, fixed = TRUE ) == TRUE  ~ "Other",
        grepl("INDECENT EXPOSURE", Charges_clean, fixed = TRUE ) == TRUE  ~ "Other",
        grepl("THREATS", Charges_clean, fixed = TRUE ) == TRUE  ~ "Other",
        grepl("SEX OFFENDER", Charges_clean, fixed = TRUE ) == TRUE  ~ "Other",
        grepl("UNLAWFUL", Charges_clean, fixed = TRUE ) == TRUE  ~ "Other",
        grepl("WEAPON", Charges_clean, fixed = TRUE ) == TRUE  ~ "Other",
        grepl("KNIVES", Charges_clean, fixed = TRUE ) == TRUE  ~ "Other",
        grepl("FIREARM", Charges_clean, fixed = TRUE ) == TRUE  ~ "Other",
        is.na(Charges_clean == TRUE) ~ "Unknown",
        grepl("T READ", Charges_clean, fixed = TRUE ) == TRUE  ~ "Unknown",
        grepl("WAIVED", Charges_clean, fixed = TRUE ) == TRUE  ~ "Unknown",
        grepl("UNKNOWN", Charges_clean, fixed = TRUE ) == TRUE  ~ "Unknown",
        grepl("NOT GIVEN", Charges_clean, fixed = TRUE ) == TRUE  ~ "Unknown",
        grepl("NOT STATED", Charges_clean, fixed = TRUE ) == TRUE  ~ "Unknown",
        grepl("NO SHOW", Charges_clean, fixed = TRUE ) == TRUE  ~ "Unknown",
        
      ),
    ChargesCategorized = ifelse(is.na(ChargesCategorized) == TRUE, "Other", ChargesCategorized),
    
    Assault.Violent.DV = 
      ifelse(
        CaseInvDV == "Yes", 1,
        ifelse(
          grepl("ASSAULT", Charges_clean, fixed = TRUE ) == TRUE, 1,
        ifelse(
          grepl("BATTERY", Charges_clean, fixed = TRUE ) == TRUE, 1,
        ifelse(
          grepl("RAPE", Charges_clean, fixed = TRUE ) == TRUE,1, 0
                  )))),
    Drug.Related = 
       ifelse(grepl("DRUG", Charges_clean, fixed = TRUE ) == TRUE, 1,
              ifelse(
                   grepl("METH", Charges_clean, fixed = TRUE ) == TRUE, 1,
               ifelse(
                   grepl("SUBSTANCE", Charges_clean, fixed = TRUE ) == TRUE, 1, 
               ifelse(
                   grepl("MARIJUANA", Charges_clean, fixed = TRUE ) == TRUE, 1 , 0))))
    ) 
  
# The following Charges still need to be categorized 
cw_charges %>%
  filter(is.na(ChargesCategorized) == TRUE) %>%
  group_by(Charges_clean) %>%
  summarize(n = n()) %>% View()


############
cw <-
cw %>%
  inner_join(
    cw_charges %>%
      select(RespondentID, ChargesCategorized, Assault.Violent.DV, Drug.Related), by = "RespondentID"
    
  )

#### Final Cleaning ####
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

  
save.image("ACLUCourtWatch.RData")
write.csv(cw, file = "courtwatch_clean.csv")

