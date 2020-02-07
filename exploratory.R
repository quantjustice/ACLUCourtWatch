
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

ggplot(cw, aes(DefRace, fill=DefRepByCounsel)) + geom_bar(position="dodge") + facet_wrap(~WhichCourt, scales="free_y")


cw %>%  
  group_by( WhichCourt, DefRace) %>%
  add_tally() %>%
  group_by( WhichCourt, DefRace, n, DefRepByCounsel) %>%
  summarize(count= n()) %>%
  ungroup() %>%
  mutate(percent = count/n*100) %>%
  spread(DefRepByCounsel, percent, fill = 0) %>%
  select(-`<NA>`) %>%
  group_by(WhichCourt, DefRace, n) %>%
  summarise(percent = sum(`1`)) %>%
  filter(is.na(WhichCourt) == FALSE) %>%
  
  ggplot( aes(x = reorder(WhichCourt, percent), y = percent, fill = reorder(DefRace, percent))) + 
  geom_bar(stat = "identity", position = "dodge", alpha = .8, color = "black") +
  
  ggtitle('Percent of Defendents Receiving Representation by Race & Court') +
  theme(
    plot.title = element_text( face="bold", hjust = 0.5 ),
    legend.title = element_text(),
    axis.title.x = element_text(vjust=-2),
    axis.title.y = element_text(vjust= 2),
    axis.text.x=element_text(vjust=-3),
    axis.ticks.x = element_blank()
    
  )+ 
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), expand = c(0, 0 ), limits = c(-1, 110)) +
  geom_text(aes(label= paste0(round(percent), "%"), y = percent+1), position = position_dodge(width = .9), size = 2  ) +
  geom_text(aes(label= paste0( "n",n), y = 0), position = position_dodge(width = .9), vjust = 3, size = 2 ) +
  labs(x="Court", y="Defendants Represented, %", fill = "Race")  +
  coord_cartesian(clip = 'off')+
  scale_fill_brewer(palette="Set1")




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


DefBondArg.Defendantishomeless

modbailset <- as.data.frame( summary(mod)$coef)
modbailset$variable <- row.names(mod1)
modbailset1 <-
  modbailset %>%
  mutate(OR = round(exp(Estimate),2),
         lower = round(exp(Estimate - (1.96*`Std. Error`)),2),
         upper = round(exp(Estimate + (1.96*`Std. Error`)),2),
         measure = paste0(OR, " (",lower,"-",upper,")"),
         p_value =round(`Pr(>|z|)`,3),
         p_value = ifelse(p_value == 0, "<0.001", p_value)) %>%
  select(variable,  measure, p_value )

modbailset1

cor(cw$CashBailSet, cw$ProsCashBail)
xtabs(cw$ProsCashBail ~ cw$CashBailSet)


# ideas: 
# calculate average time from start to end? by court?
