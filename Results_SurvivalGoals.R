library(dplyr)
library(survival)
library(tidyr)
library(ggplot2)
library(survminer)
library(npcure)
library(mixcure)

dadoscomp = read.csv("matches_home.csv")
dadoscomp2 = read.csv("matches_away.csv")
dadosmult = read.csv("matches_recurring.csv")

# Argentina - Kaplan-Meier estimate
argcomp = dadoscomp[dadoscomp$league=="Argentina",]
argcomp2 = dadoscomp2[dadoscomp2$league=="Argentina",]

argcomp$group = "Mandante"
argcomp2$group = "Visitante"
argcomp = rbind(argcomp, argcomp2)

argcompsurv = Surv(time = argcomp$time, event = argcomp$cens)
argcompekm = survfit(argcompsurv ~ argcomp$group)

ggsurvplot(argcompekm, data = argcomp, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Time", legend.labs = c("Home", "Away"),
           ylab = "Survival Probability", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

# Brazil - Kaplan-Meier estimate
bracomp = dadoscomp[dadoscomp$league=="Brasil",]
bracomp2 = dadoscomp2[dadoscomp2$league=="Brasil",]

bracomp$group = "Mandante"
bracomp2$group = "Visitante"
bracomp = rbind(bracomp, bracomp2)

bracompsurv = Surv(time = bracomp$time, event = bracomp$cens)
bracompekm = survfit(bracompsurv ~ bracomp$group)

ggsurvplot(bracompekm, data = bracomp, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Time", legend.labs = c("Home", "Away"),
           ylab = "Survival Probability", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

# Chile - Kaplan-Meier estimate
chicomp = dadoscomp[dadoscomp$league=="Chile",]
chicomp2 = dadoscomp2[dadoscomp2$league=="Chile",]

chicomp$group = "Mandante"
chicomp2$group = "Visitante"
chicomp = rbind(chicomp, chicomp2)

chicompsurv = Surv(time = chicomp$time, event = chicomp$cens)
chicompekm = survfit(chicompsurv ~ chicomp$group)

ggsurvplot(chicompekm, data = chicomp, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Time", legend.labs = c("Home", "Away"),
           ylab = "Survival Probability", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

# Colombia - Kaplan-Meier estimate
colcomp = dadoscomp[dadoscomp$league=="Colombia",]
colcomp2 = dadoscomp2[dadoscomp2$league=="Colombia",]

colcomp$group = "Mandante"
colcomp2$group = "Visitante"
colcomp = rbind(colcomp, colcomp2)

colcompsurv = Surv(time = colcomp$time, event = colcomp$cens)
colcompekm = survfit(colcompsurv ~ colcomp$group)

ggsurvplot(colcompekm, data = colcomp, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Time", legend.labs = c("Home", "Away"),
           ylab = "Survival Probability", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

# Paraguay - Kaplan-Meier estimate
parcomp = dadoscomp[dadoscomp$league=="Paraguai",]
parcomp2 = dadoscomp2[dadoscomp2$league=="Paraguai",]

parcomp$group = "Mandante"
parcomp2$group = "Visitante"
parcomp = rbind(parcomp, parcomp2)

parcompsurv = Surv(time = parcomp$time, event = parcomp$cens)
parcompekm = survfit(parcompsurv ~ parcomp$group)

ggsurvplot(parcompekm, data = parcomp, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Time", legend.labs = c("Home", "Away"),
           ylab = "Survival Probability", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

# Uruguay - Kaplan-Meier estimate
urucomp = dadoscomp[dadoscomp$league=="Uruguai",]
urucomp2 = dadoscomp2[dadoscomp2$league=="Uruguai",]

urucomp$group = "Mandante"
urucomp2$group = "Visitante"
urucomp = rbind(urucomp, urucomp2)

urucompsurv = Surv(time = urucomp$time, event = urucomp$cens)
urucompekm = survfit(urucompsurv ~ urucomp$group)

ggsurvplot(urucompekm, data = urucomp, pval = T, pval.method = T, legend.title = element_blank(),palette = "jco",
           ggtheme = theme_bw(), xlab = "Time", legend.labs = c("Home", "Away"),
           ylab = "Survival Probability", font.tickslab = c(16),
           font.x = c(16), font.legend = c(16),
           font.y = c(16), break.time.by = 15)

#First goal of the home team - Kaplan-Meier estimate

#Leagues
dadoscompsurv = Surv(time = dadoscomp$time, event = dadoscomp$cens)

ekmcomp = survfit(dadoscompsurv ~ dadoscomp$league)

ggsurvplot(ekmcomp, data = dadoscomp, legend.title = "", pval = T, pval.method = T,
           ggtheme = theme_bw(), ylab = "Survival Probability", xlab = "Time", 
           legend.labs = c("Argentina", "Brazil", "Chile", "Colombia", "Paraguay", "Uruguay"),
           font.x = c(16), font.legend = c(16), font.tickslab = c(16), 
           font.y = c(16), break.time.by = 15)

#Covid
ekmcompcovid = survfit(dadoscompsurv ~ dadoscomp$covid)

ggsurvplot(ekmcompcovid, data = dadoscomp, legend.title = "", pval = T, pval.method = T,
           ggtheme = theme_bw(), ylab = "Survival Probability", xlab = "Time", 
           legend.labs = c("Before", "After"),
           font.x = c(16), font.legend = c(16), font.tickslab = c(16), 
           font.y = c(16), break.time.by = 15)

#Quartiles
quantile(ekmcomp)

#First goal of the away team - Kaplan-Meier estimate

#Leagues
dadoscomp2surv = Surv(time = dadoscomp2$time, event = dadoscomp2$cens)

ekmcomp2 = survfit(dadoscomp2surv ~ dadoscomp2$league)

ggsurvplot(ekmcomp2, data = dadoscomp2, legend.title = "", pval = T, pval.method = T,
           ggtheme = theme_bw(), ylab = "Survival Probability", xlab = "Time", 
           legend.labs = c("Argentina", "Brazil", "Chile", "Colombia", "Paraguay", "Uruguay"),
           font.x = c(16), font.legend = c(16), font.tickslab = c(16), 
           font.y = c(16), break.time.by = 15)

#Covid
ekmcomp2covid = survfit(dadoscomp2surv ~ dadoscomp2$covid)

ggsurvplot(ekmcomp2covid, data = dadoscomp2, legend.title = "", pval = T, pval.method = T,
           ggtheme = theme_bw(), ylab = "Survival Probability", xlab = "Time", 
           legend.labs = c("Before", "After"),
           font.x = c(16), font.legend = c(16), font.tickslab = c(16), 
           font.y = c(16), break.time.by = 15)

#Multiple comparison test
pairwise_survdiff(Surv(time, cens) ~ league ,p.adjust.method = "bonferroni", rho=0,  
                  data = dadoscomp2)

#Quartiles
quantile(ekmcomp2)

#Cox Model for the home team scoring first

#Fitting the model
cox_regalt = coxph(Surv(time, cens) ~ league + covid, data = dadoscomp)
summary(cox_regalt)

#Schoenfeld residuals
testph = cox.zph(cox_regalt)
ggcoxzph(testph)


#Cox Model for the away team scoring first

#Fitting the model
cox_regalt2 = coxph(Surv(time, cens) ~ league + covid, data = dadoscomp2)
summary(cox_regalt2)

#Schoenfeld residuals
testph2 = cox.zph(cox_regalt2)
ggcoxzph(testph2)

#Cure Fraction Model for the home team scoring first

#Maller-Zhou test
testmz(t = time, d = cens, dataset = dadoscomp)

#Cure Probabilities

#Leagues
probcure = c(min(ekmcomp[1]$surv), min(ekmcomp[2]$surv), min(ekmcomp[3]$surv), min(ekmcomp[4]$surv), min(ekmcomp[5]$surv), min(ekmcomp[6]$surv))

#Covid
probcurecovid = c(min(ekmcompcovid[1]$surv), min(ekmcompcovid[2]$surv))

#Fitting the model
mixcure(Surv(time, cens) ~ league + covid, ~ league + covid, data = dadoscomp)

#Cure Fraction Model for the away team scoring first

#Maller-Zhou test
testmz(t = time, d = cens, dataset = dadoscomp2)

#Cure Probabilities

#Leagues
probcure2 = c(min(ekmcomp2[1]$surv), min(ekmcomp2[2]$surv), min(ekmcomp2[3]$surv), min(ekmcomp2[4]$surv), min(ekmcomp2[5]$surv), min(ekmcomp2[6]$surv))
#Covid
probcurecovid2 = c(min(ekmcomp2covid[1]$surv), min(ekmcomp2covid[2]$surv))

#Fitting the model
mixcure(Surv(time, cens) ~ league + covid, ~ league + covid, data = dadoscomp2)

#AG Model

#Fitting the model
agfitalt = coxph(Surv(start, stop, cens) ~ league + covid + cluster(game), data = dadosmult)
summary(agfitalt)

#Schoenfeld residuals
agtestph = cox.zph(agfitalt)
ggcoxzph(agtestph)

#PWP Model

#Fitting the model
pwpfitalt = coxph(Surv(start, stop, cens) ~ league + covid + cluster(game) + strata(stratum), data = dadosmult)
summary(pwpfitalt)

#Schoenfeld residuals
pwptestph = cox.zph(pwpfitalt)
ggcoxzph(pwptestph)





