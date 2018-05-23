## This file analyzes a summary data set "Field_data_plots_2016.csv" ##

# This data set includes data collected on entire subplots rather than survival
# of individual plants                                         #

cheatgrass <- read.csv('Field_data_plots_2016.csv')

################################################
# Model fecundity per survivor using an offset #
################################################

cheatgrass$log.survivors <- log(cheatgrass$Surviving.plants)

model.individual.fecundity <- glmer(Total.seeds.per.subplot ~ Temp.treatment*Fungal.treatment + 
                                     (1|Plot.ID), data = cheatgrass, 
                                   offset=log.survivors,family = "poisson")

plot(cheatgrass$Total.seeds.per.subplot,predict(model.individual.fecundity,type="response"),
     xlab="Observed",ylab="Predicted")
abline(0,1)

######################################################################
# Model fecundity as a continuous rate using log normal distribution #
######################################################################

cheatgrass$logSps <- log(cheatgrass$Seeds.per.survivor)
boxplot(logSps ~ Temp.treatment + Fungal.treatment, data=cheatgrass, xlab="Treatment",
        ylab="logSps", cex.names=0.1)

# May not meet assumption of normality - heavy skew on Heat treatments
model.logSps <- lmer(logSps ~ Temp.treatment*Fungal.treatment + 
                       (1|Plot.ID), data = cheatgrass)
summary(model.logSps)

plot(exp(cheatgrass$logSps),exp(predict(model.logSps)),xlab="Observed",ylab="Predicted")
abline(0, 1)

# Offset model fits better (according to predicted vs. actual) #
# Accounts for poisson distribution #

#######################################################################################
# Assign reduced models and use anova/drop1 to compare them#
#######################################################################################

# Drop interaction and compare #
model.individual.fecundity.no.in <- glmer(Total.seeds.per.subplot ~ Temp.treatment+Fungal.treatment + 
                                      (1|Plot.ID), data = cheatgrass, 
                                    offset=log.survivors,family = "poisson")

anova(model.individual.fecundity, model.individual.fecundity.no.in) # interaction is significant

#Post-hoc comparisons of fecundity treatment:
fecunditycomp <- emmeans(model.individual.fecundity, ~ Temp.treatment * Fungal.treatment)
summary(fecunditycomp, type = "Total.seeds.per.subplot")
summary(pairs(fecunditycomp), type = "Total.seeds.per.subplot")

############################################################################
# Model head smut (Ustilago bullata) infection as a function of treatments #
############################################################################

model.headsmut <- glmer(Spikelets.with.headsmut ~ Temp.treatment*Fungal.treatment + 
                           (1|Plot.ID), data = cheatgrass, family = "poisson",
                        offset = log.survivors)
summary(model.headsmut)
# This indicates that there are significant interactions

# Reduced model without interaction #
model.headsmut.no.in <- glmer(Spikelets.with.headsmut ~ Temp.treatment+Fungal.treatment + 
                          (1|Plot.ID), data = cheatgrass, family = "poisson",
                        offset = log.survivors)
summary(model.headsmut.no.in)

# Compare full and reduced model #
anova(model.headsmut, model.headsmut.no.in)
# Including interaction significantly improves model predictions #

smutcomparison <- emmeans(model.headsmut, ~ Temp.treatment * Fungal.treatment)
summary(smutcomparison, type = "Spikelets.with.headsmut")
summary(pairs(smutcomparison), type = "Spikelets.with.headsmut")
cld(smutcomparison,Letters=letters)

##################################################################
# Model biomass per cheatgrass plant as a function of treatments #
##################################################################

model.cheatgrass.biomass <- lmer(Biomass.per.survivor.g ~ Temp.treatment*Fungal.treatment + 
                                    (1|Plot.ID), data = cheatgrass)
summary(model.cheatgrass.biomass)

# Reduced model, compare both
model.cheatgrass.biomass.no.in <-lmer(Biomass.per.survivor.g ~ Temp.treatment+Fungal.treatment + 
                                        (1|Plot.ID), data = cheatgrass)

anova(model.cheatgrass.biomass.no.in, model.cheatgrass.biomass)
# Interaction is just barely not significant - p=0.08049 #


###########################################################
# Model total subplot biomass as a function of treatments #
###########################################################

model.total.biomass <- lmer(Total.biomass.g.subplot ~ Temp.treatment*Fungal.treatment + 
                               (1|Plot.ID), data = cheatgrass)
summary(model.total.biomass)

# Reduced model
model.total.biomass.no.in <-lmer(Total.biomass.g.subplot ~ Temp.treatment+Fungal.treatment + 
                                   (1|Plot.ID), data = cheatgrass)
# Compare
anova(model.total.biomass.no.in, model.total.biomass)
# Interaction not significant p=0.3543

# Remove main effects
model.total.biomass.no.fung <-lmer(Total.biomass.g.subplot ~ Temp.treatment + 
                                    (1|Plot.ID), data = cheatgrass)
model.total.biomass.no.heat <-lmer(Total.biomass.g.subplot ~ Fungal.treatment + 
                                    (1|Plot.ID), data = cheatgrass)
anova(model.total.biomass.no.in, model.total.biomass.no.heat)
anova(model.total.biomass.no.in, model.total.biomass.no.fung)

# None of the fixed effects are significant
# Total subplot biomass (cheatgrass and other) is not dependent on either treatment


###################################################
# Analyze lambda on the log scale 
###################################################

# log transform lambda
cheatgrass$logLambda <- log(cheatgrass$Lambda.estimate)

# Full Model of log.lambda
model.loglambda <- lmer(logLambda ~ Temp.treatment*Fungal.treatment +
                       (1|Plot.ID), data=cheatgrass, REML=FALSE)
summary(model.loglambda)

# No interaction model of log.lambda
model.noint.loglambda <- lmer(logLambda ~ Temp.treatment + Fungal.treatment +
                          (1|Plot.ID), data=cheatgrass, REML=FALSE)
summary(model.noint.loglambda)
anova(model.loglambda, model.noint.loglambda) # interaction not significant

# Are the main effects significant?
drop1(model.noint.loglambda,test="Chisq")
# Yes, both: Temp.treatment LRT=7.2290, p=0.007174; Fungal.treatment LRT=7.7725,

# Which treatment pairs are different?
lambdacomp <- emmeans(model.noint.loglambda, ~ Temp.treatment + Fungal.treatment, 
                      pairwise~Temp.treatment*Fungal.treatment, adjust="tukey")
summary(lambdacomp)
summary(lambdacomp, type = "logLambda")
summary(pairs(lambdacomp), type = "logLambda")

# stats tables
htmlreg(list(model.individual.fecundity,model.loglambda,model.noint.loglambda),
        "vital_rate_stats_plots.doc",
        custom.model.names = c("Fecundity","log Lambda (full)","log Lambda (reduced)"),
        custom.coef.names = c("(Intercept)","Snow melt","Fungicide","Snow mold","Snow melt x Fungicide","Snow melt x Snow mold"),
        caption="Table 3. Field experiment vital rates-plots",single.row=T,caption.above = T)

# add supplementary table for biomass and headsmut
htmlreg(list(model.cheatgrass.biomass,model.cheatgrass.biomass.no.in,model.headsmut),
        "biomass_headsmut_stats.doc",digits=4,
        custom.model.names = c("Biomass (full)","Biomass (reduced)","Head smut"),
        custom.coef.names = c("(Intercept)","Snow melt","Fungicide","Snow mold","Snow melt x Fungicide","Snow melt x Snow mold"),
        caption="Table S2. Biomass and head smut",single.row=T,caption.above = T)

#################################################################################
# Linear regression of subplot seed production as a function of subplot biomass #
#################################################################################
seedproduction.lm <- lm(Total.seeds.per.subplot~Cheatgrass.biomass.g.subplot, data=cheatgrass)
summary(seedproduction.lm)


