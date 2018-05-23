## This file analyzes germination and survival of individual plants ##
# #within subplots from the dataset "Proofed.data.cheatgrass.2016"  ##

indCG <- read.csv('Field_data_individuals_2016.csv',header=T)

# get indices for each treatment
# Fungicide <- indCG[which(indCG$Fungal.treatment=="Fungicide"),]
# Snow.mold <- indCG[which(indCG$Fungal.treatment=="Snow Mold"),]
# Control.fung <- indCG[which(indCG$Fungal.treatment=="Control"),]
# Snow.melt <- indCG[which(indCG$Temp.treatment=="Heat"),]
# Control.melt <- indCG[which(indCG$Temp.treatment=="Control"),]
# 
# ##########################################
#     Plant emergence rates              #
##########################################

# Overall emergence #
indCG$sum <- indCG$X3.09.emergence + indCG$X3.09.status + indCG$X3.22.status + indCG$X4.05.status + indCG$X4.19.status +
  indCG$X5.03.status +indCG$X7.05.status
indCG$emergence <- ifelse(indCG$sum > 0,1,0)

# Find spring emergence rates #
indCG$spring <- indCG$emergence - indCG$X3.09.emergence
indCG$spring.emerg <- ifelse(indCG$spring == 1,1,0)

###############################
# Model overall emergence rate#
###############################

# Build full model with all terms
model.emergence <- glmer(emergence ~ Temp.treatment*Fungal.treatment +
                         (1|Plot.ID/Subplot.ID), data = indCG,
                         family = "binomial")
summary(model.emergence)

# Remove interaction and use LL ratio test to compare
model.emergence.no.in <-glmer(emergence ~ Temp.treatment+Fungal.treatment +
                              (1|Plot.ID/Subplot.ID), data = indCG,
                              family = "binomial")
anova(model.emergence.no.in, model.emergence) # Not significant - chi-square=0.5173

# Remove heat as factor and use LL #
model.emergence.no.heat <-glmer(emergence ~ Fungal.treatment +
                                (1|Plot.ID/Subplot.ID), data = indCG,
                                family = "binomial")
anova(model.emergence.no.heat, model.emergence.no.in) # Not significant - chi-square=0.7175

# Remove fungicide and test for LL
model.emergence.no.fung <-glmer(emergence ~ Temp.treatment +
                                (1|Plot.ID/Subplot.ID), data = indCG,
                                family = "binomial")
anova(model.emergence.no.fung, model.emergence.no.in) # Not significant - chi-square=0.6329
   

# Model mean emergence rate, no treatment effects #
model.emergence.mean <- glmer(emergence ~ 1 + (1|Plot.ID/Subplot.ID), data = indCG,
                              family = "binomial")
summary(model.emergence.mean)
inv.logit(fixef(model.emergence.mean)[1])

###################################
# Model emergence rates by season #
###################################
model.fall.winter.emergence <- glmer(X3.09.emergence ~ Temp.treatment*Fungal.treatment +
                                    (1|Plot.ID/Subplot.ID), data = indCG,
                                    family = "binomial")
summary(model.fall.winter.emergence) # Treatment effects not significant

# Remove interaction and fixed effects, compare model performance
model.fall.winter.emergence.no.in <- glmer(X3.09.emergence ~ Temp.treatment+Fungal.treatment +
                                     (1|Plot.ID/Subplot.ID), data = indCG,
                                     family = "binomial")
model.fall.winter.emergence.no.heat <- glmer(X3.09.emergence ~ Fungal.treatment +
                                       (1|Plot.ID/Subplot.ID), data = indCG,
                                       family = "binomial")
model.fall.winter.emergence.no.fung <- glmer(X3.09.emergence ~ Temp.treatment +
                                       (1|Plot.ID/Subplot.ID), data = indCG,
                                       family = "binomial")
anova(model.fall.winter.emergence, model.fall.winter.emergence.no.in)
anova(model.fall.winter.emergence.no.in, model.fall.winter.emergence.no.heat)
anova(model.fall.winter.emergence.no.in, model.fall.winter.emergence.no.fung)
    # No significant differences in removing fixed effects - model mean

# Model mean fall/winter emergence
model.fall.winter.emergence.mean <- glmer(X3.09.emergence ~ 1 + 
                                            (1|Plot.ID/Subplot.ID), data = indCG,
                                     family = "binomial")
summary(model.fall.winter.emergence.mean)

#########################################
# Survival rates of plants that emerged #
#########################################

# Create subset that only includes plants that emerged #
CGsurvival <- indCG[ which(indCG$emergence==1), ]

model.overall.survival <- glmer(X7.05.status ~ Temp.treatment*Fungal.treatment +
                                (1|Plot.ID/Subplot.ID), 
                                data = CGsurvival, family = "binomial")
summary(model.overall.survival) # Temperature treatment significant

# Try without interaction
model.overall.survival.no.interaction <-glmer(X7.05.status ~ Temp.treatment + Fungal.treatment +
                                             (1|Plot.ID/Subplot.ID), 
                                             data = CGsurvival, family = "binomial")
summary(model.overall.survival.no.interaction) # now fungicide effect also significant
anova(model.overall.survival, model.overall.survival.no.interaction) # LR test says drop interaction

# Test individual main effects #
drop1(model.overall.survival.no.interaction, test = "Chisq")
# Both effects are significant, fungal treatment barely so (0.04897)

# Do post-hoc treatment comparisons
survivalcomp <- emmeans(model.overall.survival.no.interaction, ~ Temp.treatment + Fungal.treatment, 
                      pairwise~Temp.treatment*Fungal.treatment, adjust="tukey")
summary(survivalcomp)
summary(survivalcomp, type = "X7.05.status")
summary(pairs(survivalcomp), type = "X7.05.status")

# Fraction of plant that alive in March that die by July?
sum(CGsurvival$X7.05.status)/sum(CGsurvival$X3.09.status)

# stats tables
htmlreg(list(model.emergence,model.overall.survival,model.overall.survival.no.interaction),
        "vital_rate_stats_individs.doc",
        custom.model.names = c("Emergence","Survival (full)","Survival (reduced)"),
        custom.coef.names = c("(Intercept)","Snow melt","Fungicide","Snow mold","Snow melt x Fungicide","Snow melt x Snow mold"),
        caption="Table 2. Field experiment vital rates",single.row=T,caption.above = T)

