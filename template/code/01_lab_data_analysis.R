# lab_data_analysis.R: 
#  This script analyzes survival rates of seedlings from Nikki Pendleton's 
#  cone experiment from Summer 2014. This script is sourced from the master
#  script and assumes you are in the appropriate working directory.
#
# Authors:
#  Peter Adler (pbadler)
#  Andrew Tredennick (atredennick)


# Load and format data ----------------------------------------------------

conedata <- read.csv("../data/Lab_Data.csv")  # final collection data (9.8.2014)

# Check to make sure there are only two levels in Alive.Dead
expect_two <- function(myfactors){
  if(length(levels(myfactors)) > 2){
    stop("You have more than 2 levels for Alive.Dead!")
  }
  if(length(levels(myfactors)) < 2){
    stop("You have less than 2 levels for Alive.Dead!")
  }
}
expect_two(conedata$Alive.Dead)

# Create new column for survival
conedata <- conedata %>%
  mutate(survived = ifelse(Alive.Dead == "Alive", 1, 0))

# Remove missing data, after checking that there aren't too many instances
num_nas <- length(which(is.na(conedata$Alive.Dead)))
if(num_nas > round(0.1*nrow(conedata))) {
  warning("You are removing more than 10% of your data due to NAs. Check that.")
}

conedata <- conedata %>%
  filter(!is.na(Alive.Dead) == TRUE)



# Fit survival model ------------------------------------------------------

m1 <- glm(survived ~ Treatment, conedata, family = 'binomial')

if(VERBOSE){
  summary(m1)
  cat(paste("\nMean survival rate of control = ", 
            round(inv.logit(coef(m1)[1]),2)))
  cat(paste("\nMean survival rate of snow model infected plants = ", 
            round(inv.logit(coef(m1)[1]+coef(m1)[2]),2)),"\n")
}
  


# Save results to table ---------------------------------------------------

htmlreg(list(m1),"../tables/lab_exp_stats.doc",
        caption = "Table 1. Lab experiment seedling survival",
        single.row = TRUE,
        caption.above = TRUE)
