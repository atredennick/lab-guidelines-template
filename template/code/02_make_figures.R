# 02_make_figures.R: 
#  This script makes the figures for the manuscript. It assumes that previous 
#  scripts in '00_analysis_master.R' have been run and that all data and 
#  objects are present.
#
# Author:
#  Peter Adler (pbadler)
#  Andrew Tredennick (atredennick)



# Plot lab experiment results ---------------------------------------------

# Calculate total number of seedlings that survived in each treatment
seedling_totals <- conedata %>%
  group_by(Treatment) %>%
  summarise(total_survivors = sum(survived))

outplot <- ggplot(seedling_totals, aes(x = Treatment, y = total_survivors))+
  geom_col(width = 0.6)+
  labs(y = "Number of survivors")+
  scale_x_discrete(labels = c("Control", "Snow mold addition"))+
  theme_few()

ggsave(filename = "../figures/lab_results_fig.pdf", 
       plot = outplot, 
       height = 3.5, 
       width = 4, 
       units = "in")

