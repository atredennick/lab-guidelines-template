# 00_analysis_master.R: 
#  R script to conduct the entire snowmold analysis. This script sources
#  individual analysis scripts.
#
# NOTE: 
#  This script assumes the working directory is set to the location of
#  this source file.
#
# Authors:
#  Peter Adler (pbadler)
#  Andrew Tredennick (atredennick)


# Preliminaries -----------------------------------------------------------

rm(list=ls())  # clear the workspace

VERBOSE <- FALSE  # print output while running?


# Load packages -----------------------------------------------------------

library(tidyverse)
library(scales)
library(boot)
library(lme4)
library(MASS)
library(emmeans)
library(multcompView)
library(lmerTest)
library(texreg)
library(ggthemes)


# Source analysis scripts -------------------------------------------------

source("01_lab_data_analysis.R")  # run analysis
source("02_make_figures.R")  # make figures for manuscript
