rm(list = ls())
objects()
options(error=recover, scipen=999)

# Any package that is required by the script below is given here:
# Check to see if packages are installed, if not install.
inst_pkgs = load_pkgs =  c("data.table", "tidyverse", "magrittr", "tidyselect", "ggplot2", "gridExtra", "lubridate", "scales", "mfx", "texreg", "censReg")
inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
if(length(inst_pkgs)) install.packages(inst_pkgs)

# Dynamically load packages
pkgs_loaded = lapply(load_pkgs, require, character.only=T)


setwd("Z:/sl2763_Samantha Lau/Projects/2022 Consumer Survey/Survey Results/SurveyAnalysis")

raw <- read.csv("raw.csv", stringsAsFactors = T)
colnames(raw)

## CREATE SOME BINARY VARIABLE AND RELEVEL OTHERS ##
clean.df <- raw %>% mutate(
  notice_sellby = (notice_sellby=="Yes")*1,
  poor_exp = (poor_exp=="Yes")*1,
  female = (gender=="Female")*1,
  married = (ms=="Married")*1,
  child = (child=="Yes")*1,
  # note this is roughly the median US income 
  inc_over_70k = (hhinc %in% c("$70,000 to $79,999", "$80,000 to $89,999", "$90,000 to $99,999", "$100,000 to $149,999", "$150,000 or more"))*1,
  collegegrad = (edu %in% c("Bachelor's degree in college (4-year)", "Graduate Degree"))*1,
  is = (purch_milk_is == "In a store")*1,
  ol = (purch_milk_ol == "Online (through an app or web browser)")*1,
  cons_milk_lte_weekly = (cons_milk_freq %in% c("Once a week", "Less than once a week"))*1,
  
  purch_milk_is_gte_weekly = (purch_milk_is_freq %in% c("Approximately once a week", "Multiple times a week"))*1,
  purch_milk_ol_gte_weekly = (purch_milk_ol_freq %in% c("Approximately once a week", "Multiple times a week"))*1,
  poor_freq = (poor_exp_freq %in% c("Sometimes", "About half the time", "Most of the time", "Always"))*1,
  switch_stsp = (poor_exp_chg_switch_st == "Switch stores" | poor_exp_chg_switch_sp == "Switch service provider")*1,
  # Top-2 box calculations 
  top2_good_is = (good_exp_is_ret %in% c("Likely", "Very likely"))*1,
  top2_poor_is = (poor_exp_is_ret %in% c("Likely", "Very likely"))*1,
  top2_good_ic = (good_exp_ic_ret %in% c("Likely", "Very likely"))*1,
  top2_poor_ic = (poor_exp_ic_ret %in% c("Likely", "Very likely"))*1,
  top2_good_olsp = (good_exp_olsp_ret %in% c("Likely", "Very likely"))*1,
  top2_poor_olsp = (poor_exp_olsp_ret %in% c("Likely", "Very likely"))*1,
  top2_good_stweb = (good_exp_stweb_ret %in% c("Likely", "Very likely"))*1,
  top2_poor_stweb = (poor_exp_stweb_ret %in% c("Likely", "Very likely"))*1,
  
  purch_groc_ol_freq = relevel(purch_groc_ol_freq, ref="Once in a few months or longer"),
  purch_milk_is_freq = relevel(purch_milk_is_freq, ref="Less frequently than every 14 days"),
  purch_milk_ol_freq = relevel(purch_milk_ol_freq, ref="Less frequently than every 14 days"),
  cons_milk_freq = relevel(cons_milk_freq, ref="Once a week"),
  purch_milk_size = relevel(purch_milk_size, ref="Gallon"),
  milk_price = relevel(milk_price, ref="$0-2"),
  spend = relevel(spend, ref="Less than $25"),
  last_exp_nps = relevel(last_exp_nps, ref="Passive"),
  poor_exp_freq = relevel(poor_exp_freq, ref="Never"),
  good_exp_is_ret = relevel(good_exp_is_ret, ref="Neutral"),
  good_exp_is_nps = relevel(good_exp_is_nps, ref="Passive"),
  poor_exp_is_ret = relevel(poor_exp_is_ret, ref="Neutral"),
  poor_exp_is_nps = relevel(poor_exp_is_nps, ref="Passive"),
  good_exp_ic_ret = relevel(good_exp_ic_ret, ref="Neutral"),
  good_exp_ic_nps = relevel(good_exp_ic_nps, ref="Passive"),
  poor_exp_ic_ret = relevel(poor_exp_ic_ret, ref="Neutral"),
  poor_exp_ic_nps = relevel(poor_exp_ic_nps, ref="Passive"),
  good_exp_olsp_ret = relevel(good_exp_olsp_ret, ref="Neutral"),
  good_exp_olsp_nps = relevel(good_exp_olsp_nps, ref="Passive"),
  poor_exp_olsp_ret = relevel(poor_exp_olsp_ret, ref="Neutral"),
  poor_exp_olsp_nps = relevel(poor_exp_olsp_nps, ref="Passive"),
  good_exp_stweb_ret = relevel(good_exp_stweb_ret, ref="Neutral"),
  good_exp_stweb_nps = relevel(good_exp_stweb_nps, ref="Passive"),
  poor_exp_stweb_ret = relevel(poor_exp_stweb_ret, ref="Neutral"),
  poor_exp_stweb_nps = relevel(poor_exp_stweb_nps, ref="Passive"),
  edu = relevel(edu, ref="Did not complete high school"),
  emp = relevel(emp, ref="Working full-time"),
  hhinc = relevel(hhinc, ref="Less than $10,000")
)

### NOTE THE DEPENDENCIES ### - POPULATE THE NAs to be explicit and distinguish from "blank"
### WHEN SPECIFYING MODELS, NOTE THAT THESE DEPENDENT FIELDS WILL BE NA IF THE DATA IS NOT FILTERED CORRECTLY !!!
# in-store questions depend on purch_milk_is == "In a store"
clean.df[clean.df$purch_milk_is=="", c("purch_milk_is_freq",	"purch_milk_is_gte_weekly", "purch_milk_is_ware",	"purch_milk_is_bigbox",	"purch_milk_is_super",	"purch_milk_is_spec",	"purch_milk_is_disc",	"purch_milk_is_conv",	"purch_milk_is_local",	"purch_milk_is_oth",	"purch_milk_is_othtxt", "good_exp_is_ret",	"good_exp_is_nps",	"good_exp_is_nbr", "top2_good_is")] <- NA

# online questions depend on purch_milk_ol == "Online (through an app or web browser)"
clean.df[clean.df$purch_milk_ol=="",c("purch_milk_ol_freq",	"purch_milk_ol_gte_weekly", "purch_milk_ol_loc")] <- NA

    # Instacart questions depend purch_milk_ol_loc == "Instacart"
    clean.df[is.na(clean.df$purch_milk_ol_loc) | (clean.df$purch_milk_ol_loc !="Instacart"), c("good_exp_ic_ret",	"good_exp_ic_nps",	"good_exp_ic_nbr", "top2_good_ic")] <- NA
    
    # Online service provider questions depend on purch_milk_ol_loc %in% c("Amazon Fresh", "PeaPod", "Shipt", "FreshDirect", "Boxed")
    clean.df[is.na(clean.df$purch_milk_ol_loc) | !(clean.df$purch_milk_ol_loc  %in% c("Amazon Fresh", "PeaPod", "Shipt", "FreshDirect", "Boxed")), c("good_exp_olsp_ret",	"good_exp_olsp_nps",	"good_exp_olsp_nbr", "top2_good_olsp")] <- NA
    
    # Store website questions depend on purch_milk_ol_loc == "Store-specific online services (Stop and Shop online delivery/pickup, Walmart+, etc)"
    clean.df[is.na(clean.df$purch_milk_ol_loc) | clean.df$purch_milk_ol_loc !="Store-specific online services (Stop and Shop online delivery/pickup, Walmart+, etc)", c("good_exp_stweb_ret",	"good_exp_stweb_nps",	"good_exp_stweb_nbr", "top2_good_stweb")] <- NA
    
# poor experience questions depend on poor_exp == 1
clean.df[clean.df$poor_exp==0, c("poor_exp_taste", "poor_exp_smell",	"poor_exp_sick",	"poor_exp_sour",	"poor_exp_pkg",	"poor_exp_2day",	"poor_exp_expired",	"poor_exp_texture",	"poor_exp_oth",	"poor_exp_pna",	"poor_exp_badb4exp",	"poor_exp_othtxt",	"poor_exp_freq", "poor_freq",	"poor_exp_chg_switch_st",	"poor_exp_chg_switch_sp",	"poor_exp_chg_switch_brand",	"poor_exp_chg_switch_type",	"poor_exp_chg_nc",	"poor_exp_chg_pna", "switch_stsp")] <- NA
  
    # poor_exp_chg_switch_detail AND poor_exp_chg_switch_alloc depend on poor_exp_chg_switch_st == "Switch stores" OR	poor_exp_chg_switch_sp == "Switch service provider"
    clean.df[(is.na(clean.df$poor_exp_chg_switch_st) | clean.df$poor_exp_chg_switch_st != "Switch stores") & (is.na(clean.df$poor_exp_chg_switch_sp) | clean.df$poor_exp_chg_switch_sp != "Switch service provider"), c("poor_exp_chg_switch_detail", "poor_exp_chg_switch_alloc")] <- NA
    
    # all the location-based poor experience questions also depend on purch_milk_ol_loc or purch_milk_is
    clean.df[(clean.df$poor_exp==0 | clean.df$purch_milk_is==""), c("poor_exp_is_ret",	"poor_exp_is_nps",	"poor_exp_is_nbr", "top2_poor_is")] <- NA
    clean.df[(clean.df$poor_exp==0 | is.na(clean.df$purch_milk_ol_loc) | clean.df$purch_milk_ol_loc !="Instacart"), c("poor_exp_ic_ret",	"poor_exp_ic_nps",	"poor_exp_ic_nbr", "top2_poor_ic")] <- NA
    clean.df[(clean.df$poor_exp==0 | is.na(clean.df$purch_milk_ol_loc) | !(clean.df$purch_milk_ol_loc  %in% c("Amazon Fresh", "PeaPod", "Shipt", "FreshDirect", "Boxed"))), c("poor_exp_olsp_ret",	"poor_exp_olsp_nps",	"poor_exp_olsp_nbr", "top2_poor_olsp")] <- NA
    clean.df[(clean.df$poor_exp==0 | is.na(clean.df$purch_milk_ol_loc) | clean.df$purch_milk_ol_loc !="Store-specific online services (Stop and Shop online delivery/pickup, Walmart+, etc)"), c("poor_exp_stweb_ret",	"poor_exp_stweb_nps",	"poor_exp_stweb_nbr", "top2_poor_stweb")] <- NA

## Two "logical" variables that need NA converted to 0 (to indicate no answer)
    #pna = prefer not to answer
clean.df$diet_pna[is.na(clean.df$diet_pna)] <- 0
clean.df$poor_exp_chg_pna[clean.df$poor_exp==1 & is.na(clean.df$poor_exp_chg_pna)] <- 0 

## For all factor variables, rename "blank" factors levels to "0" (to indicate no answer) -> These are the reference levels for multi-answer questions
blanktozero <- function(x) {
  levels(x)[levels(x)==""] <- "0"
  return(x)
}
clean.df[,sapply(clean.df, is.factor)] <- lapply(clean.df[,sapply(clean.df, is.factor)], blanktozero)

## Variable groups for easy formula construction
demo.vars <- c("female", "age", "collegegrad", "inc_over_70k", "child", "married")
channel.vars <- c("purch_milk_is", "purch_milk_ol")
cons.vars <- c("cons_milk_lte_weekly", "purch_milk_size", "notice_sellby")
is_cons.vars <- c("purch_milk_is_gte_weekly", "purch_milk_is_ware", "purch_milk_is_bigbox", "purch_milk_is_super",
                  "purch_milk_is_spec", "purch_milk_is_disc", "purch_milk_is_conv", "purch_milk_is_local", "purch_milk_is_oth")
ol_cons.vars <- c("purch_milk_ol_gte_weekly", "purch_milk_ol_loc")
milk_type.vars <- c("milk_whole", "milk_2pct", "milk_1pct", "milk_skim", "milk_lf", "milk_a2", "milk_cream",
                    "milk_homo", "milk_oth")
poor_def.vars <- c("poor_exp_taste", "poor_exp_smell", "poor_exp_sick", "poor_exp_sour", "poor_exp_pkg",
                   "poor_exp_2day", "poor_exp_expired", "poor_exp_texture", "poor_exp_oth", "poor_exp_badb4exp")
poor_exp_chg.vars <- c("poor_exp_chg_switch_st", "poor_exp_chg_switch_sp", "poor_exp_chg_switch_brand", 
                       "poor_exp_chg_switch_type", "poor_exp_chg_nc")

### PREDICTORS OF POOR EXPERIENCE
# Full Sample
model.poor_exp_all <- logitmfx(formula = as.formula(paste("poor_exp ~ ", paste(c(demo.vars, channel.vars, cons.vars, milk_type.vars), collapse="+"))),
                          data = clean.df, atmean=F, robust=T)
summary(model.poor_exp_all$fit) #coefficients
summary(model.poor_exp_all)    #average marginal effects

# In Store
model.poor_exp_is <- logitmfx(formula = as.formula(paste("poor_exp ~ ", paste(c(demo.vars, cons.vars, milk_type.vars, is_cons.vars), collapse="+"))),
                               data = clean.df[clean.df$is==1,], atmean=F, robust=T)
summary(model.poor_exp_is$fit) #coefficients
summary(model.poor_exp_is)     #average marginal effects

# Online
model.poor_exp_ol <- logitmfx(formula = as.formula(paste("poor_exp ~ ", paste(c(demo.vars, cons.vars, milk_type.vars, ol_cons.vars), collapse="+"))),
                              data = clean.df[clean.df$ol==1,], atmean=F, robust=T)
summary(model.poor_exp_ol$fit) #coefficients
summary(model.poor_exp_ol)     #average marginal effects

htmlreg(l=list(model.poor_exp_all, model.poor_exp_is, model.poor_exp_ol), file="./poor_exp.html")

### POOR EXPERIENCE FREQUENCY
model.poor_freq <- logitmfx(formula = as.formula(paste("poor_freq ~ ", paste(c(demo.vars, channel.vars, cons.vars, milk_type.vars), collapse="+"))),
                           data = clean.df[clean.df$poor_exp==1,], atmean=F, robust=T)
summary(model.poor_freq$fit) #coefficients
summary(model.poor_freq)     #average marginal effects

### SWITCH STORES OR SERVICE PROVIDERS
model.switch_stsp <- logitmfx(formula = as.formula(paste("switch_stsp ~ ", paste(c(demo.vars, channel.vars, cons.vars, milk_type.vars, "poor_freq"), collapse="+"))),
                            data = clean.df[clean.df$poor_exp==1,], atmean=F, robust=T)
summary(model.switch_stsp$fit) #coefficients
summary(model.switch_stsp)     #average marginal effects

htmlreg(l=list(model.poor_freq, model.switch_stsp), file="./freq_switch.html")

### NOT ENOUGH OBS (N=47) TO LOOK AT DIFFERENT TYPES OF SWITCHING OR SPENDING ALLOCATIONS

### LIKELIHOOD OF REPURCHASE 0/1, NPS NUMBER, NPS CATEGORY ##
# Construct panel data set with all three measures
  # cluster errors by pid

top2.panel.df <- clean.df %>% pivot_longer(cols=starts_with("top2"), names_to="chan_exp", values_to="top2", values_drop_na=T) %>% 
  mutate(exp = substr(chan_exp, 6,9), channel = substring(chan_exp, 11)) %>% dplyr::select(-c(chan_exp, ends_with("nbr"), ends_with("nps")))

nbr.panel.df <- clean.df %>% pivot_longer(cols=(ends_with("nbr") & !starts_with("last")), names_to="chan_exp", values_to="nbr", values_drop_na=T) %>% 
  mutate(exp = substr(chan_exp, 1,4), channel = sapply(str_split(substring(chan_exp, 10), "_"),function(x) x[[1]])) %>% 
  dplyr::select(pid, exp, channel, nbr)

nps.panel.df <- clean.df %>% pivot_longer(cols=(ends_with("nps") & !starts_with("last")), names_to="chan_exp", values_to="nps", values_drop_na=T) %>% 
  mutate(exp = substr(chan_exp, 1,4), channel = sapply(str_split(substring(chan_exp, 10), "_"),function(x) x[[1]])) %>% 
  dplyr::select(pid, exp, channel, nps) %>% filter(nps != "0")

## MERGE ALL 3 DATASETS FOR EASE
panel.df <- top2.panel.df %>% full_join(nbr.panel.df, by=c("pid", "exp", "channel")) %>% full_join(nps.panel.df, by=c("pid", "exp", "channel")) %>%
  mutate(exp=factor(exp, levels=c("good", "poor")), channel=factor(channel, levels=c("is", "ic", "olsp", "stweb")))
# Create unified "purchase frequency" variable
panel.df %<>% mutate(purch_milk_freq = factor(ifelse(channel=="is", as.character(purch_milk_is_freq), as.character(purch_milk_ol_freq))),
                     purch_milk_freq = relevel(purch_milk_freq, ref="Less frequently than every 14 days"),
                     purch_milk_gte_weekly = (purch_milk_freq %in% c("Approximately once a week", "Multiple times a week"))*1)


# Likelihood of Repurchase
model.top2 <- logitmfx(formula = as.formula(paste("top2 ~ ", paste(c(demo.vars, cons.vars, milk_type.vars, c("exp", "channel", "purch_milk_gte_weekly")), collapse="+"))),
                              data = panel.df, atmean=F, robust=T)
summary(model.top2$fit) #coefficients
summary(model.top2)     #average marginal effects

#clustering
model.top2b <- logitmfx(formula = as.formula(paste("top2 ~ ", paste(c(demo.vars, cons.vars, milk_type.vars, c("exp", "channel", "purch_milk_gte_weekly")), collapse="+"))),
                       data = panel.df, atmean=F, clustervar1="pid")
summary(model.top2b$fit)


# NPS Number Score (0 to 10) - Tobit model
# Run a Tobit regression, clustered standard errors
model.nbr <- censReg(as.formula(paste("nbr ~ ", paste(c(demo.vars, cons.vars, milk_type.vars, c("exp", "channel", "purch_milk_gte_weekly")), collapse="+"))), 
                     left=0, right=10, data=panel.df, subset=!is.na(nbr))
summary(model.nbr)
rse.tobit <- coeftest(model.nbr, vcov = vcovCL(model.nbr, cluster = ~ pid))

htmlreg(l=list(model.switch_stsp, model.top2, model.nbr), file="./repur_nps.html",
        override.se = list(model.top2$mfxest[,2], rse.tobit[,2]),
        override.pvalues = list(model.top2$mfxest[,4], rse.tobit[,4]),)

htmlreg(l=list(model.switch_stsp, model.top2, model.nbr), file="./repur_nps2.html",
        override.se = list(model.top2$mfxest[,2], rse.tobit[,2]),
        override.pvalues = list(model.top2$mfxest[,4], rse.tobit[,4]),)

save.image("milk_survey.RData")

