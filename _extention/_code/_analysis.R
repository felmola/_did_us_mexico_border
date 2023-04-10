
################################################################################
# Initial setup.
################################################################################
# --------------- Clean working space:

rm(list=ls())

# --------------- Set working directory:

setwd("C:/Users/felmo/Dropbox/1_personal/_maestria_unibo_(operacional)/_causal_inference/_did/_did_us_mexico_border")

# --------------- Install required packages:

#install.packages("librarian")
library(librarian)

librarian::shelf(rgdal,
                 knitr,
                 tidyverse,
                 haven,
                 readxl,
                 magrittr,
                 RColorBrewer,
                 xtable,
                 openxlsx,
                 crosstable,
                 rlang,
                 did,
                 fixest,
                 stargazer,
                 gridExtra,
                 texreg,
                 quiet = T
)

#source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")

# =============== Import data set:

data_transformed <- read_dta("_extention/_data/data_transformed.dta")

################################################################################
# Replicating Bansak et al. (2022) with the new dataset.
################################################################################

new_app <- fixest::feols(ln_app ~ treated + post + treated_post,
                         data = data_transformed,
                         vcov = 'hetero',) # 'hetero' produces the same SE as in stata!.

new_app_c <- fixest::feols(ln_app ~treated + post + treated_post + months80 + lag_staff + as_factor(year),
                           data = data_transformed,
                           vcov = 'hetero',) # 'hetero' produces the same SE as in stata!.

new_death <- fixest::feols(death_ratio ~ treated + post + treated_post,
                           data = data_transformed,
                           vcov = 'hetero',) # 'hetero' produces the same SE as in stata!.

new_death_c <- fixest::feols(death_ratio ~ treated + post + treated_post + months80 + lag_staff + as_factor(year),
                             data = data_transformed,
                             vcov = 'hetero',) # 'hetero' produces the same SE as in stata!.


texreg(list(new_app, new_app_c, new_death, new_death_c),
       omit.coef = "(year)|(sector)",
       custom.coef.names = c("(intercept)", "treated", "post", "treated_post", "months80", "lag_staff"),
       reorder.coef = c(2, 3, 4, 5, 6, 1),
       include.rsquared = FALSE, include.adjrs = FALSE,
       digits = 4,
       
       custom.gof.rows = list("Fixed Effects" = c("NO", "YES", "NO", "YES"),
                              "F" = c(fitstat(new_app, "f")[1]$f$stat,
                                      fitstat(new_app_c, "f")[1]$f$stat,
                                      fitstat(new_death, "f")[1]$f$stat,
                                      fitstat(new_death_c, "f")[1]$f$stat
                              )),
       custom.header = list("ln(Apprehensions)" = 1:2, "Death/Appehensions/100.000" = 3:4),
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       caption = "Replication results from Bansak et al. (2022) Table 1 with the new data set.",
       label = "tab:table1_new",
       caption.above = TRUE,
       
       file = "C:/Users/felmo/Dropbox/1_personal/_maestria_unibo_(operacional)/_causal_inference/_did/_did_us_mexico_border/_extention/_output/table1_new.tex")

summary(new_death_c)





################################################################################
# Event Study:
################################################################################

# =============== Data transformation:

# --------------- Creating post x treated dummy (by dose):

data_transformed %<>%
  mutate(dose_post = dose * post)

data_transformed %>% group_by(dose, post, dose_post) %>% summarize(n = n())

# --------------- creating first_treated for did package:

data_transformed %<>%
  mutate(first_treated = 2010 * treated)

# =============== Event study (ln_app):

es_app <- att_gt(yname = "ln_app",
                 gname = "first_treated",
                 idname = "sector_number",
                 tname = "year",
                 xformla = ~1 ,
                 data = data_transformed,
                 est_method = "dr",
                 control_group = "notyettreated"
)

summary(es_app)

plot_es_app <- ggdid(es_app)
plot_es_app

ggsave("_extention/_output/plot_es_app.pdf", plot_es_app, width = 8.5, height = 5)

# --------------- Aggregation of ATE:

agg.simple <- aggte(es_app, type = "simple")
summary(agg.simple)

# =============== Event study (death_ratio):

es_death <- att_gt(yname = "death_ratio",
                   gname = "first_treated",
                   idname = "sector_number",
                   tname = "year",
                   xformla = ~1 ,
                   data = data_transformed,
                   est_method = "dr",
                   control_group = "notyettreated"
)

summary(es_death)

plot_es_death <- ggdid(es_death)
plot_es_death

ggsave("_extention/_output/plot_es_death.pdf", plot_es_death, width = 8.5, height = 5)

# --------------- Aggregation of ATE:

agg.simple <- aggte(es_death, type = "simple")
summary(agg.simple)





################################################################################
# Callaway's multi-valued treatment (The wrong TWFE specification for estimating ATTs)
################################################################################

# =============== Estimation (Dummy):

cw_app <- fixest::feols(ln_app ~ treated + post + as.factor(I(dose * post)),
                        data = data_transformed,
                        vcov = 'hetero',) # 'hetero' produces the same SE as in stata!.
summary(cw_app)

cw_app_c <- fixest::feols(ln_app ~ treated + post + as.factor(I(dose * post)) + as_factor(year) + months80 + lag_staff,
                          data = data_transformed,
                          vcov = 'hetero',) # 'hetero' produces the same SE as in stata!.
summary(cw_app_c)

cw_death <- fixest::feols(death_ratio ~ treated + post + as.factor(I(dose * post)),
                          data = data_transformed,
                          vcov = 'hetero') # 'hetero' produces the same SE as in stata!.
summary(cw_death)

cw_death_c <- fixest::feols(death_ratio ~ treated + post + as.factor(I(dose * post)) + as_factor(year) + months80 + lag_staff,
                            data = data_transformed,
                            vcov = 'hetero',) # 'hetero' produces the same SE as in stata!.
summary(cw_death_c)


texreg(list(cw_app, cw_app_c, cw_death, cw_death_c),
       omit.coef = "(year)|(sector)",
       custom.coef.names = c("(intercept)", "treated", "post", "dose = 60", "dose = 130", "dose = 220", "months80", "lag_staff"),
       reorder.coef = c(2, 3, 4, 5, 6, 7, 8, 1),
       include.rsquared = FALSE, include.adjrs = FALSE,
       digits = 4,
       
       custom.gof.rows = list("Fixed Effects" = c("NO", "YES", "NO", "YES"),
                              "F" = c(fitstat(cw_app, "f")[1]$f$stat,
                                      fitstat(cw_app_c, "f")[1]$f$stat,
                                      fitstat(cw_death, "f")[1]$f$stat,
                                      fitstat(cw_death_c, "f")[1]$f$stat
                              )),
       custom.header = list("ln(Apprehensions)" = 1:2, "Death/Appehensions/100.000" = 3:4),
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       caption = "TWFE estimation of ATT across doses.",
       label = "tab:cw_atts",
       caption.above = TRUE,
       
       file = "C:/Users/felmo/Dropbox/1_personal/_maestria_unibo_(operacional)/_causal_inference/_did/_did_us_mexico_border/_extention/_output/cw_atts_wrong.tex")




################################################################################
# Callaway's multi-valued treatment (ATTs from the same TWFE specification)
################################################################################

# =============== Creating dummy treated for speficic doses:

data_transformed %<>%
  mutate(treated_60 = ifelse(sector_number == 2, 1, 0),
         treated_130 = ifelse(sector_number == 3, 1, 0),
         treated_220 = ifelse(sector_number == 4, 1, 0))

data_transformed %>% 
  group_by(sector_number, treated_60, treated_130, treated_220) %>% 
  summarize(n = n())

# =============== Estimation (Dummy):

cw_app <- fixest::feols(ln_app ~ treated_60 + treated_130 + treated_220 + post + as.factor(I(dose * post)),
                        data = data_transformed,
                        vcov = 'hetero',) # 'hetero' produces the same SE as in stata!.
summary(cw_app)

cw_app_c <- fixest::feols(ln_app ~ treated_60 + treated_130 + treated_220 + post + as.factor(I(dose * post)) + as_factor(year) + months80 + lag_staff,
                          data = data_transformed,
                          vcov = 'hetero',) # 'hetero' produces the same SE as in stata!.
summary(cw_app_c)

cw_death <- fixest::feols(death_ratio ~ treated_60 + treated_130 + treated_220 + post + as.factor(I(dose * post)),
                          data = data_transformed,
                          vcov = 'hetero') # 'hetero' produces the same SE as in stata!.
summary(cw_death)

cw_death_c <- fixest::feols(death_ratio ~ treated_60 + treated_130 + treated_220 + post + as.factor(I(dose * post)) + as_factor(year) + months80 + lag_staff,
                            data = data_transformed,
                            vcov = 'hetero',) # 'hetero' produces the same SE as in stata!.
summary(cw_death_c)


texreg(list(cw_app, cw_app_c, cw_death, cw_death_c),
       omit.coef = "(year)|(sector)",
       custom.coef.names = c("(intercept)", "treated d=60", "treated d=130", "treated d=220", "post", "ATT(60$|$60)", "ATT(130$|$130)", "ATT(220$|$220)", "months80", "lag_staff"),
       reorder.coef = c(2, 3, 4, 5, 6, 7, 8, 9, 10, 1),
       include.rsquared = FALSE, include.adjrs = FALSE,
       digits = 4,
       
       custom.gof.rows = list("Fixed Effects" = c("NO", "YES", "NO", "YES"),
                              "F" = c(fitstat(cw_app, "f")[1]$f$stat,
                                      fitstat(cw_app_c, "f")[1]$f$stat,
                                      fitstat(cw_death, "f")[1]$f$stat,
                                      fitstat(cw_death_c, "f")[1]$f$stat
                              )),
       custom.header = list("ln(Apprehensions)" = 1:2, "Death/Appehensions/100.000" = 3:4),
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       caption = "TWFE estimation of ATTs across doses.",
       label = "tab:cw_atts",
       caption.above = TRUE,
       float.pos = "!ht",
       
       file = "C:/Users/felmo/Dropbox/1_personal/_maestria_unibo_(operacional)/_causal_inference/_did/_did_us_mexico_border/_extention/_output/cw_atts.tex")

# =============== Estimation of ATT (by subsampling):

data_60 <-
  data_transformed %>% 
  filter(sector_number == 2 | sector_number == 6 | sector_number == 7 | sector_number == 8)

att_60 <- fixest::feols(ln_app ~ treated + post + as.factor(I(dose * post)) + as_factor(year) + months80 + lag_staff,
                        data = data_60,
                        vcov = 'hetero',) # 'hetero' produces the same SE as in stata!.
summary(att_60)


data_130 <-
  data_transformed %>% 
  filter(sector_number == 3 | sector_number == 6 | sector_number == 7 | sector_number == 8)

att_130 <- fixest::feols(ln_app ~ treated + post + as.factor(I(dose * post)) + as_factor(year) + months80 + lag_staff,
                         data = data_130,
                         vcov = 'hetero',) # 'hetero' produces the same SE as in stata!.
summary(att_130)


data_220 <-
  data_transformed %>% 
  filter(sector_number == 4 | sector_number == 6 | sector_number == 7 | sector_number == 8)

att_220 <- fixest::feols(ln_app ~ treated + post + as.factor(I(dose * post)) + as_factor(year) + months80 + lag_staff,
                         data = data_220,
                         vcov = 'hetero',) # 'hetero' produces the same SE as in stata!.
summary(att_220)





################################################################################
# Callaway's multi-valued treatment (TWFE Beta from their eq 1)
################################################################################

cw_twfe_app <- fixest::feols(ln_app ~ treated + post + I(dose * post),
                             data = data_transformed,
                             vcov = 'hetero',) # 'hetero' produces the same SE as in stata!.
summary(cw_twfe_app)


cw_twfe_app_c <- fixest::feols(ln_app ~ treated + post + I(dose * post) + as_factor(year) + months80 + lag_staff,
                               data = data_transformed,
                               vcov = 'hetero',) # 'hetero' produces the same SE as in stata!.
summary(cw_twfe_app)

cw_twfe_death <- fixest::feols(death_ratio ~ treated + post + I(dose * post),
                               data = data_transformed,
                               vcov = 'hetero',) # 'hetero' produces the same SE as in stata!.
summary(cw_twfe_death)

cw_twfe_death_c <- fixest::feols(death_ratio ~ treated + post + I(dose * post) + as_factor(year) + months80 + lag_staff,
                                 data = data_transformed,
                                 vcov = 'hetero',) # 'hetero' produces the same SE as in stata!.
summary(cw_twfe_death)

texreg(list(cw_twfe_app, cw_twfe_app_c, cw_twfe_death, cw_twfe_death_c),
       omit.coef = "(year)|(sector)",
       custom.coef.names = c("(intercept)", "treated", "post", "dose_post", "months80", "lag_staff"),
       reorder.coef = c(2, 3, 4, 5, 6, 1),
       include.rsquared = FALSE, include.adjrs = FALSE,
       digits = 4,
       
       custom.gof.rows = list("Fixed Effects" = c("NO", "YES", "NO", "YES"),
                              "F" = c(fitstat(cw_app, "f")[1]$f$stat,
                                      fitstat(cw_app_c, "f")[1]$f$stat,
                                      fitstat(cw_death, "f")[1]$f$stat,
                                      fitstat(cw_death_c, "f")[1]$f$stat
                              )),
       custom.header = list("ln(Apprehensions)" = 1:2, "Death/Appehensions/100.000" = 3:4),
       custom.model.names = c("(1)", "(2)", "(3)", "(4)"),
       caption = "TWFE estimation.",
       label = "tab:cw_twfe",
       caption.above = TRUE,
       
       file = "C:/Users/felmo/Dropbox/1_personal/_maestria_unibo_(operacional)/_causal_inference/_did/_did_us_mexico_border/_extention/_output/cw_twfe.tex")

#END