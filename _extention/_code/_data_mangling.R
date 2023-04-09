
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

# --------------- List layers in object:

ogrListLayers("_extention/_data/01-ORIGINAL.gdb")






################################################################################
# Data mangling for pedestrian dataset:
################################################################################

# --------------- save layers:

pedestrian_fence <- readOGR("_extention/_data/01-ORIGINAL.gdb", layer="obp_baseline_ti_ped_fence_LIMIT_GOV_USE")
#plot(pedestrian_fence)
#plot(vehicular_barrier)

pedestrian_fence <- as_tibble(pedestrian_fence@data)
summary(pedestrian_fence)

# =============== Clean data set:

# --------------- Rename variables:
pedestrian_fence %<>%
  rename(state = STATE_ABBR,
         sector = SEC_CODE,
         type = FNC_TYPE,
         year = DATE_IN,
         id_length = GlobalID,
         length = Shape_Length)

pedestrian_fence %<>%
  select(-c(Shape_STLength__))

pedestrian_fence %>% 
  group_by(state) %>% 
  summarize(n = n())

pedestrian_fence %>% 
  group_by(sector) %>% 
  summarize(n = n())

pedestrian_fence %>% 
  group_by(type) %>% 
  summarize(n = n())

# --------------- Rename sectors:

pedestrian_fence %<>%
  mutate(sector = str_replace(sector, "SDC", "1_san_diego")) %>%
  mutate(sector = str_replace(sector, "ELC", "2_el_centro")) %>% 
  mutate(sector = str_replace(sector, "YUM", "3_yuma")) %>%
  mutate(sector = str_replace(sector, "TCA", "4_tucson")) %>% 
  mutate(sector = str_replace(sector, "EPT", "5_el_paso")) %>% 
  mutate(sector = str_replace(sector, "BBT", "6_big_bend")) %>% 
  mutate(sector = str_replace(sector, "DRT", "7_del_rio")) %>% 
  mutate(sector = str_replace(sector, "LRT", "8_laredo")) %>% 
  mutate(sector = str_replace(sector, "RGV", "9_rio_grande_valley"))

# --------------- Fix year column:

pedestrian_fence %<>%
  mutate(year = str_sub(pedestrian_fence$year, start = 1, end = 4))

pedestrian_fence %>% 
  group_by(year) %>% 
  summarize(n = n())

pedestrian_fence %<>%
  filter(year != "TBD",
         year != "UNK")
# --------------- Fix length column:?????????????????????????????????

# --------------- Collapse dataset:

collapsed_pedestrian <-
pedestrian_fence %>% 
  group_by(sector, year) %>% 
  summarise(sum = sum(length))

collapsed_pedestrian %<>% 
  rename(length = sum)





################################################################################
# Data mangling for vehicular dataset:
################################################################################

# --------------- save layers:

vehicular_barrier <- readOGR("_extention/_data/01-ORIGINAL.gdb", layer="obp_baseline_ti_veh_barrier_LIMIT_GOV_USE")
#plot(vehicular_barrier)

vehicular_barrier <- as_tibble(vehicular_barrier@data)

# =============== Clean data set:

vehicular_barrier %<>%
  rename(state = STATE_ABBR,
         sector = SEC_CODE,
         type = VBR_TYPE,
         year = DATE_IN,
         id_length = GlobalID,
         length = Shape_Length)

vehicular_barrier %<>%
  select(-c(Shape_STLength__))

vehicular_barrier %>% 
  group_by(state) %>% 
  summarize(n = n())

vehicular_barrier %>% 
  group_by(sector) %>% 
  summarize(n = n())

vehicular_barrier %>% 
  group_by(type) %>% 
  summarize(n = n())

# --------------- Rename sectors:

vehicular_barrier %<>%
  mutate(sector = str_replace(sector, "SDC", "1_san_diego")) %>%
  mutate(sector = str_replace(sector, "ELC", "2_el_centro")) %>% 
  mutate(sector = str_replace(sector, "YUM", "3_yuma")) %>%
  mutate(sector = str_replace(sector, "TCA", "4_tucson")) %>% 
  mutate(sector = str_replace(sector, "EPT", "5_el_paso")) %>% 
  mutate(sector = str_replace(sector, "BBT", "6_big_bend")) %>% 
  mutate(sector = str_replace(sector, "DRT", "7_del_rio")) %>% 
  mutate(sector = str_replace(sector, "LRT", "8_laredo")) %>% 
  mutate(sector = str_replace(sector, "RGV", "9_rio_grande_valley"))

# --------------- Fix year column:

vehicular_barrier %<>%
  mutate(year = str_sub(vehicular_barrier$year, start = 1, end = 4))

vehicular_barrier %>% 
  group_by(year) %>% 
  summarize(n = n())

vehicular_barrier %<>%
  filter(year != "TBD",
         year != "UNK")

# --------------- Fix length column:?????????????????????????????????

# --------------- Collapse dataset:

collapsed_vehicle <-
  vehicular_barrier %>% 
  group_by(sector, year) %>% 
  summarise(sum = sum(length))

collapsed_vehicle %<>%
  rename(length = sum)





################################################################################
# Data mangling adding vehicular and pedestrian dataset:
################################################################################

# --------------- Merging the two datasets:

collapsed_all <- full_join(collapsed_pedestrian, collapsed_vehicle,
                 by = c("sector", "year"))


# --------------- The replacing NA's with 0 as if no construction took place:
collapsed_all %<>%
  mutate(length.x = ifelse(is.na(length.x), 0, length.x),
         length.y = ifelse(is.na(length.y), 0, length.y))

collapsed_all %<>%
  mutate(year = as.numeric(year))

# --------------- Express length units in miles:

collapsed_all %<>%
  mutate(length = length.x + length.y) %>% 
  select(-c(length.x, length.y)) %>% 
  mutate(length = length*100*0.621371)  # This is in miles

collapsed_all %<>%
  ungroup() %>% 
  mutate(length = round(collapsed_all$length, digits = 0))

# -------------- Add cumulative sum:

collapsed_all %<>% 
  arrange(sector, year) %>% 
  group_by(sector) %>%
  mutate(cum_len = cumsum(length)) %>% 
  ungroup()

#crosstab(collapsed_all, row.vars = "year")






# ============== Some plots

# Define the number of colors you want
nb.cols <- 9
mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)

# -------------- Yearly miles constructed:

collapsed_year <-
  collapsed_all %>% 
  group_by(year) %>% 
  summarize(length = sum(length))

plot_year <-
  collapsed_year %>% 
  ggplot(aes(x=as.numeric(year), y=length)) +
  geom_line()

plot_year    

# -------------- CDF:

collapsed_all %>% 
  group_by(sector) %>% 
  summarize(n = sum(length))

plot_cdf <-
  collapsed_all %>% 
  group_by(sector) %>% 
  ggplot(aes(x=as.numeric(year), y=cum_len, color=sector)) +
  
  theme_bw() +
  
  labs(x = "Year",
       y = "Cumulative Length (Miles)",
       color = "Sector") +
  scale_x_continuous(breaks = seq(min(collapsed_all$year), max(collapsed_all$year), 2)) +
  scale_y_continuous(breaks = seq(min(collapsed_all$cum_len), max(collapsed_all$cum_len), 20)) +
  
  geom_line(size = 1) +
  scale_color_manual(values = mycolors) +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5))

plot_cdf

ggsave("_extention/_output/plot_cdf.pdf", plot_cdf, width = 10, height = 5)





################################################################################
# Merging fencing and apprehensions/deaths databases:
################################################################################

outcome_variables <- as_tibble(read_dta("_replication/_data/AEAPP.dta"))

summary(outcome_variables) # sector_number and sector from fencing databse match.

# =============== Clean database:

# --------------- Rename sectors:

outcome_variables %<>%
  rename(sector = sector_name) %>% 
  mutate(sector = str_replace(sector, "San Diego", "1_san_diego")) %>%
  mutate(sector = str_replace(sector, "El Centro", "2_el_centro")) %>% 
  mutate(sector = str_replace(sector, "Yuma", "3_yuma")) %>%
  mutate(sector = str_replace(sector, "Tucson", "4_tucson")) %>% 
  mutate(sector = str_replace(sector, "El Paso", "5_el_paso")) %>% 
  mutate(sector = str_replace(sector, "Big Bend", "6_big_bend")) %>% 
  mutate(sector = str_replace(sector, "Del Rio", "7_del_rio")) %>% 
  mutate(sector = str_replace(sector, "Laredo", "8_laredo")) %>% 
  mutate(sector = str_replace(sector, "Rio Grande Valley", "9_rio_grande_valley"))

# --------------- Remove NA rows from apprehensions:

outcome_variables %<>%
  filter(!is.na(apprehensions))

outcome_variables %>% 
  group_by(sector) %>% 
  summarize(n=n())

# =============== Merging the databases:

summary(collapsed_all)
summary(outcome_variables)

data <- full_join(collapsed_all, outcome_variables, by = c("sector", "year")) %>% 
  arrange(sector, year)

# There are a lot of missing values because outcome_variables has information
# on almost all the years, but collapsed_all not. Easy fix: replace NA's in
# length with 0 as no construction took place:

# --------------- Replace NA for 0:

data %<>%
  mutate(length = ifelse(is.na(length), 0, length))

# --------------- Remove NA rows from apprehensions:

data %<>%
  filter(!is.na(apprehensions))

# -------------- Add cumulative sum for length:

data %<>% 
  arrange(sector, year) %>% 
  group_by(sector) %>%
  mutate(cum_len = cumsum(length)) %>% 
  ungroup()

# -------------- Add cumulative sum for apprehensions:

data %<>% 
  arrange(sector, year) %>% 
  group_by(sector) %>%
  mutate(cum_app = cumsum(apprehensions)) %>% 
  ungroup()

# --------------- Log-transforming apprehensions:

data %<>%
  mutate(ln_app = log(apprehensions))

# --------------- Deaths/apprehensions * 100.000 ratio:

data %<>%
  mutate(death_ratio = (deaths/apprehensions) * 100000)




# =============== Some plots:

# --------------- Cumulative fencing sector by year:

data %>% 
  group_by(sector) %>% 
  summarize(n = sum(length))

plot_cum_length <-
  data %>% 
  group_by(sector) %>% 
  ggplot(aes(x=as.numeric(year), y=cum_len, color=sector)) +
  
  theme_bw() +
  
  labs(x = "Year",
       y = "Cumulative Length (Miles)",
       color = "Sector") +
  scale_x_continuous(breaks = seq(min(data$year), max(data$year), 2)) +
  scale_y_continuous(breaks = seq(min(data$cum_len), max(data$cum_len), 20)) +
  
  geom_line(size = 1) +
  scale_color_manual(values = mycolors) +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5))

plot_cum_length

ggsave("_extention/_output/plot_cum_length.pdf", plot_cum_length, width = 10, height = 5)

# --------------- Cumulative apprehensions sector by year:

data %>% 
  group_by(sector) %>% 
  summarize(n = sum(apprehensions))

plot_cum_app <-
  data %>% 
  group_by(sector) %>% 
  ggplot(aes(x=as.numeric(year), y=cum_app, color=sector)) +
  
  theme_bw() +
  
  labs(x = "Year",
       y = "Cumulative Apprehension (Millions)",
       color = "Sector") +
  scale_x_continuous(breaks = seq(min(data$year), max(data$year), 2)) +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  
  geom_line(size = 1) +
  scale_color_manual(values = mycolors) +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5))

plot_cum_app

ggsave("_extention/_output/plot_cum_app.pdf", plot_cum_app, width = 10, height = 5)

# --------------- Cumulative apprehensions sector by year:

data %>% 
  group_by(sector) %>% 
  summarize(n = sum(apprehensions))

plot_cum_app <-
  data %>% 
  group_by(sector) %>% 
  ggplot(aes(x=as.numeric(year), y=cum_app, color=sector)) +
  
  theme_bw() +
  
  labs(x = "Year",
       y = "Cumulative Apprehension (Millions)",
       color = "Sector") +
  scale_x_continuous(breaks = seq(min(data$year), max(data$year), 2)) +
  scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6)) +
  
  geom_line(size = 1) +
  scale_color_manual(values = mycolors) +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5))

plot_cum_app

ggsave("_extention/_output/plot_cum_app.pdf", plot_cum_app, width = 10, height = 5)



################################################################################
# Data transformation to comply with TWFE a la Callaway (continuous treatments):
################################################################################


# --------------- Keeping years where treatment is constant:

data %<>% 
  filter(year >= 2000) %>% 
  filter(!(year >= 2005 & year <= 2009))

# --------------- Standardizing time for graphs:

data %<>% 
  mutate(time = case_when(year >= 2010 ~ year-2010,
                          year <= 2004 ~ year-2005))

#crosstab(data, row.vars = "year", col.vars = "time")
table((data[c("year", "time")]))

# =============== Redefining doses:

# --------------- Visualizing cum_len by year by sector in a table:

cum_lengths <- data %>%
  group_by(sector_number, time) %>% 
  select(sector_number, time, cum_len)

cum_lengths %<>%
  as.data.frame() %>% # need to call as.data.frame. Otherwise outputs something different.
  reshape(v.names = c("cum_len"),
          timevar = "sector_number",
          idvar = "time",
          direction = "wide") %>%
  as_tibble()

# --------------- Standardizing doses:

data_transformed <-
  data %>% 
  mutate(dose = NA) %>% 
  mutate(dose = case_when(sector_number == 1 & time <= -1 ~ 40,
                          sector_number == 1 & time >= 0 ~ 60,
                          
                          sector_number == 2 & time <= -1 ~ 0,
                          sector_number == 2 & time >= 0 ~ 60,
                          
                          sector_number == 3 & time <= -1 ~ 0,
                          sector_number == 3 & time >= 0 ~ 130,
                          
                          sector_number == 4 & time <= -1 ~ 0,
                          sector_number == 4 & time >= 0 ~ 220,
                          
                          sector_number == 5 & time <= -1 ~ 20,
                          sector_number == 5 & time >= 0 ~ 180,
                          
                          sector_number == 6 & time <= -1 ~ 0,
                          sector_number == 6 & time >= 0 ~ 0,
                          
                          sector_number == 7 & time <= -1 ~ 0,
                          sector_number == 7 & time >= 0 ~ 0,
                          
                          sector_number == 8 & time <= -1 ~ 0,
                          sector_number == 8 & time >= 0 ~ 0,
                          
                          sector_number == 9 & time <= -1 ~ 0,
                          sector_number == 9 & time >= 0 ~ 50,))

# --------------- Dropping observations that do not comply with d=0 at t=0:

data_transformed %<>%
  filter(!(sector_number == 1 | sector_number == 5))

data_transformed %>% group_by(sector) %>% 
  summarize(n = n())

# --------------- Dropping 9_rio_grande_valley bc of not paralell trends:

data_transformed %<>%
  filter(!(sector_number == 9))

data_transformed %>% group_by(sector) %>% 
  summarize(n = n())

# --------------- Creating treatment dummy (treated):

data_transformed %<>%
  mutate(treated = case_when(sector_number == 4 |
                                sector_number == 3 |
                                sector_number == 2 ~ 1,
                              sector_number == 6 |
                                sector_number == 7 |
                                sector_number == 8 ~ 0))

#crosstab(data_transformed, row.vars = "sector_number", col.vars = "treated")

# --------------- Creating post treatment dummy (post):

data_transformed %<>%
  mutate(post = case_when(time < 0 ~ 0,
                          time >= 0 ~1))

#crosstab(data_transformed, row.vars = "time", col.vars = "post")

# --------------- Creating post x treated dummy (treated_post):

data_transformed %<>% 
  mutate(treated_post = treated * post)

data_transformed %>% group_by(treated, post, treated_post) %>% summarize(n = n())

# --------------- creating lag variables (replicates l.variable in STATA):

data_transformed %<>%
  group_by(sector_number, post) %>%
  dplyr::mutate(lag_staff = lag(staff, n = 1, default = NA))






# =============== Some plots:

# Define the number of colors you want

nb.cols <- 9
mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)

# --------------- Cumulative dose by sector across time:

plot_dose <-
  data_transformed %>% 
  group_by(sector) %>% 
  ggplot(aes(x=as.numeric(time), y=dose, color=sector)) +
  geom_vline(xintercept=-0.5, linetype = "dashed") +
  
  theme_bw() +
  
  labs(x = "Time after treatment",
       y = "Cumulative Length (Miles)",
       color = "Sector") +
  scale_x_continuous(breaks = seq(min(data_transformed$time), max(data_transformed$time), 1)) +
  scale_y_continuous(breaks = seq(min(data_transformed$dose), max(data_transformed$dose), 20)) +
  
  geom_line(size = 1) +
  scale_color_manual(values = mycolors) +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5))

plot_dose

ggsave("_extention/_output/plot_dose.pdf", plot_dose, width = 10, height = 5)

# --------------- Cumulative apprehensions by time:

plot_app_transformed <-
  data_transformed %>% 
  group_by(sector) %>% 
  ggplot(aes(x=as.numeric(time), y=apprehensions, color=sector)) +
  geom_vline(xintercept=-0.5, linetype = "dashed") +
  
  theme_bw() +
  
  labs(x = "Time after treatment",
       y = "Apprehensions",
       color = "Sector") +
  scale_x_continuous(breaks = seq(min(data_transformed$time), max(data_transformed$time), 1)) +
  # scale_y_continuous(breaks = seq(min(data_transformed$apprehensions), max(data_transformed$apprehensions), 20)) +
  
  geom_line(size = 1) +
  scale_color_manual(values = mycolors) +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5))

plot_app_transformed

# --------------- ln_app by sector across time:

plot_ln_app_transformed <-
  data_transformed %>% 
  group_by(sector) %>% 
  ggplot(aes(x=as.numeric(time), y=ln_app, color=sector)) +
  geom_vline(xintercept=-0.5, linetype = "dashed") +
  
  theme_bw() +
  
  labs(x = "Time to treatment",
       y = "log(Apprehensions)",
       color = "Sector") +
  scale_x_continuous(breaks = seq(min(data_transformed$time), max(data_transformed$time), 1)) +
  # scale_y_continuous(breaks = seq(min(data_transformed$apprehensions), max(data_transformed$apprehensions), 20)) +
  
  geom_line(size = 1) +
  scale_color_manual(values = mycolors) +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5))

plot_ln_app_transformed

# --------------- death_ratio by sector across time:

plot_death_ratio <-
  data_transformed %>% 
  group_by(sector) %>% 
  ggplot(aes(x=as.numeric(time), y=death_ratio, color=sector)) +
  geom_vline(xintercept=-0.5, linetype = "dashed") +
  
  theme_bw() +
  
  labs(x = "Time to treatment",
       y = "Deaths Ratio",
       color = "Sector") +
  scale_x_continuous(breaks = seq(min(data_transformed$time), max(data_transformed$time), 1)) +
  # scale_y_continuous(breaks = seq(min(data_transformed$apprehensions), max(data_transformed$apprehensions), 20)) +
  
  geom_line(size = 1) +
  scale_color_manual(values = mycolors) +
  theme(text = element_text(size = 18)) +
  theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5))

plot_death_ratio

ggsave("_extention/_output/plot_death_ratio.pdf", plot_death_ratio, width = 10, height = 5)





################################################################################
# Export dataset and list of installed packages and its versions for replciation:
################################################################################

# =============== Export dataset:

write_dta(data_transformed, "_extention/_data/data_transformed.dta")

# =============== Export packages and versions:

installed_packages <- as_tibble(installed.packages())
write_dta(installed_packages, "_extention/_code/installed_packages.dta")

#END