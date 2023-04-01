
################################################################################
# Initial setup.
################################################################################
# --------------- Clean working space:

rm(list=ls())

# --------------- Set working directory:

setwd("C:/Users/felmo/Dropbox/1_personal/_maestria_unibo_(operacional)/_causal_inference/_replication_study")


# --------------- Install required packages:

#install.packages("librarian")
library(librarian)

librarian::shelf(rgdal,
                 knitr,
                 tidyverse,
                 haven,
                 readxl,
                 magrittr,
                 apollo,
                 ggplot2,
                 RColorBrewer,
                 xtable,
                 openxlsx,
                 quiet = T)

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





################################################################################
# Some graphs:
################################################################################

# Define the number of colors you want
nb.cols <- 9
mycolors <- colorRampPalette(brewer.pal(8, "Paired"))(nb.cols)

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

# =============== Export dataset:

write.xlsx(data, "_extention/_data/data.xlsx")
getwd()





################################################################################
# TWFE:
################################################################################

