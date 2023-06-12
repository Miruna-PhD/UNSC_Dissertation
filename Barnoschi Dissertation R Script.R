### DISSERTATION R SCRIPT ###
#### Miruna Barnoschi ####

# Libraries
library(readxl)
library(tidyverse)
library(dplyr)
library(magrittr)
library(corrplot)
library(corrr)
library(modelr)
library(janitor)
library(rsample)
library(gtools)
library(skimr)
library(leaps) # best subset selection
library(glmnet) # ridge & lasso
library(glmnetUtils) # improves working with glmnet
library(pls) # pcr and pls
library(broom)
library(ggplot2)
library(GGally)
library(plotly)
library(ggplotlyExtra)
library(kableExtra)
library(splitstackshape)
library(ggthemes)
library(gridExtra)
library(pander)
library(ISLR)
library(kernlab)
library(ggdendro)

## For random forests
library(ranger)
library(vip)
library(pdp)
library(tree)
library(randomForest)
library(ipred) # bagging 
library(rpart)

## For boosted models
library(xgboost)
library(onehot)

## For SVMs
library(e1071)
library(pROC)
library(caret)


# FINAL TIME SERIES DATASET
UNSC_Conflict_Data_TS <- MyData %>%
  select(conflict_id, location, year, side_a, side_b, incompatibility, territory_name, intensity_level, starts_with("bdead"), type_of_conflict, region, start_date, start_date2, ep_end_date, conflict_name, UNSC_res_pass, UNSC_res_sum, UNSC_unanimous_res_sum, UNSC_unanimous_res_dummy, starts_with("al_"), starts_with("mad"), starts_with("atop"), starts_with("vdem"))
# 3339 observations of 49 variables

UNSC_Conflict_Data_TS <- UNSC_Conflict_Data_TS %>%
  select(-X1) %>%
  filter(year < 1992)

# write.csv(UNSC_Conflict_Data_TS,'data/UNSC_Conflict_Data_TS.csv')

# FINAL UNSC CONFLICT DATASET
UNSC_Conflict_Data <- UNSC_Conflict_Data_TS %>%
  group_by(conflict_id, location) %>%
  summarize(start_year = min(year),
            end_year = max(year)) %>%
  mutate(duration = end_year - start_year) %>%
  mutate(conflict_years = ifelse(duration == 0, 1, duration))

FinalData <- UNSC_Conflict_Data %>%
  select(conflict_id, location, start_year, end_year, conflict_years)

UNSC_Conflict_Data1 <- UNSC_Conflict_Data_TS %>%
  group_by(conflict_id, location) %>%
  summarize(death_toll = sum(bdeadbes))

FinalData <- FinalData %>%
  left_join(UNSC_Conflict_Data1,
            by = c("conflict_id", "location"))

UNSC_Conflict_Data2 <- UNSC_Conflict_Data_TS %>%
  group_by(conflict_id, location) %>%
  summarize(total_UNSC_res = sum(UNSC_res_sum),
            total_UNSC_unanimous_res = sum(UNSC_unanimous_res_sum)) %>%
  mutate(UNSC_res_dummy = ifelse(total_UNSC_res > 0, 1, 0),
         UNSC_res_unanimous_dummy = ifelse(total_UNSC_unanimous_res == total_UNSC_res &
                                             total_UNSC_unanimous_res > 0, 1, 0))

FinalData <- FinalData %>%
  left_join(UNSC_Conflict_Data2,
            by = c("conflict_id", "location"))  

UNSC_Conflict_Data3 <- UNSC_Conflict_Data_TS %>%
  group_by(conflict_id, location) %>%
  dplyr::slice(which.min(year)) %>% # gives all variables for start year of conflict
  select(conflict_id, location, side_a, side_b, type_of_conflict, incompatibility, territory_name, region, conflict_name, starts_with("al_"), starts_with("mad"), starts_with("atop"), starts_with("vdem")) %>% select(-vdem_elvotbuy, -vdem_gcrrpt)

FinalData <- FinalData %>%
  left_join(UNSC_Conflict_Data3,
            by = c("conflict_id", "location"))
# 207 observations (conflicts) and 43 variables

# write.csv(FinalData,'data/FinalData.csv')

FinalData <- FinalData %>%
  mutate_if(is_character, as_factor) %>%
  select(-X1)

FinalData %>%
  summarise(n = n(), 
            sum_res = sum(total_UNSC_res), sum_unanimous = sum(total_UNSC_unanimous_res)) %>%
  kable(col.names = c("Total Number of Conflicts", 
                      "Total Number of UNSC Resolutions", 
                      "Total Number of Unanimously Adopted UNSC Resolutions"), 
        "latex", booktabs = T) %>% 
  column_spec(1:3, width = "5cm")

# Total Number of Conflicts with No UNSC Resolution vs. UNSC Resolution
FinalData %>%
  count(total_UNSC_res > 0) %>%
  pivot_wider(names_from = "total_UNSC_res > 0", values_from = n) %>%
  kable(col.names = c("No UNSC Resolution", 
                      "UNSC Resolution"), 
        "latex", booktabs = T) %>% 
  column_spec(1:2, width = "2cm") 

# Total Number of Conflicts with No Unanimously Adopted UNSC Resolution vs. Unanimously Adopted UNSC Resolution
FinalData %>%
  count(total_UNSC_unanimous_res > 0) %>%
  pivot_wider(names_from = "total_UNSC_unanimous_res > 0", values_from = n) %>%
  kable(col.names = c("No Unanimously Adopted UNSC Resolution", 
                      "Unanimously Adopted UNSC Resolution"), 
        "latex", booktabs = T) %>% 
  column_spec(1:2, width = "5cm") 

# Conflicts with Ambivalent Support for UNSC Intervention
FinalData %>% 
  mutate(type_of_conflict = case_when(
    type_of_conflict == "1" ~ "Extrasystemic",
    type_of_conflict == "2" ~ "Interstate",
    type_of_conflict == "3" ~ "Internal",
    type_of_conflict == "4" ~ "Internationalized Internal")) %>%
  mutate_if(is.factor, as.character) %>%
  filter(total_UNSC_res > 0) %>%
  filter(!(total_UNSC_unanimous_res > 0)) %>%
  mutate(conflict_name = case_when(
    location == "North Yemen" & is.na(conflict_name) ~ "North Yemen Civil War",
    location == "Laos" & is.na(conflict_name) ~ "Laotian Civil War",
    TRUE ~ conflict_name
  )) %>%
  mutate(start = start_year + 1) %>%
  mutate(start = replace(start, conflict_name=="Corfu Channel Case", 1946)) %>%
  mutate(end = end_year - 1) %>%
  select(conflict_name, total_UNSC_res, start, end, location, death_toll, type_of_conflict) %>%
  kable(col.names = c("Conflict", 
                      "Total UNSC Resolutions", 
                      "Start", 
                      "End", 
                      "Location", 
                      "Death Toll", 
                      "Type of Conflict"), 
        "latex", booktabs = T)  %>% 
  column_spec(2, width = "3cm") %>%
  kable_styling(full_width = FALSE, position = "center", latex_options=c("scale_down","hold_position"), bootstrap_options = "striped")

# UNSC Resolutions on Conflict and Conflict Years
UNSC_Res_Conflict_Years_Data <- FinalData %>%
  mutate(conflict_years = case_when(
    conflict_years < 11 ~ "Less than 11 Conflict Years",
    TRUE ~ "Greater than 11 Conflict Years")) %>%
  filter(total_UNSC_res > 0) %>%
  group_by(conflict_years) %>%
  tally(total_UNSC_res)

UNSC_Res_Conflict_Years_Data$conflict_years <- factor(UNSC_Res_Conflict_Years_Data$conflict_years, levels = c("Less than 11 Conflict Years", "Greater than 11 Conflict Years"))

UNSC_Res_Conflict_Years_Data %>%
  ggplot(mapping = aes(x = conflict_years, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_y_continuous(limits = c(0, 378)) +
  ggtitle("UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Unanimously Adopted UNSC Resolutions on Conflict and Conflict Years
UNSC_Res_Conflict_Years_Data1 <- FinalData %>%
  mutate(conflict_years = case_when(
    conflict_years < 11 ~ "Less than 11 Conflict Years",
    TRUE ~ "Greater than 11 Conflict Years")) %>%
  filter(total_UNSC_unanimous_res > 0) %>%
  group_by(conflict_years) %>%
  tally(total_UNSC_unanimous_res)

UNSC_Res_Conflict_Years_Data1$conflict_years <- factor(UNSC_Res_Conflict_Years_Data1$conflict_years, levels = c("Less than 11 Conflict Years", "Greater than 11 Conflict Years"))

UNSC_Res_Conflict_Years_Data1 %>%
  ggplot(mapping = aes(x = conflict_years, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5) +
  ggtitle("Unanimously Adopted UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 11, face = "bold")) 

# UNSC Resolutions on Conflict and Total Deaths
UNSC_Res_Death_Toll_Data <- FinalData %>%
  mutate(death_toll = case_when(death_toll >= 11236 ~ 'Greater than 11,236 deaths',
                                death_toll >= 1114 ~ 'Between 1,114 and 11,236 deaths',
                                death_toll >= 116 ~ 'Between 116 and 1,114 deaths',
                                TRUE ~ 'Fewer than 116 deaths')) %>%
  filter(total_UNSC_res > 0) %>%
  group_by(death_toll) %>%
  tally(total_UNSC_res)

UNSC_Res_Death_Toll_Data$death_toll <- factor(UNSC_Res_Death_Toll_Data$death_toll, levels = c("Fewer than 116 deaths", "Between 116 and 1,114 deaths", "Between 1,114 and 11,236 deaths", "Greater than 11,236 deaths"))

UNSC_Res_Death_Toll_Data %>%
  ggplot(mapping = aes(x = death_toll, y = n)) +
  geom_col() +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  scale_y_continuous(limits = c(0, 275)) +
  geom_text(aes(label = n), vjust = -0.5) +
  ggtitle("UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Unanimously Adopted UNSC Resolutions on Conflict and Total Deaths
UNSC_Res_Death_Toll_Data1 <- FinalData %>%
  mutate(death_toll = case_when(death_toll >= 11236 ~ 'Greater than 11,236 deaths',
                                death_toll >= 1114 ~ 'Between 1,114 and 11,236 deaths',
                                death_toll >= 116 ~ 'Between 116 and 1,114 deaths',
                                TRUE ~ 'Fewer than 116 deaths')) %>%
  filter(total_UNSC_unanimous_res > 0) %>%
  group_by(death_toll) %>%
  tally(total_UNSC_unanimous_res)

UNSC_Res_Death_Toll_Data1$death_toll <- factor(UNSC_Res_Death_Toll_Data$death_toll, levels = c("Fewer than 116 deaths", "Between 116 and 1,114 deaths", "Between 1,114 and 11,236 deaths", "Greater than 11,236 deaths"))

UNSC_Res_Death_Toll_Data1 %>%
  ggplot(mapping = aes(x = death_toll, y = n)) +
  geom_col() +
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  scale_y_continuous(limits = c(0, 132)) +
  geom_text(aes(label = n), vjust = -0.5) +
  ggtitle("Unanimously Adopted UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 11, face = "bold")) 

# GDP Per Capita of Primary Country and Number of Conflicts
Conflict_GDP_Data <- FinalData %>%
  mutate(mad_gdppc = case_when(mad_gdppc >= 5342 ~ '$5,342 - $27,674 GDP per capita',
                               mad_gdppc >= 2116 ~ '$2,116 - $5,342 GDP per capita',
                               mad_gdppc >= 1228 ~ '$1,228 - $2,116 GDP per capita',
                               TRUE ~ '$511 - $1,228 GDP per capita')) %>%
  group_by(mad_gdppc) %>%
  count(mad_gdppc)
Conflict_GDP_Data

# UNSC Resolutions on Conflict and GDP Per Capita
UNSC_Res_GDP_Data <- FinalData %>%
  mutate(mad_gdppc = case_when(mad_gdppc >= 5342 ~ '$5,342 - $27,674 GDP per capita',
                               mad_gdppc >= 2116 ~ '$2,116 - $5,342 GDP per capita',
                               mad_gdppc >= 1228 ~ '$1,228 - $2,116 GDP per capita',
                               TRUE ~ '$511 - $1,228 GDP per capita')) %>%
  filter(total_UNSC_res > 0) %>%
  group_by(mad_gdppc) %>%
  tally(total_UNSC_res)

UNSC_Res_GDP_Data$mad_gdppc <- factor(UNSC_Res_GDP_Data$mad_gdppc, levels = c("$511 - $1,228 GDP per capita", "$1,228 - $2,116 GDP per capita", "$2,116 - $5,342 GDP per capita", "$5,342 - $27,674 GDP per capita"))

UNSC_Res_GDP_Data %>%
  ggplot(mapping = aes(x = mad_gdppc, y = n)) +
  geom_col() +
  scale_x_discrete(guide = guide_axis(n.dodge=4)) +
  scale_y_continuous(limits = c(0, 260)) +
  geom_text(aes(label = n), vjust = -0.5) +
  ggtitle("UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 14, face = "bold"), axis.text.x = element_text(size = 8))

# Unanimously Adopted UNSC Resolutions on Conflict and GDP Per Capita
UNSC_Res_GDP_Data1 <- FinalData %>%
  mutate(mad_gdppc = case_when(mad_gdppc >= 5342 ~ '$5,342 and $27,674 GDP per capita',
                               mad_gdppc >= 2116 ~ '$2,116 and $5,342 GDP per capita',
                               mad_gdppc >= 1228 ~ '$1,228 and $2,116 GDP per capita',
                               TRUE ~ '$511 and $1,228 GDP per capita')) %>%
  filter(total_UNSC_unanimous_res > 0) %>%
  group_by(mad_gdppc) %>%
  tally(total_UNSC_unanimous_res)

UNSC_Res_GDP_Data1$mad_gdppc <- factor(UNSC_Res_GDP_Data1$mad_gdppc, levels = c("$511 and $1,228 GDP per capita", "$1,228 and $2,116 GDP per capita", "$2,116 and $5,342 GDP per capita", "$5,342 and $27,674 GDP per capita"))

UNSC_Res_GDP_Data1 %>%
  ggplot(mapping = aes(x = mad_gdppc, y = n)) +
  geom_col() +
  scale_x_discrete(guide = guide_axis(n.dodge=4)) +
  scale_y_continuous(limits = c(0, 106)) +
  geom_text(aes(label = n), vjust = -0.5) +
  ggtitle("Unanimously Adopted UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 11, face = "bold"), axis.text.x = element_text(size = 8)) 

# Number of Military Alliances of Primary Country and Number of Conflicts
Conflict_Allies_Data <- FinalData %>%
  mutate(atop_number = case_when(atop_number >= 10 ~ 'Ten Alliances',
                                 atop_number >= 9 ~ 'Nine Alliances',
                                 atop_number >= 8 ~ 'Eight Alliances',
                                 atop_number >= 7 ~ 'Seven Alliances',
                                 atop_number >= 6 ~ 'Six Alliances',
                                 atop_number >= 5 ~ 'Five Alliances',
                                 atop_number >= 4 ~ 'Four Alliances',
                                 atop_number >= 3 ~ 'Three Alliances',
                                 atop_number >= 2 ~ 'Two Alliances',
                                 TRUE ~ 'One Alliance')) %>%
  group_by(atop_number) %>%
  count(atop_number)
Conflict_Allies_Data

# UNSC Resolutions on Conflict and Military Alliances
UNSC_Res_Allies_Data <- FinalData %>%
  mutate(atop_number = case_when(atop_number >= 10 ~ 'Ten Alliances',
                                 atop_number >= 9 ~ 'Nine Alliances',
                                 atop_number >= 8 ~ 'Eight Alliances',
                                 atop_number >= 7 ~ 'Seven Alliances',
                                 atop_number >= 6 ~ 'Six Alliances',
                                 atop_number >= 5 ~ 'Five Alliances',
                                 atop_number >= 4 ~ 'Four Alliances',
                                 atop_number >= 3 ~ 'Three Alliances',
                                 atop_number >= 2 ~ 'Two Alliances',
                                 TRUE ~ 'One Alliance')) %>%
  filter(total_UNSC_res > 0) %>%
  group_by(atop_number) %>%
  tally(total_UNSC_res)

UNSC_Res_Allies_Data$atop_number <- factor(UNSC_Res_Allies_Data$atop_number, 
                                           levels = c("One Alliance", 
                                                      "Two Alliances", 
                                                      "Three Alliances", 
                                                      "Four Alliances",
                                                      "Five Alliances",
                                                      "Six Alliances",
                                                      "Seven Alliances",
                                                      "Eight Alliances",
                                                      "Nine Alliances",
                                                      "Ten Alliances"))

UNSC_Res_Allies_Data %>%
  ggplot(mapping = aes(x = atop_number, y = n)) +
  geom_col() +
  scale_x_discrete(guide = guide_axis(n.dodge=4), drop=FALSE) +
  scale_y_continuous(limits = c(0, 405)) +
  geom_text(aes(label = n), vjust = -0.5) +
  ggtitle("UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 14, face = "bold"), axis.text.x = element_text(size = 7), panel.grid.major = element_blank())

# Unanimously Adopted UNSC Resolutions on Conflict and Military Alliances
UNSC_Res_Allies_Data1 <- FinalData %>%
  mutate(atop_number = case_when(atop_number >= 10 ~ 'Ten Alliances',
                                 atop_number >= 9 ~ 'Nine Alliances',
                                 atop_number >= 8 ~ 'Eight Alliances',
                                 atop_number >= 7 ~ 'Seven Alliances',
                                 atop_number >= 6 ~ 'Six Alliances',
                                 atop_number >= 5 ~ 'Five Alliances',
                                 atop_number >= 4 ~ 'Four Alliances',
                                 atop_number >= 3 ~ 'Three Alliances',
                                 atop_number >= 2 ~ 'Two Alliances',
                                 TRUE ~ 'One Alliance')) %>%
  filter(total_UNSC_unanimous_res > 0) %>%
  group_by(atop_number) %>%
  tally(total_UNSC_unanimous_res)

UNSC_Res_Allies_Data1$atop_number <- factor(UNSC_Res_Allies_Data1$atop_number, 
                                            levels = c("One Alliance", 
                                                       "Two Alliances", 
                                                       "Three Alliances", 
                                                       "Four Alliances",
                                                       "Five Alliances",
                                                       "Six Alliances",
                                                       "Seven Alliances",
                                                       "Eight Alliances",
                                                       "Nine Alliances",
                                                       "Ten Alliances"))

UNSC_Res_Allies_Data1 %>%
  ggplot(mapping = aes(x = atop_number, y = n)) +
  geom_col() +
  scale_x_discrete(guide = guide_axis(n.dodge=4), drop=FALSE) +
  scale_y_continuous(limits = c(0, 173)) +
  geom_text(aes(label = n), vjust = -0.5) +
  ggtitle("Unanimously Adopted UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 11, face = "bold"), axis.text.x = element_text(size = 7), panel.grid.major = element_blank())

# Type of Conflict and Number of Conflicts
Conflict_Type_Data <- FinalData %>%
  mutate(type_of_conflict = case_when(
    type_of_conflict == "1" ~ "Extrasystemic",
    type_of_conflict == "2" ~ "Interstate",
    type_of_conflict == "3" ~ "Internal",
    type_of_conflict == "4" ~ "Internationalized Internal")) %>%
  group_by(type_of_conflict) %>%
  count(type_of_conflict)
Conflict_Type_Data

# UNSC Resolutions on Conflict and Type of Conflict
UNSC_Res_Conflict_Type_Data <- FinalData %>%
  mutate(type_of_conflict = case_when(
    type_of_conflict == "1" ~ "Extrasystemic",
    type_of_conflict == "2" ~ "Interstate",
    type_of_conflict == "3" ~ "Internal",
    type_of_conflict == "4" ~ "Internationalized Internal")) %>%
  filter(total_UNSC_res > 0) %>%
  group_by(type_of_conflict) %>%
  tally(total_UNSC_res)

UNSC_Res_Conflict_Type_Data %>%
  ggplot(mapping = aes(x = type_of_conflict, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5) +
  ggtitle("UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Unanimously Adopted UNSC Resolutions on Conflict and Type of Conflict
UNSC_Res_Conflict_Type_Data1 <- FinalData %>%
  mutate(type_of_conflict = case_when(
    type_of_conflict == "1" ~ "Extrasystemic",
    type_of_conflict == "2" ~ "Interstate",
    type_of_conflict == "3" ~ "Internal",
    type_of_conflict == "4" ~ "Internationalized Internal")) %>%
  filter(total_UNSC_unanimous_res > 0) %>%
  group_by(type_of_conflict) %>%
  tally(total_UNSC_unanimous_res)

UNSC_Res_Conflict_Type_Data1 %>%
  ggplot(mapping = aes(x = type_of_conflict, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5) +
  ggtitle("Unanimously Adopted UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 11, face = "bold")) 

# Conflict Issue and Number of Conflicts
Conflict_Incompatibility_Data <- FinalData %>%
  mutate(incompatibility = case_when(
    incompatibility == "1" ~ "Dispute over Territory",
    incompatibility == "2" ~ "Dispute over Government",
    incompatibility == "3" ~ "Dispute over both")) %>%
  group_by(incompatibility) %>%
  count(incompatibility)
Conflict_Incompatibility_Data

# UNSC Resolutions on Conflict and Conflict Issue
UNSC_Res_Incompatibility_Data <- FinalData %>%
  mutate(incompatibility = case_when(
    incompatibility == "1" ~ "Dispute over Territory",
    incompatibility == "2" ~ "Dispute over Government",
    incompatibility == "3" ~ "Dispute over both")) %>%
  filter(total_UNSC_res > 0) %>%
  group_by(incompatibility) %>%
  tally(total_UNSC_res)

UNSC_Res_Incompatibility_Data$incompatibility <- factor(UNSC_Res_Incompatibility_Data$incompatibility, levels = c("Dispute over Government", "Dispute over Territory", "Dispute over both"))

UNSC_Res_Incompatibility_Data %>%
  ggplot(mapping = aes(x = incompatibility, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5) +
  ggtitle("UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Unanimously Adopted UNSC Resolutions on Conflict and Conflict Issue
UNSC_Res_Incompatibility_Data1 <- FinalData %>%
  mutate(incompatibility = case_when(
    incompatibility == "1" ~ "Dispute over Territory",
    incompatibility == "2" ~ "Dispute over Government",
    incompatibility == "3" ~ "Dispute over both")) %>%
  filter(total_UNSC_unanimous_res > 0) %>%
  group_by(incompatibility) %>%
  tally(total_UNSC_unanimous_res)

UNSC_Res_Incompatibility_Data1$incompatibility <- factor(UNSC_Res_Incompatibility_Data$incompatibility, levels = c("Dispute over Government", "Dispute over Territory", "Dispute over both"))

UNSC_Res_Incompatibility_Data1 %>%
  ggplot(mapping = aes(x = incompatibility, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5) +
  ggtitle("Unanimously Adopted UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 11, face = "bold")) 

# Region and Number of Conflicts
Conflict_Region_Data <- FinalData %>%
  mutate(region = case_when(
    region == "1" ~ "Europe",
    region == "2" ~ "Middle East",
    region == "3" ~ "Asia",
    region == "4" ~ "Africa",
    region == "5" ~ "Americas")) %>%
  group_by(region) %>%
  count(region)
Conflict_Region_Data

# UNSC Resolutions on Conflict and Region
UNSC_Res_Region_Data <- FinalData %>%
  mutate(region = case_when(
    region == "1" ~ "Europe",
    region == "2" ~ "Middle East",
    region == "3" ~ "Asia",
    region == "4" ~ "Africa",
    region == "5" ~ "Americas")) %>%
  filter(total_UNSC_res > 0) %>%
  group_by(region) %>%
  tally(total_UNSC_res)

UNSC_Res_Region_Data %>%
  ggplot(mapping = aes(x = region, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5) +
  ggtitle("UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Unanimously Adopted UNSC Resolutions on Conflict and Region
UNSC_Res_Region_Data1 <- FinalData %>%
  mutate(region = case_when(
    region == "1" ~ "Europe",
    region == "2" ~ "Middle East",
    region == "3" ~ "Asia",
    region == "4" ~ "Africa",
    region == "5" ~ "Americas")) %>%
  filter(total_UNSC_unanimous_res > 0) %>%
  group_by(region) %>%
  tally(total_UNSC_unanimous_res)

UNSC_Res_Region_Data1 %>%
  ggplot(mapping = aes(x = region, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5) +
  ggtitle("Unanimously Adopted UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 11, face = "bold")) 

# UNSC Resolutions on Conflict and Political Corruption
UNSC_Res_Corruption_Data <- FinalData %>%
  filter(total_UNSC_res > 0)

UNSC_Res_Corruption_Data %>%
  ggplot(mapping = aes(x = vdem_corr, y = total_UNSC_res)) +
  geom_point() +
  ggtitle("UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "Political Corruption", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Unanimously Adopted UNSC Resolutions on Conflict and Political Corruption
UNSC_Res_Corruption_Data1 <- FinalData %>%
  filter(total_UNSC_unanimous_res > 0)

UNSC_Res_Corruption_Data1 %>%
  ggplot(mapping = aes(x = vdem_corr, y = total_UNSC_unanimous_res)) +
  geom_point() +
  ggtitle("Unanimously Adopted UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "Political Corruption", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 11, face = "bold")) 

# UNSC Resolutions on Conflict and Political Regime
UNSC_Res_Democracy_Data <- FinalData %>%
  filter(total_UNSC_res > 0)

UNSC_Res_Democracy_Data %>%
  ggplot(mapping = aes(x = vdem_libdem, y = total_UNSC_res)) +
  geom_point() +
  ggtitle("UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "Liberal Democracy", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Unanimously Adopted UNSC Resolutions on Conflict and Political Regime
UNSC_Res_Democracy_Data1 <- FinalData %>%
  filter(total_UNSC_unanimous_res > 0)

UNSC_Res_Democracy_Data1 %>%
  ggplot(mapping = aes(x = vdem_libdem, y = total_UNSC_unanimous_res)) +
  geom_point() +
  ggtitle("Unanimously Adopted UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "Liberal Democracy", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 11, face = "bold"))

# UNSC Resolutions on Conflict and Ethnic Fractionalization
UNSC_Res_Ethnic_Data <- FinalData %>%
  filter(total_UNSC_res > 0)

UNSC_Res_Ethnic_Data %>%
  ggplot(mapping = aes(x = al_ethnic, y = total_UNSC_res)) +
  geom_point() +
  ggtitle("UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "Ethnic Fractionalization", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Unanimously Adopted UNSC Resolutions on Conflict and Ethnic Fractionalization
UNSC_Res_Ethnic_Data1 <- FinalData %>%
  filter(total_UNSC_unanimous_res > 0)

UNSC_Res_Ethnic_Data1 %>%
  ggplot(mapping = aes(x = al_ethnic, y = total_UNSC_unanimous_res)) +
  geom_point() +
  ggtitle("Unanimously Adopted UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "Ethnic Fractionalization", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 11, face = "bold")) 

# UNSC Resolutions on Conflict and Linguistic Fractionalization
UNSC_Res_Language_Data <- FinalData %>%
  filter(total_UNSC_res > 0)

UNSC_Res_Language_Data %>%
  ggplot(mapping = aes(x = al_language, y = total_UNSC_res)) +
  geom_point() +
  ggtitle("UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "Linguistic Fractionalization", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Unanimously Adopted UNSC Resolutions on Conflict and Linguistic Fractionalization
UNSC_Res_Language_Data1 <- FinalData %>%
  filter(total_UNSC_unanimous_res > 0)

UNSC_Res_Language_Data1 %>%
  ggplot(mapping = aes(x = al_language, y = total_UNSC_unanimous_res)) +
  geom_point() +
  ggtitle("Unanimously Adopted UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "Linguistic Fractionalization", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 11, face = "bold")) 

# UNSC Resolutions on Conflict and Religious Fractionalization
UNSC_Res_Religion_Data <- FinalData %>%
  filter(total_UNSC_res > 0)

UNSC_Res_Religion_Data %>%
  ggplot(mapping = aes(x = al_religion, y = total_UNSC_res)) +
  geom_point() +
  ggtitle("UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "Religious Fractionalization", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# Unanimously Adopted UNSC Resolutions on Conflict and Religious Fractionalization
UNSC_Res_Religion_Data1 <- FinalData %>%
  filter(total_UNSC_unanimous_res > 0)

UNSC_Res_Religion_Data1 %>%
  ggplot(mapping = aes(x = al_religion, y = total_UNSC_unanimous_res)) +
  geom_point() +
  ggtitle("Unanimously Adopted UNSC Resolutions on Armed Conflict during Cold War") +
  labs(x = "Religious Fractionalization", y = "Number of UNSC Resolutions") +
  theme_economist_white() + 
  scale_colour_economist() +
  theme(plot.title = element_text(size = 11, face = "bold")) 

# Correlation with UNSC_res_dummy
cor_with_UNSC_res <- FinalData %>%
  select(UNSC_res_dummy, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal, atop_number) %>%
  na.omit() %>% 
  corrr::correlate() %>%
  corrr::focus(UNSC_res_dummy)

cor_with_UNSC_res %>%
  arrange(desc(UNSC_res_dummy)) %>%
  kable() %>%
  kable_styling(position = "center")

cor_with_UNSC_res %>%
  mutate(term = factor(term, levels = term[order(UNSC_res_dummy)])) %>%  # Order by correlation strength
  ggplot(aes(x = term, y = UNSC_res_dummy)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with UNSC_res_dummy") +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=14,face="bold")) +
  coord_flip()

# Correlation with UNSC_res_unanimous_dummy
cor_with_UNSC_res <- FinalData %>%
  select(UNSC_res_unanimous_dummy, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal, atop_number) %>%
  na.omit() %>% 
  corrr::correlate() %>%
  corrr::focus(UNSC_res_unanimous_dummy)

cor_with_UNSC_res %>%
  arrange(desc(UNSC_res_unanimous_dummy)) %>%
  kable() %>%
  kable_styling(position = "center")

cor_with_UNSC_res %>%
  mutate(term = factor(term, levels = term[order(UNSC_res_unanimous_dummy)])) %>%
  ggplot(aes(x = term, y = UNSC_res_unanimous_dummy)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with UNSC_res_unanimous_dummy") +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=14,face="bold")) +
  coord_flip()

# Correlation with UNSC_res_dummy WITHOUT na.omit()
cor_with_UNSC_res_with_NA <- FinalData %>%
  select(UNSC_res_dummy, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal, atop_number) %>%
  #  na.omit() %>% 
  corrr::correlate() %>%
  corrr::focus(UNSC_res_dummy)

cor_with_UNSC_res_with_NA %>%
  arrange(desc(UNSC_res_dummy)) %>%
  kable() %>%
  kable_styling(position = "center")

cor_with_UNSC_res_with_NA %>%
  mutate(term = factor(term, levels = term[order(UNSC_res_dummy)])) %>%  # Order by correlation strength
  ggplot(aes(x = term, y = UNSC_res_dummy)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with UNSC_res_dummy") +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=14,face="bold")) +
  coord_flip()

# Correlation with UNSC_res_unanimous_dummy WITHOUT na.omit()
cor_with_UNSC_res_with_NA <- FinalData %>%
  select(UNSC_res_unanimous_dummy, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal, atop_number) %>%
  #  na.omit() %>% 
  corrr::correlate() %>%
  corrr::focus(UNSC_res_unanimous_dummy)

cor_with_UNSC_res_with_NA %>%
  arrange(desc(UNSC_res_unanimous_dummy)) %>%
  kable() %>%
  kable_styling(position = "center")

cor_with_UNSC_res_with_NA %>%
  mutate(term = factor(term, levels = term[order(UNSC_res_unanimous_dummy)])) %>%
  ggplot(aes(x = term, y = UNSC_res_unanimous_dummy)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with UNSC_res_unanimous_dummy") +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=14,face="bold")) +
  coord_flip()

# Data without outlier ISRAELI-PALESTINIAN CONFLICT (conflict id 234)
NoOutlierData <- FinalData %>%
  filter(total_UNSC_res < 158)

# Correlation with UNSC_res_dummy
cor_with_UNSC_res_1 <- NoOutlierData %>%
  select(UNSC_res_dummy, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal, atop_number) %>%
  na.omit() %>% 
  corrr::correlate() %>%
  corrr::focus(UNSC_res_dummy)

cor_with_UNSC_res_1 %>%
  arrange(desc(UNSC_res_dummy)) %>%
  kable() %>%
  kable_styling(position = "center")

cor_with_UNSC_res_1 %>%
  mutate(term = factor(term, levels = term[order(UNSC_res_dummy)])) %>%  # Order by correlation strength
  ggplot(aes(x = term, y = UNSC_res_dummy)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with UNSC_res_dummy (no Israeli-Palestinian Conflict)") +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=10,face="bold")) +
  coord_flip()

# Data without outlier ISRAELI-PALESTINIAN CONFLICT (conflict id 234) WITHOUT na.omit()

# Correlation with UNSC_res_dummy
cor_with_UNSC_res_with_NA_1 <- NoOutlierData %>%
  select(UNSC_res_dummy, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal, atop_number) %>%
  #  na.omit() %>% 
  corrr::correlate() %>%
  corrr::focus(UNSC_res_dummy)

cor_with_UNSC_res_with_NA_1 %>%
  arrange(desc(UNSC_res_dummy)) %>%
  kable() %>%
  kable_styling(position = "center")

cor_with_UNSC_res_with_NA_1 %>%
  mutate(term = factor(term, levels = term[order(UNSC_res_dummy)])) %>%  # Order by correlation strength
  ggplot(aes(x = term, y = UNSC_res_dummy)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with UNSC_res_dummy (no Israeli-Palestinian Conflict)") +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=10,face="bold")) +
  coord_flip()

# Data without outlier ISRAELI-PALESTINIAN CONFLICT (conflict id 234) 

# Correlation with UNSC_res_unanimous_dummy
cor_with_UNSC_res_1 <- NoOutlierData %>%
  select(UNSC_res_unanimous_dummy, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal, atop_number) %>%
  na.omit() %>% 
  corrr::correlate() %>%
  corrr::focus(UNSC_res_unanimous_dummy)

cor_with_UNSC_res_1 %>%
  arrange(desc(UNSC_res_unanimous_dummy)) %>%
  kable() %>%
  kable_styling(position = "center")

cor_with_UNSC_res_1 %>%
  mutate(term = factor(term, levels = term[order(UNSC_res_unanimous_dummy)])) %>%
  ggplot(aes(x = term, y = UNSC_res_unanimous_dummy)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with UNSC_res_unanimous_dummy (no Israeli-Palestinian Conflict)") +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=10,face="bold")) +
  coord_flip()

# Data without outlier ISRAELI-PALESTINIAN CONFLICT (conflict id 234) WITHOUT na.omit()

# Correlation with UNSC_res_unanimous_dummy
cor_with_UNSC_res_with_NA_1 <- NoOutlierData %>%
  select(UNSC_res_unanimous_dummy, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal, atop_number) %>%
  #  na.omit() %>% 
  corrr::correlate() %>%
  corrr::focus(UNSC_res_unanimous_dummy)

cor_with_UNSC_res_with_NA_1 %>%
  arrange(desc(UNSC_res_unanimous_dummy)) %>%
  kable() %>%
  kable_styling(position = "center")

cor_with_UNSC_res_with_NA_1 %>%
  mutate(term = factor(term, levels = term[order(UNSC_res_unanimous_dummy)])) %>%
  ggplot(aes(x = term, y = UNSC_res_unanimous_dummy)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with UNSC_res_unanimous_dummy (no Israeli-Palestinian Conflict)") +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=10,face="bold")) +
  coord_flip()

# Data without outlier VIETNAM WAR (conflict id 293)
NoVietnamData <- FinalData %>%
  filter(death_toll < 1461050)

# Correlation with UNSC_res_dummy
cor_with_UNSC_res_2 <- NoVietnamData %>%
  select(UNSC_res_dummy, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal, atop_number) %>%
  na.omit() %>% 
  corrr::correlate() %>%
  corrr::focus(UNSC_res_dummy)

cor_with_UNSC_res_2 %>%
  arrange(desc(UNSC_res_dummy)) %>%
  kable() %>%
  kable_styling(position = "center")

cor_with_UNSC_res_2 %>%
  mutate(term = factor(term, levels = term[order(UNSC_res_dummy)])) %>%  # Order by correlation strength
  ggplot(aes(x = term, y = UNSC_res_dummy)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with UNSC_res_dummy (no Vietnam War)") +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=10,face="bold")) +
  coord_flip()

# Data without outlier VIETNAM WAR (conflict id 293) WITHOUT na.omit()

# Correlation with UNSC_res_dummy
cor_with_UNSC_res_with_NA_2 <- NoVietnamData %>%
  select(UNSC_res_dummy, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal, atop_number) %>%
  #  na.omit() %>% 
  corrr::correlate() %>%
  corrr::focus(UNSC_res_dummy)

cor_with_UNSC_res_with_NA_2 %>%
  arrange(desc(UNSC_res_dummy)) %>%
  kable() %>%
  kable_styling(position = "center")

cor_with_UNSC_res_with_NA_2 %>%
  mutate(term = factor(term, levels = term[order(UNSC_res_dummy)])) %>%  # Order by correlation strength
  ggplot(aes(x = term, y = UNSC_res_dummy)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with UNSC_res_dummy (no Vietnam War)") +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=10,face="bold")) +
  coord_flip()

# Data without outlier VIETNAM WAR (conflict id 293) 

# Correlation with UNSC_res_unanimous_dummy
cor_with_UNSC_res_2 <- NoVietnamData %>%
  select(UNSC_res_unanimous_dummy, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal, atop_number) %>%
  na.omit() %>% 
  corrr::correlate() %>%
  corrr::focus(UNSC_res_unanimous_dummy)

cor_with_UNSC_res_2 %>%
  arrange(desc(UNSC_res_unanimous_dummy)) %>%
  kable() %>%
  kable_styling(position = "center")
 
cor_with_UNSC_res_2 %>%
  mutate(term = factor(term, levels = term[order(UNSC_res_unanimous_dummy)])) %>%
  ggplot(aes(x = term, y = UNSC_res_unanimous_dummy)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with UNSC_res_unanimous_dummy (no Vietnam War)") +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=10,face="bold")) +
  coord_flip()

# Data without outlier VIETNAM WAR (conflict id 293) WITHOUT na.omit()

# Correlation with UNSC_res_unanimous_dummy
cor_with_UNSC_res_with_NA_2 <- NoVietnamData %>%
  select(UNSC_res_unanimous_dummy, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal, atop_number) %>%
  #  na.omit() %>% 
  corrr::correlate() %>%
  corrr::focus(UNSC_res_unanimous_dummy)

cor_with_UNSC_res_with_NA_2 %>%
  arrange(desc(UNSC_res_unanimous_dummy)) %>%
  kable() %>%
  kable_styling(position = "center")

cor_with_UNSC_res_with_NA_2 %>%
  mutate(term = factor(term, levels = term[order(UNSC_res_unanimous_dummy)])) %>%
  ggplot(aes(x = term, y = UNSC_res_unanimous_dummy)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with UNSC_res_unanimous_dummy (no Vietnam War)") +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=10,face="bold")) +
  coord_flip()

# Data without outlier VIETNAM WAR (conflict id 293) and CHINESE CIVIL WAR (conflict id 202)
NoVietnamChinaData <- FinalData %>%
  filter(death_toll < 1200000)

# Correlation with UNSC_res_dummy
cor_with_UNSC_res_3 <- NoVietnamChinaData %>%
  select(UNSC_res_dummy, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal, atop_number) %>%
  na.omit() %>% 
  corrr::correlate() %>%
  corrr::focus(UNSC_res_dummy)

cor_with_UNSC_res_3 %>%
  arrange(desc(UNSC_res_dummy)) %>%
  kable() %>%
  kable_styling(position = "center")

cor_with_UNSC_res_3 %>%
  mutate(term = factor(term, levels = term[order(UNSC_res_dummy)])) %>%  # Order by correlation strength
  ggplot(aes(x = term, y = UNSC_res_dummy)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with UNSC_res_dummy (no Vietnam or Chinese Civil War)") +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=10,face="bold")) +
  coord_flip()

# Data without outlier VIETNAM WAR (conflict id 293) and CHINESE CIVIL WAR (conflict id 202) WITHOUT na.omit()

# Correlation with UNSC_res_dummy
cor_with_UNSC_res_with_NA_3 <- NoVietnamChinaData %>%
  select(UNSC_res_dummy, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal, atop_number) %>%
  #  na.omit() %>% 
  corrr::correlate() %>%
  corrr::focus(UNSC_res_dummy)

cor_with_UNSC_res_with_NA_3 %>%
  arrange(desc(UNSC_res_dummy)) %>%
  kable() %>%
  kable_styling(position = "center")

cor_with_UNSC_res_with_NA_3 %>%
  mutate(term = factor(term, levels = term[order(UNSC_res_dummy)])) %>%  # Order by correlation strength
  ggplot(aes(x = term, y = UNSC_res_dummy)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with UNSC_res_dummy (no Vietnam or Chinese Civil War)") +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=10,face="bold")) +
  coord_flip()

# Data without outlier VIETNAM WAR (conflict id 293) and CHINESE CIVIL WAR (conflict id 202)

# Correlation with UNSC_res_unanimous_dummy
cor_with_UNSC_res_3 <- NoVietnamChinaData %>%
  select(UNSC_res_unanimous_dummy, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal, atop_number) %>%
  na.omit() %>% 
  corrr::correlate() %>%
  corrr::focus(UNSC_res_unanimous_dummy)

cor_with_UNSC_res_3 %>%
  arrange(desc(UNSC_res_unanimous_dummy)) %>%
  kable() %>%
  kable_styling(position = "center")

cor_with_UNSC_res_3 %>%
  mutate(term = factor(term, levels = term[order(UNSC_res_unanimous_dummy)])) %>%
  ggplot(aes(x = term, y = UNSC_res_unanimous_dummy)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with UNSC_res_unanimous_dummy (no Vietnam or Chinese Civil War)") +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=9,face="bold")) +
  coord_flip()

# Data without outlier VIETNAM WAR (conflict id 293) and CHINESE CIVIL WAR (conflict id 202) WITHOUT na.omit()

# Correlation with UNSC_res_unanimous_dummy
cor_with_UNSC_res_with_NA_3 <- NoVietnamChinaData %>%
  select(UNSC_res_unanimous_dummy, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal, atop_number) %>%
  #  na.omit() %>% 
  corrr::correlate() %>%
  corrr::focus(UNSC_res_unanimous_dummy)

cor_with_UNSC_res_with_NA_3 %>%
  arrange(desc(UNSC_res_unanimous_dummy)) %>%
  kable() %>%
  kable_styling(position = "center")

cor_with_UNSC_res_with_NA_3 %>%
  mutate(term = factor(term, levels = term[order(UNSC_res_unanimous_dummy)])) %>%
  ggplot(aes(x = term, y = UNSC_res_unanimous_dummy)) +
  geom_bar(stat = "identity") +
  ylab("Correlation with UNSC_res_unanimous_dummy (no Vietnam or Chinese Civil War)") +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=9,face="bold")) +
  coord_flip()

# K-Means Clustering

set.seed(27182) # Used digits from e

# Extract within-cluster SS from K-means object
get_within_ss <- function(kmean_obj){
  return(kmean_obj$tot.withinss)
}

# Get cluster labels for the data
get_cluster <- function(x, clust_obj){
  
  if(class(clust_obj) == "kmeans"){
    clust = clust_obj$cluster
  } else {
    clust = clust_obj
  }
  
  out = x %>% 
    mutate(cluster = clust)
  
  return(out)
}

# Plot data with cluster labels
plot_cluster <- function(cluster_dat, title){
  
  plt = ggplot(cluster_dat) + 
    geom_point(aes(x1, x2, color = factor(cluster)))
  
  return(plt)
}

# Add labels to plots
label_plot <- function(plot, title){
  
  return(plot + labs(title = title))
}

set.seed(27182) # Used digits from e

FinalData1 <- FinalData %>%
  select(conflict_id, UNSC_res_dummy, location, total_UNSC_res, region, type_of_conflict, conflict_years, death_toll, mad_gdppc, vdem_corr, vdem_liberal, atop_number, al_ethnic, al_language, al_religion, incompatibility, vdem_egal) %>%
  na.omit()

FinalData2 <- FinalData %>%
  select(total_UNSC_res, UNSC_res_dummy, region, type_of_conflict, conflict_years, death_toll, mad_gdppc, vdem_corr, vdem_liberal, atop_number, al_ethnic, al_language, al_religion, incompatibility, vdem_egal) %>%
  na.omit()

dat_kmeans = tibble(xmat = list(FinalData2)) %>%
  crossing(nclust = 2:6)

dat_kmeans = dat_kmeans %>% 
  mutate(kmean = map2(xmat, nclust, kmeans, nstart=20), # Fit K-means
         within_ss = map_dbl(kmean, get_within_ss), # Get within-cluster SS
         clusters = map2(xmat, kmean, get_cluster), # Get DFs with cluster affiliation
         plots = map(clusters, plot_cluster)) # Plot clusters

dat_kmeans %>% select(nclust, within_ss) %>% 
  kable()

wss <- dat_kmeans %>%
  select(nclust, within_ss)

ggplot(wss) + 
  geom_line(aes(nclust, within_ss))
# four clusters are optimal

dat_kmeans$clusters[[3]] %>%
  ggplot() +
  geom_point(aes(x = total_UNSC_res, y = death_toll, color = factor(cluster))) +
  geom_hline(yintercept = 31000) +
  geom_hline(yintercept = 300000) +
  geom_hline(yintercept = 550000)

# Conflicts during the Cold War by Death Toll (four groups)
Data_to_Plot_1 <- FinalData %>%
  select(UNSC_res_dummy, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal, atop_number) %>%
  mutate(category=cut(death_toll, breaks=c(-Inf, 100, 1000, 5000, Inf), 
                      labels=c("less than 100","100-1000","1000-5000", "greater than 5000")))  %>%
  mutate(UNSC_res_dummy = case_when(
    UNSC_res_dummy == "0" ~ "No",
    UNSC_res_dummy == "1" ~ "Yes"
  )) %>%
  count(category, UNSC_res_dummy, sort = TRUE)

Data_to_Plot_1 %>%
  ggplot(mapping = aes(category, n, fill = factor(UNSC_res_dummy))) +
  geom_col() +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5), size = 2.5) +
  ggtitle("Conflicts during the Cold War by Death Toll") +
  scale_fill_discrete(name = "Was there a UNSC resolution?") +
  labs(x = "Total Number of Deaths", y = "Count", fill = "UNSC resolution") +
  scale_fill_brewer(palette="Greys") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 5, b = 0, l = 0), size=11,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0), size=11,face="bold"))

# CLASSIFICATION MODELING SECTION

# CLEAN FINAL DATA FOR MODELING
FinalData <- read_csv("~/FinalData.csv")

FinalData <- FinalData %>%
  mutate_if(is_character, as_factor) %>%
  select(-X1)

set.seed(27182) # Used digits from e

ModelData <- FinalData %>%
  select(conflict_id, location, start_year, end_year, total_UNSC_res, total_UNSC_unanimous_res, UNSC_res_dummy, UNSC_res_unanimous_dummy, side_a, side_b, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal)

ModelData <- ModelData %>%
  na.omit() %>%
  mutate(intervention = as.factor(ifelse(UNSC_res_dummy == 0, "No", "Yes")), 
         # add column for 'intervention'
         intervention = fct_relevel(intervention, "Yes")) %>%
  mutate(unanimous_intervention = as.factor(ifelse(UNSC_res_unanimous_dummy == 0, "No", "Yes")), 
         # add column for 'unanimous_intervention'
         unanimous_intervention = fct_relevel(unanimous_intervention, "Yes"))
# 115 observations rather than 207 observations due to missingness

# SPLIT DATA INTO TRAINING SET AND TEST SET
ModelData <- ModelData %>% 
  mutate(id = row_number())

# Create training set
conflicts_train <- ModelData %>% sample_frac(.70)
# Create test set
conflicts_test  <- anti_join(ModelData, conflicts_train, by = 'id')

conflicts_split <- tibble(train = conflicts_train %>% list(),
                          test  = conflicts_test %>% list())

# Classification Modeling - Outcome: UNSC_res_dummy

# LOGISTIC REGRESSION WITH 10-FOLD CROSS VALIDATION

set.seed(27182) # Used digits from e

# Function to calculate misclassification rate
error_rate_glm <- function(data, model){
  data %>% 
    mutate(pred_prob = predict(model, newdata = data, type = "response"),
           pred_intervention = if_else(pred_prob > 0.5, 1, 0),
           error = pred_intervention != UNSC_res_dummy) %>% 
    pull(error) %>% 
    mean()
}

# Set up candidate model formula
fmla <- c(
  "UNSC_res_dummy ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal"
) %>% map(as.formula)
model_setup <- tibble(method = str_c("logistic_model"),
                      fmla = fmla)

# Fit logistic model
conflicts_logistic_model <- conflicts_split %>%
  crossing(model_setup) %>%
  mutate(
    model_fit = map2(fmla, train, glm, family = "binomial"),
    test_misclass = map2_dbl(test, model_fit, error_rate_glm)
  )

# Test misclassification error for logistic model
logistic_misclass <- conflicts_logistic_model %>%
  group_by(method) %>%
  select(method, test_misclass)
logistic_misclass %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

# RIDGE LOGISTIC REGRESSION WITH 10-FOLD CROSS VALIDATION

set.seed(27182) # Used digits from e

# Lambda grid
lambda_grid <- 10^seq(-2, 10, length = 200)

ridge_cv <- conflicts_split %>%
  pluck("train", 1) %>%
  glmnetUtils::cv.glmnet(
    formula = UNSC_res_dummy ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal,
    data = .,
    alpha = 0, # for ridge regression
    nfolds = 10,
    lambda = lambda_grid
  )

# Check plot for lambdas
plot(ridge_cv)

# Ridge's best lambdas
ridge_lambda_min <- ridge_cv$lambda.min
ridge_lambda_min 
ridge_lambda_1se <- ridge_cv$lambda.1se
ridge_lambda_1se

# Test error for ridge regression
conflicts_glmnet <- conflicts_split %>%
  mutate(
    ridge_min = map(train, ~ glmnetUtils::glmnet(UNSC_res_dummy ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal,
                                                 data = .x,
                                                 alpha = 0,
                                                 lambda = ridge_lambda_min)),
    ridge_1se = map(train, ~ glmnetUtils::glmnet(UNSC_res_dummy ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal,
                                                 data = .x,
                                                 alpha = 0,
                                                 lambda = ridge_lambda_1se))
  ) %>%
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

ridge_misclass <- conflicts_glmnet %>%
  mutate(pred = map2(fit, test, predict),
         test_misclass = map2_dbl(test, pred, ~ mean((.x$UNSC_res_dummy - .y)^2))) %>%
  unnest(test_misclass) %>%
  select(method, test_misclass)
ridge_misclass %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

# LASSO LOGISTIC REGRESSION WITH 10-FOLD CROSS VALIDATION

set.seed(27182) # Used digits from e

# Lambda grid
lambda_grid <- 10^seq(-2, 10, length = 200)

lasso_cv <- conflicts_split %>%
  pluck("train", 1) %>%
  glmnetUtils::cv.glmnet(
    formula = UNSC_res_dummy ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal,
    data = .,
    alpha = 1, # for ridge regression
    nfolds = 10,
    lambda = lambda_grid
  )

# Check plot for lambdas
plot(lasso_cv)

# Ridge's best lambdas
lasso_lambda_min <- lasso_cv$lambda.min
lasso_lambda_min
lasso_lambda_1se <- lasso_cv$lambda.1se
lasso_lambda_1se

# Test error for ridge regression
conflicts_glmnet <- conflicts_split %>%
  mutate(
    lasso_min = map(train, ~ glmnetUtils::glmnet(UNSC_res_dummy ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal,
                                                 data = .x,
                                                 alpha = 1,
                                                 lambda = lasso_lambda_min)),
    lasso_1se = map(train, ~ glmnetUtils::glmnet(UNSC_res_dummy ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal,
                                                 data = .x,
                                                 alpha = 1,
                                                 lambda = lasso_lambda_1se))
  ) %>%
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

lasso_misclass <- conflicts_glmnet %>%
  mutate(pred = map2(fit, test, predict),
         test_misclass = map2_dbl(test, pred, ~ mean((.x$UNSC_res_dummy - .y)^2))) %>%
  unnest(test_misclass) %>%
  select(method, test_misclass)
lasso_misclass %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

# BAGGING AND RANDOM FORESTS WITH 10-FOLD CROSS VALIDATION

# Helper function to get misclass rate from a ranger object
#' @name misclass_ranger
#' @param model ranger object, a fitted random forest
#' @param test tibble/resample object, a test set
#' @param outcome string, indicates the outcome variable in the data
#' @returns misclassification rate of the model on the test set
misclass_ranger <- function(model, test, outcome){
  
  # Check if test is a tibble
  if(!is_tibble(test)){
    test <- test %>% as_tibble()
  }
  
  # Make predictions
  preds <- predict(model, test)$predictions
  
  # Compute misclass rate
  misclass <- mean(test[[outcome]] != preds)
  
  return(misclass)
  
}

set.seed(27182) # Used digits from e

# Fit and tune classification models
conflict_rf_class <- conflicts_train %>%
  crossv_kfold(10, id = "fold") %>% # take resample objects and put them as a tibble to unpack them
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  ) %>% 
  crossing(mtry = 1:12) %>% # list all mtry values (12 predictors so 12 mtry)
  # Fit the model for each mtry value
  mutate(model = map2(.x = train, .y = mtry, 
                      .f = function(x, y) ranger(intervention ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal,
                                                 mtry = y, 
                                                 data = x, 
                                                 splitrule = "gini", # Use the gini index
                                                 importance = "impurity")), # Record impurity at each split
         # Get the train, test and OOB misclassification rates
         train_misclass = map2(model, train, misclass_ranger, outcome = "intervention"),
         test_misclass = map2(model, test, misclass_ranger, outcome = "intervention"), 
         oob_misclass = map(.x = model, 
                            .f = function(x) x[["prediction.error"]])
  )

# Test misclassification for candidate models
conflict_rf_class_error <- conflict_rf_class %>%
  group_by(mtry) %>%
  unnest(c(test_misclass, oob_misclass, train_misclass)) %>%
  select(mtry, test_misclass, oob_misclass, train_misclass) %>%
  group_by(mtry) %>%
  summarise(test_misclass = mean(test_misclass), oob_misclass = mean(oob_misclass), train_misclass = mean(train_misclass)) %>%
  arrange(test_misclass)
conflict_rf_class_error %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

# Plot the training, test, and OOB misclassification rates versus mtry
ggplot(conflict_rf_class_error) + 
  geom_line(aes(mtry, unlist(oob_misclass), color = "OOB Error")) +
  geom_line(aes(mtry, unlist(train_misclass), color = "Training Error")) +
  geom_line(aes(mtry, unlist(test_misclass), color = "Test Error")) + 
  labs(x = "mtry", y = "Misclassification Rate") + 
  scale_color_manual("", values = c("purple", "blue", "red")) + 
  theme_bw()

rf_class_error <- conflicts_split %>% 
  mutate(rf_mod = map(train, .f = function(x) ranger(intervention ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, mtry = 9)), 
         test_misclass = map2_dbl(rf_mod, test, misclass_ranger, outcome = "intervention"))
bagged_class_error <- conflicts_split %>% 
  mutate(rf_mod = map(train, .f = function(x) ranger(intervention ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, mtry = 12)), 
         test_misclass = map2_dbl(rf_mod, test, misclass_ranger, outcome = "intervention"))

rf_class_error <- rf_class_error %>%
  mutate(method = str_c("random_forest")) %>%
  select(method, test_misclass)

bagging_class_error <- bagged_class_error %>%
  mutate(method = str_c("bagged_classification")) %>%
  select(method, test_misclass)

bagging_class_error %>%
  bind_rows(rf_class_error) %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

# BOOSTING WITH 10-FOLD CROSS VALIDATION

# Helper function
#' @name xgb_matrix
#' @param dat tibble, dataset
#' @param outcome string, indicates the outcome variable in the data
#' @param exclude_vars string (or list of strings) to indicate variables to exclude
#' @returns MSE of the model on the test set
xgb_matrix <- function(dat, outcome, exclude_vars){
  
  # Sanitize input: check that dat is a tibble
  if(!is_tibble(dat)){
    
    dat <- as_tibble(dat)
    
  }
  
  # Sanitize input: check that data has factors, not characters
  dat_types <- dat %>% map_chr(class)
  
  outcome_type <- class(dat[[outcome]])
  
  if("character" %in% dat_types){
    
    # If we need to re-code, leave that outside of the function
    print("You must encode characters as factors.")
    return(NULL)
    
  } else {
    
    # If we're doing binary outcomes, they need to be 0-1
    if(outcome_type == "factor" & nlevels(dat[[outcome]]) == 2){
      tmp <- dat %>% select(outcome) %>% onehot::onehot() %>% predict(dat)  
      lab <- tmp[,1]
    } else {
      lab <- dat[[outcome]]
    }
    
    # Make our DMatrix
    mat <- dat %>% dplyr::select(-outcome, -all_of(exclude_vars)) %>% # encode on full boston df
      onehot::onehot() %>% # use onehot to encode variables
      predict(dat) # get OHE matrix
    
    return(xgb.DMatrix(data = mat, 
                       label = lab))
    
  }
  
}

# Helper function
#' @name xg_error
#' @param model xgb object, a fitted boosted model
#' @param test_mat DMatrix, a test set
#' @param metric string (either "mse" or "misclass"), indicates the error metric
#' @returns MSE/misclass rate of the model on the test set
xg_error <- function(model, test_mat, metric = "mse"){
  
  # Get predictions and actual values
  preds = predict(model, test_mat)
  vals = getinfo(test_mat, "label")
  
  if(metric == "mse"){
    
    # Compute MSE if that's what we need
    err <- mean((preds - vals)^2)
    
  } else if(metric == "misclass") {
    
    # Otherwise, get the misclass rate
    err <- mean(preds != vals)
    
  }
  
  return(err)
}

set.seed(27182) # Used digits from e

conflicts_xg_class <- conflicts_train %>%
  crossv_kfold(10, id = "fold") %>% # take resample objects and put them as a tibble to unpack them
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  ) %>%
  crossing(learn_rate = 10^seq(-10, -.1, length.out = 20)) %>% # tune the learning rate
  mutate(# Build xgb Dmatrices for training and test set
    train_mat = map(train, xgb_matrix, outcome = "intervention", exclude_vars = c("conflict_id", "location", "start_year", "end_year", "total_UNSC_res", "total_UNSC_unanimous_res", "UNSC_res_dummy", "UNSC_res_unanimous_dummy", "side_a", "side_b", "id", "unanimous_intervention")), 
    test_mat = map(test, xgb_matrix, outcome = "intervention", exclude_vars = c("conflict_id", "location", "start_year", "end_year", "total_UNSC_res", "total_UNSC_unanimous_res", "UNSC_res_dummy", "UNSC_res_unanimous_dummy", "side_a", "side_b", "id", "unanimous_intervention")),
    # Fit models to each learning rate
    xg_model = map2(.x = train_mat, .y = learn_rate, 
                    .f = function(x, y) xgb.train(params = list(eta = y, # set learning rate
                                                                depth = 10, # tree depth, can tune
                                                                objective = "multi:softmax", # output labels
                                                                num_class = 2, verbosity = 0), # binary class problem 
                                                  data = x, 
                                                  nrounds = 500, # 500 trees, can be tuned
                                                  silent = TRUE)), 
    # Get training and test error
    xg_train_misclass = map2(xg_model, train_mat, xg_error, metric = "misclass"),
    xg_test_misclass = map2(xg_model, test_mat, xg_error, metric = "misclass") 
  )

# Test misclassification for candidate models
conflicts_xg_class_error <- conflicts_xg_class %>%
  group_by(learn_rate) %>%
  unnest(c(xg_train_misclass, xg_test_misclass)) %>%
  select(learn_rate, xg_test_misclass, xg_train_misclass) %>%
  group_by(learn_rate) %>%
  summarise(test_misclass = mean(xg_test_misclass)) %>%
  arrange(test_misclass)
conflicts_xg_class_error %>%
  knitr::kable() %>%
  kable_styling(full_width = FALSE, position = "center")

boosting_class_error <- conflicts_split %>%
  mutate(train_mat = map(train, xgb_matrix, outcome = "intervention", exclude_vars = c("conflict_id", "location", "start_year", "end_year", "total_UNSC_res", "total_UNSC_unanimous_res", "UNSC_res_dummy", "UNSC_res_unanimous_dummy", "side_a", "side_b", "id")),
         test_mat = map(test, xgb_matrix, outcome = "intervention", exclude_vars = c("conflict_id", "location", "start_year", "end_year", "total_UNSC_res", "total_UNSC_unanimous_res", "UNSC_res_dummy", "UNSC_res_unanimous_dummy", "side_a", "side_b", "id")),
         xg_model = map2(.x = train_mat, .y = 0.0065432, 
                         .f = function(x, y) xgb.train(params = list(eta = y, # set learning rate
                                                                     depth = 10, # tree depth, can tune
                                                                     objective = "multi:softmax", # output labels
                                                                     num_class = 2, verbosity = 0), # binary class problem 
                                                       data = x, 
                                                       nrounds = 500, # 500 trees, can be tuned
                                                       silent = TRUE)),
         test_misclass = map2_dbl(xg_model, test_mat, xg_error))

boosting_class_error <- boosting_class_error %>%
  mutate(method = str_c("boosted_classification")) %>%
  select(method, test_misclass)
boosting_class_error %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

# Helper Function to Make SVM Dataset
#' @name make_svm_df
#' @param n_obs integer, number of rows in df
#' @returns data frame with n_obs rows, an outcome 'y', and 
#'          and two predictors 'x1', 'x2'
make_svm_df <- function(n_obs){
  
  if(n_obs %% 2 == 1){
    n_obs = n_obs + 1
  }
  
  dat <- tibble(x1 = rnorm(n_obs), 
                x2 = rnorm(n_obs), 
                y = rep(c(-1, 1), each = n_obs/2)) %>%
    mutate(x1 = x1 + as.integer(y == 1), 
           x2 = x2 + as.integer(y == 1), 
           y = as.factor(y))
  
  return(dat)
}

# SVM with Linear Kernel
set.seed(27182) # Used digits from e

svm_cv <- conflicts_split %>%
  mutate(tune_svm_linear = map(.x = train, 
                               .f = function(x){ 
                                 return(tune(svm, as.factor(UNSC_res_dummy) ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, kernel = "linear", 
                                             # let cost range over several values
                                             ranges = list(cost = c(0.01, 0.1, 1, 5, 10)))
                                 )
                               }))

linear_params <- svm_cv %>% 
  pluck("tune_svm_linear", 1) %>%
  pluck("best.parameters")
linear_params

svm_linear_model <- conflicts_split %>%
  mutate(model_fit = map(.x = train, # fit the model
                         .f = function(x) svm(as.factor(UNSC_res_dummy) ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, cost = linear_params$cost, kernel = "linear")), 
         test_pred = map2(model_fit, test, predict), # get predictions on test set
         confusion_matrix = map2(.x = test, .y = test_pred,  # get confusion matrix
                                 .f = function(x, y) caret::confusionMatrix(as.factor(x$UNSC_res_dummy), y)))

table <- svm_linear_model %>% 
  pluck("confusion_matrix", 1) %>% 
  tidy() %>%
  head(1)

table <- table[, -c(1, 2)]

table %>%
  as_tibble() %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

# Helper function to make svm dataset
#' @name make_svm_nonlinear_df
#' @param n_obs integer, number of rows in df
#' @returns data frame with n_obs rows, an outcome 'y', and 
#'          and two predictors 'x1', 'x2'
make_svm_nonlinear_df <- function(n_obs){
  
  if(n_obs %% 2 == 1){
    n_obs = n_obs + 1
  }
  
  dat <- tibble(x1 = rnorm(n_obs, sd = 0.95), 
                x2 = rnorm(n_obs, sd = 0.95)) %>%
    mutate(y = as.integer((pnorm(x1) + pnorm(x2) + 
                             rnorm(n(), sd = 0.1) < 1) |
                            (x1^2 + x2^2 < 0.99)),
           y = as.factor(y)) # set y as a factor
  
  return(dat)
}

# SVM with Radial Kernel
set.seed(27182) # Used digits from e

svm_nl_cv <- conflicts_split %>%
  mutate(train_cv = map(.x = train, 
                        .f = function(x) tune(svm, as.factor(UNSC_res_dummy) ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, kernel = "radial", 
                                              ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 50, 100, 200, 500), 
                                                            gamma = seq(0.1, 10, length.out = 5))
                        )
  ))

radial_params <- svm_nl_cv %>%
  pluck("train_cv", 1)  %>%
  pluck("best.parameters")
radial_params

svm_nl <- conflicts_split %>%
  mutate(model_fit = map(.x = train, 
                         .f = function(x) svm(as.factor(UNSC_res_dummy) ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, kernel = "radial", 
                                              cost = radial_params$cost, gamma = radial_params$gamma, 
                                              probability = TRUE)))

svm_nl <- svm_nl %>%
  mutate(train_pred = map2(model_fit, train, predict, probablilty = TRUE), # training predictions for ROC later 
         test_pred = map2(model_fit, test, predict, probability = TRUE), # test predcitions
         test_confusion = map2(.x = test, .y = test_pred, # test confusion matrix
                               .f = function(x, y) caret::confusionMatrix(as.factor(x$UNSC_res_dummy), y)))

table <- svm_nl %>% 
  pluck("test_confusion", 1) %>% 
  tidy() %>%
  head(1)

table <- table[, -c(1, 2)]

table %>%
  as_tibble() %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

# SVM with Polynomial Kernel
set.seed(27182) # Used digits from e

svm_poly_cv <- conflicts_split %>%
  mutate(train_cv = map(.x = train, 
                        .f = function(x) tune(svm, as.factor(UNSC_res_dummy) ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, kernel = "polynomial", 
                                              ranges = list(cost = c(0.1, 1, 10, 50, 100, 500), 
                                                            degree = c(2, 3, 4, 5), 
                                                            tunecontrol = tune.control(cross = 5))
                        )
  ))

poly_params <- svm_poly_cv %>%
  pluck("train_cv", 1)  %>%
  pluck("best.parameters")
poly_params

svm_poly <- conflicts_split %>%
  mutate(model_fit = map(.x = train, 
                         .f = function(x) svm(as.factor(UNSC_res_dummy) ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, kernel = "polynomial", 
                                              cost = poly_params$cost, 
                                              degree = poly_params$degree,
                                              probability = TRUE)))
svm_poly <- svm_poly %>%
  mutate(train_pred = map2(model_fit, train, predict, probablilty = TRUE), # training predictions for ROC later 
         test_pred = map2(model_fit, test, predict, probability = TRUE), # test predcitions
         test_confusion = map2(.x = test, .y = test_pred, # test confusion matrix
                               .f = function(x, y) caret::confusionMatrix(as.factor(x$UNSC_res_dummy), y)))

table <- svm_poly %>% 
  pluck("test_confusion", 1) %>% 
  tidy() %>%
  head(1)

table <- table[, -c(1, 2)]

table %>%
  as_tibble() %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

# Function to extract the test error
svm_error_rate <- function(confusion_matrix) {
  
  # Compute and return the error rate
  return(1 - as.numeric(unlist(confusion_matrix)[["overall.Accuracy"]]))
}

# Get only test errors
linear_svm_error <- svm_linear_model %>%
  select(confusion_matrix) %>%
  mutate(method = "svm_linear", 
         test_misclass = map_dbl(.x = confusion_matrix, .f = svm_error_rate)) %>%
  select(method, test_misclass)

radial_svm_error <- svm_nl %>%
  select(test_confusion) %>% 
  mutate(method = "svm_radial", 
         test_misclass = map_dbl(.x = test_confusion, .f = svm_error_rate)) %>% 
  select(method, test_misclass)

poly_svm_error <- svm_poly %>%
  select(test_confusion) %>% 
  mutate(method = "svm_polynomial", 
         test_misclass = map_dbl(.x = test_confusion, .f = svm_error_rate)) %>% 
  select(method, test_misclass)

svm_results <- linear_svm_error %>% 
  bind_rows(radial_svm_error) %>%
  bind_rows(poly_svm_error) %>%
  arrange(test_misclass) %>% 
  knitr::kable(digits = 3)
svm_results

# Tibble comparing test misclassification rates from each method
model_comparison <- logistic_misclass %>%
  bind_rows(ridge_misclass) %>% 
  bind_rows(lasso_misclass) %>%
  bind_rows(bagging_class_error) %>% 
  bind_rows(rf_class_error) %>% 
  bind_rows(boosting_class_error) %>%
  bind_rows(linear_svm_error) %>% 
  bind_rows(radial_svm_error) %>%
  bind_rows(poly_svm_error) %>%
  arrange(test_misclass) 

model_comparison %>%
  rowid_to_column(var="rank") %>%
  dplyr::select(rank, method, test_misclass) %>%
  knitr::kable(digits = 5) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options= "hold_position")

set.seed(27182) # Used digits from e
# Variable importance plot with `mtry` 12
rf_class_mtry12 = ranger(intervention ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal,
                         data = conflicts_train,
                         mtry = 12,
                         importance = "impurity",
                         splitrule = "gini", 
                         probability = TRUE)
vip_plot_1 <- vip(rf_class_mtry12) # death_toll is most important variable
vip_plot_1 <- vip_plot_1 +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=14,face="bold"))

vip_plot_1

set.seed(27182) # Used digits from e
# Variable importance p-values (Altmann)
imp <- importance_pvalues(
  rf_class_mtry12,
  method = "altmann",
  num.permutations = 1000,
  formula = intervention ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
  data = conflicts_train)

as_tibble(imp, rownames = "term") %>%
  arrange(pvalue) %>%
  kable() %>%
  kable_styling(position = "center")

set.seed(27182) # Used digits from e
# Variable importance plot with `mtry` 9
rf_class_mtry9 = ranger(intervention ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal,
                        data = conflicts_train,
                        mtry = 9,
                        importance = "impurity",
                        splitrule = "gini", 
                        probability = TRUE)
vip_plot_2 <- vip(rf_class_mtry9) # death_toll is most important variable
vip_plot_2 <- vip_plot_2 +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=14,face="bold"))

vip_plot_2

set.seed(27182) # Used digits from e
# Variable importance p-values (Altmann)
imp <- importance_pvalues(
  rf_class_mtry9,
  method = "altmann",
  num.permutations = 1000,
  formula = intervention ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
  data = conflicts_train)

as_tibble(imp, rownames = "term") %>%
  arrange(pvalue) %>%
  kable() %>%
  kable_styling(position = "center")

# Classification Modeling - Outcome: UNSC_res_unanimous_dummy

# LOGISTIC REGRESSION WITH 10-FOLD CROSS VALIDATION

set.seed(27182) # Used digits from e

# Function to calculate misclassification rate
error_rate_glm <- function(data, model){
  data %>% 
    mutate(pred_prob = predict(model, newdata = data, type = "response"),
           pred_intervention = if_else(pred_prob > 0.5, 1, 0),
           error = pred_intervention != UNSC_res_unanimous_dummy) %>% 
    pull(error) %>% 
    mean()
}

# Set up candidate model formula
fmla <- c(
  "UNSC_res_unanimous_dummy ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal"
) %>% map(as.formula)
model_setup <- tibble(method = str_c("logistic_model"),
                      fmla = fmla)

# Fit logistic model
conflicts_logistic_model <- conflicts_split %>%
  crossing(model_setup) %>%
  mutate(
    model_fit = map2(fmla, train, glm, family = "binomial"),
    test_misclass = map2_dbl(test, model_fit, error_rate_glm)
  )

# Test misclassification error for logistic model
logistic_misclass <- conflicts_logistic_model %>%
  group_by(method) %>%
  select(method, test_misclass)
logistic_misclass %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

# RIDGE LOGISTIC REGRESSION WITH 10-FOLD CROSS VALIDATION

set.seed(27182) # Used digits from e

# Lambda grid
lambda_grid <- 10^seq(-2, 10, length = 200)

ridge_cv <- conflicts_split %>%
  pluck("train", 1) %>%
  glmnetUtils::cv.glmnet(
    formula = UNSC_res_unanimous_dummy ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal,
    data = .,
    alpha = 0, # for ridge regression
    nfolds = 10,
    lambda = lambda_grid
  )

# Check plot for lambdas
plot(ridge_cv)

# Ridge's best lambdas
ridge_lambda_min <- ridge_cv$lambda.min
ridge_lambda_min 
ridge_lambda_1se <- ridge_cv$lambda.1se
ridge_lambda_1se 

# Test error for ridge regression
conflicts_glmnet <- conflicts_split %>%
  mutate(
    ridge_min = map(train, ~ glmnetUtils::glmnet(UNSC_res_unanimous_dummy ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal,
                                                 data = .x,
                                                 alpha = 0,
                                                 lambda = ridge_lambda_min)),
    ridge_1se = map(train, ~ glmnetUtils::glmnet(UNSC_res_unanimous_dummy ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal,
                                                 data = .x,
                                                 alpha = 0,
                                                 lambda = ridge_lambda_1se))
  ) %>%
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

ridge_misclass <- conflicts_glmnet %>%
  mutate(pred = map2(fit, test, predict),
         test_misclass = map2_dbl(test, pred, ~ mean((.x$UNSC_res_unanimous_dummy - .y)^2))) %>%
  unnest(test_misclass) %>%
  select(method, test_misclass)
ridge_misclass %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

# LASSO LOGISTIC REGRESSION WITH 10-FOLD CROSS VALIDATION

set.seed(27182) # Used digits from e

# Lambda grid
lambda_grid <- 10^seq(-2, 10, length = 200)

lasso_cv <- conflicts_split %>%
  pluck("train", 1) %>%
  glmnetUtils::cv.glmnet(
    formula = UNSC_res_unanimous_dummy ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal,
    data = .,
    alpha = 1, # for lasso regression
    nfolds = 10,
    lambda = lambda_grid
  )

# Check plot for lambdas
plot(lasso_cv)

# Ridge's best lambdas
lasso_lambda_min <- lasso_cv$lambda.min
lasso_lambda_min
lasso_lambda_1se <- lasso_cv$lambda.1se
lasso_lambda_1se 

# Test error for lasso regression
conflicts_glmnet <- conflicts_split %>%
  mutate(
    lasso_min = map(train, ~ glmnetUtils::glmnet(UNSC_res_unanimous_dummy ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal,
                                                 data = .x,
                                                 alpha = 1,
                                                 lambda = lasso_lambda_min)),
    lasso_1se = map(train, ~ glmnetUtils::glmnet(UNSC_res_unanimous_dummy ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal,
                                                 data = .x,
                                                 alpha = 1,
                                                 lambda = lasso_lambda_1se))
  ) %>%
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

lasso_misclass <- conflicts_glmnet %>%
  mutate(pred = map2(fit, test, predict),
         test_misclass = map2_dbl(test, pred, ~ mean((.x$UNSC_res_unanimous_dummy - .y)^2))) %>%
  unnest(test_misclass) %>%
  select(method, test_misclass)
lasso_misclass %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

# BAGGING AND RANDOM FORESTS WITH 10-FOLD CROSS VALIDATION

# Helper function to get misclass rate from a ranger object
#' @name misclass_ranger
#' @param model ranger object, a fitted random forest
#' @param test tibble/resample object, a test set
#' @param outcome string, indicates the outcome variable in the data
#' @returns misclassification rate of the model on the test set
misclass_ranger <- function(model, test, outcome){
  
  # Check if test is a tibble
  if(!is_tibble(test)){
    test <- test %>% as_tibble()
  }
  
  # Make predictions
  preds <- predict(model, test)$predictions
  
  # Compute misclass rate
  misclass <- mean(test[[outcome]] != preds)
  
  return(misclass)
  
}

set.seed(27182) # Used digits from e

# Fit and tune classification models
conflict_rf_class <- conflicts_train %>%
  crossv_kfold(10, id = "fold") %>% # take resample objects and put them as a tibble to unpack them
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  ) %>% 
  crossing(mtry = 1:12) %>% # list all mtry values (12 predictors so 12 mtry)
  # Fit the model for each mtry value
  mutate(model = map2(.x = train, .y = mtry, 
                      .f = function(x, y) ranger(unanimous_intervention ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal,
                                                 mtry = y, 
                                                 data = x, 
                                                 splitrule = "gini", # Use the gini index
                                                 importance = "impurity")), # Record impurity at each split
         # Get the train, test and OOB misclassification rates
         train_misclass = map2(model, train, misclass_ranger, outcome = "unanimous_intervention"),
         test_misclass = map2(model, test, misclass_ranger, outcome = "unanimous_intervention"), 
         oob_misclass = map(.x = model, 
                            .f = function(x) x[["prediction.error"]])
  )

# Test misclassification for candidate models
conflict_rf_class_error <- conflict_rf_class %>%
  group_by(mtry) %>%
  unnest(c(test_misclass, oob_misclass, train_misclass)) %>%
  select(mtry, test_misclass, oob_misclass, train_misclass) %>%
  group_by(mtry) %>%
  summarise(test_misclass = mean(test_misclass), oob_misclass = mean(oob_misclass), train_misclass = mean(train_misclass)) %>%
  arrange(test_misclass)
conflict_rf_class_error %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

# Plot the training, test, and OOB misclassification rates versus mtry
ggplot(conflict_rf_class_error) + 
  geom_line(aes(mtry, unlist(oob_misclass), color = "OOB Error")) +
  geom_line(aes(mtry, unlist(train_misclass), color = "Training Error")) +
  geom_line(aes(mtry, unlist(test_misclass), color = "Test Error")) + 
  labs(x = "mtry", y = "Misclassification Rate") + 
  scale_color_manual("", values = c("purple", "blue", "red")) + 
  theme_bw()

rf_class_error <- conflicts_split %>% 
  mutate(rf_mod = map(train, .f = function(x) ranger(unanimous_intervention ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, mtry = 5)), 
         test_misclass = map2_dbl(rf_mod, test, misclass_ranger, outcome = "unanimous_intervention"))
bagged_class_error <- conflicts_split %>% 
  mutate(rf_mod = map(train, .f = function(x) ranger(unanimous_intervention ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, mtry = 12)), 
         test_misclass = map2_dbl(rf_mod, test, misclass_ranger, outcome = "unanimous_intervention"))

rf_class_error <- rf_class_error %>%
  mutate(method = str_c("random_forest")) %>%
  select(method, test_misclass)

bagging_class_error <- bagged_class_error %>%
  mutate(method = str_c("bagged_classification")) %>%
  select(method, test_misclass)

bagging_class_error %>%
  bind_rows(rf_class_error) %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

## BOOSTING WITH 10-FOLD CROSS VALIDATION

# Helper function
#' @name xgb_matrix
#' @param dat tibble, dataset
#' @param outcome string, indicates the outcome variable in the data
#' @param exclude_vars string (or list of strings) to indicate variables to exclude
#' @returns MSE of the model on the test set
xgb_matrix <- function(dat, outcome, exclude_vars){
  
  # Sanitize input: check that dat is a tibble
  if(!is_tibble(dat)){
    
    dat <- as_tibble(dat)
    
  }
  
  # Sanitize input: check that data has factors, not characters
  dat_types <- dat %>% map_chr(class)
  
  outcome_type <- class(dat[[outcome]])
  
  if("character" %in% dat_types){
    
    # If we need to re-code, leave that outside of the function
    print("You must encode characters as factors.")
    return(NULL)
    
  } else {
    
    # If we're doing binary outcomes, they need to be 0-1
    if(outcome_type == "factor" & nlevels(dat[[outcome]]) == 2){
      tmp <- dat %>% select(outcome) %>% onehot::onehot() %>% predict(dat)  
      lab <- tmp[,1]
    } else {
      lab <- dat[[outcome]]
    }
    
    # Make our DMatrix
    mat <- dat %>% dplyr::select(-outcome, -all_of(exclude_vars)) %>% # encode on full boston df
      onehot::onehot() %>% # use onehot to encode variables
      predict(dat) # get OHE matrix
    
    return(xgb.DMatrix(data = mat, 
                       label = lab))
    
  }
  
}

# Helper function
#' @name xg_error
#' @param model xgb object, a fitted boosted model
#' @param test_mat DMatrix, a test set
#' @param metric string (either "mse" or "misclass"), indicates the error metric
#' @returns MSE/misclass rate of the model on the test set
xg_error <- function(model, test_mat, metric = "mse"){
  
  # Get predictions and actual values
  preds = predict(model, test_mat)
  vals = getinfo(test_mat, "label")
  
  if(metric == "mse"){
    
    # Compute MSE if that's what we need
    err <- mean((preds - vals)^2)
    
  } else if(metric == "misclass") {
    
    # Otherwise, get the misclass rate
    err <- mean(preds != vals)
    
  }
  
  return(err)
}

set.seed(27182) # Used digits from e

conflicts_xg_class <- conflicts_train %>%
  crossv_kfold(10, id = "fold") %>% # take resample objects and put them as a tibble to unpack them
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  ) %>%
  crossing(learn_rate = 10^seq(-10, -.1, length.out = 20)) %>% # tune the learning rate
  mutate(# Build xgb Dmatrices for training and test set
    train_mat = map(train, xgb_matrix, outcome = "unanimous_intervention", exclude_vars = c("conflict_id", "location", "start_year", "end_year", "total_UNSC_res", "total_UNSC_unanimous_res", "UNSC_res_dummy", "UNSC_res_unanimous_dummy", "side_a", "side_b", "id", "intervention")), 
    test_mat = map(test, xgb_matrix, outcome = "unanimous_intervention", exclude_vars = c("conflict_id", "location", "start_year", "end_year", "total_UNSC_res", "total_UNSC_unanimous_res", "UNSC_res_dummy", "UNSC_res_unanimous_dummy", "side_a", "side_b", "id", "intervention")),
    # Fit models to each learning rate
    xg_model = map2(.x = train_mat, .y = learn_rate, 
                    .f = function(x, y) xgb.train(params = list(eta = y, # set learning rate
                                                                depth = 10, # tree depth, can tune
                                                                objective = "multi:softmax", # output labels
                                                                num_class = 2, verbosity = 0), # binary class problem 
                                                  data = x, 
                                                  nrounds = 500, # 500 trees, can be tuned
                                                  silent = TRUE)), 
    # Get training and test error
    xg_train_misclass = map2(xg_model, train_mat, xg_error, metric = "misclass"),
    xg_test_misclass = map2(xg_model, test_mat, xg_error, metric = "misclass") 
  )

# Test misclassification for candidate models
conflicts_xg_class_error <- conflicts_xg_class %>%
  group_by(learn_rate) %>%
  unnest(c(xg_train_misclass, xg_test_misclass)) %>%
  select(learn_rate, xg_test_misclass, xg_train_misclass) %>%
  group_by(learn_rate) %>%
  summarise(test_misclass = mean(xg_test_misclass)) %>%
  arrange(test_misclass)
conflicts_xg_class_error %>%
  knitr::kable() %>%
  kable_styling(full_width = FALSE, position = "center")

boosting_class_error <- conflicts_split %>%
  mutate(train_mat = map(train, xgb_matrix, outcome = "unanimous_intervention", exclude_vars = c("conflict_id", "location", "start_year", "end_year", "total_UNSC_res", "total_UNSC_unanimous_res", "UNSC_res_dummy", "UNSC_res_unanimous_dummy", "side_a", "side_b", "id", "intervention")),
         test_mat = map(test, xgb_matrix, outcome = "unanimous_intervention", exclude_vars = c("conflict_id", "location", "start_year", "end_year", "total_UNSC_res", "total_UNSC_unanimous_res", "UNSC_res_dummy", "UNSC_res_unanimous_dummy", "side_a", "side_b", "id", "intervention")),
         xg_model = map2(.x = train_mat, .y = 1.213977e-08, 
                         .f = function(x, y) xgb.train(params = list(eta = y, # set learning rate
                                                                     depth = 10, # tree depth, can tune
                                                                     objective = "multi:softmax", # output labels
                                                                     num_class = 2, verbosity = 0), # binary class problem 
                                                       data = x, 
                                                       nrounds = 500, # 500 trees, can be tuned
                                                       silent = TRUE)),
         test_misclass = map2_dbl(xg_model, test_mat, xg_error))

boosting_class_error <- boosting_class_error %>%
  mutate(method = str_c("boosted_classification")) %>%
  select(method, test_misclass)
boosting_class_error %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

# Helper Function to Make SVM Dataset
#' @name make_svm_df
#' @param n_obs integer, number of rows in df
#' @returns data frame with n_obs rows, an outcome 'y', and 
#'          and two predictors 'x1', 'x2'
make_svm_df <- function(n_obs){
  
  if(n_obs %% 2 == 1){
    n_obs = n_obs + 1
  }
  
  dat <- tibble(x1 = rnorm(n_obs), 
                x2 = rnorm(n_obs), 
                y = rep(c(-1, 1), each = n_obs/2)) %>%
    mutate(x1 = x1 + as.integer(y == 1), 
           x2 = x2 + as.integer(y == 1), 
           y = as.factor(y))
  
  return(dat)
}

# SVM with Linear Kernel
set.seed(27182) # Used digits from e

svm_cv <- conflicts_split %>%
  mutate(tune_svm_linear = map(.x = train, 
                               .f = function(x){ 
                                 return(tune(svm, as.factor(UNSC_res_unanimous_dummy) ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, kernel = "linear", 
                                             # let cost range over several values
                                             ranges = list(cost = c(0.01, 0.1, 1, 5, 10)))
                                 )
                               }))

linear_params <- svm_cv %>% 
  pluck("tune_svm_linear", 1) %>%
  pluck("best.parameters")
linear_params

svm_linear_model <- conflicts_split %>%
  mutate(model_fit = map(.x = train, # fit the model
                         .f = function(x) svm(as.factor(UNSC_res_unanimous_dummy) ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, cost = linear_params$cost, kernel = "linear")), 
         test_pred = map2(model_fit, test, predict), # get predictions on test set
         confusion_matrix = map2(.x = test, .y = test_pred,  # get confusion matrix
                                 .f = function(x, y) caret::confusionMatrix(as.factor(x$UNSC_res_unanimous_dummy), y)))

table <- svm_linear_model %>% 
  pluck("confusion_matrix", 1) %>% 
  tidy() %>%
  head(1)

table <- table[, -c(1, 2)]

table %>%
  as_tibble() %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

# Helper function to make svm dataset
#' @name make_svm_nonlinear_df
#' @param n_obs integer, number of rows in df
#' @returns data frame with n_obs rows, an outcome 'y', and 
#'          and two predictors 'x1', 'x2'
make_svm_nonlinear_df <- function(n_obs){
  
  if(n_obs %% 2 == 1){
    n_obs = n_obs + 1
  }
  
  dat <- tibble(x1 = rnorm(n_obs, sd = 0.95), 
                x2 = rnorm(n_obs, sd = 0.95)) %>%
    mutate(y = as.integer((pnorm(x1) + pnorm(x2) + 
                             rnorm(n(), sd = 0.1) < 1) |
                            (x1^2 + x2^2 < 0.99)),
           y = as.factor(y)) # set y as a factor
  
  return(dat)
}

# SVM with Radial Kernel
set.seed(27182) # Used digits from e

svm_nl_cv <- conflicts_split %>%
  mutate(train_cv = map(.x = train, 
                        .f = function(x) tune(svm, as.factor(UNSC_res_unanimous_dummy) ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, kernel = "radial", 
                                              ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 50, 100, 200, 500), 
                                                            gamma = seq(0.1, 10, length.out = 5))
                        )
  ))

radial_params <- svm_nl_cv %>%
  pluck("train_cv", 1)  %>%
  pluck("best.parameters")
radial_params

svm_nl <- conflicts_split %>%
  mutate(model_fit = map(.x = train, 
                         .f = function(x) svm(as.factor(UNSC_res_unanimous_dummy) ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, kernel = "radial", 
                                              cost = radial_params$cost, gamma = radial_params$gamma, 
                                              probability = TRUE)))

svm_nl <- svm_nl %>%
  mutate(train_pred = map2(model_fit, train, predict, probablilty = TRUE), # training predictions for ROC later 
         test_pred = map2(model_fit, test, predict, probability = TRUE), # test predcitions
         test_confusion = map2(.x = test, .y = test_pred, # test confusion matrix
                               .f = function(x, y) caret::confusionMatrix(as.factor(x$UNSC_res_unanimous_dummy), y)))

table <- svm_nl %>% 
  pluck("test_confusion", 1) %>% 
  tidy() %>%
  head(1)

table <- table[, -c(1, 2)]

table %>%
  as_tibble() %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

# SVM with Polynomial Kernel
set.seed(27182) # Used digits from e

svm_poly_cv <- conflicts_split %>%
  mutate(train_cv = map(.x = train, 
                        .f = function(x) tune(svm, as.factor(UNSC_res_unanimous_dummy) ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, kernel = "polynomial", 
                                              ranges = list(cost = c(0.1, 1, 10, 50, 100, 500), 
                                                            degree = c(2, 3, 4, 5), 
                                                            tunecontrol = tune.control(cross = 5))
                        )
  ))

poly_params <- svm_poly_cv %>%
  pluck("train_cv", 1)  %>%
  pluck("best.parameters")
poly_params

svm_poly <- conflicts_split %>%
  mutate(model_fit = map(.x = train, 
                         .f = function(x) svm(as.factor(UNSC_res_unanimous_dummy) ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, kernel = "polynomial", 
                                              cost = poly_params$cost, 
                                              degree = poly_params$degree,
                                              probability = TRUE)))
svm_poly <- svm_poly %>%
  mutate(train_pred = map2(model_fit, train, predict, probablilty = TRUE), # training predictions for ROC later 
         test_pred = map2(model_fit, test, predict, probability = TRUE), # test predcitions
         test_confusion = map2(.x = test, .y = test_pred, # test confusion matrix
                               .f = function(x, y) caret::confusionMatrix(as.factor(x$UNSC_res_unanimous_dummy), y)))

table <- svm_poly %>% 
  pluck("test_confusion", 1) %>% 
  tidy() %>%
  head(1)

table <- table[, -c(1, 2)]

table %>%
  as_tibble() %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

# Function to extract the test error
svm_error_rate <- function(confusion_matrix) {
  
  # Compute and return the error rate
  return(1 - as.numeric(unlist(confusion_matrix)[["overall.Accuracy"]]))
}

# Get only  test errors
linear_svm_error <- svm_linear_model %>%
  select(confusion_matrix) %>%
  mutate(method = "svm_linear", 
         test_misclass = map_dbl(.x = confusion_matrix, .f = svm_error_rate)) %>%
  select(method, test_misclass)

radial_svm_error <- svm_nl %>%
  select(test_confusion) %>% 
  mutate(method = "svm_radial", 
         test_misclass = map_dbl(.x = test_confusion, .f = svm_error_rate)) %>% 
  select(method, test_misclass)

poly_svm_error <- svm_poly %>%
  select(test_confusion) %>% 
  mutate(method = "svm_polynomial", 
         test_misclass = map_dbl(.x = test_confusion, .f = svm_error_rate)) %>% 
  select(method, test_misclass)

svm_results <- linear_svm_error %>% 
  bind_rows(radial_svm_error) %>%
  bind_rows(poly_svm_error) %>%
  arrange(test_misclass) %>% 
  knitr::kable(digits = 3)
svm_results

# Tibble comparing test misclassification rates from each method
model_comparison <- logistic_misclass %>%
  bind_rows(ridge_misclass) %>% 
  bind_rows(lasso_misclass) %>%
  bind_rows(bagging_class_error) %>% 
  bind_rows(rf_class_error) %>% 
  bind_rows(boosting_class_error) %>%
  bind_rows(linear_svm_error) %>% 
  bind_rows(radial_svm_error) %>%
  bind_rows(poly_svm_error) %>%
  arrange(test_misclass) 

model_comparison %>%
  rowid_to_column(var="rank") %>%
  dplyr::select(rank, method, test_misclass) %>%
  knitr::kable(digits = 5) %>%
  kable_styling(full_width = FALSE, position = "center", latex_options= "hold_position")

set.seed(27182) # Used digits from e
# Variable importance plot with `mtry` 12
rf_class_mtry12 = ranger(unanimous_intervention ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal,
                         data = conflicts_train,
                         mtry = 12,
                         importance = "impurity",
                         splitrule = "gini", 
                         probability = TRUE)
vip_plot_1 <- vip(rf_class_mtry12) # death_toll is most important variable
vip_plot_1 <- vip_plot_1 +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=14,face="bold"))

vip_plot_1

set.seed(27182) # Used digits from e
# Variable importance p-values (Altmann)
imp <- importance_pvalues(
  rf_class_mtry12,
  method = "altmann",
  num.permutations = 1000,
  formula = unanimous_intervention ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
  data = conflicts_train)

as_tibble(imp, rownames = "term") %>%
  arrange(pvalue) %>%
  kable() %>%
  kable_styling(position = "center")

set.seed(27182) # Used digits from e
# Variable importance plot with `mtry` 5
rf_class_mtry5 = ranger(unanimous_intervention ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal,
                        data = conflicts_train,
                        mtry = 5,
                        importance = "impurity",
                        splitrule = "gini", 
                        probability = TRUE)
vip_plot_2 <- vip(rf_class_mtry5) # death_toll is most important variable
vip_plot_2 <- vip_plot_2 +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=14,face="bold"))

vip_plot_2

set.seed(27182) # Used digits from e
# Variable importance p-values (Altmann)
imp <- importance_pvalues(
  rf_class_mtry5,
  method = "altmann",
  num.permutations = 1000,
  formula = unanimous_intervention ~ conflict_years + death_toll + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
  data = conflicts_train)

as_tibble(imp, rownames = "term") %>%
  arrange(pvalue) %>%
  kable() %>%
  kable_styling(position = "center")

# PREDICTIVE REGRESSION MODELING SECTION

# CLEAN FINAL DATA FOR MODELING
FinalData <- read_csv("~/FinalData.csv")

FinalData <- FinalData %>%
  mutate_if(is_character, as_factor) %>%
  select(-X1)

set.seed(27182) # Used digits from e

ModelData <- FinalData %>%
  select(conflict_id, location, start_year, end_year, total_UNSC_res, total_UNSC_unanimous_res, UNSC_res_dummy, UNSC_res_unanimous_dummy, side_a, side_b, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal)

ModelData <- ModelData %>%
  na.omit() %>%
  mutate(intervention = as.factor(ifelse(UNSC_res_dummy == 0, "No", "Yes")), 
         # add column for 'intervention'
         intervention = fct_relevel(intervention, "Yes")) %>%
  mutate(unanimous_intervention = 
           as.factor(ifelse(UNSC_res_unanimous_dummy == 0, "No", "Yes")), 
         # add column for 'unanimous_intervention'
         unanimous_intervention = fct_relevel(unanimous_intervention, "Yes"))
# 115 observations rather than 207 observations due to missingness

# SPLIT DATA INTO TRAINING SET AND TEST SET
ModelData <- ModelData %>% 
  mutate(id = row_number())

# Create training set
conflicts_train <- ModelData %>% sample_frac(.70)
# Create test set
conflicts_test  <- anti_join(ModelData, conflicts_train, by = 'id')

conflicts_split <- tibble(train = conflicts_train %>% list(),
                          test  = conflicts_test %>% list())

# Predictive Regression Modeling - Predictor: UNSC_res_dummy

# VARIATION
test_death_var <- conflicts_split %>% 
  pluck("test", 1, "death_toll") %>% 
  var()

pred_variables <- names(ModelData) %>% 
  setdiff("death_toll")

# LINEAR (OLS) REGRESSION WITH 10-FOLD CROSS VALIDATION

set.seed(27182) # Used digits from e

# LINEAR MODEL -- OLS REGRESSION WITH TRAINING AND TEST SET
lm_model <- tibble(
  fmla = c(
    "death_toll ~ UNSC_res_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal"),
  method = c("ols_model")
) %>% 
  mutate(
    fmla = map(fmla, as.formula)
  )

# Test error for linear model
conflict_lm_fits <- conflicts_split %>% 
  crossing(lm_model) %>% 
  mutate(
    model_fit = map2(fmla, train, lm),
    test_error = map2_dbl(model_fit, test , modelr::mse),
    prop_var_unexplained = test_error / test_death_var,
    prop_var_explained = 1 - prop_var_unexplained
  )

conflict_lm_fits %>% 
  select(method, test_error, prop_var_explained, prop_var_unexplained) %>%
  kable() %>%
  kable_styling(position = "center")

ols <- conflict_lm_fits %>% 
  mutate(
    mod_glance  = map(model_fit, glance),
    mod_tidy    = map(model_fit, tidy)
  ) %>%
  unnest(mod_tidy)

ols %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate(signif = stars.pval(p.value)) %>%
  knitr::kable(digits = 3) %>%
  kable_styling(position = "center")

#VIF to check for multicollinearity
vif <- conflict_lm_fits %>%
  pluck("model_fit", 1) %>%
  car::vif()
vif

ols_error <- conflict_lm_fits %>% 
  select(method, test_error)
ols_error

# CORRELATION PLOT
Cor3 <- FinalData %>%
  select(death_toll, UNSC_res_dummy, conflict_years, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal) %>%
  na.omit() %>%
  cor()

par(xpd=TRUE)
corrplot::corrplot.mixed(Cor3, lower.col = "black", number.cex = 0.6, tl.col = "black", tl.pos = "lt", tl.cex = 0.5, tl.srt=70, mar=c(0.5,1,2,1))

# RIDGE REGRESSION WITH 10-FOLD CROSS VALIDATION

set.seed(27182) # Used digits from e

# Lambda grid
lambda_grid <- 10^seq(-2, 10, length = 200)

ridge_cv <- conflicts_split %>%
  pluck("train", 1) %>% 
  glmnetUtils::cv.glmnet(
    formula = death_toll ~ UNSC_res_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
    data = .,  
    alpha = 0, # for ridge regression
    nfolds = 10,
    lambda = lambda_grid
  )

# Check plot for lambdas
plot(ridge_cv)

# Ridge's best lambdas
ridge_lambda_min <- ridge_cv$lambda.min
ridge_lambda_min
ridge_lambda_1se <- ridge_cv$lambda.1se
ridge_lambda_1se

# Test error for ridge regression
conflict_glmnet <- conflicts_split %>% 
  mutate(
    ridge_min = map(train, ~ glmnetUtils::glmnet(death_toll ~ UNSC_res_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
                                                 data = .x,
                                                 alpha = 0, 
                                                 lambda = ridge_lambda_min)),
    ridge_1se = map(train, ~ glmnetUtils::glmnet(death_toll ~ UNSC_res_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
                                                 data = .x,
                                                 alpha = 0, 
                                                 lambda = ridge_lambda_1se))
  ) %>% 
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

conflict_glmnet %>% 
  mutate(pred = map2(fit, test, predict),
         test_error = map2_dbl(test, pred, ~ mean((.x$death_toll - .y)^2))) %>% 
  unnest(test_error) %>% 
  select(method, test_error) %>% 
  mutate(prop_var_explained = 1 - test_error/test_death_var) 

# Inspect model coefficient estimates
conflict_glmnet %>% 
  pluck("fit") %>% 
  map( ~ coef(.x) %>% 
         as.matrix() %>% 
         as.data.frame() %>% 
         rownames_to_column("name")) %>%
  reduce(full_join, by = "name") %>% 
  mutate_if(is.double, ~ if_else(. == 0, NA_real_, .)) %>% 
  rename(ridge_min = s0.x,
         ridge_1se = s0.y)

# Ridge min regression only
conflict_glmnet_final <- conflicts_split %>% 
  mutate(
    ridge_min = map(train, ~ glmnetUtils::glmnet(death_toll ~ UNSC_res_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
                                                 data = .x,
                                                 alpha = 0, 
                                                 lambda = ridge_lambda_min))
  ) %>% 
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

conflict_glmnet_final %>% 
  mutate(pred = map2(fit, test, predict),
         test_error = map2_dbl(test, pred, ~ mean((.x$death_toll - .y)^2))) %>% 
  unnest(test_error) %>% 
  select(method, test_error) 

ridge_error <- conflict_glmnet_final %>%
  mutate(pred = map2(fit, test, predict),
         test_error = map2_dbl(test, pred, ~ mean((.x$death_toll - .y)^2))) %>%
  unnest(test_error) %>%
  select(method, test_error)

ridge_error

# LASSO REGRESSION WITH 10-FOLD CROSS VALIDATION

set.seed(27182) # Used digits from e

# Lambda grid
lambda_grid <- 10^seq(-3, 3, length = 200)

lasso_cv <- conflicts_split %>%
  pluck("train", 1) %>% 
  glmnetUtils::cv.glmnet(
    formula = death_toll ~ UNSC_res_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
    data = .,  
    alpha = 1, # for lasso regression
    nfolds = 10,
    lambda = lambda_grid
  )

# Check plot for lambdas
plot(lasso_cv)

# Lasso's best lambdas
lasso_lambda_min <- lasso_cv$lambda.min
lasso_lambda_min
lasso_lambda_1se <- lasso_cv$lambda.1se
lasso_lambda_1se

# Test error for lasso regression
conflict_glmnet <- conflicts_split %>% 
  mutate(
    lasso_min = map(train, ~ glmnetUtils::glmnet(death_toll ~ UNSC_res_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
                                                 data = .x,
                                                 alpha = 1, 
                                                 lambda = lasso_lambda_min)),
    lasso_1se = map(train, ~ glmnetUtils::glmnet(death_toll ~ UNSC_res_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
                                                 data = .x,
                                                 alpha = 1, 
                                                 lambda = lasso_lambda_1se))
  ) %>% 
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

conflict_glmnet %>% 
  mutate(pred = map2(fit, test, predict),
         test_error = map2_dbl(test, pred, ~ mean((.x$death_toll - .y)^2))) %>% 
  unnest(test_error) %>% 
  select(method, test_error) %>% 
  mutate(prop_var_explained = 1 - test_error/test_death_var) 

# Inspect model coefficient estimates
conflict_glmnet %>% 
  pluck("fit") %>% 
  map( ~ coef(.x) %>% 
         as.matrix() %>% 
         as.data.frame() %>% 
         rownames_to_column("name")) %>%
  reduce(full_join, by = "name") %>% 
  mutate_if(is.double, ~ if_else(. == 0, NA_real_, .)) %>% 
  rename(lasso_min = s0.x,
         lasso_1se = s0.y)

# Lasso min regression only
conflict_glmnet_final <- conflicts_split %>% 
  mutate(
    lasso_min = map(train, ~ glmnetUtils::glmnet(death_toll ~ UNSC_res_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
                                                 data = .x,
                                                 alpha = 1, 
                                                 lambda = lasso_lambda_min))
  ) %>% 
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

conflict_glmnet_final %>% 
  mutate(pred = map2(fit, test, predict),
         test_error = map2_dbl(test, pred, ~ mean((.x$death_toll - .y)^2))) %>% 
  unnest(test_error) %>% 
  select(method, test_error)

lasso_error <- conflict_glmnet_final %>%
  mutate(pred = map2(fit, test, predict),
         test_error = map2_dbl(test, pred, ~ mean((.x$death_toll - .y)^2))) %>%
  unnest(test_error) %>%
  select(method, test_error)

lasso_error

# BAGGING AND RANDOM FORESTS WITH 10-FOLD CROSS VALIDATION

# Helper function to get misclass rate from a ranger object
#' @name mse_ranger
#' @param model ranger object, a fitted random forest
#' @param test tibble/resample object, a test set
#' @param outcome string, indicates the outcome variable in the data
#' @returns MSE of the model on the test set
mse_ranger <- function(model, test, outcome){
  
  # Check if test is a tibble
  if(!is_tibble(test)){
    test <- test %>% as_tibble()
  }
  
  # Make predictions
  preds <- predict(model, test)$predictions
  
  # Compute MSE
  mse <- mean((test[[outcome]] - preds)^2)
  
  return(mse)
  
}

set.seed(27182) # Used digits from e

# Fit and tune regression models
conflicts_rf_reg <- conflicts_train %>%
  crossv_kfold(10, id = "fold") %>% # take resample objects and put them as a tibble to unpack them
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  ) %>%
  crossing(mtry = 1:12) %>% # list all mtry values (12 predictors so 12 mtry)
  mutate(# Fit models for each value of mtry
    model = map2(.x = train, .y = mtry, 
                 .f = function(x, y) ranger(death_toll ~ UNSC_res_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
                                            mtry = y, 
                                            data = x, 
                                            splitrule = "variance", # use 'variance' for regression
                                            importance = "impurity")), 
    # Get training, test, and OOB error
    train_err = map2(model, train, mse_ranger, outcome = "death_toll"), 
    test_err = map2(model, test, mse_ranger, outcome = "death_toll"), 
    oob_err = map(.x = model, 
                  .f = function(x) x[["prediction.error"]])
  )

# Test error for candidate models
conflicts_rf_reg_error <- conflicts_rf_reg %>%
  group_by(mtry) %>%
  unnest(c(test_err, oob_err, train_err)) %>%
  select(mtry, test_err, oob_err, train_err) %>%
  group_by(mtry) %>%
  summarise(test_error = mean(test_err), oob_err = mean(oob_err), train_err = mean(train_err)) %>%
  arrange(test_error)
conflicts_rf_reg_error

# Plot the training, test, and OOB error versus mtry
ggplot(conflicts_rf_reg_error) + 
  geom_line(aes(mtry, unlist(oob_err), color = "OOB Error")) +
  geom_line(aes(mtry, unlist(train_err), color = "Training Error")) +
  geom_line(aes(mtry, unlist(test_error), color = "Test Error")) + 
  labs(x = "mtry", y = "MSE") + 
  scale_color_manual("", values = c("purple", "red", "blue")) + 
  theme_bw()

rf_reg_error <- conflicts_split %>% 
  mutate(rf_mod = map(train, .f = function(x) ranger(death_toll ~ UNSC_res_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, mtry = 6)), 
         test_error = map2_dbl(rf_mod, test, mse_ranger, outcome = "death_toll"))
bagged_reg_error <- conflicts_split %>% 
  mutate(rf_mod = map(train, .f = function(x) ranger(death_toll ~ UNSC_res_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, mtry = 12)), 
         test_error = map2_dbl(rf_mod, test, mse_ranger, outcome = "death_toll"))

rf_reg_error <- rf_reg_error %>%
  mutate(method = str_c("random_forest")) %>%
  select(method, test_error)
rf_reg_error

bagging_reg_error <- bagged_reg_error %>%
  mutate(method = str_c("bagged_regression")) %>%
  select(method, test_error)
bagging_reg_error

# Tibble comparing test MSE rates from each method
model_comparison <- ols_error %>%
  bind_rows(ridge_error) %>% 
  bind_rows(lasso_error) %>% 
  bind_rows(rf_reg_error) %>% 
  bind_rows(bagging_reg_error) %>%
  arrange(test_error) 

model_comparison %>%
  rowid_to_column(var="rank") %>% 
  mutate(prop_var_explained = 1 - test_error/test_death_var) %>% 
  mutate(prop_var_unexplained = test_error/test_death_var) %>%
  dplyr::select(rank, method, test_error, prop_var_explained, prop_var_unexplained) %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

# Variable importance plot with `mtry` 6
set.seed(27182) # Used digits from e
rf_reg_mtry6 = ranger(death_toll ~ UNSC_res_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
                      data = conflicts_train, 
                      mtry = 6,
                      importance = "impurity", 
                      splitrule = "variance")
vip_plot_1 <- vip(rf_reg_mtry6)
vip_plot_1 <- vip_plot_1 +
  xlab("Variables") +
  ylab("Importance (impurity)") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=8)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=12,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=12,face="bold"))

set.seed(27182) # Used digits from e
rf_reg_mtry_imp = ranger(death_toll ~ UNSC_res_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
                         data = conflicts_train, 
                         mtry = 6,
                         importance = "impurity_corrected", 
                         splitrule = "variance")
vip_plot_2 <- vip(rf_reg_mtry_imp)
vip_plot_2 <- vip_plot_2 +
  xlab("Variables") +
  ylab("Importance (impurity corrected)") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=8)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0), size=11,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=11,face="bold"))

vip_plot_1
vip_plot_2

set.seed(27182) # Used digits from e
imp <- importance_pvalues(
  rf_reg_mtry_imp,
  method = "altmann",
  num.permutations = 1000,
  formula = death_toll ~ UNSC_res_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
  data = conflicts_train)

as_tibble(imp, rownames = "term") %>%
  kable() %>%
  kable_styling(position = "center")

# Predictive Regression Modeling - Predictor: UNSC_res_unanimous_dummy

# VARIATION
test_death_var <- conflicts_split %>% 
  pluck("test", 1, "death_toll") %>% 
  var()

pred_variables <- names(ModelData) %>% 
  setdiff("death_toll")

# LINEAR (OLS) REGRESSION WITH 10-FOLD CROSS VALIDATION

set.seed(27182) # Used digits from e

# LINEAR MODEL -- OLS REGRESSION WITH TRAINING AND TEST SET
lm_model <- tibble(
  fmla = c(
    "death_toll ~ UNSC_res_unanimous_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal"),
  method = c("ols_model")
) %>% 
  mutate(
    fmla = map(fmla, as.formula)
  )

# Test error for linear model
conflict_lm_fits <- conflicts_split %>% 
  crossing(lm_model) %>% 
  mutate(
    model_fit = map2(fmla, train, lm),
    test_error = map2_dbl(model_fit, test , modelr::mse),
    prop_var_unexplained = test_error / test_death_var,
    prop_var_explained = 1 - prop_var_unexplained
  )

conflict_lm_fits %>% 
  select(method, test_error, prop_var_explained, prop_var_unexplained) %>%
  kable() %>%
  kable_styling(position = "center")

ols <- conflict_lm_fits %>% 
  mutate(
    mod_glance  = map(model_fit, glance),
    mod_tidy    = map(model_fit, tidy)
  ) %>%
  unnest(mod_tidy)

ols %>%
  select(term, estimate, std.error, statistic, p.value) %>%
  mutate(signif = stars.pval(p.value)) %>%
  knitr::kable(digits = 3) %>%
  kable_styling(position = "center")

ols_error <- conflict_lm_fits %>% 
  select(method, test_error)

ols_error

# RIDGE REGRESSION WITH 10-FOLD CROSS VALIDATION

set.seed(27182) # Used digits from e

# Lambda grid
lambda_grid <- 10^seq(-2, 10, length = 200)

ridge_cv <- conflicts_split %>%
  pluck("train", 1) %>% 
  glmnetUtils::cv.glmnet(
    formula = death_toll ~ UNSC_res_unanimous_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
    data = .,  
    alpha = 0, # for ridge regression
    nfolds = 10,
    lambda = lambda_grid
  )

# Check plot for lambdas
plot(ridge_cv)

# Ridge's best lambdas
ridge_lambda_min <- ridge_cv$lambda.min
ridge_lambda_min
ridge_lambda_1se <- ridge_cv$lambda.1se
ridge_lambda_1se

# Test error for ridge regression
conflict_glmnet <- conflicts_split %>% 
  mutate(
    ridge_min = map(train, ~ glmnetUtils::glmnet(death_toll ~ UNSC_res_unanimous_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
                                                 data = .x,
                                                 alpha = 0, 
                                                 lambda = ridge_lambda_min)),
    ridge_1se = map(train, ~ glmnetUtils::glmnet(death_toll ~ UNSC_res_unanimous_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
                                                 data = .x,
                                                 alpha = 0, 
                                                 lambda = ridge_lambda_1se))
  ) %>% 
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

conflict_glmnet %>% 
  mutate(pred = map2(fit, test, predict),
         test_error = map2_dbl(test, pred, ~ mean((.x$death_toll - .y)^2))) %>% 
  unnest(test_error) %>% 
  select(method, test_error) %>% 
  mutate(prop_var_explained = 1 - test_error/test_death_var) 

# Inspect model coefficient estimates
conflict_glmnet %>% 
  pluck("fit") %>% 
  map( ~ coef(.x) %>% 
         as.matrix() %>% 
         as.data.frame() %>% 
         rownames_to_column("name")) %>%
  reduce(full_join, by = "name") %>% 
  mutate_if(is.double, ~ if_else(. == 0, NA_real_, .)) %>% 
  rename(ridge_min = s0.x,
         ridge_1se = s0.y) 

# Ridge min regression only
conflict_glmnet_final <- conflicts_split %>% 
  mutate(
    ridge_min = map(train, ~ glmnetUtils::glmnet(death_toll ~ UNSC_res_unanimous_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
                                                 data = .x,
                                                 alpha = 0, 
                                                 lambda = ridge_lambda_min))
  ) %>% 
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

conflict_glmnet_final %>% 
  mutate(pred = map2(fit, test, predict),
         test_error = map2_dbl(test, pred, ~ mean((.x$death_toll - .y)^2))) %>% 
  unnest(test_error) %>% 
  select(method, test_error) 

ridge_error <- conflict_glmnet_final %>%
  mutate(pred = map2(fit, test, predict),
         test_error = map2_dbl(test, pred, ~ mean((.x$death_toll - .y)^2))) %>%
  unnest(test_error) %>%
  select(method, test_error)

ridge_error

# LASSO REGRESSION WITH 10-FOLD CROSS VALIDATION

set.seed(27182) # Used digits from e

# Lambda grid
lambda_grid <- 10^seq(-3, 3, length = 200)

lasso_cv <- conflicts_split %>%
  pluck("train", 1) %>% 
  glmnetUtils::cv.glmnet(
    formula = death_toll ~ UNSC_res_unanimous_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
    data = .,  
    alpha = 1, # for lasso regression
    nfolds = 10,
    lambda = lambda_grid
  )

# Check plot for lambdas
plot(lasso_cv)

# Lasso's best lambdas
lasso_lambda_min <- lasso_cv$lambda.min
lasso_lambda_min
lasso_lambda_1se <- lasso_cv$lambda.1se
lasso_lambda_1se

# Test error for lasso regression
conflict_glmnet <- conflicts_split %>% 
  mutate(
    lasso_min = map(train, ~ glmnetUtils::glmnet(death_toll ~ UNSC_res_unanimous_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
                                                 data = .x,
                                                 alpha = 1, 
                                                 lambda = lasso_lambda_min)),
    lasso_1se = map(train, ~ glmnetUtils::glmnet(death_toll ~ UNSC_res_unanimous_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
                                                 data = .x,
                                                 alpha = 1, 
                                                 lambda = lasso_lambda_1se))
  ) %>% 
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

conflict_glmnet %>% 
  mutate(pred = map2(fit, test, predict),
         test_error = map2_dbl(test, pred, ~ mean((.x$death_toll - .y)^2))) %>% 
  unnest(test_error) %>% 
  select(method, test_error) %>% 
  mutate(prop_var_explained = 1 - test_error/test_death_var)

# Inspect model coefficient estimates
conflict_glmnet %>% 
  pluck("fit") %>% 
  map( ~ coef(.x) %>% 
         as.matrix() %>% 
         as.data.frame() %>% 
         rownames_to_column("name")) %>%
  reduce(full_join, by = "name") %>% 
  mutate_if(is.double, ~ if_else(. == 0, NA_real_, .)) %>% 
  rename(lasso_min = s0.x,
         lasso_1se = s0.y) 

# Lasso min regression only
conflict_glmnet_final <- conflicts_split %>% 
  mutate(
    lasso_min = map(train, ~ glmnetUtils::glmnet(death_toll ~ UNSC_res_unanimous_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
                                                 data = .x,
                                                 alpha = 1, 
                                                 lambda = lasso_lambda_min))
  ) %>% 
  pivot_longer(cols = c(-test, -train), names_to = "method", values_to = "fit")

conflict_glmnet_final %>% 
  mutate(pred = map2(fit, test, predict),
         test_error = map2_dbl(test, pred, ~ mean((.x$death_toll - .y)^2))) %>% 
  unnest(test_error) %>% 
  select(method, test_error)

lasso_error <- conflict_glmnet_final %>%
  mutate(pred = map2(fit, test, predict),
         test_error = map2_dbl(test, pred, ~ mean((.x$death_toll - .y)^2))) %>%
  unnest(test_error) %>%
  select(method, test_error)

lasso_error

# BAGGING AND RANDOM FORESTS WITH 10-FOLD CROSS VALIDATION

# Helper function to get misclass rate from a ranger object
#' @name mse_ranger
#' @param model ranger object, a fitted random forest
#' @param test tibble/resample object, a test set
#' @param outcome string, indicates the outcome variable in the data
#' @returns MSE of the model on the test set
mse_ranger <- function(model, test, outcome){
  
  # Check if test is a tibble
  if(!is_tibble(test)){
    test <- test %>% as_tibble()
  }
  
  # Make predictions
  preds <- predict(model, test)$predictions
  
  # Compute MSE
  mse <- mean((test[[outcome]] - preds)^2)
  
  return(mse)
  
}

set.seed(27182) # Used digits from e

# Fit and tune regression models
conflicts_rf_reg <- conflicts_train %>%
  crossv_kfold(10, id = "fold") %>% # take resample objects and put them as a tibble to unpack them
  mutate(
    train = map(train, as_tibble),
    test = map(test, as_tibble)
  ) %>%
  crossing(mtry = 1:12) %>% # list all mtry values (12 predictors so 12 mtry)
  mutate(# Fit models for each value of mtry
    model = map2(.x = train, .y = mtry, 
                 .f = function(x, y) ranger(death_toll ~ UNSC_res_unanimous_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
                                            mtry = y, 
                                            data = x, 
                                            splitrule = "variance", # use 'variance' for regression
                                            importance = "impurity")), 
    # Get training, test, and OOB error
    train_err = map2(model, train, mse_ranger, outcome = "death_toll"), 
    test_err = map2(model, test, mse_ranger, outcome = "death_toll"), 
    oob_err = map(.x = model, 
                  .f = function(x) x[["prediction.error"]])
  )

# Test error for candidate models
conflicts_rf_reg_error <- conflicts_rf_reg %>%
  group_by(mtry) %>%
  unnest(c(test_err, oob_err, train_err)) %>%
  select(mtry, test_err, oob_err, train_err) %>%
  group_by(mtry) %>%
  summarise(test_error = mean(test_err), oob_err = mean(oob_err), train_err = mean(train_err)) %>%
  arrange(test_error)
conflicts_rf_reg_error

# Plot the training, test, and OOB error versus mtry
ggplot(conflicts_rf_reg_error) + 
  geom_line(aes(mtry, unlist(oob_err), color = "OOB Error")) +
  geom_line(aes(mtry, unlist(train_err), color = "Training Error")) +
  geom_line(aes(mtry, unlist(test_error), color = "Test Error")) + 
  labs(x = "mtry", y = "MSE") + 
  scale_color_manual("", values = c("purple", "red", "blue")) + 
  theme_bw()

rf_reg_error <- conflicts_split %>% 
  mutate(rf_mod = map(train, .f = function(x) ranger(death_toll ~ UNSC_res_unanimous_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, mtry = 6)), 
         test_error = map2_dbl(rf_mod, test, mse_ranger, outcome = "death_toll"))
bagged_reg_error <- conflicts_split %>% 
  mutate(rf_mod = map(train, .f = function(x) ranger(death_toll ~ UNSC_res_unanimous_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, data = x, mtry = 12)), 
         test_error = map2_dbl(rf_mod, test, mse_ranger, outcome = "death_toll"))

rf_reg_error <- rf_reg_error %>%
  mutate(method = str_c("random_forest")) %>%
  select(method, test_error)
rf_reg_error

bagging_reg_error <- bagged_reg_error %>%
  mutate(method = str_c("bagged_regression")) %>%
  select(method, test_error)
bagging_reg_error

# Tibble comparing test MSE rates from each method
model_comparison <- ols_error %>%
  bind_rows(ridge_error) %>% 
  bind_rows(lasso_error) %>% 
  bind_rows(rf_reg_error) %>% 
  bind_rows(bagging_reg_error) %>%
  arrange(test_error) 

model_comparison %>%
  rowid_to_column(var="rank") %>% 
  mutate(prop_var_explained = 1 - test_error/test_death_var) %>% 
  mutate(prop_var_unexplained = test_error/test_death_var) %>%
  dplyr::select(rank, method, test_error, prop_var_explained, prop_var_unexplained) %>%
  knitr::kable(digits = 3) %>%
  kable_styling(full_width = FALSE, position = "center")

# Variable importance plot with `mtry` 12
set.seed(27182) # Used digits from e
rf_reg_mtry12 = ranger(death_toll ~ UNSC_res_unanimous_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
                       data = conflicts_train, 
                       mtry = 12,
                       importance = "impurity", 
                       splitrule = "variance")
vip_plot_1 <- vip(rf_reg_mtry12)
vip_plot_1 +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=14,face="bold"))

set.seed(27182) # Used digits from e
rf_reg_mtry_imp = ranger(death_toll ~ UNSC_res_unanimous_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
                         data = conflicts_train, 
                         mtry = 12,
                         importance = "impurity_corrected", 
                         splitrule = "variance")
vip_plot_2 <- vip(rf_reg_mtry_imp)
vip_plot_2 +
  xlab("Variables") +
  theme_economist_white() +
  theme(text=element_text(family="Times", size=12)) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0), size=14,face="bold")) +
  theme(axis.title.x = element_text(margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0), size=14,face="bold"))

set.seed(27182) # Used digits from e
imp <- importance_pvalues(
  rf_reg_mtry_imp,
  method = "altmann",
  num.permutations = 1000,
  formula = death_toll ~ UNSC_res_unanimous_dummy + conflict_years + type_of_conflict + incompatibility + region + al_ethnic + al_language + al_religion + mad_gdppc + vdem_corr + vdem_egal + vdem_liberal, 
  data = conflicts_train)

as_tibble(imp, rownames = "term") %>%
  kable() %>%
  kable_styling(position = "center")

# Cold War Conflicts with Highest Death Toll
SpecificData <- FinalData %>%
  select(conflict_id, conflict_name, location, start_year, end_year, total_UNSC_res, total_UNSC_unanimous_res, UNSC_res_dummy, UNSC_res_unanimous_dummy, side_a, side_b, conflict_years, death_toll, type_of_conflict, incompatibility, region, al_ethnic, al_language, al_religion, mad_gdppc, vdem_corr, vdem_egal, vdem_liberal) %>%
  mutate(start = start_year + 1) %>%
  mutate(end = end_year - 1) %>%
  arrange(desc(death_toll)) %>%
  head(15)

TopFifteen <- SpecificData %>% 
  mutate(type_of_conflict = case_when(
    type_of_conflict == "1" ~ "Extrasystemic",
    type_of_conflict == "2" ~ "Interstate",
    type_of_conflict == "3" ~ "Internal",
    type_of_conflict == "4" ~ "Internationalized Internal")) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(conflict_name = case_when(
    conflict_id == 293 & is.na(conflict_name) ~ "Vietnam War",
    conflict_id == 202 & is.na(conflict_name) ~ "Chinese Civil War",
    conflict_id == 216 & is.na(conflict_name) ~ "First Indochina War",
    conflict_id == 246 & is.na(conflict_name) ~ "Algerian War",
    conflict_id == 249 & is.na(conflict_name) ~ "Second Indochina War",
    conflict_id == 275 & is.na(conflict_name) ~ "Eritrean War of Independence",
    conflict_id == 332 & is.na(conflict_name) ~ "Mozambican Civil War",
    conflict_id == 314 & is.na(conflict_name) ~ "War in Uganda",
    TRUE ~ conflict_name
  )) %>%
  mutate(start = replace(start, conflict_id == 202, 1946)) %>%
  mutate(end = replace(end, conflict_id == 333, 1991)) %>%
  mutate(start = replace(start, conflict_id == 216, 1946)) %>%
  mutate(end = replace(end, conflict_id == 300, 1991)) %>%
  mutate(start = replace(start, conflict_id == 203, 1946)) %>%
  mutate(end = replace(end, conflict_id == 275, 1991)) %>%
  mutate(end = replace(end, conflict_id == 332, 1991)) %>%
  mutate(end = replace(end, conflict_id == 314, 1991)) %>%
  mutate(end = replace(end, conflict_id == 327, 1991)) %>%
  select(conflict_id, conflict_name, location, start, end, total_UNSC_res, total_UNSC_unanimous_res, death_toll, type_of_conflict)

TopFifteen %>%
  select(conflict_name, location, start, end, total_UNSC_res, total_UNSC_unanimous_res, death_toll, type_of_conflict) %>%
  kable(col.names = c("Conflict", "Location", "Start", "End", "Number of UNSC Resolutions", "Number of unanimously adopted UNSC Resolutions", "Death Toll", "Type of Conflict"), "latex", booktabs = T)  %>% 
  column_spec(2, width = "3cm") %>%
  kable_styling(full_width = FALSE, position = "center", latex_options=c("scale_down","hold_position"), bootstrap_options = "striped")
