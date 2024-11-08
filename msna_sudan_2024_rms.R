# Adapted RMS indicators for the 2024 MSNA Sudan

## Preparation
### Load packages
library(haven)
library(tidyverse)
library(readxl)
library(srvyr)
library(ggplot2)
library(robotoolbox)
library(labelled)
library(remotes)
library(dm)
library(janitor)
library(visdat)
library(dplyr)
library(writexl)

### Load data
main <- read_excel("data/Sudan_MSNA_HH_IN_PERSON_DC_2024.xlsx", sheet = "Sudan MSNA HH IN PERSON DC")
roster <- read_excel("data/Sudan_MSNA_HH_IN_PERSON_DC_2024.xlsx", sheet = "hh_roster")

# Core Impact Indicators

## 2.2 Proportion of people residing in physically safe and secure settlements with access to basic facilities

### Step.1. Electricity

##Here we will create a binary variable if they use anything for lighting (LIGHT01), 
##and the light source for most of the time is electricity (LIGHT02) - 
##exclude cases if they selected LIGHT03 - 0 ( no electricity)

main <- main %>%
  mutate(electricity = ifelse(hh_used_energy == "electricity", 1, 0)
  ) %>%
  mutate( electricity = labelled(electricity,
                                 labels = c(
                                   "Yes" = 1,
                                   "No" = 0
                                 ),
                                 label = "Access to electricity"))

table(main$electricity)

###Step2. Healthcare

##Access to healthcare if household has any facility available excluding 'don't know' and 'other' 
##within one hour distance (cannot be > 60) (walking or any other type of transport)

main <- main %>%
  mutate(healthcare = case_when(hh_min_health %in% c("15", "1630", "3160") ~ 1, 
                                hh_min_health == "more_than_ 60_60" ~ 0)
  ) %>%
  mutate( healthcare = labelled(healthcare,
                                labels = c(
                                  "Yes" = 1,
                                  "No" = 0
                                ),
                                label = "Access to healthcare facility"))

table(main$healthcare)

###Step3. Drinking water

##Access to drinking water is calculated as below
##use improved sources of drinking water in their housing or within 30 minutes round trip collection time

main <- main %>%
  mutate(dwa_cond1=case_when(hh_time_to_water %in% c("15", "1630") ~ 1, 
                             TRUE ~ 0) # reachable under 30 minutes or NA
  ) %>% 
  mutate(dwa_cond2=case_when(hh_water_source %in% c("handpumps_boreholes", "piped_connection",
                                                    "protected_spring", "protected_well", "public_tap", 
                                                    "water_seller_kiosks") ~ 1,
                             TRUE ~ 0) # improved source only
  ) %>% 
  mutate(drinkingwater=case_when(
    (dwa_cond1==1 & dwa_cond2==1 ) ~ 1, TRUE ~ 0
  )) %>%
  mutate(drinkingwater = labelled(drinkingwater,
                                  labels = c(
                                    "Yes" = 1,
                                    "No" = 0
                                  ),
                                  label = "Access to drinking water"))

table(main$drinkingwater)

###Step 4. Habitable housing

##Condition 1
##Classify as habitable for below conditions - if unk selected, put into missing

main <- main %>%
  mutate(hh_shelter_issues = case_when(hh_shelter_issues == "unk" ~ NA_character_, TRUE ~ hh_shelter_issues)) |>
  mutate(housing = case_when(grepl("leak|lock|lack_space", hh_shelter_issues) ~ 0, TRUE ~ 1))

table(main$housing)

##Condition 2
####Calculate crowding index - overcrowded when more than 3 persons share one room to sleep
###Overcrowding may cause health issues, thus not considered as physically safe


###Can't measure overcrowding here, use proxy lack_space 

###Combine both conditions for habitable housing

main <- main %>%
  mutate(shelter = case_when(
    housing == 1 &
      hh_shelter_type %in% c("solid_apartment", "solid_house") ~ 1,
    TRUE ~ 0
  ))

table(main$shelter)


##Step 5. Safe and secure settlements are those with no risks and hazards like flooding, landslides, 
###landmines, and close proximity to military installations and hazardous zones

# Need to use proxy for flooding in past 12 months: classified here as frequency == never - to check. No question on landmines

main <- main %>%
  mutate(secure=case_when(
    hh_flood_frequency %in% c("1", "2") ~ 1,
    TRUE ~ 1
  ))

table(main$secure)

##Step 6. Combine all services

###Calculate impact indicator based on electricity, healthcare, drinkingwater, shelter and secure

##Impact 2.2 is "1" if all services above are accessible

main <- main %>%
  mutate(
    impact2_2 = case_when(
      shelter == 0 |
        electricity == 0 |
        drinkingwater == 0 | healthcare == 0 | secure == 0 ~ 0,
      shelter == 1 &
        electricity == 1 &
        drinkingwater == 1 & healthcare == 1 & secure == 1 ~ 1
    )
  ) %>%
  mutate(
    impact2_2 = labelled(
      impact2_2,
      labels = c("Yes" = 1, "No" = 0),
      label = "Proportion of people residing in physically safe and secure settlements with access to basic facilities"
    )
  )


table(main$impact2_2)

#table_impact2_2 <- table(main$impact2_2, main$pop_groups)
#percentage_impact2_2 <- prop.table(table_impact2_2, margin = 2) * 100


###Check distribution

ggplot(main, aes(x = factor(impact2_2))) +
  geom_bar(stat = "count", fill = "skyblue", color = "black") +
  labs(title = "Distribution of Impact Indicator 2.2",
       x = "Impact Indicator 2.2",
       y = "Count") +
  theme_minimal()

###2.3 Proportion of people with access to health services

##Calculate those who needed and accessed health services

##No follow up question if people were not able to access healthcare why not
main <- main %>%
  mutate(impact2_3=case_when(
    hh_health_prob=="yes" & hh_obtain_health =="yes" ~ 1,
    hh_health_prob %in% c("no", "not_applicable", "unk") ~ NA ,
    TRUE ~ 0))  %>%
  mutate(impact2_3=labelled(impact2_3,
                            labels =c(
                              "Yes"=1,
                              "No"=0
                            ),
                            label="Proportion of people with access to health"))

table(main$impact2_3)


ggplot(main, aes(x = factor(impact2_3), fill = factor(impact2_3))) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  labs(title = "Proportion of People with Access to Health Services",
       x = "Access to Health Services",
       y = "Count") +
  scale_x_discrete(labels = c("No" = 0, "Yes" = 1)) +
  theme_minimal()

###3.2a: Proportion of children and young people enrolled in primary education

roster <- roster %>%
  mutate(edu = case_when(hh_school_attendace_4days == "yes" ~ 1, TRUE ~ 0)) %>%
  mutate(age_primary = case_when(age_years >= 6 &
                                   age_years <= 10 ~ 1, TRUE ~ 0)) |> #Adjust age group for primary school  enrollment )
  mutate(impact3_2a = case_when(
    edu == 1 & age_primary == 1 ~ 1,
    edu == 0 & age_primary == 1 ~ 0,
    TRUE ~ NA_real_
  )) %>%
  mutate(
    impact3_2a = labelled(
      impact3_2a,
      labels = c("Yes" = 1, "No" = 0),
      label = "Proportion of children and young people enrolled in primary education"
    )
  )

mean(roster$impact3_2a, na.rm = TRUE)
table(roster$impact3_2a)

ggplot(roster, aes(x = factor(impact3_2a), fill = factor(impact3_2a))) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  labs(title = "Proportion of children and young people enrolled in primary education",
       x = "Enrollment in Primary Education",
       y = "Count") +
  scale_x_discrete(labels = c("No" = 0, "Yes" = 1)) +
  theme_minimal()

###3.2b: Proportion of children and young people enrolled in secondary education

roster <- roster %>%
  mutate(age_secondary=case_when(
    age_years >= 11 & age_years <=18 ~ 1, TRUE ~ 0)
    
    #Adjust age group for secondary school enrollment 
    ##calculated as 11 to 18 above 
  ) %>%
  mutate(impact3_2b= case_when(
    edu == 1 & age_secondary == 1 ~ 1,
    edu == 0 & age_secondary == 1 ~ 0,
    TRUE ~ NA_real_)
  ) %>%
  mutate(impact3_2b=labelled(impact3_2b,
                             labels =c(
                               "Yes"=1,
                               "No"=0
                             ),
                             label="Proportion of children and young people enrolled in secondary education"))

mean(roster$impact3_2b, na.rm = TRUE)
table(roster$impact3_2b)

ggplot(ind, aes(x = factor(impact3_2b), fill = factor(impact3_2a))) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  labs(title = "Proportion of Persons Enrolled in Secondary Education",
       x = "Enrollment in Primary Education",
       y = "Count") +
  scale_x_discrete(labels = c("No" = 0, "Yes" = 1)) +
  theme_minimal()

###8.2 Proportion of people with primary reliance on clean (cooking) fuels and technology

main <- main %>%
  mutate(outcome8_2 = case_when(
    hh_fuel_type %in% c("electric_stove", "solar", "solar_cooker", "lpg_cooking_gas") ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(
    outcome8_2 = labelled(
      outcome8_2,
      labels = c("No" = 0, "Yes" = 1),
      label = "Proportion of people with primary reliance on clean (cooking) fuels and technology"
    )
  )

table(main$outcome8_2)

ggplot(main, aes(x = factor(outcome8_2), fill = factor(outcome8_2))) +
  geom_bar(stat = "count",
           position = "stack",
           show.legend = FALSE) +
  labs(title = "Proportion of people with primary reliance on clean (cooking) fuels and technology", x = "Primary reliance on clean (cooking) fuels and technology", y = "Count") +
  scale_x_discrete(labels = c("No" = 0, "Yes" = 1)) +
  theme_minimal()

###9.2 Proportion of people that have energy to ensure lighting
### The below Calculates percentage of PoC having access to clean fuel for lighting and / or basic connectivity (9.1 Outcome Indicator)

main <- main %>%
  mutate(outcome9_2 =
           case_when(
             hh_used_energy %in% c(
               "battery_flashlight",
               "electricity",
               "lpg_lamp",
               "rechargeable_flashlight",
               "solar_lantern"
             ) ~ 1,
             TRUE ~ 0
           )) %>%
  mutate(
    outcome9_2 = labelled(
      outcome9_2,
      labels = c("Yes" = 1, "No" = 0),
      label = "Proportion of people that have energy to ensure lighting"
    )
  )

table(main$outcome9_2)

ggplot(main, aes(x = factor(outcome9_2), fill = factor(outcome9_2))) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  labs(title = "Proportion of people that have energy to ensure lighting",
       x = "Have energy to ensure lighting",
       y = "Count") +
  scale_x_discrete(labels = c("No" = 0, "Yes" = 1)) +
  theme_minimal()

###12.1 Proportion of people using at least basic drinking water services

##There are three conditions as below 
##improved source, in dwelling/yard/plot or reachable under 30 minutes 

main <- main %>%
  mutate(dwa_cond1=case_when(hh_time_to_water %in% c("15", "1630") ~ 1, 
                             TRUE ~ 0) # reachable under 30 minutes or NA
  ) %>% 
  mutate(dwa_cond2=case_when(hh_water_source %in% c("handpumps_boreholes", "piped_connection",
                                                    "protected_spring", "protected_well", "public_tap", 
                                                    "water_seller_kiosks") ~ 1,
                             TRUE ~ 0) # improved source only
  ) %>% 
  mutate(outcome12_1=case_when(
    (dwa_cond1==1 & dwa_cond2==1 ) ~ 1, TRUE ~ 0
  )) %>%
  mutate(outcome12_1 = labelled(outcome12_1,
                                labels = c(
                                  "Yes" = 1,
                                  "No" = 0
                                ),
                                label = "Access to drinking water"))

table(main$outcome12_1)

ggplot(main, aes(x = factor(outcome12_1), fill = factor(outcome12_1))) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  labs(title = "Proportion of people using at least basic drinking water services",
       x = "People using at least basic drinking water services",
       y = "Count") +
  scale_x_discrete(labels = c("No" = 0, "Yes" = 1)) +
  theme_minimal()

###12.2 Proportion of people with access to a safe household toilet

##This indicator measures the proportion of people with access to at 
##least basic sanitation services.

##Here no information on safe disposal

main <- main %>%
  mutate(toi_cond1=case_when(hh_sanitation_facility %in% c("flush_toilet", "pit_latrine_with_slab") ~ 1,
                             hh_sanitation_facility %in% "other_specify" ~ NA_real_,
                             TRUE ~ 0)) %>%
  # mutate(toi_cond2=case_when(
  #   TOI02==1 & (TOI03==5 |TOI03==96 | TOI03==98) ~ 0, #Unsafe disposal
  #   TOI02==1 & (TOI03==1 |TOI03==2 |TOI03==3 |TOI03==4 ) ~ 1, #safe
  #   TOI02==2 ~ 0, TOI02==98 ~ 0, TRUE ~ NA_real_)
  # ) %>%
  mutate(toi_cond3=case_when(
    hh_share_facility=="no" ~ 1, TRUE ~ 0) # toilet not shared with other households
  ) %>%
  mutate(outcome12_2=case_when(
    toi_cond1==1 & toi_cond3==1 ~ 1,
    TRUE ~ 0)
  ) %>%
  mutate(outcome12_2 = labelled(outcome12_2,
                                labels = c(
                                  "Yes" = 1,
                                  "No" = 0
                                ),
                                label = "Proportion of people with access to at least basic sanitation services."))

table(main$outcome12_2)

ggplot(main, aes(x = factor(outcome12_2), fill = factor(outcome12_2))) +
  geom_bar(stat = "count", position = "stack", show.legend = FALSE) +
  labs(title = "Proportion of people with access to a safe household toilet",
       x = "People with access to a safe household toilet",
       y = "Count") +
  scale_x_discrete(labels = c("No" = 0, "Yes" = 1)) +
  theme_minimal()
