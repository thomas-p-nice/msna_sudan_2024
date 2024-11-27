# Adapted RMS indicators for the 2024 MSNA Sudan

# Preparation -------------------------------------------------------------

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
library(unhcrthemes)
library(extrafont)
library(scales)

#font_import()
loadfonts(device = "win")

### Load data

#### Refugees
main_ref <- read_excel(
  "data/Sudan_MSNA_HH_IN_PERSON_DC_2024.xlsx",
  sheet = "Sudan MSNA HH IN PERSON DC"
)

#### IDPs
main_idp <- read_excel(
  "data/IOM SUDAN MSNA 2024 Data set.xlsx",
  sheet = "SDN_MSNA_HH_data_clean")

##### Format columns for easy binding
main_ref <- main_ref |>
  mutate(across(everything(), as.character))

main_idp <- main_idp |>
  mutate(across(everything(), as.character))

#### Merge 
main <- bind_rows(main_ref, main_idp)

#### Remove returnees (only few observations, probably errors)
main <- main |>
  filter(!strata %in% c("returnees", "non_disp")) |>
  mutate(strata = case_when(strata == "refugee" ~ "Refugees",
                            strata == "idp" ~ "IDPs",
                            TRUE ~ strata))

# Core Impact Indicators --------------------------------------------------

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
                                   "No" = 0,
                                   "Yes" = 1
                                 ),
                                 label = "Access to electricity"))
# Function for table by group
group_table <- function(dat = main, var) {
  dat_grouped <- dat |>
    filter(!is.na({{var}}), strata != "returnees") |>
    group_by(strata, {{var}}) |>
    summarise(n = n()) |>
    group_by(strata) |>
    mutate(prop = n / sum(n)) |> 
    ungroup() |>
    to_factor() |>
    filter({{var}} == "Yes")
  
  return(dat_grouped)
}
group_table(var = electricity)

###Step2. Healthcare

##Access to healthcare if household has any facility available excluding 'don't know' and 'other'
##within one hour distance (cannot be > 60) (walking or any other type of transport)

main <- main %>%
  mutate(healthcare = case_when(hh_min_health %in% c("15", "1630", "3160") ~ 1,
                                hh_min_health == "more_than_ 60_60" ~ 0)
  ) %>%
  mutate(healthcare = labelled(healthcare,
                               labels = c(
                                  "No" = 0,
                                  "Yes" = 1
                                ),
                                label = "Access to healthcare facility"))

group_table(var = healthcare)

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
                                    "No" = 0,
                                    "Yes" = 1
                                  ),
                                  label = "Access to drinking water"))

group_table(var = drinkingwater)

###Step 4. Habitable housing

##Classify as habitable unless respondent reports leaking, inability to lock or a lack of space - if unk selected, put into missing

main <- main %>%
  mutate(hh_shelter_issues = case_when(hh_shelter_issues == "unk" ~ NA_character_, TRUE ~ hh_shelter_issues)) |>
  mutate(shelter = case_when(grepl("leak|lock|lack_space", hh_shelter_issues) ~ 0, TRUE ~ 1))

group_table(var = shelter)

##Step 5. Safe and secure settlements are those with no risks and hazards like flooding, landslides,
###landmines, and close proximity to military installations and hazardous zones

# Need to use proxy for flooding in past 12 months: classified here as frequency being never or rarely. No question on landmines

main <- main %>%
  mutate(secure=case_when(
    hh_flood_frequency %in% c("never_happens", "rarely_heavy_rain") ~ 1,
    TRUE ~ 0
  ))

group_table(var = secure)

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
      labels = c("No" = 0, 
                 "Yes" = 1),
      label = "Households residing in physically safe and secure settlements with access to basic facilities"
    )
  )

impact2_2_tab <- group_table(var = impact2_2)


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
                              "No" = 0,
                              "Yes" = 1
                            ),
                            label="Household with access to health services"))

# Outcome indicators ------------------------------------------------------

###8.2 Proportion of people with primary reliance on clean (cooking) fuels and technology

main <- main %>%
  mutate(outcome8_2 = case_when(
    hh_fuel_type %in% c("electric_stove", "solar", "solar_cooker", "lpg_cooking_gas") ~ 1,
    TRUE ~ 0
  )) %>%
  mutate(
    outcome8_2 = labelled(
      outcome8_2,
      labels = c("No" = 0, 
                 "Yes" = 1),
      label = "Household with primary reliance on clean (cooking) fuels and technology"
    )
  )

###9.2 Households that have energy to ensure lighting
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
      labels = c("No" = 0, 
                 "Yes" = 1),
      label = "Household with energy to ensure lighting"
    )
  )

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
                                  "No" = 0,
                                  "Yes" = 1
                                ),
                                label = "Household with at least basic drinking water services"))

###12.2 Proportion of people with access to a safe household toilet

##This indicator measures the proportion of people with access to at
##least basic sanitation services.

main <- main %>%
  mutate(toi_cond1=case_when(hh_sanitation_facility %in% c("flush_toilet", "pit_latrine_with_slab") ~ 1,
                             hh_sanitation_facility %in% "other_specify" ~ NA_real_,
                             TRUE ~ 0)) %>%
  mutate(toi_cond2=case_when(
    grepl("not functioning or full", hh_sanitation_problems) ~ 0, TRUE ~ 1) # assume lack of disposal if full
  ) %>%
  mutate(toi_cond3=case_when(
    hh_share_facility=="no" ~ 1, TRUE ~ 0) # toilet not shared with other households
  ) %>%
  mutate(outcome12_2=case_when(
    toi_cond1==1 & toi_cond2 == 1 & toi_cond3==1 ~ 1,
    TRUE ~ 0)
  ) %>%
  mutate(outcome12_2 = labelled(outcome12_2,
                                labels = c(
                                  "No" = 0,
                                  "Yes" = 1
                                ),
                                label = "Household with access to at least basic sanitation services."))

###13.2 Proportion of people who self-report positive changes in their income compared to previous year

# Here there is a single question on whether the household income dropped in the last month, meaning that the indicator measures something different.

main <- main %>%
  mutate(outcome13_2 = case_when(
    hh_perceived_income_drop == "no" ~ 1,  # Income did not decrease
    TRUE ~ 0  # Default to 0
  )) %>%
  mutate(outcome13_2 = labelled(outcome13_2,
                                labels = c(
                                  "No" = 0,
                                  "Yes" = 1
                                ),
                                label = "No self-reported negative changes in their last monthly income income compared to previous other months"
  ))


###16.2 Proportion of people covered by national social protection systems

##Adapted to here look at whether government assistance/benefits are one of three main income sources during previous three months
main <- main %>%
  mutate(outcome16_2 = case_when(
      grepl("government_assistance", hh_main_income) ~ 1, TRUE ~ 0)
  ) %>%
  mutate(outcome16_2=labelled(outcome16_2,
                              labels=c(
                                'No'=0,
                                'Yes'=1
                              ),
                              label="Household covered by national social protection (one of three main income sources)"
                              )
  )

# Extrapolation -----------------------------------------------------------
## Multiply the indicator value with the hhsize and sum

extr <- main |>
  mutate(across(starts_with("impact") | starts_with("outcome"), 
                .fns = ~ . * as.numeric(hh_size), 
                .names = "{.col}_num")) |>
  group_by(strata) |>
  summarise(Denominator = sum(as.numeric(hh_size), na.rm = TRUE),
            across(ends_with("_num"), ~ sum(.x, na.rm = TRUE), .names = "sum_{.col}")) |>
  pivot_longer(cols = ends_with("num"), names_to = "Indicator", values_to = "Numerator")

## Clean Indicator column. Add percentage
extr <- extr |>
  mutate(
    Indicator = str_remove_all(Indicator, "sum_|_num") |>
      str_replace_all("impact", "impact ") |>
      str_replace_all("outcome", "outcome ") |>
      str_replace_all("_", ".") |>
      str_to_title()
  ) |>
  mutate(Percentage = Numerator/Denominator) |>
  select(Stratum = strata, Indicator, Numerator, Denominator, Percentage)

## Add percentage in clean format
extr_lbl <- extr |>
  mutate(Percentage = percent(Percentage, .01))

# Plots -------------------------------------------------------------------
### Using extrapolated values
# Function for bar charts, using the extrapolated table indicator and title need to be adapted
bar_chart <- function(ind, title_text) {
  ggplot(extr |>
           filter(Indicator == ind), 
         aes(x = Stratum, fill = ind)) +
    geom_bar(aes(y = Percentage),
             width = 0.6,
             position = "dodge",
             stat = "identity") +
    geom_text(aes(label = percent(Percentage, 1), y = Percentage),
              hjust = -0.25,
              stat = "identity") +
    labs(title = title_text, x = "", y = "") +
    theme_unhcr() +
    scale_y_continuous(expand = expansion(c(0, 0.1)),
                       labels = percent,
                       limits = c(0, 1)) +
    scale_fill_unhcr_d(palette = "pal_unhcr") +
    theme_unhcr(grid = "X",
                axis = "y",
                axis_title = FALSE) +
    coord_flip() +
    guides(fill = "none")
}

impact2_2_plot <- bar_chart("Impact 2.2", "Impact 2.2: Proportion of households residing in physically safe and secure settlements with access to basic facilities")
impact2_2_plot
ggsave("figures/impact2_2.png", width=10, height=4)

impact2_3_plot <- bar_chart("Impact 2.3", "Impact 2.3: Proportion of households with access to health services")
impact2_3_plot 
ggsave("figures/impact2_3.png", width=10, height=4)

<<<<<<< HEAD
outcome8_2_plot <- bar_chart("Outcome 8.2", "Outcome 8.2: Proportion of households with primary reliance on clean (cooking) fuels and technology")
outcome8_2_plot
ggsave("figures/outcome8_2.png", width=10, height=4)

outcome9_2_plot <- bar_chart("Outcome 9.2", "Outcome 9.2: Proportion of households with clean energy to ensure lighting")
outcome9_2_plot
ggsave("figures/outcome9_2.png", width=10, height=4)

outcome12_1_plot <- bar_chart("Outcome 12.1", "Outcome 12.1: Proportion of households with at least basic drinking water services")
outcome12_1_plot
ggsave("figures/outcome12_1.png", width=10, height=4)

outcome12_2_plot <- bar_chart("Outcome 12.2", "Outcome 12.2: Proportion of households with access to a safe household toilet")
outcome12_2_plot
ggsave("figures/outcome12_2.png", width=10, height=4)

outcome13_2_plot <- bar_chart("Outcome 13.2", "Outcome 13.2: Proportion of households who do not self-report negative changes in their last monthly income compared to other months*")
outcome13_2_plot
ggsave("figures/outcome13_2.png", width=10, height=4)

outcome16_2_plot <- bar_chart("Outcome 16.2", "Outcome 16.2: Proportion of households covered by national social protection floors/systems*")
outcome16_2_plot
ggsave("figures/outcome16_2.png", width=10, height=4)

# Save tables -------------------------------------------------------------

write_xlsx(extr_lbl, "data/Sudan_MSNA_2024_RMS_extrapolated.xlsx")
write_xlsx(main, "data/Sudan_MSNA_HH_IN_PERSON_DC_2024_RMS_ind.xlsx")
