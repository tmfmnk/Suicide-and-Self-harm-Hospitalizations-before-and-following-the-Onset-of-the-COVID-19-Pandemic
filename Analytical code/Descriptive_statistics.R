#Libraries

library(data.table)
library(tidyverse)
library(lubridate)
library(survival)
library(broom)
library(patchwork)
library(ggtext)
library(patchwork)
library(survminer)
library(ggpubr)
library(scales)

#Import data

matched_cohorts <-  list.files(path = "path/Data/UZIS/Matched_cohorts", 
                               full.names = TRUE) %>%
  set_names(map(., 
                ~ fread(., 
                        nThread = 16)),
            .)

#Rename lists

matched_cohorts <- set_names(matched_cohorts, str_extract(names(matched_cohorts), "(?<=suic_).+(?=.csv)"))

#Data transformation
#Changing outcomes to a numeric variable
#Individuals who died because of other reasons are censored in models on suicide risk
#Individuals who died are censored in models on self-harm hospitalization risk

matched_cohorts <- map(.x = matched_cohorts,
                       ~ .x %>%
                         mutate(suicide_binary = as.numeric(umrti == "umrti sebevražda"),
                                selfharm_binary = as.numeric(hospit_pro_sebev == "hospitalizace pro sebevraždu"),
                                exposure = ifelse(str_detect(cohort, "KS"), "unexposed", "exposed"),
                                exposure_factor = factor(exposure, levels = c("unexposed", "exposed"))) %>%
                         rename(time_to_suicide = doba_do_umrti,
                                time_to_selfharm = doba_do_sebev,
                                sex = pohlavi,
                                age_group = vek_skupina,
                                index_year = index_record_rok,
                                index_month = index_record_mesic,
                                strata = propojeni_cohort_KSbezDg))

#Models
#Comparison with counterparts without F diagnosis
#Splitting data per years
#Removing unmatched exposed individuals

matched_cohorts_per_years_dg <- imap(.x = matched_cohorts,
                                     ~ .x %>%
                                       filter(strata != -1) %>%
                                       add_count(strata, index_year) %>%
                                       filter(n > 1) %>%
                                       filter(!age_group %in% c("0-4", "5-9")) %>%
                                       mutate(age_group = case_when(age_group %in% c("10-14", "15-19") ~ "10-19",
                                                                    age_group %in% c("20-24", "25-29") ~ "20-29",
                                                                    age_group %in% c("30-34", "35-39") ~ "30-39",
                                                                    age_group %in% c("40-44", "45-49") ~ "40-49",
                                                                    age_group %in% c("50-54", "55-59") ~ "50-59",
                                                                    age_group %in% c("60-64", "65-69") ~ "60-69",
                                                                    age_group %in% c("70-74", "75-79") ~ "70-79",
                                                                    age_group %in% c("80-84", "85-89", "90-94", "95-99", "100-104") ~ "80+")) %>%
                                       select(-n) %>%
                                       mutate(cohort = .y,
                                              cohort_name = case_when(cohort == "F0F1F2F3F4G30" ~ "Any mental disorder",
                                                                      cohort == "F0G30" ~ "Organic mental disorders",
                                                                      cohort == "F1" ~ "Substance use disorders",
                                                                      cohort == "F2" ~ "Psychotic disorders",
                                                                      cohort == "F3" ~ "Mood disorders",
                                                                      cohort == "F4" ~ "Anxiety disorders"),
                                              cohort_name = factor(cohort_name,
                                                                   levels = c("Any mental disorder", 
                                                                              "Organic mental disorders",
                                                                              "Substance use disorders",
                                                                              "Psychotic disorders",
                                                                              "Mood disorders",
                                                                              "Anxiety disorders"))) %>%
                                       group_split(index_year))

#Descriptive statistics

map_dfr(matched_cohorts_per_years_dg,
        function(data_f_diag) {
          
          map_dfr(data_f_diag,
                  function(data_year) {
                    
                    data_year %>%
                      group_by(exposure_factor, index_year, cohort, cohort_name) %>%
                      summarise(n = formatC(n(), big.mark = " "),
                                males = paste(formatC(sum(sex == "M"), big.mark = " "),
                                              paste0("(",
                                                     formatC(round(sum(sex == "M")/n() * 100, 2), format = "f", digits = 2),
                                                     ")")),
                                females = paste(formatC(sum(sex == "Z"), big.mark = " "),
                                                paste0("(",
                                                       formatC(round(sum(sex == "Z")/n() * 100, 2), format = "f", digits = 2),
                                                       ")"))) %>%
                      ungroup() %>%
                      pivot_wider(names_from = "exposure_factor",
                                  names_glue = "{exposure_factor}_{.value}",
                                  values_from = c("n", "males", "females"))
                  })
        }) %>%
  write.csv(file = "path/Results/Descriptive_statistics.csv",
            row.names = FALSE)
