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
#Excluding individuals aged 0 to 9 years

matched_cohorts_per_years_dg <- imap(.x = matched_cohorts,
                                     ~ .x %>%
                                       filter(strata != -1) %>%
                                       add_count(strata, index_year) %>%
                                       filter(n > 1) %>%
                                       select(-n) %>%
                                       filter(!age_group %in% c("0-4", "5-9")) %>%
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

#Risk of suicide

models_suicide_dg <- map_dfr(matched_cohorts_per_years_dg,
                             function(data_f_diag) {
                               
                               map_dfr(data_f_diag, 
                                       function(data_year) {
                                         
                                         tidy(coxph(Surv(time_to_suicide, suicide_binary) ~ exposure_factor + sex + age_group + index_year + index_month + strata(strata),
                                                    data = data_year),
                                              conf.int = TRUE,
                                              exponentiate = TRUE) %>%
                                           filter(term == "exposure_factorexposed") %>%
                                           mutate(year = data_year$index_year[1],
                                                  cohort = data_year$cohort[1],
                                                  cohort_name = data_year$cohort_name[1])
                                       })
                             })

#Risk of self-harm hospitalization

models_selfharm_dg <- map_dfr(matched_cohorts_per_years_dg,
                              function(data_f_diag) {
                                
                                map_dfr(data_f_diag, 
                                        function(data_year) {
                                          
                                          tidy(coxph(Surv(time_to_selfharm, selfharm_binary) ~ exposure_factor + sex + age_group + index_year + index_month + strata(strata),
                                                     data = data_year),
                                               conf.int = TRUE,
                                               exponentiate = TRUE) %>%
                                            filter(term == "exposure_factorexposed") %>%
                                            mutate(year = data_year$index_year[1],
                                                   cohort = data_year$cohort[1],
                                                   cohort_name = data_year$cohort_name[1])
                                        })
                              })

#Graphs with results from stratified Cox models
#Risk of suicide when compared with individuals with no diagnosis

p_suicide_dg <- models_suicide_dg %>%
  group_by(cohort) %>%
  mutate(color = rep(c("white", "gray95"), length.out = n())) %>%
  ungroup() %>%
  mutate(year = fct_rev(as.character(year))) %>%
  ggplot(aes(x = estimate, y = year, xmin = conf.low, xmax = conf.high)) +
  geom_hline(aes(yintercept = year, color = color), size = 7) + 
  geom_pointrange(shape = 22, fill = "black") +
  geom_vline(xintercept = 1, linetype = 3) +
  theme_classic() +
  scale_colour_identity() +
  scale_x_log10() +
  facet_wrap(. ~ cohort_name,
             scales = "free_x",
             ncol = 2) +
  labs(title = "Suicide risk compared with matched \ncounterparts without the target mental disorder",
       y = "Year",
       x = "Adjusted hazard ratio (95% CI)") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 13),
        panel.border = element_rect(colour = "black", fill = NA))

#Risk of self-harm when compared with individuals with no diagnosis

p_selfharm_dg <- models_selfharm_dg %>%
  group_by(cohort) %>%
  mutate(color = rep(c("white", "gray95"), length.out = n())) %>%
  ungroup() %>%
  mutate(year = fct_rev(as.character(year))) %>%
  ggplot(aes(x = estimate, y = year, xmin = conf.low, xmax = conf.high)) +
  geom_hline(aes(yintercept = year, color = color), size = 7) + 
  geom_pointrange(shape = 22, fill = "black") +
  geom_vline(xintercept = 1, linetype = 3) +
  theme_classic() +
  scale_colour_identity() +
  scale_x_log10() +
  facet_wrap(. ~ cohort_name,
             scales = "free_x",
             ncol = 2) +
  labs(title = "Self-harm risk compared with matched \ncounterparts without the target mental disorder",
       y = "Year",
       x = "Adjusted hazard ratio (95% CI)") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 13),
        panel.border = element_rect(colour = "black", fill = NA))

#Combine subplots

p_suicide_dg + p_selfharm_dg

#Save graph

ggsave(file = "path/Results/Matched_cohorts_HR.eps",
       device = cairo_ps,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 300)

#Cumulative event graphs
#Define custom plotting function

custom_theme <- function() {
  theme_survminer() %+replace%
    theme(plot.title = element_text(face = "bold", hjust = 0.5),
          axis.title.x = element_text(face = "bold", size = 10),
          axis.title.y = element_text(face = "bold", size = 10, angle = 90),
          axis.text.x = element_text(face = "bold", size = 10),
          axis.text.y = element_text(face = "bold", size = 10),
          axis.ticks.x = element_blank(),    
          axis.ticks.y = element_blank(),    
          legend.text = element_text(face = "bold", size = 10),
          legend.title = element_blank())
}

#Avoid scientific notation

options(scipen = 999)

#Programmatic ploting 
#Risk of suicide when compared with individuals with no diagnosis

map(matched_cohorts_per_years_dg,
        function(data_f_diag) {
          
          map(data_f_diag, 
                  function(data_year) {
                    
                    p <- ggsurvplot(surv_fit(Surv(time_to_suicide, suicide_binary) ~ exposure_factor,
                                             data = data_year,
                                             match.fd = FALSE),
                                    fun = "event",
                                    conf.int = TRUE, 
                                    conf.int.alpha = 0.3,
                                    risk.table = TRUE,
                                    cumcensor = TRUE,
                                    cumevents = TRUE,
                                    legend.labs = c(paste0("Matched counterparts without ", tolower(data_year$cohort_name[1])), paste0("Individuals with ", tolower(data_year$cohort_name[1]))),
                                    title = paste0("Suicide in people with ", tolower(data_year$cohort_name[1]), " in year ", data_year$index_year[1]),
                                    xlab = "Time (days) since the index record",
                                    ylab = "Cumulative event (95% CI)",
                                    palette = c("#0072B2", "#D55E00"),
                                    legend.title = "",
                                    ggtheme = custom_theme())
                    
                    p1 = p$plot
                    p2 = p$table
                    p3 = p$ncensor.plot
                    p4 <- p$cumevents
                    plots = cowplot::plot_grid(p1, p2, p3, p4, align = "v", ncol = 1, rel_heights = c(4, 1, 1, 1))
                    
                    ggsave(plot = plots,
                           filename = paste0("Cumulative_event_plot_suicide_dg_", tolower(data_year$cohort_name[1]), "_", data_year$index_year[1], ".png"),
                           path = "path/Results/Cumulative_event_plots/",
                           device = "png",
                           width = 12, 
                           height = 7, 
                           dpi = 300)
                             
                    })
          })
          
#Risk of self-harm when compared with individuals with no diagnosis

map(matched_cohorts_per_years_dg,
    function(data_f_diag) {
      
      map(data_f_diag, 
          function(data_year) {
            
            p <- ggsurvplot(surv_fit(Surv(time_to_selfharm, selfharm_binary) ~ exposure_factor,
                                     data = data_year,
                                     match.fd = FALSE),
                            fun = "event",
                            conf.int = TRUE, 
                            conf.int.alpha = 0.3,
                            risk.table = TRUE,
                            cumcensor = TRUE,
                            cumevents = TRUE,
                            legend.labs = c(paste0("Matched counterparts without ", tolower(data_year$cohort_name[1])), paste0("Individuals with ", tolower(data_year$cohort_name[1]))),
                            title = paste0("Self-harm in people with ", tolower(data_year$cohort_name[1]), " in year ", data_year$index_year[1]),
                            xlab = "Time (days) since the index record",
                            ylab = "Cumulative event (95% CI)",
                            palette = c("#0072B2", "#D55E00"),
                            legend.title = "",
                            ggtheme = custom_theme())
            
            p1 = p$plot
            p2 = p$table
            p3 = p$ncensor.plot
            p4 <- p$cumevents
            plots = cowplot::plot_grid(p1, p2, p3, p4, align = "v", ncol = 1, rel_heights = c(4, 1, 1, 1))
            
            ggsave(plot = plots,
                   filename = paste0("Cumulative_event_plot_self-harm_dg_", tolower(data_year$cohort_name[1]), "_", data_year$index_year[1], ".png"),
                   path = "path/Results/Cumulative_event_plots/",
                   device = "png",
                   width = 12, 
                   height = 7, 
                   dpi = 300)
            
          })
    })

#Table
#Number of outcomes with % and risk of suicide when compared with individuals with no diagnosis 

tab_suicide_dg <- map_dfr(matched_cohorts_per_years_dg,
                          function(data_f_diag) {
                            
                            map_dfr(data_f_diag,
                                    function(data_year) {
                                      
                                      data_year %>%
                                        group_by(exposure_factor, index_year, cohort, cohort_name) %>%
                                        summarise(n = sum(suicide_binary),
                                                  prop = n/n() * 100) %>%
                                        ungroup() %>%
                                        mutate(n_prop = paste(n, 
                                                              paste0("(",
                                                                     formatC(round(prop, 3), format = "f", digits = 3),
                                                                     ")"))) %>%
                                        select(-n, 
                                               -prop) %>%
                                        pivot_wider(names_from = "exposure_factor",
                                                    names_glue = "model_suicide_dg_{exposure_factor}_n_prop",
                                                    values_from = "n_prop")
                                    })
                          }) %>%
  left_join(models_suicide_dg %>%
              transmute(cohort,
                        cohort_name,
                        year,
                        model_suicide_dg_estimate = paste(formatC(round(estimate, 2), format = "f", digits = 2),
                                                          paste0("(", 
                                                                 formatC(round(conf.low, 2), format = "f", digits = 2),
                                                                 "; ",
                                                                 formatC(round(conf.high, 2), format = "f", digits = 2),
                                                                 ")"))),
            by = c("index_year" = "year", 
                   "cohort",
                   "cohort_name")) 

#Number of outcomes with % and risk of self-harm when compared with individuals with no diagnosis 

tab_selfharm_dg <- map_dfr(matched_cohorts_per_years_dg,
                           function(data_f_diag) {
                             
                             map_dfr(data_f_diag,
                                     function(data_year) {
                                       
                                       data_year %>%
                                         group_by(exposure_factor, index_year, cohort, cohort_name) %>%
                                         summarise(n = sum(selfharm_binary),
                                                   prop = n/n() * 100) %>%
                                         ungroup() %>%
                                         mutate(n_prop = paste(n, 
                                                               paste0("(",
                                                                      formatC(round(prop, 3), format = "f", digits = 3),
                                                                      ")"))) %>%
                                         select(-n, 
                                                -prop) %>%
                                         pivot_wider(names_from = "exposure_factor",
                                                     names_glue = "model_selfharm_dg_{exposure_factor}_n_prop",
                                                     values_from = "n_prop")
                                     })
                           }) %>%
  left_join(models_selfharm_dg %>%
              transmute(cohort,
                        cohort_name,
                        year,
                        model_selfharm_dg_estimate = paste(formatC(round(estimate, 2), format = "f", digits = 2),
                                                           paste0("(", 
                                                                  formatC(round(conf.low, 2), format = "f", digits = 2),
                                                                  "; ",
                                                                  formatC(round(conf.high, 2), format = "f", digits = 2),
                                                                  ")"))),
            by = c("index_year" = "year", 
                   "cohort",
                   "cohort_name")) 

#Combine and save tables

tab_suicide_dg %>%
  left_join(tab_selfharm_dg,
            by = c("index_year", 
                   "cohort", 
                   "cohort_name")) %>%
  write.csv(file = "path/Results/Matched_cohorts_events_and_HR.csv",
            row.names = FALSE)
