#Libraries

library(data.table)
library(tidyverse)
library(patchwork)
library(popEpi)

#Import data on suicides and self-harm in people with F diagnoses

suicides_and_selfharm_f_dg_2015_2021 <- read.csv("path/Data/UZIS/outcomes_v_roce_ind_zaznamu.csv",
                                                 header = TRUE, 
                                                 stringsAsFactors = FALSE)

suicides_and_selfharm_f_dg_2022 <- read.csv("path/Data/UZIS/suic_doplneni_2022.csv",
                                            header = TRUE, 
                                            stringsAsFactors = FALSE)

#Compute total number of people with F diagnoses and number of outcomes in them
#Suicides
#Excluding individuals aged 0 to 9 years

suicides_f_dg <- suicides_and_selfharm_f_dg_2015_2021 %>%
  group_by(cohort,
           year = index_record_rok,
           sex = pohlavi,
           age_group = case_when(vek_skupina == "0-4" | vek_skupina == "5-9" ~ "0-9",
                                 vek_skupina == "80-84" | vek_skupina == "85-89"  | vek_skupina == "90-94"  | vek_skupina == "95-99"  | vek_skupina == "100-104"  | vek_skupina == "105-109" ~ "80+",
                                 TRUE ~ as.character(vek_skupina))) %>%
  summarise(n_suicides_f_dg = sum(pocet[umrti == "umrti sebevražda" & umrti_stejny_rok == 1]),
            n_total_f_dg = sum(pocet)) %>%
  ungroup() %>%
  bind_rows(suicides_and_selfharm_f_dg_2022 %>%
              group_by(cohort,
                       year = index_record_rok,
                       sex = pohlavi,
                       age_group = case_when(vek_skupina == "0-4" | vek_skupina == "5-9" ~ "0-9",
                                             vek_skupina == "80-84" | vek_skupina == "85-89"  | vek_skupina == "90-94"  | vek_skupina == "95-99"  | vek_skupina == "100-104"  | vek_skupina == "105-109" ~ "80+",
                                             TRUE ~ as.character(vek_skupina))) %>%
              summarise(n_suicides_f_dg = sum(pocet[umrti == "umrti sebevražda"]),
                        n_total_f_dg = sum(pocet)) %>%
              ungroup() %>%
              filter(sex != "")) %>%
  filter(age_group != "0-9")

#Self-harm
#Excluding individuals aged 0 to 9 years

selfharm_f_dg <- suicides_and_selfharm_f_dg_2015_2021 %>%
  group_by(cohort,
           year = index_record_rok,
           sex = pohlavi,
           age_group = case_when(vek_skupina == "0-4" | vek_skupina == "5-9" ~ "0-9",
                                 vek_skupina == "80-84" | vek_skupina == "85-89"  | vek_skupina == "90-94"  | vek_skupina == "95-99"  | vek_skupina == "100-104"  | vek_skupina == "105-109" ~ "80+",
                                 TRUE ~ as.character(vek_skupina))) %>%
  summarise(n_selfharm_f_dg = sum(pocet[hospit_pro_sebev == "hospitalizace pro sebevraždu" & hospit_pro_sebev_stejny_rok == 1]),
            n_total_f_dg = sum(pocet)) %>%
  ungroup() %>%
  bind_rows(suicides_and_selfharm_f_dg_2022 %>%
              group_by(cohort,
                       year = index_record_rok,
                       sex = pohlavi,
                       age_group = case_when(vek_skupina == "0-4" | vek_skupina == "5-9" ~ "0-9",
                                             vek_skupina == "80-84" | vek_skupina == "85-89"  | vek_skupina == "90-94"  | vek_skupina == "95-99"  | vek_skupina == "100-104"  | vek_skupina == "105-109" ~ "80+",
                                             TRUE ~ as.character(vek_skupina))) %>%
              summarise(n_selfharm_f_dg = sum(pocet[hospit_pro_sebev == "hospitalizace pro sebevraždu"]),
                        n_total_f_dg = sum(pocet)) %>%
              ungroup() %>%
              
              filter(sex != "")) %>%
  filter(age_group != "0-9")

#Import data on suicides and self-harm in the general population

suicides_gen_pop <- read.csv("path/Data/UZIS/zemreli_X60_X84.csv",
                             header = TRUE, 
                             stringsAsFactors = FALSE,
                             encoding = "Latin-1")

selfharm_gen_pop <- read.csv("path/Data/UZIS/hospitalizace_po_pokusu_o_sebevrazdu.csv",
                             header = TRUE, 
                             stringsAsFactors = FALSE,
                             encoding = "Latin-1")

#Compute total number of outcomes in the general population
#Suicides
#Excluding individuals aged 0 to 9 years

suicides_gen_pop_count <- suicides_gen_pop %>%
  group_by(year = rok_umrti,
           sex = pohlavi,
           age_group = case_when(vek_skupina == "0-4" | vek_skupina == "5-9" ~ "0-9",
                                 vek_skupina == "80-84" | vek_skupina == "85-89"  | vek_skupina == "90-94"  | vek_skupina == "95-99"  | vek_skupina == "100-104"  | vek_skupina == "105-109" ~ "80+",
                                 TRUE ~ as.character(vek_skupina))) %>%
  summarise(n_suicides_gen_pop = n()) %>%
  ungroup() %>%
  filter(age_group != "0-9")

#Self-harm
#Randomly choose one record for individuals with multiple records in one year
#Excluding individuals aged 0 to 9 years

set.seed(123)

selfharm_gen_pop_count <- selfharm_gen_pop %>%
  group_by(year = rok_propusteni,
           ID = ID_pacienta) %>%
  filter(row_number() %in% sample(1:n(), 1)) %>%
  ungroup() %>%
  group_by(year,
           sex = pohlavi,
           age_group = case_when(vek_skupina == "0-4" | vek_skupina == "5-9" ~ "0-9",
                                 vek_skupina == "80-84" | vek_skupina == "85-89"  | vek_skupina == "90-94"  | vek_skupina == "95-99"  | vek_skupina == "100-104"  | vek_skupina == "105-109" ~ "80+",
                                 TRUE ~ as.character(vek_skupina))) %>%
  summarise(n_selfharm_gen_pop = n()) %>%
  ungroup() %>%
  filter(age_group != "0-9")


#Import data on overall general population size

gen_pop_raw <- fread(file = "path/Data/CSU/GenPop_NbOfInhabitans_2010_2022.csv",
                     header = TRUE, 
                     stringsAsFactors = FALSE,
                     encoding = "Latin-1")

#Unify format

gen_pop <- gen_pop_raw %>%
  transmute(year = rok,
            sex = pohlavi,
            age_group = sub("–", "-", vek_skupina),
            n_gen_pop = as.numeric(sub(",","",obyv_k_1_7)))

#Models
#Vector of mental disorders

f_codes <- c("F0F1F2F3F4G30", "F1", "F2", "F3", "F4", "F0G30")

#Standardized suicide ratio

stand_suicide_ratio <- map(.x = f_codes,
                           ~ suicides_f_dg %>%
                             filter(cohort == .x) %>%
                             inner_join(suicides_gen_pop_count) %>%
                             inner_join(gen_pop)) %>%
  set_names(f_codes) %>%
  imap_dfr(~ sir(coh.data = .x, coh.obs = "n_suicides_f_dg", coh.pyrs = "n_total_f_dg",
                 ref.data = .x, ref.obs = "n_suicides_gen_pop", ref.pyrs = "n_gen_pop", 
                 adjust = c("year", "sex", "age_group"), 
                 print = "year") %>%
             mutate(cohort = .y))

#Standardized self-harm ratio

stand_selfharm_ratio <- map(.x = f_codes,
                            ~ selfharm_f_dg %>%
                              filter(cohort == .x) %>%
                              inner_join(selfharm_gen_pop_count) %>%
                              inner_join(gen_pop)) %>%
  set_names(f_codes) %>%
  imap_dfr(~ sir(coh.data = .x, coh.obs = "n_selfharm_f_dg", coh.pyrs = "n_total_f_dg",
                 ref.data = .x, ref.obs = "n_selfharm_gen_pop", ref.pyrs = "n_gen_pop", 
                 adjust = c("year", "sex", "age_group"), 
                 print = "year") %>%
             mutate(cohort = .y))

#Table with incidence rates and standardized incidence rate ratios

stand_suicide_ratio %>%
  left_join(suicides_f_dg %>%
              group_by(cohort,
                       year) %>%
              summarise(incidence_f_dg = sum(n_suicides_f_dg)/sum(n_total_f_dg) * 100000) %>%
              ungroup()) %>%
  transmute(cohort,
            year,
            outcome = "suicide",
            incidence_f_dg,
            sir,
            sir.lo,
            sir.hi) %>%
  bind_rows(stand_selfharm_ratio %>%
              left_join(selfharm_f_dg %>%
                          group_by(cohort,
                                   year) %>%
                          summarise(incidence_f_dg = sum(n_selfharm_f_dg)/sum(n_total_f_dg) * 100000) %>%
                          ungroup()) %>%
              transmute(cohort,
                        year,
                        outcome = "self-harm",
                        incidence_f_dg,
                        sir,
                        sir.lo,
                        sir.hi)) %>%
  mutate(incidence_f_dg = formatC(round(incidence_f_dg, 2), format = "f", digits = 2),
         sir_estimate_ci = paste(formatC(round(sir, 2), format = "f", digits = 2),
                                 paste0("(", 
                                        formatC(round(sir.lo, 2), format = "f", digits = 2),
                                        "; ",
                                        formatC(round(sir.hi, 2), format = "f", digits = 2),
                                        ")"))) %>%
  mutate(cohort_name = case_when(cohort == "F0F1F2F3F4G30" ~ "Any mental disorder",
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
  select(-sir,
         -sir.lo,
         -sir.hi) %>%
  pivot_wider(names_from = "outcome",
              values_from = c("incidence_f_dg", "sir_estimate_ci")) %>%
  select(cohort,
         cohort_name,
         year,
         ends_with("suicide"),
         ends_with("self-harm")) %>%
  arrange(cohort_name, year)%>%
  write.csv(file = "path/Results/Standardized_ratios.csv",
            row.names = FALSE)

#Graphs
#Standardized suicide ratio

p_suicide <- stand_suicide_ratio %>%
  group_by(cohort) %>%
  mutate(color = rep(c("white", "gray95"), length.out = n())) %>%
  ungroup() %>%
  mutate(cohort_name = case_when(cohort == "F0F1F2F3F4G30" ~ "Any mental disorder",
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
                                         "Anxiety disorders")),
         year = fct_rev(as.character(year))) %>%
  ggplot(aes(x = sir, y = year, xmin = sir.lo, xmax = sir.hi)) +
  geom_hline(aes(yintercept = year, color = color), size = 7) + 
  geom_pointrange(shape = 22, fill = "black") +
  geom_vline(xintercept = 1, linetype = 3) +
  theme_classic() +
  scale_colour_identity() +
  scale_x_log10() +
  facet_wrap(. ~ cohort_name,
             scales = "free_x",
             ncol = 2) +
  labs(title = "Suicide incidence in \npeople with pre-existing mental disorders \nrelative to the general population",
       y = "Year",
       x = "Standardized suicide incidence rate ratio (95% CI)") +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 13),
        panel.border = element_rect(colour = "black", fill = NA))

#Risk of selh-harm when compared with individuals with no diagnosis

p_selfharm <- stand_selfharm_ratio %>%
  group_by(cohort) %>%
  mutate(color = rep(c("white", "gray95"), length.out = n())) %>%
  ungroup() %>%
  mutate(cohort_name = case_when(cohort == "F0F1F2F3F4G30" ~ "Any mental disorder",
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
                                         "Anxiety disorders")),
         year = fct_rev(as.character(year))) %>%
  ggplot(aes(x = sir, y = year, xmin = sir.lo, xmax = sir.hi)) +
  geom_hline(aes(yintercept = year, color = color), size = 7) + 
  geom_pointrange(shape = 22, fill = "black") +
  geom_vline(xintercept = 1, linetype = 3) +
  scale_colour_identity() +
  scale_x_log10() +
  facet_wrap(. ~ cohort_name,
             scales = "free_x",
             ncol = 2) +
  labs(title = "Self-harm incidence in \npeople with pre-existing mental disorders \nrelative to the general population",
       y = "Year",
       x = "Standardized self-harm incidence rate ratio (95% CI)") +
  theme_classic() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.title.x = element_text(margin = margin(10, 0, 0, 0)),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_text(size = 13),
        panel.border = element_rect(colour = "black", fill = NA))

#Combine subplots

p_suicide + p_selfharm

#Save graph

ggsave(file = "path/Results/Standardized_ratios.eps",
       device = cairo_ps,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 300)
