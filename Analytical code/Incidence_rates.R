#Libraries

library(data.table)
library(tidyverse)
library(patchwork)

#Import CSU data

data_csu_raw <- fread(file = "path/Data/CSU/GenPop_NbOfInhabitans_2010_2022.csv",
                      encoding = "Latin-1")


#Process data

data_csu <- data_csu_raw %>%
  set_names(., c("age_group", "sex", "year", "pop_size")) %>%
  mutate(pop_size = as.numeric(sub(",", "", pop_size)),
         age_group = sub("–", "-", age_group),
         age_group = case_when(age_group %in% c("10-14", "15-19") ~ "10-19",
                               age_group %in% c("20-24", "25-29") ~ "20-29",
                               age_group %in% c("30-34", "35-39") ~ "30-39",
                               age_group %in% c("40-44", "45-49") ~ "40-49",
                               age_group %in% c("50-54", "55-59") ~ "50-59",
                               age_group %in% c("60-64", "65-69") ~ "60-69",
                               age_group %in% c("70-74", "75-79") ~ "70-79",
                               age_group == "80+" ~ "80+",
                               TRUE ~ "0-9")) %>%
  group_by(age_group, sex, year) %>%
  summarise(pop_size = sum(pop_size)) %>%
  ungroup()

#Import UZIS data
#Suicides

data_suicides_raw <- fread(file = "path/Data/UZIS/zemreli_X60_X84.csv")

#Self-harm

data_selfharm_raw <- fread(file = "path/Data/UZIS/hospitalizace_po_pokusu_o_sebevrazdu.csv")

#Process data
#Suicides

data_suicides <- data_suicides_raw %>%
  transmute(year = rok_umrti,
            sex = pohlavi,
            age_group = vek_skupina,
            f_history = F0F1F2F3F4G30_5LetPred,
            f0_history = F0G30_5LetPred,
            f1_history = F1_5LetPred,
            f2_history = F2_5LetPred,
            f3_history = F3_5LetPred,
            f4_history = F4_5LetPred) %>%
  mutate(age_group = case_when(age_group %in% c("10-14", "15-19") ~ "10-19",
                               age_group %in% c("20-24", "25-29") ~ "20-29",
                               age_group %in% c("30-34", "35-39") ~ "30-39",
                               age_group %in% c("40-44", "45-49") ~ "40-49",
                               age_group %in% c("50-54", "55-59") ~ "50-59",
                               age_group %in% c("60-64", "65-69") ~ "60-69",
                               age_group %in% c("70-74", "75-79") ~ "70-79",
                               age_group %in% c("80-84", "85-89", "90-94", "95-99", "100-104") ~ "80+")) 

#Self-harm
#Randomly choose one record for individuals with multiple records in one year

set.seed(123)

data_selfharm <- data_selfharm_raw %>%
  group_by(year = rok_propusteni,
           ID = ID_pacienta) %>%
  filter(row_number() %in% sample(1:n(), 1)) %>%
  ungroup() %>%
  transmute(year,
            sex = pohlavi,
            age_group = vek_skupina,
            f_history = F0F1F2F3F4G30_5LetPred,
            f0_history = F0G30_5LetPred,
            f1_history = F1_5LetPred,
            f2_history = F2_5LetPred,
            f3_history = F3_5LetPred,
            f4_history = F4_5LetPred) %>%
  filter(!age_group %in% c("0-4", "5-9")) %>%
  mutate(age_group = case_when(age_group %in% c("10-14", "15-19") ~ "10-19",
                               age_group %in% c("20-24", "25-29") ~ "20-29",
                               age_group %in% c("30-34", "35-39") ~ "30-39",
                               age_group %in% c("40-44", "45-49") ~ "40-49",
                               age_group %in% c("50-54", "55-59") ~ "50-59",
                               age_group %in% c("60-64", "65-69") ~ "60-69",
                               age_group %in% c("70-74", "75-79") ~ "70-79",
                               age_group %in% c("80-84", "85-89", "90-94", "95-99", "100-104") ~ "80+"))
  
#Table
#Rates per 100000 individuals, age and sex specific 

data_suicides %>%
  count(age_group, sex, year) %>%
  left_join(data_csu,
            by = c("age_group", "sex", "year")) %>%
  mutate(outcome = "suicides",
         incidence_rate = n/pop_size * 100000) %>%
  bind_rows(data_selfharm %>%
              count(age_group, sex, year) %>%
              left_join(data_csu,
                        by = c("age_group", "sex", "year")) %>%
              mutate(outcome = "self-harm",
                     incidence_rate = n/pop_size * 100000)) %>%
  mutate(incidence_rate = formatC(round(incidence_rate, 2), format = "f", digits = 2),
         sex = ifelse(sex == "M", "males", "females")) %>%
  select(-n,
         -pop_size) %>%
  pivot_wider(names_from = c("sex", "outcome"),
              values_from = "incidence_rate") %>%
  write.csv(file = "path/Results/IR_age_and_sex_specific.csv",
            row.names = FALSE)

#Graphs 
#Suicides
#Rates per 100000 individuals, age and sex specific 

p_suicide_age_and_sex_specific <- data_suicides %>%
  count(age_group, sex, year) %>%
  left_join(data_csu,
            by = c("age_group", "sex", "year")) %>%
  mutate(incidence_rate = n/pop_size * 100000,
         sex = ifelse(sex == "M", "males", "females")) %>%
  ggplot(aes(x = year, y = incidence_rate, color = sex)) +
  geom_line(linewidth = 1,
            alpha = 0.7) +
  facet_wrap(. ~ age_group,
             scales = "free_y",
             ncol = 2) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(breaks = 2010:2022) +
  scale_color_manual(values = c("#D55E00", "#0072B2"),
                     name = "Sex") +
  ggtitle("Suicide incidence rate per 100,000 person-years \nage- and sex-specific") +
  xlab("Year") +
  theme_classic() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        legend.direction = "horizontal", 
        legend.box = "horizontal",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) +
  guides(color = guide_legend(order = 1,
                              title.position = "top", 
                              title.hjust = 0.5,
                              label.position = "bottom",
                              keywidth = 5))

#Self-harm
#Rates per 100000 individuals, age and sex specific 

p_selfharm_age_and_sex_specific <- data_selfharm %>%
  count(age_group, sex, year) %>%
  left_join(data_csu,
            by = c("age_group", "sex", "year")) %>%
  mutate(incidence_rate = n/pop_size * 100000,
         sex = ifelse(sex == "M", "males", "females")) %>%
  ggplot(aes(x = year, y = incidence_rate, color = sex)) +
  geom_line(linewidth = 1,
            alpha = 0.7) +
  facet_wrap(. ~ age_group,
             scales = "free_y",
             ncol = 2) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(breaks = 2010:2022) +
  scale_color_manual(values = c("#D55E00", "#0072B2"),
                     name = "Sex") +
  ggtitle("Self-harm hospitalizations incidence rate per 100,000 person-years \nage- and sex-specific") +
  xlab("Year") +
  theme_classic() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        legend.direction = "horizontal", 
        legend.box = "horizontal",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) +
  guides(color = guide_legend(order = 1,
                              title.position = "top", 
                              title.hjust = 0.5,
                              label.position = "bottom",
                              keywidth = 5))

#Combine subplots

p_suicide_age_and_sex_specific + 
  p_selfharm_age_and_sex_specific + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

#Save plot

ggsave(file = "path/Results/IR_age_and_sex_specific.eps",
       device = cairo_ps,
       width = 50,
       height = 30,
       units = "cm",
       dpi = 300)

#Table
#Rates per 100000 individuals, age specific 

data_suicides %>%
  count(age_group, year) %>%
  left_join(data_csu %>%
              group_by(year,
                       age_group) %>%
              summarise(pop_size = sum(pop_size)) %>%
              ungroup(),
            by = c("age_group", "year")) %>%
  mutate(outcome = "suicides",
         incidence_rate = n/pop_size * 100000) %>%
  bind_rows(data_selfharm %>%
              count(age_group, year) %>%
              left_join(data_csu %>%
                          group_by(year,
                                   age_group) %>%
                          summarise(pop_size = sum(pop_size)) %>%
                          ungroup(),
                        by = c("age_group", "year")) %>%
              mutate(outcome = "self-harm",
                     incidence_rate = n/pop_size * 100000)) %>%
  mutate(incidence_rate = formatC(round(incidence_rate, 2), format = "f", digits = 2)) %>%
  select(-n,
         -pop_size) %>%
  pivot_wider(names_from = "outcome",
              values_from = "incidence_rate") %>%
  write.csv(file = "path/Results/IR_age_specific.csv",
            row.names = FALSE)

#Graphs 
#Suicides
#Rates per 100000 individuals, age specific 

p_suicide_age_specific <- data_suicides %>%
  count(age_group, year) %>%
  left_join(data_csu %>%
              group_by(year,
                       age_group) %>%
              summarise(pop_size = sum(pop_size)) %>%
              ungroup(),
            by = c("age_group", "year")) %>%
  mutate(incidence_rate = n/pop_size * 100000) %>%
  ggplot(aes(x = year, y = incidence_rate)) +
  geom_line(linewidth = 1,
            alpha = 0.7) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(breaks = 2010:2022) +
  facet_wrap(. ~ age_group,
             scales = "free_y",
             ncol = 2) +
  ggtitle("Suicide incidence rate per 100,000 person-years \nage-specific") +
  xlab("Year") +
  theme_classic() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) 

#Self-harm
#Rates per 100000 individuals, age and sex specific 

p_selfharm_age_specific <- data_selfharm %>%
  count(age_group, year) %>%
  left_join(data_csu %>%
              group_by(year,
                       age_group) %>%
              summarise(pop_size = sum(pop_size)) %>%
              ungroup(),
            by = c("age_group", "year")) %>%
  mutate(incidence_rate = n/pop_size * 100000) %>%
  ggplot(aes(x = year, y = incidence_rate)) +
  geom_line(linewidth = 1,
            alpha = 0.7) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(breaks = 2010:2022) +
  facet_wrap(. ~ age_group,
             scales = "free_y",
             ncol = 2) +
  ggtitle("Self-harm hospitalizations incidence rate per 100,000 person-years \nage-specific") +
  xlab("Year") +
  theme_classic() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) 

#Combine subplots

p_suicide_age_specific + 
  p_selfharm_age_specific + 
  plot_layout(guides = "collect") 

#Save plot

ggsave(file = "path/Results/IR_age_specific.eps",
       device = cairo_ps,
       width = 50,
       height = 30,
       units = "cm",
       dpi = 300)

#Table
#Rates per 100000 individuals, sex specific 

data_suicides %>%
  count(sex, year) %>%
  left_join(data_csu %>%
              group_by(year,
                       sex) %>%
              summarise(pop_size = sum(pop_size)) %>%
              ungroup(),
            by = c("sex", "year")) %>%
  mutate(outcome = "suicides",
         incidence_rate = n/pop_size * 100000) %>%
  bind_rows(data_selfharm %>%
              count(sex, year) %>%
              left_join(data_csu %>%
                          group_by(year,
                                   sex) %>%
                          summarise(pop_size = sum(pop_size)) %>%
                          ungroup(),
                        by = c("sex", "year")) %>%
              mutate(outcome = "self-harm",
                     incidence_rate = n/pop_size * 100000)) %>%
  mutate(incidence_rate = formatC(round(incidence_rate, 2), format = "f", digits = 2),
         sex = ifelse(sex == "M", "males", "females")) %>%
  select(-n,
         -pop_size) %>%
  pivot_wider(names_from = c("sex", "outcome"),
              values_from = "incidence_rate") %>%
  write.csv(file = "path/Results/IR_sex_specific.csv",
            row.names = FALSE)

#Graphs 
#Suicides
#Rates per 100000 individuals, sex specific 

p_suicide_sex_specific <- data_suicides %>%
  count(sex, year) %>%
  left_join(data_csu %>%
              group_by(year,
                       sex) %>%
              summarise(pop_size = sum(pop_size)) %>%
              ungroup(),
            by = c("sex", "year")) %>%
  mutate(incidence_rate = n/pop_size * 100000,
         sex = ifelse(sex == "M", "males", "females")) %>%
  ggplot(aes(x = year, y = incidence_rate, color = sex)) +
  geom_line(linewidth = 1,
            alpha = 0.7) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(breaks = 2010:2022) +
  scale_color_manual(values = c("#D55E00", "#0072B2"),
                     name = "Sex") +
  ggtitle("Suicide incidence rate per 100,000 person-years \nsex-specific") +
  xlab("Year") +
  theme_classic() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        legend.direction = "horizontal", 
        legend.box = "horizontal",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) +
  guides(color = guide_legend(order = 1,
                              title.position = "top", 
                              title.hjust = 0.5,
                              label.position = "bottom",
                              keywidth = 5))

#Self-harm
#Rates per 100000 individuals, age and sex specific 

p_selfharm_sex_specific <- data_selfharm %>%
  count(sex, year) %>%
  left_join(data_csu %>%
              group_by(year,
                       sex) %>%
              summarise(pop_size = sum(pop_size)) %>%
              ungroup(),
            by = c("sex", "year")) %>%
  mutate(incidence_rate = n/pop_size * 100000,
         sex = ifelse(sex == "M", "males", "females")) %>%
  ggplot(aes(x = year, y = incidence_rate, color = sex)) +
  geom_line(linewidth = 1,
            alpha = 0.7) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_continuous(breaks = 2010:2022) +
  scale_color_manual(values = c("#D55E00", "#0072B2"),
                     name = "Sex") +
  ggtitle("Self-harm hospitalizations incidence rate per 100,000 persons \nsex-specific") +
  xlab("Year") +
  theme_classic() +
  theme(plot.title = element_text(size = 15, hjust = 0.5),
        legend.direction = "horizontal", 
        legend.box = "horizontal",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 13),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        panel.border = element_rect(colour = "black", fill = NA)) +
  guides(color = guide_legend(order = 1,
                              title.position = "top", 
                              title.hjust = 0.5,
                              label.position = "bottom",
                              keywidth = 5))
#Combine subplots

p_suicide_sex_specific + 
  p_selfharm_sex_specific + 
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

#Save plot

ggsave(file = "path/Results/IR_sex_specific.eps",
       device = cairo_ps,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 300)
