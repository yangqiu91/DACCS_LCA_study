install.packages('ggplot2')
install.packages('readxl')
install.packages('dplyr')
install.packages('tidyr')
install.packages('scales')
install.packages('formattable')
install.packages('cowplot')
install.packages('tidyverse')
install.packages("ggplotify")

library(ggplot2)
library(readxl)
library(tidyr)
library(dplyr)
library(scales)
library(formattable)
library(gridExtra)
library(cowplot)
library(magrittr)
library(forcats)
library(XLConnect)
library(readr)
library(lubridate)
library(stringr)
library(ggpubr)
library(patternplot)
library(png)
library(ggplotify)
library(ggbreak)


## Data Import
###############################################

## DACCS LCA results 
# SSP2_Baseline scenario
DAC_data_baseline <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/LCA_result/LCA_sol_sor_country_summary_table_spread_BAU.xlsx")

# SSP2_RCP1.9_with_DACCS scenario (no learning)
DAC_data_RCP19_DAC <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/LCA_result/LCA_sol_sor_country_summary_table_spread_RCP19_DAC.xlsx")

# SSP2_RCP1.9_with_DACCS scenario (with learning based on reference learning rate)
DAC_data_RCP19_DAC_LR <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/LCA_result/LCA_sol_sor_country_summary_table_spread_RCP19_DAC_LR.xlsx")

# SSP2_RCP1.9_with_DACCS scenario (with learning based on slow learning rate)
DAC_data_RCP19_DAC_LR_low <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/LCA_result/LCA_sol_sor_country_summary_table_spread_RCP19_DAC_LR_low.xlsx")

# SSP2_RCP1.9_with_DACCS scenario (with learning based on fast learning rate)
DAC_data_RCP19_DAC_LR_high <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/LCA_result/LCA_sol_sor_country_summary_table_spread_RCP19_DAC_LR_high.xlsx")

DAC_data_baseline$scenario = 'Baseline'
DAC_data_RCP19_DAC$scenario = 'RCP19_DAC'
DAC_data_RCP19_DAC_LR$scenario = 'RCP19_DAC_LR'
DAC_data_RCP19_DAC_LR_low$scenario = 'RCP19_DAC_LR_low'
DAC_data_RCP19_DAC_LR_high$scenario = 'RCP19_DAC_LR_high'




## DACCS LCA contribution analysis result
# SSP2_RCP1.9_with_DACCS scenario (no learning)
LCA_con_sol_RCP19_data <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/LCA_result/LCA_sol_RCP19_contribution_all_summary_table.xlsx")
LCA_con_sor_RCP19_data <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/LCA_result/LCA_sor_RCP19_contribution_all_summary_table.xlsx")

# SSP2_RCP1.9_with_DACCS scenario (with learning based on reference learning rate)
LCA_con_sol_RCP19_LR_data <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/LCA_result/LCA_sol_RCP19_DAC_contribution_all_summary_table.xlsx")
LCA_con_sor_RCP19_LR_data <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/LCA_result/LCA_sor_RCP19_DAC_contribution_all_summary_table.xlsx")

LCA_con_sol_RCP19_data$scenario = 'SSP2_RCP19_DAC'
LCA_con_sol_RCP19_LR_data$scenario = 'SSP2_RCP19_DAC_L'
LCA_con_sor_RCP19_data$scenario = 'SSP2_RCP19_DAC'
LCA_con_sor_RCP19_LR_data$scenario = 'SSP2_RCP19_DAC_L'


## Electricity generation LCA (Figure 3)
Kwh_elec_impact_US <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_environmental_impact/US_1kwh_elec_impact_year_summary_table.xlsx")


## Electricity generation LCA (contribution by technologies, SI-Figure 9)
# SSP2-Baseline scenario
Elec_impact_US_baseline <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_environmental_impact/US_hv_Baseline.xlsx")

# SSP2-RCP1.9 w/o DACCS scenario
Elec_impact_US_RCP19_noDAC <- read_excel("C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/Electricity_environmental_impact/US_hv_RCP19_noDAC.xlsx")

# SSP2-RCP1.9 w/ DACCS scenario
Elec_impact_US_RCP19 <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_environmental_impact/US_hv_RCP19.xlsx")

###############################################




## Basic setups
###############################################

impact_keep <- c("Climate change (kg CO2-Eq)", 
                 "Human toxicity (kg 14-DCB)",  
                 "Freshwater eutrophication (kg P-Eq)", 
                 "Freshwater ecotoxicity (kg 14-DCB)",
                 "Terrestrial acidification (kg SO2-Eq)", 
                 "Terrestrial ecotoxicity (kg 14-DCB)", 
                 "Metal depletion (kg Fe-Eq)", 
                 "Water depletion (m3 water)")



category_list = c('Climate change (kg CO2-Eq)',
                  'Ozone depletion (kg CFC-11)', 
                  'Ionising radiation (kg U235-Eq)', 
                  'Photochemical oxidant formation (kg NMVOC)', 
                  'Particulate matter formation (kg PM10-Eq)',
                  'Terrestrial acidification (kg SO2-Eq)', 
                  'Freshwater ecotoxicity (kg 14-DCB)',
                  'Marine ecotoxicity (kg 14-DCB)', 
                  'Terrestrial ecotoxicity (kg 14-DCB)', 
                  'Human toxicity (kg 14-DCB)',
                  'Freshwater eutrophication (kg P-Eq)',
                  'Marine eutrophication (kg N-Eq)', 
                  'Fossil depletion (kg oil-Eq)',
                  'Metal depletion (kg Fe-Eq)',   
                  'Water depletion (m3 water)',
                  'Agricultural land occupation (square meter-year)',
                  'Natural land transformation (square meter)', 
                  'Urban land occupation (square meter-year)')


gentech_sort = c('electricity production, hard coal',
                 'electricity production, lignite',
                 'Electricity, at power plant/hard coal, IGCC, no CCS/2025',
                 'Electricity, at power plant/lignite, IGCC, no CCS/2025',
                 'heat and power co-generation, hard coal',
                 "heat and power co-generation, lignite",
                 'electricity production, hard coal with CCS',
                 'electricity production, lignite with CCS',
                 'electricity production, natural gas, combined cycle power plant',
                 'electricity production, natural gas, conventional power plant',
                 'heat and power co-generation, natural gas, conventional power plant, 100MW electrical',
                 'heat and power co-generation, natural gas, combined cycle power plant, 400MW electrical',
                 'electricity production, natural gas with CCS',
                 'electricity production, oil',
                 'heat and power co-generation, oil',
                 'electricity production, nuclear, boiling water reactor',
                 'electricity production, nuclear, pressure water reactor',
                 'electricity production, nuclear, pressure water reactor, heavy water moderated',
                 'electricity production, hydro, run-of-river',
                 "electricity production, hydro, reservoir, alpine region", 
                 'electricity production, hydro, reservoir, non-alpine region',
                 'electricity production, wind, 1-3MW turbine, onshore',
                 'electricity production, wind, <1MW turbine, onshore',
                 'electricity production, wind, >3MW turbine, onshore',
                 'electricity production, wind, 1-3MW turbine, offshore',
                 'electricity production, solar thermal parabolic trough, 50 MW',
                 'electricity production, solar tower power plant, 20 MW',
                 'electricity production, photovoltaic, 3kWp slanted-roof installation, multi-Si, panel, mounted',
                 'electricity production, photovoltaic, 570kWp open ground installation, multi-Si',
                 'electricity production, wood, future',
                 "Electricity, at BIGCC power plant 450MW, no CCS/2025",
                 'Electricity, at BIGCC power plant 450MW, pre, pipeline 200km, storage 1000m/2025, US',
                 'heat and power co-generation, biogas, gas engine',
                 'heat and power co-generation, wood chips, 6667 kW',
                 'heat and power co-generation, wood chips, 6667 kW, state-of-the-art 2014',
                 'electricity production, biomass with CCS',
                 'electricity production, wave',
                 'electricity production, deep geothermal',
                 "market for electricity, high voltage",                                                          
                 "market for transmission network, electricity, high voltage",                                    
                 "market for transmission network, long-distance")



eco_color_scheme = c('electricity production, hard coal' = 'black',
                     'electricity production, lignite' = 'black',
                     'Electricity, at power plant/hard coal, IGCC, no CCS/2025' = 'black',
                     'Electricity, at power plant/lignite, IGCC, no CCS/2025' = 'black',
                     'heat and power co-generation, hard coal' = 'black',
                     "heat and power co-generation, lignite" = 'black',
                     
                     'electricity production, hard coal with CCS' = 'gray30',
                     'electricity production, lignite with CCS' = 'gray30',
                     
                     'electricity production, natural gas, combined cycle power plant' = 'grey50',
                     'electricity production, natural gas, conventional power plant' = 'grey50',
                     'heat and power co-generation, natural gas, conventional power plant, 100MW electrical' = 'grey50',
                     'heat and power co-generation, natural gas, combined cycle power plant, 400MW electrical' = 'grey50',
                     
                     'electricity production, natural gas with CCS' = "gray80",
                     
                     'electricity production, oil' = "brown3",
                     'heat and power co-generation, oil' = "brown3",
                     
                     'electricity production, nuclear, boiling water reactor' = 'cyan',
                     'electricity production, nuclear, pressure water reactor' = 'cyan',
                     'electricity production, nuclear, pressure water reactor, heavy water moderated' = 'cyan',
                     
                     'electricity production, hydro, run-of-river' = rgb(170/256, 255/256, 195/256), 
                     "electricity production, hydro, reservoir, alpine region" = rgb(170/256, 255/256, 195/256), 
                     'electricity production, hydro, reservoir, non-alpine region' = rgb(170/256, 255/256, 195/256), 
                     
                     'electricity production, wind, 1-3MW turbine, onshore' = 'blue', 
                     'electricity production, wind, <1MW turbine, onshore' = 'blue', 
                     'electricity production, wind, >3MW turbine, onshore' = 'blue', 
                     'electricity production, wind, 1-3MW turbine, offshore' = 'blue', 
                     
                     'electricity production, solar thermal parabolic trough, 50 MW' = 'yellow',
                     'electricity production, solar tower power plant, 20 MW' = 'yellow',
                     'electricity production, photovoltaic, 3kWp slanted-roof installation, multi-Si, panel, mounted' = 'yellow',
                     'electricity production, photovoltaic, 570kWp open ground installation, multi-Si' = 'yellow',
                     
                     'electricity production, wood, future' = 'green3',
                     "Electricity, at BIGCC power plant 450MW, no CCS/2025" = 'green3',
                     'heat and power co-generation, biogas, gas engine' = 'green3',
                     'heat and power co-generation, wood chips, 6667 kW' = 'green3',
                     'heat and power co-generation, wood chips, 6667 kW, state-of-the-art 2014' = 'green3',
                     
                     'Electricity, at BIGCC power plant 450MW, pre, pipeline 200km, storage 1000m/2025, US' = 'green',
                     'electricity production, biomass with CCS' = 'green',
                     
                     'electricity production, wave' = 'red',
                     'electricity production, deep geothermal' = 'red',
                     
                     "market for electricity, high voltage" = rgb(25/256, 58/256, 113/256),                                                          
                     "market for transmission network, electricity, high voltage" = rgb(25/256, 58/256, 113/256),                                    
                     "market for transmission network, long-distance" = rgb(25/256, 58/256, 113/256))


###############################################




## Figure 1 -- LCA result of DACCS
###############################################


## Data wrangling (Left line plot for each panel)
Main_DAC_data_together <- DAC_data_baseline %>% 
  rbind(DAC_data_RCP19_DAC) %>%
  rbind(DAC_data_RCP19_DAC_LR) %>%
  rbind(DAC_data_RCP19_DAC_LR_low) %>%
  rbind(DAC_data_RCP19_DAC_LR_high) %>%
  gather(key = 'Year', value = 'value', 3:11) %>%
  set_colnames(c("impact_category", "heat_elec_region", "scenario", "year", "value")) %>%
  mutate(tech_heat = substr(heat_elec_region, 1, 10),
         region = substr(heat_elec_region, 12, 13)) %>%
  select(impact_category, tech_heat, region, scenario, year, value) %>%
  spread(key = 'scenario', value = 'value') %>%
  filter(impact_category %in% impact_keep)


Main_DAC_data_together$year = as.numeric(Main_DAC_data_together$year)



## Data wrangling (right bar plot for each panel)
Side_bar_data_raw <- 
  Main_DAC_data_together %>% filter(year %in% c(2020, 2100)) %>%
  gather(key = 'scenario', value = 'value', 5:9) %>%
  spread(key = 'year', value = 'value') %>%
  filter(scenario %in% c('RCP19_DAC', 'RCP19_DAC_LR', 'RCP19_DAC_LR_low', 'RCP19_DAC_LR_high')) %>%
  set_colnames(c("impact_category", 'tech_heat',  'region', 'scenario', 'y2020', 'y2100')) %>%
  mutate(rela_change_2100_2020 = (y2100 - y2020)/y2020) %>%
  select(impact_category, tech_heat,  region, scenario, rela_change_2100_2020) %>%
  spread(key = 'scenario', value = 'rela_change_2100_2020') %>%
  mutate(RCP19_DAC_m = NA, RCP19_DAC_LR_m = NA, RCP19_DAC_LR_lm = NA, RCP19_DAC_LR_hm = NA)


for (i in 1:nrow(Side_bar_data_raw)) {
  if (Side_bar_data_raw[i, 1] == 'Climate change (kg CO2-Eq)') {
    Side_bar_data_raw[i, 8] = Side_bar_data_raw[i, 4] * (-1) 
    Side_bar_data_raw[i, 9] = Side_bar_data_raw[i, 5] * (-1)
    Side_bar_data_raw[i, 10] = Side_bar_data_raw[i, 6] * (-1) 
    Side_bar_data_raw[i, 11] = Side_bar_data_raw[i, 7] * (-1) 
  } else {
    Side_bar_data_raw[i, 8] = Side_bar_data_raw[i, 4] * 1 
    Side_bar_data_raw[i, 9] = Side_bar_data_raw[i, 5] * 1 
    Side_bar_data_raw[i, 10] = Side_bar_data_raw[i, 6] * 1 
    Side_bar_data_raw[i, 11] = Side_bar_data_raw[i, 7] * 1 
  }
}

Side_bar_data <- 
  Side_bar_data_raw %>% 
  select(impact_category, tech_heat,  region, RCP19_DAC_m, RCP19_DAC_LR_m, RCP19_DAC_LR_lm, RCP19_DAC_LR_hm) %>%
  set_colnames(c("impact_category", 'tech_heat',  'region', 'SSP2_RCP19_DAC', 'SSP2_RCP19_DAC_L', 'RCP19_DAC_L_low', 'RCP19_DAC_L_high'))

Side_bar_data_main <- 
  Side_bar_data %>% select(impact_category, tech_heat, region, SSP2_RCP19_DAC, SSP2_RCP19_DAC_L) %>%
  gather(key = 'scenario', value = 'impact_change', 4:5)

Side_bar_data_error_bar_1 <- 
  Side_bar_data_raw %>% select(impact_category, tech_heat, region, RCP19_DAC_LR_lm, RCP19_DAC_LR_hm) %>%
  mutate(scenario = 'SSP2_RCP19_DAC_L') %>%
  select(impact_category, tech_heat, region, scenario, RCP19_DAC_LR_lm, RCP19_DAC_LR_hm) %>%
  set_colnames(c("impact_category", 'tech_heat',  'region', 'scenario', 'lower_error', 'upper_error'))

Side_bar_data_error_bar_2 <- 
  Side_bar_data_error_bar_1 %>% select(impact_category, tech_heat, region) %>%
  mutate(scenario = 'SSP2_RCP19_DAC',
         lower_error = NA,
         upper_error = NA)

Side_bar_data_error_bar <- Side_bar_data_error_bar_1 %>% rbind(Side_bar_data_error_bar_2)

Side_bar_data_overall <- Side_bar_data_main %>% 
  merge(Side_bar_data_error_bar, by = c('impact_category', 'tech_heat', 'region', 'scenario')) %>%
  mutate(tech_heat_num = NA)


for (i in 1:nrow(Side_bar_data_overall)) {
  if (Side_bar_data_overall[i, 2] == "Solvent_BG") {
    Side_bar_data_overall[i, 8] = 1
  }
  else if (Side_bar_data_overall[i, 2] == "Solvent_NG") {
    Side_bar_data_overall[i, 8] = 2
  }
  else if (Side_bar_data_overall[i, 2] == "Sorbent_BG") {
    Side_bar_data_overall[i, 8] = 3
  }
  else if (Side_bar_data_overall[i, 2] == "Sorbent_HP") {
    Side_bar_data_overall[i, 8] = 4
  }
  
}

Side_bar_data_overall_1 <- Side_bar_data_overall

for (i in 1:nrow(Side_bar_data_overall_1)) {
  if (Side_bar_data_overall_1[i, 1] == "Climate change (kg CO2-Eq)" &
      Side_bar_data_overall_1[i, 2] == 'Sorbent_HP' &
      Side_bar_data_overall_1[i, 3] == 'US' &
      Side_bar_data_overall_1[i, 4] == 'SSP2_RCP19_DAC') {
    Side_bar_data_overall_1[i, 5] = Side_bar_data_overall[i, 5] /1.8
  }
  else if (Side_bar_data_overall_1[i, 1] == "Climate change (kg CO2-Eq)" &
           Side_bar_data_overall_1[i, 2] == 'Sorbent_HP' &
           Side_bar_data_overall_1[i, 3] == 'US' &
           Side_bar_data_overall_1[i, 4] == 'SSP2_RCP19_DAC_L') {
    Side_bar_data_overall_1[i, 5] = Side_bar_data_overall[i, 5] /1.8
    Side_bar_data_overall_1[i, 6] = Side_bar_data_overall[i, 6] /1.8
    Side_bar_data_overall_1[i, 7] = Side_bar_data_overall[i, 7] /1.8
  }
  
} 


## Figure function
Impact_figure_function <- function(region_abb, impact_name, m_lowerbound, m_upperbound, low_break, upp_break){
  
  DAC_type_color_scheme <- c("Solvent_NG"  = '#87cefa', 
                    "Solvent_BG" = '#1c86ee',
                    "Sorbent_HP" = 'orange', 
                    "Sorbent_BG"  = 'red')
  
  #region_abb = 'US'
  #impact_name = 'Climate change (kg CO2-Eq)'
  #m_lowerbound = -1280
  #m_upperbound = 0
  #low_break = -1200
  #upp_break = 0
  
  
  main_figure_data <- Main_DAC_data_together %>% 
    filter(region == region_abb & impact_category == impact_name)
  
  side_figure_data <- Side_bar_data_overall_1 %>% 
    filter(region == region_abb & impact_category == impact_name)
  

  
  main_figure <- ggplot(main_figure_data) +
    geom_line(aes(x = year, y = Baseline, group = tech_heat, color = tech_heat), linetype = "solid", size = 1.2) +
    geom_line(aes(x = year, y = RCP19_DAC, group = tech_heat, color = tech_heat), linetype = "dotted", size = 1.2) +
    #geom_line(aes(x = year, y = RCP19_DAC_LR, group = tech_heat, color = tech_heat), linetype = "dotted", size = 1.1) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 20,angle = 45, vjust = 0.6, hjust=0.7),
          axis.text.y = element_text(size = 20),
          axis.ticks.x = element_blank(),
          axis.line = element_line(colour = "black", size = 0.5),
          strip.text = element_text(size = 16),
          strip.background = element_blank(),
          legend.text = element_text(size = 18, face = "bold"),
          legend.title = element_blank(),
          legend.position="none",
          legend.spacing.x = unit(0.3, 'cm'),
          legend.spacing.y = unit(0.3, 'cm'),
          plot.margin = margin(t = 0.5, r = 0.14, b = 0.9, l = 0.15, "cm"),
          plot.title = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.background = element_blank(),
          panel.spacing.x = unit(1, 'cm')) +
    scale_y_continuous(limits = c(m_lowerbound, m_upperbound), breaks = c(seq(low_break, upp_break, (upp_break - low_break)/4)), name = "Value") +
    scale_color_manual(values=DAC_type_color_scheme)
  
  
  
  side_figure <- ggplot() +
    geom_bar(data = side_figure_data %>% filter(scenario == 'RCP19_DAC'),
             aes(x = tech_heat_num - 0.2, y = impact_change, color = tech_heat), fill = 'white', stat = "identity", width = 0.32, size = 1) + 
    geom_bar(data = side_figure_data %>% filter(scenario == 'RCP19_DAC_LR'),
             aes(x = tech_heat_num + 0.2, y = impact_change, color = tech_heat, fill = tech_heat), stat = "identity", width = 0.32, size = 0.5) + 
    
    geom_errorbar(data = side_figure_data %>% filter(scenario == 'RCP19_DAC_LR'),
                  aes(x = tech_heat_num + 0.2, ymin=lower_error, ymax=upper_error), width=.3,position=position_dodge(0.8)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.7) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 20, angle = 90, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 20),
          axis.ticks.x = element_blank(),
          axis.line = element_line(colour = "black", size = 0.5),
          
          legend.text = element_text(size = 18, face = "bold"),
          legend.title = element_blank(),
          legend.position="none",
          legend.spacing.x = unit(0.3, 'cm'),
          legend.spacing.y = unit(0.3, 'cm'),
          plot.margin = margin(t = 0.5, r = 4.5, b = 0.9, l = 0.1, "cm"),
          plot.title = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          panel.background = element_blank(),
          panel.spacing.x = unit(1, 'cm')) +
    scale_x_continuous(breaks = c(1, 2, 3, 4), label = c("SV+BM", "SV+NG", "SB+BM", "SB+HP")) +
    scale_y_continuous(limits = c(-1, 1), breaks = c(-1, -0.5, 0, 0.5, 1), name = "Value", position = 'right', labels = scales::percent) +
    scale_color_manual(values = DAC_type_color_scheme) + 
    scale_fill_manual(values = DAC_type_color_scheme)

  aggregated_figure <- plot_grid(main_figure,  side_figure, ncol=2, align="h")  
  
  aggregated_figure
  }


US_cli_chan_figure <- Impact_figure_function (region_abb = "US", impact_name = "Climate change (kg CO2-Eq)", m_lowerbound = -1280, m_upperbound = 0, low_break = -1200, upp_break = 0)

US_cli_chan_figure


##Making figures
US_cli_chan_figure <- Impact_figure_function (region_abb = "US", impact_name = "Climate change (kg CO2-Eq)", m_lowerbound = -1280, m_upperbound = 0, low_break = -1200, upp_break = 0)
US_fre_eutr_figure <- Impact_figure_function (region_abb = "US", impact_name = "Freshwater eutrophication (kg P-Eq)", m_lowerbound = 0, m_upperbound = 0.8, low_break = 0, upp_break = 0.8)
US_fre_ecot_figure <- Impact_figure_function (region_abb = "US", impact_name = "Freshwater ecotoxicity (kg 14-DCB)", m_lowerbound = 0, m_upperbound = 36, low_break = 0, upp_break = 36)
US_ter_acid_figure <- Impact_figure_function (region_abb = "US", impact_name = "Terrestrial acidification (kg SO2-Eq)", m_lowerbound = 0, m_upperbound = 2, low_break = 0, upp_break = 2)
US_ter_ecot_figure <- Impact_figure_function (region_abb = "US", impact_name = "Terrestrial ecotoxicity (kg 14-DCB)", m_lowerbound = 0, m_upperbound = 0.16, low_break = 0, upp_break = 0.16)
US_hum_toxi_figure <- Impact_figure_function (region_abb = "US", impact_name = "Human toxicity (kg 14-DCB)", m_lowerbound = 0, m_upperbound = 440, low_break = 0, upp_break = 440)
US_met_depl_figure <- Impact_figure_function (region_abb = "US", impact_name = "Metal depletion (kg Fe-Eq)", m_lowerbound = 0, m_upperbound = 52, low_break = 0, upp_break = 52)
US_wat_delp_figure <- Impact_figure_function (region_abb = "US", impact_name = "Water depletion (m3 water)", m_lowerbound = 0, m_upperbound = 12, low_break = 0, upp_break = 12)


plot_grid(US_cli_chan_figure, US_hum_toxi_figure, US_fre_eutr_figure, US_fre_ecot_figure, 
          ncol=2, align="h") 

plot_grid(US_ter_acid_figure, US_ter_ecot_figure, US_met_depl_figure, US_wat_delp_figure, 
          ncol=2, align="h")  



## exporting the data to excel sheet
main_figure_export_data <- Main_DAC_data_together %>% 
  select(impact_category, tech_heat,  region,  year, Baseline, RCP19_DAC) %>%
  filter(impact_category %in% impact_keep, region == 'US') %>%
  set_colnames(c('impact_category', 'tech_heat',  'region',  'year', 'SSP2_Baseline', 'SSP2_RCP19_DAC')) %>%
  gather(key = 'scenario', value = 'value', 5:6)

side_figure_export_data <- Side_bar_data_overall_1 %>% 
  filter(impact_category %in% impact_keep, region == 'US') %>%
  select(impact_category, tech_heat,  region,  scenario, impact_change, lower_error,   upper_error)


writeWorksheetToFile("C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/LCA_result/Figure_1_data.xlsx", data = main_figure_export_data, sheet = "line_plot_left_panel")
writeWorksheetToFile("C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/LCA_result/Figure_1_data.xlsx", data = side_figure_export_data, sheet = "bar_plot_right_panel")


## Some calculation for writing 
side_figure_result_for_writing <- Side_bar_data_overall_1 %>% 
  filter(region == 'US' & impact_category %in% impact_keep) %>%
  select(impact_category, tech_heat, region, scenario, impact_change) %>%
  spread(key = scenario, value = impact_change) %>%
  mutate(relative_change = RCP19_DAC/RCP19_DAC_LR)


side_figure_result_for_writing <- Side_bar_data_overall_1 %>% 
  filter(region == 'US' & impact_category %in% impact_keep & scenario == 'RCP19_DAC_LR') %>%
  select(impact_category, tech_heat, region, scenario, impact_change, lower_error, upper_error) %>%
  mutate(relative_change_low = lower_error - impact_change, 
         relative_change_up = impact_change - upper_error)


###############################################
## End of Figure 1




## Figure 3b -- LCA result of electricity generation
###############################################

## Data wrangling
Kwh_elec_impact_US_DAC_per_change_2020 <- Kwh_elec_impact_US %>% 
  gather(key = 'impact_category', value = 'value', 2:9) %>%
  spread(key = 'year', value = 'value') %>%
  mutate(r2020 = `2020` / `2020`- 1,
         r2030 = `2030` / `2020`- 1,
         r2040 = `2040` / `2020`- 1,
         r2050 = `2050` / `2020`- 1,
         r2060 = `2060` / `2020`- 1,
         r2070 = `2070` / `2020`- 1,
         r2080 = `2080` / `2020`- 1,
         r2090 = `2090` / `2020`- 1,
         r2100 = `2100` / `2020`- 1) %>%
  select(Climate_scenario, impact_category, r2020, r2030, r2040, r2050, r2060, r2070, r2080, r2090, r2100) %>%
  set_colnames(c("Climate_scenario", 'impact_category',  2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)) %>%
  gather(key = 'year', value = 'rela_change_2020', 3:11)

Kwh_elec_impact_US_DAC_diff_change_scenario <- Kwh_elec_impact_US %>% 
  gather(key = 'impact_category', value = 'value', 2:9) %>%
  spread(key = 'Climate_scenario', value = 'value') %>%
  mutate(kwh_elec_impact_DAC_diff = RCP19_DAC - RCP19) %>%
  select(year, impact_category, kwh_elec_impact_DAC_diff)



Kwh_elec_impact_US_DAC_per_change_2020 = Kwh_elec_impact_US_DAC_per_change_2020 %>% filter(impact_category %in% impact_keep)
Kwh_elec_impact_US_DAC_per_change_2020$impact_category <- factor(Kwh_elec_impact_US_DAC_per_change_2020$impact_category, levels = impact_keep)


Kwh_elec_impact_US_DAC_diff_change_scenario = Kwh_elec_impact_US_DAC_diff_change_scenario %>% filter(impact_category %in% impact_keep)
Kwh_elec_impact_US_DAC_diff_change_scenario$impact_category <- factor(Kwh_elec_impact_US_DAC_diff_change_scenario$impact_category, levels = impact_keep)


## Figure function
Elec_impact_change_fig_fun <- function(impact_name, scale, m_upperbound, m_lowerbound) {
  
  Impact_diff <- Kwh_elec_impact_US_DAC_diff_change_scenario %>% filter(impact_category == impact_name)
  Impact_time_change <- Kwh_elec_impact_US_DAC_per_change_2020 %>% filter(impact_category == impact_name)
  
  scale_num = scale
  
  elec_impact_change_figure <- ggplot() + 
    #geom_point(data = Kwh_elec_impact_US_DAC_per_change, aes(x = year, y = value, group = Climate_scenario, color = Climate_scenario), size = 2.6) + 
    
    geom_bar(data = Impact_diff, aes(x = year, y = kwh_elec_impact_DAC_diff), fill = "tan1", stat="identity") +
    geom_line(data = Impact_time_change, aes(x = year, y = rela_change_2020/scale_num, group = Climate_scenario, color = Climate_scenario), size = 1.3) + 
    geom_hline(yintercept = 0, color = "black", linetype='dashed',  size = 0.5) +
    theme(plot.margin = margin(1.3, 2, 0.1, 0.1, "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16),
          axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.text = element_blank(),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_blank(),
          #legend.direction = "horizontal",
          legend.position = "none",
          #legend.spacing.x = unit(0.8, 'cm'),
          #legend.spacing.y = unit(0.8, 'cm'),
          #legend.key.size = unit(1, "cm"),
          panel.spacing.x = unit(0.8, 'cm'),
          panel.spacing.y = unit(1, 'cm')) +
    
    scale_x_discrete(breaks=c(2020, 2040, 2060, 2080, 2100)) +
    scale_color_manual(name = "Climate scenario", 
                       values = c(RCP19 = 'red', RCP19_DAC = 'blue')) +
    scale_y_continuous(limits = c(m_lowerbound, m_upperbound), breaks = c(seq(m_lowerbound, m_upperbound, (m_upperbound - m_lowerbound)/4)), name = "Value",
                       sec.axis = sec_axis(~.*scale_num, name="Second Axis", breaks = c(-1.6, -0.8, 0, 0.8, 1.6), labels = scales::percent))
  
  elec_impact_change_figure
}


## Making figures
elec_impact_change_figure_CCI <- Elec_impact_change_fig_fun('Climate change (kg CO2-Eq)', 75, 0.02, -0.02)
elec_impact_change_figure_HTI<- Elec_impact_change_fig_fun("Human toxicity (kg 14-DCB)", 250, 0.006, -0.006)
elec_impact_change_figure_FEI <- Elec_impact_change_fig_fun("Freshwater eutrophication (kg P-Eq)", 400000, 4e-6, -4e-6)
elec_impact_change_figure_FTI <- Elec_impact_change_fig_fun("Freshwater ecotoxicity (kg 14-DCB)", 1900, 8e-4, -8e-4)
elec_impact_change_figure_TAI <- Elec_impact_change_fig_fun("Terrestrial acidification (kg SO2-Eq)", 80000, 2e-5, -2e-5)
elec_impact_change_figure_TTI <- Elec_impact_change_fig_fun('Terrestrial ecotoxicity (kg 14-DCB)', 400000, 4e-6, -4e-6)
elec_impact_change_figure_MD <- Elec_impact_change_fig_fun("Metal depletion (kg Fe-Eq)", 850, 2e-3, -2e-3)
elec_impact_change_figure_WD <- Elec_impact_change_fig_fun("Water depletion (m3 water)", 16000, 1e-4, -1e-4)


plot_grid(elec_impact_change_figure_CCI, elec_impact_change_figure_HTI, elec_impact_change_figure_FEI, elec_impact_change_figure_FTI, 
          elec_impact_change_figure_TAI, elec_impact_change_figure_TTI, elec_impact_change_figure_MD, elec_impact_change_figure_WD,
          ncol=2, align="h")  


## Export result to excel


writeWorksheetToFile("C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/LCA_result/Figure_3_data.xlsx", 
                     data = Kwh_elec_impact_US_DAC_diff_change_scenario, sheet = "Figure_3b_bar")

writeWorksheetToFile("C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/LCA_result/Figure_3_data.xlsx", 
                     data = Kwh_elec_impact_US_DAC_per_change_2020, sheet = "Figure_3b_line")

###############################################
## End of Figure 3b




## Figure 4 -- LCA result of DACCS (regional results)
###############################################

# Only calculate results for SSP2-RCP1.9 with DACCS scenario (with learning, reference learning rates)

Impact_category_factor <- c("Climate change (kg CO2-Eq)",
                            "Ozone depletion (kg CFC-11)",
                            "Ionising radiation (kg U235-Eq)",   
                            "Photochemical oxidant formation (kg NMVOC)",
                            "Particulate matter formation (kg PM10-Eq)", 
                            "Terrestrial acidification (kg SO2-Eq)", 
                            "Freshwater ecotoxicity (kg 14-DCB)",  
                            "Marine ecotoxicity (kg 14-DCB)", 
                            "Terrestrial ecotoxicity (kg 14-DCB)",   
                            "Human toxicity (kg 14-DCB)",      
                            "Freshwater eutrophication (kg P-Eq)",
                            "Marine eutrophication (kg N-Eq)", 
                            "Agricultural land occupation (square meter-year)",
                            "Natural land transformation (square meter)",
                            "Urban land occupation (square meter-year)", 
                            "Fossil depletion (kg oil-Eq)",  
                            "Metal depletion (kg Fe-Eq)",   
                            "Water depletion (m3 water)")

DAC_data_RCP19_DAC_LR$`Impact category` <- factor(DAC_data_RCP19_DAC_LR$`Impact category`, levels = Impact_category_factor)


## We don't include the captured 1t CO2 in this specific results for climate change impact, we we add the captured 1t CO2 back to the climate change impact

for (i in 1:nrow(DAC_data_RCP19_DAC_LR)) {
  if (DAC_data_RCP19_DAC_LR[i, 1] == 'Climate change (kg CO2-Eq)') {
    DAC_data_RCP19_DAC_LR[i, 3:11] = DAC_data_RCP19_DAC_LR[i, 3:11] + 1000
  } else {
    DAC_data_RCP19_DAC_LR[i, 3:11] = DAC_data_RCP19_DAC_LR[i, 3:11] + 0
  }
}



GL_2020_data <- DAC_data_RCP19_DAC_LR %>% 
  mutate(tech = substr(Heat_elec_region, 0, 10),
         region = substr(Heat_elec_region, 12, 13)) %>%
  select(`Impact category`, tech, region, `2020`) %>%
  filter(region == 'GL') 

All_data <- DAC_data_RCP19_DAC_LR %>% 
  mutate(tech = substr(Heat_elec_region, 0, 10),
         region = substr(Heat_elec_region, 12, 13)) %>%
  select(`Impact category`, tech, region, `2020`, `2060`, `2100`) 

merge_data <- merge(x = All_data, y = GL_2020_data, by = c('Impact category', 'tech'), all.x = TRUE) 
merge_data <- as_tibble(merge_data)
merge_data$`Impact category` <- factor(merge_data$`Impact category`, levels = Impact_category_factor)

gathered_data <- merge_data%>%
  set_colnames(c('Impact_category', 'tech', 'region', 'y2020', 'y2060', 'y2100', 'region_d', 'b2020')) %>%
  select('Impact_category', 'tech', 'region', 'y2020', 'y2060', 'y2100', 'b2020') %>%
  mutate(r_2020 = y2020/b2020,
         r_2060 = y2060/b2020,
         r_2100 = y2100/b2020) %>%
  select('Impact_category', 'tech', 'region', 'r_2020', 'r_2060', 'r_2100') %>%
  gather(key = 'year', value = 'value', 4:6) %>%
  spread(key = `Impact_category`, value = 'value') %>%
  setNames(., c("tech", "region", "year", 'CCI', 'ODI', 'IRI', 'POI', 'PMI', 'TAI', 'FTI', 'MTI', 'TTI', 'HTI', 'FEI', 'MEI', 'AL', 'NL', 'UL', 'FD', 'MD', 'WD')) %>%
  select('tech', "region", 'year', 'CCI', 'HTI', 'FEI', 'FTI', 'TAI', 'TTI', 'MD', 'WD') %>%
  gather(key = 'impact_category', value = 'Impact_change', 4:11)


GL_data <- gathered_data %>% filter(region == 'GL')
region_data <- gathered_data %>% filter(region != 'GL')
Impact_factor <- c('CCI', 'HTI', 'FEI', 'FTI', 'TAI', 'TTI', 'MD', 'WD')
GL_data$impact_category <- factor(GL_data$impact_category, levels = Impact_factor)
region_data$impact_category <- factor(region_data$impact_category, levels = Impact_factor)


## Figure function
Impact_tradeoff_figure_function <- function(tech_name, lowerbound, upperbound, length) {
  
  GL_data_use <- GL_data %>% filter(tech == tech_name)
  region_data_use <- region_data %>% filter(tech == tech_name)
  
  Impact_trade_off_point_plot <- 
    ggplot() + 
    geom_point(data = GL_data_use, aes(x = year, y = Impact_change, group=1), shape = 0, size = 5) + 
    geom_line(data = GL_data_use, aes(x = year, y = Impact_change, group=1), size=1, alpha=.3) +
    geom_point(data = region_data_use, aes(x = year, y = Impact_change, group = region, color = region), shape = 16, size = 5) + 
    geom_line(data = region_data_use, aes(x = year, y = Impact_change, group = region, color = region), size = 1, alpha=.3) +
    geom_hline(yintercept = 1, linetype="dashed", color = "black") +
    facet_wrap(~ impact_category, nrow = 1) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 20, angle = 90, hjust = 0.5, vjust = 0.5),
          axis.text.y = element_text(size = 20),
          axis.ticks.x = element_blank(),
          axis.line = element_line(colour = "black", size = 0.5),
          strip.background = element_blank(),
          strip.text.x = element_text(size = 20),
          legend.position="none",
          legend.text = element_text(colour="black", size=16),
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, color = "black"),
          panel.spacing.x = unit(0.5,"cm"))  +
    scale_x_discrete(labels=c("r_2020" = "2020", "r_2060" = "2060", "r_2100" = "2100")) +
    scale_y_continuous(limits = c(lowerbound, upperbound), breaks = c(seq(0, 2, length)), name = "Value", labels = scales::percent) +
    scale_color_manual(name = "", 
                       values = c("CN" = "blue", "US" = "red", "EU" = "gray50", 'RU' = 'orange' ),
                       guide = guide_legend(reverse=TRUE))
  
  Impact_trade_off_point_plot
  
}



## Only make figures for Solvent-based DACCS with natural gas and Sorbent-based DACCS with heatpump
Solvent_NG_RCP19_fig <- Impact_tradeoff_figure_function('Solvent_NG', 0, 2, 0.5)
Sorbent_HP_RCP19_fig <- Impact_tradeoff_figure_function('Sorbent_HP',-0.04, 2, 0.5)

plot_grid(Solvent_NG_RCP19_fig, Sorbent_HP_RCP19_fig, ncol=1, align="v")


## export data to excel
region_LCA_result_export <- gathered_data %>% filter(tech %in% c('Solvent_NG', 'Sorbent_HP'))
writeWorksheetToFile("C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/LCA_result/Figure_4_data.xlsx", data = region_LCA_result_export, sheet = "DACCS_regional_LCA_result")

###############################################
## End of Figure 4




## SI-Figure 6 -- LCA contribution analysis (US data only)
###############################################


## Data wrangling 
process_level <- c("0_CO2_capture", "1_Construction", "3_Op_electricity", "4_Op_heat", "2_Op_material", "5_EoL_treat")
tech_heat_level <- c('Sol_BG', 'Sol_NG', 'Sor_BG', 'Sor_HP')

LCA_con_data_summary <- LCA_con_sol_RCP19_data %>% 
  rbind(LCA_con_sol_RCP19_LR_data) %>%
  rbind(LCA_con_sor_RCP19_data) %>%
  rbind(LCA_con_sor_RCP19_LR_data) %>% 
  mutate(tech_heat = substr(Tech_heat_region, 1, 6),
         region = substr(Tech_heat_region, 8, 9)) %>%
  set_colnames(c('year', 'impact_category', 'tech_heat_region', 'process', 'value', 'scenario', 'tech_heat', 'region')) %>%
  select(year, impact_category, scenario, tech_heat, process, value) %>%
  filter(impact_category %in% impact_keep & year %in% c(2020, 2040, 2060, 2080, 2100))

LCA_con_data_summary$process <- factor(LCA_con_data_summary$process, levels = process_level)
LCA_con_data_summary$tech_heat <- factor(LCA_con_data_summary$tech_heat, levels = tech_heat_level)


## Figure function
Contribution_figure_fun <- function (impact_name, lower_l, upper_l, lower_b, upper_b) {
  
  data_input <- LCA_con_data_summary %>% filter(impact_category == impact_name)
  data_input$year <- as.numeric(data_input$year)
  
  contribution_color_scheme <- c(
    "0_CO2_capture" = 'black',    
    "1_Construction" = 'gray50', 
    "4_Op_heat" =  'gray80', 
    "3_Op_electricity" = "#0070ff", ## blue
    "2_Op_material" =  '#ffdf00',  ## gold
    "5_EoL_treat" = '#00cd00'
  )
  
  Contribution_figure <-ggplot()+
    geom_bar(data = data_input %>% filter(scenario == 'SSP2_RCP19_DAC'),
             aes(x = year - 3.5, y = value, fill = process), position = "stack", stat = "identity", width = 6) + 
    geom_bar(data = data_input %>% filter(scenario == 'SSP2_RCP19_DAC_L'),
             aes(x = year + 3.5, y = value, fill = process), position = "stack", stat = "identity", width = 6) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.7) +
    facet_wrap(~ tech_heat , nrow = 1) + 
    
    theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 20, angle = 45, vjust = 0.6, hjust=0.7),
          axis.text.y = element_text(size = 20),
          axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
          panel.background = element_blank(),
          #panel.border = element_blank(),
          strip.text = element_blank(),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.direction = "horizontal",
          legend.position = "none",
          legend.spacing.x = unit(0.5, 'cm'),
          legend.spacing.y = unit(0.8, 'cm'),
          legend.key.size = unit(1, "cm"),
          panel.spacing.x = unit(0.8, 'cm')) +
    scale_fill_manual(values = contribution_color_scheme) +
    scale_x_continuous(breaks = c(2020, 2040, 2060, 2080, 2100)) +
    scale_y_continuous(limits = c(lower_l, upper_l), breaks = c(seq(lower_b, upper_b, (upper_b - lower_b)/4)))
  
  Contribution_figure
}
  

Contribution_figure_fun


## US
US_cli_chan_con_figure <- Contribution_figure_fun (impact_name = "Climate change (kg CO2-Eq)", lower_l = -1400, upper_l = 700, lower_b = -1400, upper_b = 700)
US_fre_eutr_con_figure <- Contribution_figure_fun (impact_name = "Freshwater eutrophication (kg P-Eq)", lower_l = -0.01, upper_l = 0.61, lower_b = 0, upper_b = 0.6)
US_fre_ecot_con_figure <- Contribution_figure_fun (impact_name = "Freshwater ecotoxicity (kg 14-DCB)", lower_l = -0.8, upper_l = 20, lower_b = 0, upper_b = 20)
US_ter_acid_con_figure <- Contribution_figure_fun (impact_name = "Terrestrial acidification (kg SO2-Eq)", lower_l = -0.01, upper_l = 2, lower_b = 0, upper_b = 2)
US_ter_ecot_con_figure <- Contribution_figure_fun (impact_name = "Terrestrial ecotoxicity (kg 14-DCB)", lower_l = -0.01, upper_l = 0.08, lower_b = 0, upper_b = 0.08)
US_hum_toxi_con_figure <- Contribution_figure_fun (impact_name = "Human toxicity (kg 14-DCB)", lower_l = -1, upper_l = 440, lower_b = 0, upper_b = 440)
US_met_depl_con_figure <- Contribution_figure_fun (impact_name = "Metal depletion (kg Fe-Eq)", lower_l = -0.5, upper_l = 40, lower_b = 0, upper_b = 40)
US_wat_delp_con_figure <- Contribution_figure_fun (impact_name = "Water depletion (m3 water)", lower_l = -0.1, upper_l = 12, lower_b = 0, upper_b = 12)


plot_grid(US_cli_chan_con_figure, US_hum_toxi_con_figure, 
          ncol=1, align="v") 

plot_grid(US_fre_eutr_con_figure, US_fre_ecot_con_figure, 
          ncol=1, align="v") 

plot_grid(US_ter_acid_con_figure, US_ter_ecot_con_figure, 
          ncol=1, align="h")  

plot_grid(US_met_depl_con_figure, US_wat_delp_con_figure, 
          ncol=1, align="h")  


## Legend

data_input <- LCA_con_data_summary %>% filter(impact_category == "Climate change (kg CO2-Eq)")
data_input$year <- as.numeric(data_input$year)

con_analysis_legend <- 
  ggplot()+
  geom_bar(data = data_input %>% filter(scenario == 'SSP2_RCP19_DAC'),
           aes(x = year - 3.5, y = value, fill = process), position = "stack", stat = "identity", width = 6) + 
  geom_bar(data = data_input %>% filter(scenario == 'SSP2_RCP19_DAC_L'),
           aes(x = year + 3.5, y = value, fill = process), position = "stack", stat = "identity", width = 6) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.7) +
  facet_wrap(~ tech_heat , nrow = 1) + 
  
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 20,angle = 45, vjust = 0.6, hjust=0.7),
        axis.text.y = element_text(size = 20),
        axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
        panel.background = element_blank(),
        #panel.border = element_blank(),
        strip.text = element_text(size = 16),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.spacing.x = unit(0.5, 'cm'),
        legend.spacing.y = unit(0.8, 'cm'),
        legend.key.size = unit(1, "cm"),
        panel.spacing.x = unit(0.8, 'cm')) +
  scale_fill_manual(values = contribution_color_scheme,
                    guide = guide_legend(reverse=FALSE, nrow = 1)) +
  scale_x_continuous(breaks = c(2020, 2040, 2060, 2080, 2100)) +
  scale_y_continuous(limits = c(lower_l, upper_l), breaks = c(seq(lower_b, upper_b, (upper_b - lower_b)/4)))
  

## end of figure


## Export data to excel

writeWorksheetToFile("C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/LCA_result/SI_Figure_6_data.xlsx", data = LCA_con_data_summary, sheet = "DACCS_LCA_contribution")

###############################################
## End of SI-Figure 6




## SI-Figure 9 -- Electricity generation LCA result (contribution by technology (US data only))
###############################################

## Data wrangling
Elec_impact_US_baseline$tech <- factor(Elec_impact_US_baseline$tech, level = gentech_sort)
Elec_impact_US_baseline$impact_category <- factor(Elec_impact_US_baseline$impact_category, level = category_list)
Elec_impact_US_baseline$year <- as.numeric(Elec_impact_US_baseline$year)

Elec_impact_US_RCP19$tech <- factor(Elec_impact_US_RCP19$tech, level = gentech_sort)
Elec_impact_US_RCP19$impact_category <- factor(Elec_impact_US_RCP19$impact_category, level = category_list)
Elec_impact_US_RCP19$year <- as.numeric(Elec_impact_US_RCP19$year)

Elec_impact_US_RCP19_noDAC$tech <- factor(Elec_impact_US_RCP19_noDAC$tech, level = gentech_sort)
Elec_impact_US_RCP19_noDAC$impact_category <- factor(Elec_impact_US_RCP19_noDAC$impact_category, level = category_list)
Elec_impact_US_RCP19_noDAC$year <- as.numeric(Elec_impact_US_RCP19_noDAC$year)


## Figure function
Elec_impact_figure_fun = function(data_name, category, upperbound, lowerbound, up_limit, low_limit) {
  ggplot() +
    geom_area(data = data_name %>% filter(impact_category == category) %>% filter(tech %in% gentech_sort),
              aes(x = year, y = value, fill = tech)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 16),
          axis.text.y = element_text(size = 16),
          axis.ticks.x = element_blank(),
          axis.line = element_line(colour = "black", size = 0.5),
          strip.text = element_text(size = 16),
          strip.background = element_blank(),
          legend.text = element_text(size = 18, face = "bold"),
          legend.title = element_blank(),
          legend.position="none",
          legend.spacing.x = unit(0.3, 'cm'),
          legend.spacing.y = unit(0.3, 'cm'),
          plot.margin = margin(0.9,0.5,0.1,0.6, "cm"),
          plot.title = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          panel.spacing.x = unit(1, 'cm')) +
    scale_y_continuous(limits = c(lowerbound, upperbound), breaks = c(seq(low_limit, up_limit, (up_limit - low_limit)/4)), name = "Value") +
    scale_fill_manual(values = eco_color_scheme)}


## Making figure
# SSP2-Baseline  
CLI_elec_impact_baseline <- Elec_impact_figure_fun(Elec_impact_US_baseline, 'Climate change (kg CO2-Eq)', 0.6, -0.05, 0.6, 0)
TAD_elec_impact_baseline <- Elec_impact_figure_fun(Elec_impact_US_baseline, 'Terrestrial acidification (kg SO2-Eq)', 0.0012, 0, 0.0012, 0)
FET_elec_impact_baseline <- Elec_impact_figure_fun(Elec_impact_US_baseline, 'Freshwater ecotoxicity (kg 14-DCB)', 0.016, 0, 0.016, 0)
TET_elec_impact_baseline <- Elec_impact_figure_fun(Elec_impact_US_baseline, 'Terrestrial ecotoxicity (kg 14-DCB)', 0.00008, 0, 0.00008, 0)
HTO_elec_impact_baseline <- Elec_impact_figure_fun(Elec_impact_US_baseline, 'Human toxicity (kg 14-DCB)', 0.32, 0, 0.32, 0)
FEU_elec_impact_baseline <- Elec_impact_figure_fun(Elec_impact_US_baseline, 'Freshwater eutrophication (kg P-Eq)', 0.00048, 0, 0.00048, 0)
MDP_elec_impact_baseline <- Elec_impact_figure_fun(Elec_impact_US_baseline, 'Metal depletion (kg Fe-Eq)', 0.020, 0, 0.020, 0)
WDP_elec_impact_baseline <- Elec_impact_figure_fun(Elec_impact_US_baseline, 'Water depletion (m3 water)', 0.0024, 0, 0.0024, 0)


# SSP2-RCP19 w/o DACCS  
CLI_elec_impact_RCP19_noDAC <- Elec_impact_figure_fun(Elec_impact_US_RCP19_noDAC, 'Climate change (kg CO2-Eq)', 0.6, -0.05, 0.6, 0)
TAD_elec_impact_RCP19_noDAC <- Elec_impact_figure_fun(Elec_impact_US_RCP19_noDAC, 'Terrestrial acidification (kg SO2-Eq)', 0.0012, 0, 0.0012, 0)
FET_elec_impact_RCP19_noDAC <- Elec_impact_figure_fun(Elec_impact_US_RCP19_noDAC, 'Freshwater ecotoxicity (kg 14-DCB)', 0.016, 0, 0.016, 0)
TET_elec_impact_RCP19_noDAC <- Elec_impact_figure_fun(Elec_impact_US_RCP19_noDAC, 'Terrestrial ecotoxicity (kg 14-DCB)', 0.00008, 0, 0.00008, 0)
HTO_elec_impact_RCP19_noDAC <- Elec_impact_figure_fun(Elec_impact_US_RCP19_noDAC, 'Human toxicity (kg 14-DCB)', 0.32, 0, 0.32, 0)
FEU_elec_impact_RCP19_noDAC <- Elec_impact_figure_fun(Elec_impact_US_RCP19_noDAC, 'Freshwater eutrophication (kg P-Eq)', 0.00048, 0, 0.00048, 0)
MDP_elec_impact_RCP19_noDAC <- Elec_impact_figure_fun(Elec_impact_US_RCP19_noDAC, 'Metal depletion (kg Fe-Eq)', 0.020, 0, 0.020, 0)
WDP_elec_impact_RCP19_noDAC <- Elec_impact_figure_fun(Elec_impact_US_RCP19_noDAC, 'Water depletion (m3 water)', 0.0024, 0, 0.0024, 0)

# SSP2-RCP19 w/ DACCS  
CLI_elec_impact_RCP19 <- Elec_impact_figure_fun(Elec_impact_US_RCP19, 'Climate change (kg CO2-Eq)', 0.6, -0.05, 0.6, 0)
TAD_elec_impact_RCP19 <- Elec_impact_figure_fun(Elec_impact_US_RCP19, 'Terrestrial acidification (kg SO2-Eq)', 0.0012, 0, 0.0012, 0)
FET_elec_impact_RCP19 <- Elec_impact_figure_fun(Elec_impact_US_RCP19, 'Freshwater ecotoxicity (kg 14-DCB)', 0.016, 0, 0.016, 0)
TET_elec_impact_RCP19 <- Elec_impact_figure_fun(Elec_impact_US_RCP19, 'Terrestrial ecotoxicity (kg 14-DCB)', 0.00008, 0, 0.00008, 0)
HTO_elec_impact_RCP19 <- Elec_impact_figure_fun(Elec_impact_US_RCP19, 'Human toxicity (kg 14-DCB)', 0.32, 0, 0.32, 0)
FEU_elec_impact_RCP19 <- Elec_impact_figure_fun(Elec_impact_US_RCP19, 'Freshwater eutrophication (kg P-Eq)', 0.00048, 0 , 0.00048, 0)
MDP_elec_impact_RCP19 <- Elec_impact_figure_fun(Elec_impact_US_RCP19, 'Metal depletion (kg Fe-Eq)', 0.020, 0, 0.020, 0)
WDP_elec_impact_RCP19 <- Elec_impact_figure_fun(Elec_impact_US_RCP19, 'Water depletion (m3 water)', 0.0024, 0, 0.0024, 0)



plot_grid(CLI_elec_impact_baseline, CLI_elec_impact_RCP19_noDAC, CLI_elec_impact_RCP19,
          HTO_elec_impact_baseline, HTO_elec_impact_RCP19_noDAC, HTO_elec_impact_RCP19,
          FEU_elec_impact_baseline, FEU_elec_impact_RCP19_noDAC, FEU_elec_impact_RCP19,
          nrow = 3, align="v")

plot_grid(FET_elec_impact_baseline, FET_elec_impact_RCP19_noDAC, FET_elec_impact_RCP19, 
          TAD_elec_impact_baseline, TAD_elec_impact_RCP19_noDAC, TAD_elec_impact_RCP19, 
          TET_elec_impact_baseline, TET_elec_impact_RCP19_noDAC, TET_elec_impact_RCP19,
          nrow = 3, align="v")

plot_grid(TET_elec_impact_baseline, TET_elec_impact_RCP19_noDAC, TET_elec_impact_RCP19,
          MDP_elec_impact_baseline, MDP_elec_impact_RCP19_noDAC, MDP_elec_impact_RCP19,  
          WDP_elec_impact_baseline, WDP_elec_impact_RCP19_noDAC, WDP_elec_impact_RCP19,
          nrow = 3, align="v")


legend <- ggplot() +
  geom_area(data = Elec_impact_data_baseline %>% filter(impact == 'Climate change (kg CO2-Eq)'),
            aes(x = year, y = value, fill = tech)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        axis.ticks.x = element_blank(),
        axis.line = element_line(colour = "black", size = 0.5),
        strip.text = element_text(size = 16),
        strip.background = element_blank(),
        legend.text = element_text(size = 18, face = "bold"),
        legend.title = element_blank(),
        legend.position = "left",
        legend.spacing.x = unit(0.3, 'cm'),
        legend.spacing.y = unit(0.3, 'cm'),
        plot.margin = margin(0.1,0.1,0.1,2.5, "cm"),
        plot.title = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.spacing.x = unit(1, 'cm')) +
  scale_y_continuous(limits = c(0, 0.8), breaks = c(seq(0, 0.8, (0.8)/4)), name = "Value") +
  scale_fill_manual(values = eco_color_scheme)

# End of figure


## Export data to excel

writeWorksheetToFile("C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/LCA_result/SI_Figure_9_data.xlsx", data = Elec_impact_US_baseline, sheet = "SSP2_Baseline")
writeWorksheetToFile("C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/LCA_result/SI_Figure_9_data.xlsx", data = Elec_impact_US_RCP19_noDAC, sheet = "SSP2_RCP19")
writeWorksheetToFile("C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/LCA_result/SI_Figure_9_data.xlsx", data = Elec_impact_US_RCP19, sheet = "SSP2_RCP19_DAC")


###############################################
## End of SI-Figure 9




