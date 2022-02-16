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


## Data Import
###############################################

# Electricity generation and mix data
elec_mix_baseline_im <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/elec_mix_summary_SSP2_baseline.xlsx")
elec_mix_RCP19_im <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/elec_mix_summary_SSP2_RCP19.xlsx")
elec_mix_RCP19_DAC_im <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/elec_mix_summary_SSP2_RCP19_DAC.xlsx")

# Generation results are only used for Figure 2a
elec_prod_RCP19_im <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/elec_prod_summary_SSP2_RCP19.xlsx")
elec_prod_RCP19_DAC_im <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/elec_prod_summary_SSP2_RCP19_DAC.xlsx")

# Annual DACCS operational capacity
DAC_operation_capacity <- read_excel('C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/Learning curve/DAC_deployment_result.xlsx', sheet = 'DAC_operation_Gt')


# Electricity CO2 emission data
elec_co2_baseline_im <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/elec_CO2_summary_SSP2_baseline.xlsx")
elec_co2_RCP19_im <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/elec_CO2_summary_SSP2_RCP19.xlsx")
elec_co2_RCP19_DAC_im <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/elec_CO2_summary_SSP2_RCP19_DAC.xlsx")


# DACCS electricity demand data
DAC_elec_demand_im <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/DACCS_elect_demand_data.xlsx")


# Primary energy consumption
PE_use_RCP19_im <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/PE_use_RCP19_data.xlsx")
PE_use_RCP19_DAC_im <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/PE_use_RCP19_DAC_data.xlsx")


# CO2 sequestration data
DAC_co2_seq_RCP19_im <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/DAC_co2_seq_RCP19_data.xlsx")
BECCS_co2_seq_RCP19_im <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/BECCS_co2_seq_RCP19_data.xlsx")
total_co2_seq_RCP19_im <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/total_co2_seq_RCP19_data.xlsx")

DAC_co2_seq_RCP19_DAC_im <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/DAC_co2_seq_RCP19_DAC_data.xlsx")
BECCS_co2_seq_RCP19_DAC_im <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/BECCS_co2_seq_RCP19_DAC_data.xlsx")
total_co2_seq_RCP19_DAC_im <- read_excel("C:/Users/YQIU/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/total_co2_seq_RCP19_DAC_data.xlsx")

###############################################


## Basic setups
###############################################

tech_sort = c('Coal_ST', 
             'IGCC',
             'Coal_CHP', 
             'Coal_CCS', 
             'Coal_CHP_CCS', 
             'Natural_gas_CC', 
             'Natural_gas_OC',
             'Natural_gas_CHP', 
             'Natural_gas_CCS',
             'Natural_gas_CHP_CCS', 
             'Oil_ST', 
             'Oil_CC', 
             'Oil_CHP', 
             'Oil_CCS', 
             'Oil_CHP_CCS', 
             'Nuclear', 
             'Hydro', 
             'Wind_onshore', 
             'Wind_offshore',
             'CSP', 
             'Solar_PV_decen', 
             'Solar_PV_cen', 
             'Biomass_ST', 
             'Biomass_CC', 
             'Biomass_CHP', 
             'Biomass_CCS',
             'Biomass_CHP_CCS', 
             'Wave', 
             'Other_renewables')

tech_sort_agg = c('Coal', 
                  'Coal_CCS', 
                  'Natural_gas', 
                  'Natural_gas_CCS',
                  'Oil', 
                  'Oil_CCS',
                  'Nuclear', 
                  'Hydro', 
                  'Wind', 
                  'Solar', 
                  'Biomass', 
                  'Biomass_CCS',
                  'Other_renewables')


gen_type_color_scheme = c('Coal_ST' = 'black', 
    'IGCC' = 'black',
    'Coal_CHP' = 'black', 
    'Coal_CCS' = 'gray30', 
    'Coal_CHP_CCS' = 'gray30', 
    'Natural_gas_CC' = "gray50", 
    'Natural_gas_OC' = "gray50",
    'Natural_gas_CHP' = "gray50", 
    'Natural_gas_CCS' = "gray80",
    'Natural_gas_CHP_CCS' = "gray80", 
    'Oil_ST' = "brown3", 
    'Oil_CC' = "brown3", 
    'Oil_CHP' = "brown3", 
    'Oil_CCS' = "brown3", 
    'Oil_CHP_CCS' = "brown3", 
    'Nuclear' = "cyan", 
    'Hydro' = rgb(170/256, 255/256, 195/256), 
    'Wind_onshore' = "blue", 
    'Wind_offshore' = "blue",
    'CSP' = "yellow", 
    'Solar_PV_decen' = "yellow", 
    'Solar_PV_cen' = "yellow", 
    'Biomass_ST' = "green3", 
    'Biomass_CC' = "green3", 
    'Biomass_CHP' = "green3", 
    'Biomass_CCS' = "green",
    'Biomass_CHP_CCS' = "green", 
    'Wave' = "red", 
    'Other_renewables' = "red")

color_scheme_agg = c('Coal' = 'black', 
                     'Coal_CCS' = 'gray30', 
                     'Natural_gas' = "gray50", 
                     'Natural_gas_CCS' = "gray80",
                     'Oil' = "brown3", 
                     'Oil_CCS' = "brown3",
                     'Nuclear' = "cyan", 
                     'Hydro' = rgb(170/256, 255/256, 195/256), 
                     'Wind' = "blue", 
                     'Solar' = "yellow", 
                     'Biomass' = "green3", 
                     'Biomass_CCS' = "green",
                     'Other_renewables' = "red")


Climate_color_scheme = c('SSP2' = 'black', 
                         'RCP19'= 'blue',
                         'RCP19_DAC' = 'skyblue2')

###############################################



## Figure 2 and SI-Figure 2-5 -- Electricity mix and CO2 emission
###############################################

## Data wrangling (Electricity production and mix data)
elec_data_clean_fun <- function(data) {
  
  data_1 <- data[2:8]
  
  cleaned_data <- 
    data_1 %>% 
    set_colnames(c("year", "tech", "China", "US", "Western_EU", "Russia", "World")) %>%
    gather(key = 'region', value = 'elec_gen', 3:7)
  
  cleaned_data$tech <- fct_rev(factor(cleaned_data$tech, level = tech_sort))
  
  cleaned_data
  
}

elec_mix_baseline <- elec_data_clean_fun(elec_mix_baseline_im)
elec_mix_RCP19 <- elec_data_clean_fun(elec_mix_RCP19_im)
elec_mix_RCP19_DAC <- elec_data_clean_fun(elec_mix_RCP19_DAC_im)

Elect_mix_summary <- elec_mix_baseline %>% set_colnames(c("year", "tech", "region", "SSP2"))
Elect_mix_summary$RCP19 <- elec_mix_RCP19$elec_gen
Elect_mix_summary$RCP19_DAC <- elec_mix_RCP19_DAC$elec_gen

Elect_mix_data_fig <- 
  Elect_mix_summary %>% 
  gather(key = 'Climate', value = 'mix', 4:6) %>% 
  filter(Climate %in% c('SSP2', 'RCP19', 'RCP19_DAC'))

climate_order <- c('SSP2', 'RCP19', 'RCP19_DAC')

Elect_mix_data_fig$Climate <- factor(Elect_mix_data_fig$Climate, levels = climate_order) 



## Data wrangling (Electricity CO2 emission data)
elec_co2_data_clean_fun <- function(data) {
  
  data_1 <- data[2:7]
  
  cleaned_data <- 
    data_1 %>% 
    set_colnames(c("year", "China", "US", "Western_EU", "Russia", "World")) %>%
    gather(key = 'region', value = 'elec_CO2_Gt', 2:6)
  
  cleaned_data
}

elec_co2_baseline_cleaned <- elec_co2_data_clean_fun(elec_co2_baseline_im)
elec_co2_RCP19_cleaned <- elec_co2_data_clean_fun(elec_co2_RCP19_im)
elec_co2_RCP19_DAC_cleaned <- elec_co2_data_clean_fun(elec_co2_RCP19_DAC_im)


Elect_co2_summary_Gt <- elec_co2_baseline_cleaned %>% set_colnames(c("year", "region", "SSP2"))
Elect_co2_summary_Gt$RCP19 <- elec_co2_RCP19_cleaned$elec_CO2_Gt
Elect_co2_summary_Gt$RCP19_DAC <- elec_co2_RCP19_DAC_cleaned$elec_CO2_Gt


Elect_co2_data_fig <- 
  Elect_co2_summary_Gt %>% 
  gather(key = 'Climate', value = 'elec_co2_Gt', 3:5) %>% 
  filter(Climate %in% c('SSP2', 'RCP19', 'RCP19_DAC'))

Elect_co2_data_fig$Climate <- factor(Elect_co2_data_fig$Climate, levels = climate_order) 



## Data wrangling (DAC electricity demand data)
DAC_elec_demand_clean <- 
  DAC_elec_demand_im[2:8] %>% 
  set_colnames(c("year", "Tech", "China", "US", "Western_EU", "Russia", "World")) %>%
  gather(key = 'region', value = 'elec_demand_TWh', 3:7) %>%
  select(year, region, elec_demand_TWh)

Region_total_gen <- Elect_gen_summary %>% 
  select(year, tech, region, SSP2_RCP19_DAC) %>%
  group_by(year, region) %>%
  summarize(Total_gen = sum(SSP2_RCP19_DAC))

DAC_elec_demand_ratio <- merge(DAC_elec_demand_clean, Region_total_gen, by = c("year", "region")) %>%
  mutate(DAC_elect_demand_r = elec_demand_TWh/Total_gen) %>%
  select(year, region, DAC_elect_demand_r)

## End of Data wrangling 



## Start to make figures
# Function for Figure 2a, 2b
elec_mix_figure_fun <- function(climate_scenario, region_name, sec_coeff_value){
  
  year_10 <- c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)
  elec_mix_data_figure <- Elect_mix_data_fig %>% filter(region == region_name & Climate == climate_scenario & year %in% year_10)

  
  sec_coeff = sec_coeff_value
  
  elec_mix_figure <- 
    ggplot() + 
    geom_area(data = elec_mix_data_figure, aes(x = year, y = mix, fill = tech)) + 

    theme(plot.margin = margin(0.5,0.5,1.5,2.0, "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.text = element_text(size = 16),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.direction = "horizontal",
          legend.position = "none",
          legend.spacing.x = unit(0.5, 'cm'),
          legend.spacing.y = unit(0.8, 'cm'),
          legend.key.size = unit(1, "cm"),
          panel.spacing.x = unit(0.8, 'cm')) +
    scale_fill_manual(name = "Generation technologies", 
                      values = gen_type_color_scheme,
                      guide = guide_legend(reverse=TRUE, nrow = 5)) +
    scale_y_continuous(labels = scales::percent)
  
  elec_mix_figure
}


# Function for Figure 2c
elec_mix_figure_fun2 <- function(climate_scenario, region_name, sec_coeff_value, scale_num_value){
  
  year_10 <- c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)
  
  elec_mix_data_figure <- Elect_mix_data_fig %>% filter(region == region_name & Climate == climate_scenario & year %in% year_10)
  DAC_elec_demand_figure <- DAC_elec_demand_ratio %>% filter(region == region_name & year %in% year_10)
  
  sec_coeff = sec_coeff_value
  scale_num = scale_num_value
  
  elec_mix_figure <- 
    ggplot() + 
    geom_area(data = elec_mix_data_figure, aes(x = year, y = mix, fill = tech)) + 
    geom_line(data = DAC_elec_demand_figure, aes(x = year, y = DAC_elect_demand_r*scale_num), color = 'red', linetype = 'dashed', size = 1.6) + 
    theme(plot.margin = margin(0.5,0.5,1.5,2.0, "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.text = element_text(size = 16),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.direction = "horizontal",
          legend.position = "none",
          legend.spacing.x = unit(0.5, 'cm'),
          legend.spacing.y = unit(0.8, 'cm'),
          legend.key.size = unit(1, "cm"),
          panel.spacing.x = unit(0.8, 'cm')) +
    scale_fill_manual(name = "Generation technologies", 
                      values = gen_type_color_scheme,
                      guide = guide_legend(reverse=TRUE, nrow = 5)) +
    scale_y_continuous(labels = scales::percent,
                       sec.axis = sec_axis(~./scale_num, name="Second Axis", labels = scales::percent_format(accuracy = 1)))
  
  elec_mix_figure
}

# Function for Figure 2d
elec_co2_figure_fun <- function(region_name, lower_b, upper_b){
  
  elec_co2_data_figure <- Elect_co2_data_fig %>% filter(region == region_name)
  
  elec_co2_figure <- 
    ggplot() + 
    
    geom_line(data = elec_co2_data_figure, aes(x = year, y = elec_co2_Gt, group = Climate, color = Climate), size = 1.6) + 
    geom_hline(yintercept = 0, linetype="dashed", color = "black") +
    theme(plot.margin = margin(0.5,0.5,1.5,2.0, "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.text = element_text(size = 16),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.direction = "horizontal",
          legend.position = "none",
          legend.spacing.x = unit(0.5, 'cm'),
          legend.spacing.y = unit(0.8, 'cm'),
          legend.key.size = unit(1, "cm"),
          panel.spacing.x = unit(0.8, 'cm')) +
    scale_color_manual(values = Climate_color_scheme) +
    scale_y_continuous(limits = c(lower_b, upper_b), breaks = c(seq(lower_b, upper_b, (upper_b - lower_b)/4)))
  
  elec_co2_figure
}



## Making figures

US_elec_SSP2_figure <- elec_mix_figure_fun(climate_scenario = 'SSP2', region_name = 'US', 8000)
US_elec_RCP19_figure <- elec_mix_figure_fun(climate_scenario = 'RCP19', region_name = 'US', 8000)
US_elec_RCP19_DAC_figure <- elec_mix_figure_fun2(climate_scenario = 'RCP19_DAC', region_name = 'US', 8000, 17)
US_elec_CO2_figure <- elec_co2_figure_fun(region_name = 'US', lower_b = -2, upper_b = 2)

plot_grid(US_elec_SSP2_figure, US_elec_RCP19_figure, US_elec_RCP19_DAC_figure, US_elec_CO2_figure, ncol=2, align="v")  


CN_elec_SSP2_figure <- elec_mix_figure_fun(climate_scenario = 'SSP2', region_name = 'China', 16000)
CN_elec_RCP19_figure <- elec_mix_figure_fun(climate_scenario = 'RCP19', region_name = 'China', 16000)
CN_elec_RCP19_DAC_figure <- elec_mix_figure_fun2(climate_scenario = 'RCP19_DAC', region_name = 'China', 16000, 33)
CN_elec_CO2_figure <- elec_co2_figure_fun(region_name = 'China', lower_b = -3, upper_b = 9)

plot_grid(CN_elec_SSP2_figure,CN_elec_RCP19_figure, CN_elec_RCP19_DAC_figure,  CN_elec_CO2_figure, ncol=2, align="v")  


RU_elec_SSP2_figure <- elec_mix_figure_fun(climate_scenario = 'SSP2', region_name = 'Russia', 3600)
RU_elec_RCP19_figure <- elec_mix_figure_fun(climate_scenario = 'RCP19', region_name = 'Russia', 3600)
RU_elec_RCP19_DAC_figure <- elec_mix_figure_fun2(climate_scenario = 'RCP19_DAC', region_name = 'Russia', 3600, 2.5)
RU_elec_CO2_figure <- elec_co2_figure_fun(region_name = 'Russia', lower_b = -0.3, upper_b = 0.9)

plot_grid(RU_elec_SSP2_figure,RU_elec_RCP19_figure,  RU_elec_RCP19_DAC_figure, RU_elec_CO2_figure, ncol=2, align="v")  


EU_elec_SSP2_figure <- elec_mix_figure_fun(climate_scenario = 'SSP2', region_name = 'Western_EU', 6000)
EU_elec_RCP19_figure <- elec_mix_figure_fun(climate_scenario = 'RCP19', region_name = 'Western_EU', 6000)
EU_elec_RCP19_DAC_figure <- elec_mix_figure_fun2(climate_scenario = 'RCP19_DAC', region_name = 'Western_EU', 6000, 33)
EU_elec_CO2_figure <- elec_co2_figure_fun(region_name = 'Western_EU', lower_b = -1.6, upper_b = 1.6)

plot_grid(EU_elec_SSP2_figure, EU_elec_RCP19_figure, EU_elec_RCP19_DAC_figure, EU_elec_CO2_figure, ncol=2, align="v")  


GL_elec_SSP2_figure <- elec_mix_figure_fun(climate_scenario = 'SSP2', region_name = 'World', 106000)
GL_elec_RCP19_figure <- elec_mix_figure_fun(climate_scenario = 'RCP19', region_name = 'World', 106000)
GL_elec_RCP19_DAC_figure <- elec_mix_figure_fun2(climate_scenario = 'RCP19_DAC', region_name = 'World', 106000, 13)
GL_elec_CO2_figure <- elec_co2_figure_fun(region_name = 'World', lower_b = -26, upper_b = 26)

plot_grid(GL_elec_SSP2_figure, GL_elec_RCP19_figure, GL_elec_RCP19_DAC_figure, GL_elec_CO2_figure, ncol=2, align="v")  



# Making legend 
legend <- 
  ggplot(Elect_gen_data) + 
  geom_area(aes(x = year, y = World/1000, fill = tech)) + 
  facet_wrap(~ Climate, nrow = 1) +
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30),
        axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
        panel.background = element_blank(),
        panel.border = element_blank(),
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
  scale_fill_manual(name = "Generation technologies", 
                    values = gen_type_color_scheme,
                    guide = guide_legend(reverse=TRUE, nrow = 6)) +
  scale_y_continuous(limits = c(0, 105), breaks = c(0, 25, 50, 75, 100), name = "Value")


## End of electricity mix figures (Figure 2)


## exporting the data to excel sheet

Elect_mix_data_fig
Elect_co2_data_fig
DAC_elec_demand_ratio

elec_mix_export <- 
  Elect_mix_data_fig %>% 
  filter(year %in% year_10) %>%
  spread(key = 'Climate', value = 'mix') %>%
  set_colnames(c('year', 'tech', 'region', 'SSP2_Baseline', 'SSP2_RCP19_noDAC', 'SSP2_RCP19_DAC')) %>%
  gather(key = 'Climate_scenario', value = 'Grid_mix', 4:6) 


elec_co2_export <- 
  Elect_co2_data_fig %>% 
  filter(year %in% year_10) %>%
  spread(key = 'Climate', value = 'elec_co2_Gt') %>%
  set_colnames(c('year', 'region', 'SSP2_Baseline', 'SSP2_RCP19_noDAC', 'SSP2_RCP19_DAC')) %>%
  gather(key = 'Climate_scenario', value = 'Grid_mix', 3:5) 

DAC_elec_demand_percentage_export <- 
  DAC_elec_demand_ratio %>% 
  filter(year %in% year_10) %>%
  set_colnames(c('year', 'region', 'DAC_elect_demand_percentage')) 


writeWorksheetToFile("C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/Figure_2_data.xlsx", data = elec_mix_export, sheet = "elec_grid_mix")
writeWorksheetToFile("C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/Figure_2_data.xlsx", data = elec_co2_export, sheet = "elec_co2_emission")
writeWorksheetToFile("C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/Figure_2_data.xlsx", data = DAC_elec_demand_percentage_export, sheet = "DAC_elec_demand_percentage")


###############################################
## End of Figure 2 and SI-Figure 2-5



## Figure 3a Generation difference between SSP2-RCP1.9 with and without DACCS and annual DACCS operational capacity
###############################################

## Data wrangling (generation difference)
elec_prod_RCP19 <- elec_data_clean_fun(elec_prod_RCP19_im)
elec_prod_RCP19_DAC <- elec_data_clean_fun(elec_prod_RCP19_DAC_im)

Elec_gen_RCP19_diff_data <- elec_prod_RCP19 %>% 
  merge(elec_prod_RCP19_DAC, by = c('year', 'tech', 'region')) %>%
  set_colnames(c("year", "tech", "region", "SSP2_RCP19", 'SSP2_RCP19_DAC')) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  mutate(RCP19_diff = SSP2_RCP19_DAC - SSP2_RCP19) %>%
  select(year, tech, region, RCP19_diff) %>%
  filter(year %in% year_10 & region == 'US')
  
Elec_gen_RCP19_diff_data$year <- as.numeric(Elec_gen_RCP19_diff_data$year)


Elec_gen_RCP19_diff_data

Elec_gen_RCP19_diff_data['tech_name'] <- NA

for (i in 1:length(Elec_gen_RCP19_diff_data$tech)) {
  if (Elec_gen_RCP19_diff_data[i, 2] == 'Coal_ST') {
    Elec_gen_RCP19_diff_data[i, 5] = "Coal"} 
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'IGCC') {
    Elec_gen_RCP19_diff_data[i, 5] = "Coal"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Coal_CHP') {
    Elec_gen_RCP19_diff_data[i, 5] = "Coal"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Coal_CCS') {
    Elec_gen_RCP19_diff_data[i, 5] = "Coal_CCS"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Coal_CHP_CCS') {
    Elec_gen_RCP19_diff_data[i, 5] = "Coal_CCS"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Natural_gas_CC') {
    Elec_gen_RCP19_diff_data[i, 5] = "Natural_gas"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Natural_gas_OC') {
    Elec_gen_RCP19_diff_data[i, 5] = "Natural_gas"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Natural_gas_CHP') {
    Elec_gen_RCP19_diff_data[i, 5] = "Natural_gas"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Natural_gas_CCS') {
    Elec_gen_RCP19_diff_data[i, 5] = "Natural_gas_CCS"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Natural_gas_CHP_CCS') {
    Elec_gen_RCP19_diff_data[i, 5] = "Natural_gas_CCS"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Oil_ST') {
    Elec_gen_RCP19_diff_data[i, 5] = "Oil"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Oil_CC') {
    Elec_gen_RCP19_diff_data[i, 5] = "Oil"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Oil_CHP') {
    Elec_gen_RCP19_diff_data[i, 5] = "Oil"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Oil_CCS') {
    Elec_gen_RCP19_diff_data[i, 5] = "Oil_CCS"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Oil_CHP_CCS') {
    Elec_gen_RCP19_diff_data[i, 5] = "Oil_CCS"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Nuclear') {
    Elec_gen_RCP19_diff_data[i, 5] = "Nuclear"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Hydro') {
    Elec_gen_RCP19_diff_data[i, 5] = "Hydro"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Wind_onshore') {
    Elec_gen_RCP19_diff_data[i, 5] = "Wind"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Wind_offshore') {
    Elec_gen_RCP19_diff_data[i, 5] = "Wind"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'CSP') {
    Elec_gen_RCP19_diff_data[i, 5] = "Solar"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Solar_PV_decen') {
    Elec_gen_RCP19_diff_data[i, 5] = "Solar"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Solar_PV_cen') {
    Elec_gen_RCP19_diff_data[i, 5] = "Solar"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Biomass_ST') {
    Elec_gen_RCP19_diff_data[i, 5] = "Biomass"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Biomass_CC') {
    Elec_gen_RCP19_diff_data[i, 5] = "Biomass"}
  else if (Elec_gen_RCP19_diff_data[i, 2] =='Biomass_CHP') {
    Elec_gen_RCP19_diff_data[i, 5] = "Biomass"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Biomass_CCS') {
    Elec_gen_RCP19_diff_data[i, 5] = "Biomass_CCS"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Biomass_CHP_CCS') {
    Elec_gen_RCP19_diff_data[i, 5] = "Biomass_CCS"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Other_renewables') {
    Elec_gen_RCP19_diff_data[i, 5] = "Other_renewables"}
  else if (Elec_gen_RCP19_diff_data[i, 2] == 'Wave') {
    Elec_gen_RCP19_diff_data[i, 5] = "Other_renewables"}
}


Elec_gen_RCP19_diff_data$tech_name <- fct_rev(factor(Elec_gen_RCP19_diff_data$tech_name, levels = tech_sort_agg))

Elec_gen_RCP19_diff_data_agg <- 
  Elec_gen_RCP19_diff_data %>% 
  group_by(year, tech_name, region) %>%
  summarise(RCP19_diff_agg = sum(RCP19_diff))


Annual_elec_gen_RCP19_diff_data <- 
  Elec_gen_RCP19_diff_data_agg %>%
  group_by(year, region) %>%
  summarise(RCP19_diff_annual = sum(RCP19_diff_agg))

Annual_elec_gen_RCP19_diff_data$year <- as.numeric(Annual_elec_gen_RCP19_diff_data$year)



## Data wrangling (annual DACCS operation capacity)
DAC_operation_capacity_data <- DAC_operation_capacity %>% gather(key = 'year', value = 'value', 2:82) %>%
  set_colnames(c('region', 'year', 'DAC_opr_cap_Gt')) %>%
  filter(region %in% c('USA', 'China Region', 'Western Europe', 'Russia Region', 'World')) %>%
  filter(year %in% year_10)

DAC_operation_capacity_data$year <- as.numeric(DAC_operation_capacity_data$year)


## Making figure
Elec_gen_diff_figure <- ggplot() + 
  geom_bar(data = Elec_gen_RCP19_diff_data_agg , 
           aes(x = year, y = RCP19_diff_agg, fill = tech_name), position="stack", stat="identity") + 
  geom_line(data = Annual_elec_gen_RCP19_diff_data, 
            aes(x = year, y = RCP19_diff_annual), color = "red", size = 1.2) + 
  geom_line(data = DAC_operation_capacity_data %>% filter(region == 'USA'), aes(x = year, y = DAC_opr_cap_Gt*500), color = "black", size = 1.2) + 
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  #  facet_wrap(~ region, nrow = 1) +
  theme(plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        strip.text = element_text(size = 16),
        strip.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 20),
        legend.direction = "horizontal",
        legend.position = "none",
        legend.spacing.x = unit(0.5, 'cm'),
        legend.spacing.y = unit(0.8, 'cm'),
        legend.key.size = unit(1, "cm"),
        panel.spacing.x = unit(0.8, 'cm')) +
  scale_fill_manual(name = "Generation technologies", 
                    values = color_scheme_agg,
                    guide = guide_legend(reverse=TRUE, nrow = 6)) +
  scale_x_continuous(breaks=c(2020, 2040, 2060, 2080, 2100)) +
  #scale_y_continuous(limits = c(-1000, 1000), breaks = c(seq(-1000, 1000, 500)), name = "Value")
  scale_y_continuous(limits = c(-650, 650), breaks = c(seq(-600, 600, 300)), name = "Value",
                     sec.axis = sec_axis(~.*0.002, name="Second Axis", breaks = c(seq(-1.0, 1.0, 0.5))))

## end of figure 


## Export data to excel

writeWorksheetToFile("C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/LCA_result/Figure_3_data.xlsx", data = Elec_gen_RCP19_diff_data_agg, sheet = "Figure_3a_bar")
writeWorksheetToFile("C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/LCA_result/Figure_3_data.xlsx", data = Annual_elec_gen_RCP19_diff_data, sheet = "Figure_3a_red_line")
writeWorksheetToFile("C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/LCA_result/Figure_3_data.xlsx", data = DAC_operation_capacity_data, sheet = "Figure_3a_black_line")
###############################################
## End of Figure_3b



## SI-Figure 7 -- Primary Energy use (unit PJ = 1,000,000 GJ, EJ = 1000 PJ)
###############################################


PE_use_RCP19 <- PE_use_RCP19_im[2:8] %>%
  set_colnames(c("year", 'tech', "China", "US", "Western_EU", "Russia", "World")) %>%
  gather(key = 'region', value = 'PE_use_PJ', 3:7) %>%
  mutate(scenario = 'RCP19') %>%
  select(year, tech, region, scenario, PE_use_PJ)

PE_use_RCP19_DAC <- PE_use_RCP19_DAC_im[2:8] %>%
  set_colnames(c("year", 'tech', "China", "US", "Western_EU", "Russia", "World")) %>%
  gather(key = 'region', value = 'PE_use_PJ', 3:7) %>%
  mutate(scenario = 'RCP19_DAC') %>%
  select(year, tech, region, scenario, PE_use_PJ)


fossil_fuel <- c("Coal", "Conventional Oil", "Unconventional Oil", "Natural Gas")

PE_use_overall <- PE_use_RCP19 %>% 
  rbind(PE_use_RCP19_DAC) %>%
  filter(year %in% year_10 & tech %in% fossil_fuel) %>%
  mutate(PE_use_EJ = PE_use_PJ/1000) %>%
  select(year, tech, region, scenario, PE_use_EJ) %>%
  spread(key = scenario, value = PE_use_EJ) %>%
  mutate(PE_use_diff_EJ = RCP19_DAC - RCP19) %>%
  select(year, tech, region, PE_use_diff_EJ)

PE_use_overall_point <- PE_use_overall %>% group_by(year, region) %>%
  summarise(annual_net_diff_EJ = sum(PE_use_diff_EJ))


# Figure function
PE_use_figure_fun  <- function(region_name, lower_l, upper_l, lower_b, upper_b){
  
  
  #figure_data <- PE_use_overall %>% filter(region == 'China')
  figure_data <- PE_use_overall %>% filter(region == region_name)
  point_data <- PE_use_overall_point %>%  filter(region == region_name)
  
  
  PE_use_color_scheme = c('Coal' = 'black', 
                          'Conventional Oil'= 'gray50',
                          'Unconventional Oil'= 'gray50',
                          'Natural Gas' = 'gray80')
  
  PE_use_figure <- 
    ggplot() + 
    
    geom_bar(data = figure_data,
             aes(x = year, y = PE_use_diff_EJ, fill = tech), position = "stack", stat = "identity", width = 5) + 
    geom_point(data = point_data,
               aes(x = year, y = annual_net_diff_EJ), color = 'red', size = 7) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
    
    theme(plot.margin = margin(0.5,0.5,1.5,2.0, "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 26),
          axis.text.y = element_text(size = 26),
          axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.text = element_text(size = 16),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.direction = "horizontal",
          legend.position = "none",
          legend.spacing.x = unit(0.5, 'cm'),
          legend.spacing.y = unit(0.8, 'cm'),
          legend.key.size = unit(1, "cm"),
          panel.spacing.x = unit(0.8, 'cm')) +
    scale_fill_manual(values = NG_use_color_scheme) +
    #    scale_x_continuous(breaks = year_10) +
    scale_y_continuous(limits = c(lower_l, upper_l), breaks = c(seq(lower_b, upper_b, (upper_b - lower_b)/4)))
  
  
  PE_use_figure 
}


# Making figures
US_PE_use_figure <- PE_use_figure_fun(region_name = 'US', lower_l = -1, upper_l = 12, lower_b = 0, upper_b = 12)
CN_PE_use_figure <- PE_use_figure_fun(region_name = 'China', lower_l = -1, upper_l = 12, lower_b = 0, upper_b = 12)
RU_PE_use_figure <- PE_use_figure_fun(region_name = 'Russia', lower_l = -1.8, upper_l = 12, lower_b = 0, upper_b = 12)
EU_PE_use_figure <- PE_use_figure_fun(region_name = 'Western_EU', lower_l = -1, upper_l = 12, lower_b = 0, upper_b = 12)
GL_PE_use_figure <- PE_use_figure_fun(region_name = 'World', lower_l = -2, upper_l = 200, lower_b = 0, upper_b = 200)


plot_grid(US_PE_use_figure, CN_PE_use_figure, ncol=1, align="v")  
plot_grid(RU_PE_use_figure, EU_PE_use_figure, ncol=1, align="v")
plot_grid(GL_PE_use_figure, GL_PE_use_figure, ncol=1, align="v")  


# Export data
writeWorksheetToFile("C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/SI_Figure_7_data.xlsx", data = PE_use_overall, sheet = "Primary_energy_diff_bar")
writeWorksheetToFile("C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/SI_Figure_7_data.xlsx", data = PE_use_overall_point, sheet = "Primary_energy_diff_point")

###############################################
## End of SI-Figure_7



## SI-Figure 8 -- CO2 sequestration
###############################################

## Data wrangling

DAC_co2_seq_RCP19 <- DAC_co2_seq_RCP19_im[2:7] %>%
  set_colnames(c("year", "China", "US", "Western_EU", "Russia", "World")) %>%
  gather(key = 'region', value = 'co2_seq_Gt', 2:6) %>%
  mutate(tech = 'DACCS',
         scenario = 'RCP19') %>%
  select(year, tech, region, scenario, co2_seq_Gt)

BECCS_co2_seq_RCP19 <- BECCS_co2_seq_RCP19_im[2:7] %>%
  set_colnames(c("year", "China", "US", "Western_EU", "Russia", "World")) %>%
  gather(key = 'region', value = 'co2_seq_Gt', 2:6) %>%
  mutate(tech = 'BECCS',
         scenario = 'RCP19') %>%
  select(year, tech, region, scenario, co2_seq_Gt)

total_co2_seq_RCP19 <- total_co2_seq_RCP19_im[2:7] %>%
  set_colnames(c("year", "China", "US", "Western_EU", "Russia", "World")) %>%
  gather(key = 'region', value = 'co2_seq_Gt', 2:6) %>%
  mutate(tech = 'total',
         scenario = 'RCP19') %>%
  select(year, tech, region, scenario, co2_seq_Gt)


DAC_co2_seq_RCP19_DAC <- DAC_co2_seq_RCP19_DAC_im[2:7] %>%
  set_colnames(c("year", "China", "US", "Western_EU", "Russia", "World")) %>%
  gather(key = 'region', value = 'co2_seq_Gt', 2:6) %>%
  mutate(tech = 'DACCS',
         scenario = 'RCP19_DAC') %>%
  select(year, tech, region, scenario, co2_seq_Gt)

BECCS_co2_seq_RCP19_DAC <- BECCS_co2_seq_RCP19_DAC_im[2:7] %>%
  set_colnames(c("year", "China", "US", "Western_EU", "Russia", "World")) %>%
  gather(key = 'region', value = 'co2_seq_Gt', 2:6) %>%
  mutate(tech = 'BECCS',
         scenario = 'RCP19_DAC') %>%
  select(year, tech, region, scenario, co2_seq_Gt)

total_co2_seq_RCP19_DAC <- total_co2_seq_RCP19_DAC_im[2:7] %>%
  set_colnames(c("year", "China", "US", "Western_EU", "Russia", "World")) %>%
  gather(key = 'region', value = 'co2_seq_Gt', 2:6) %>%
  mutate(tech = 'total',
         scenario = 'RCP19_DAC') %>%
  select(year, tech, region, scenario, co2_seq_Gt)


tech_name <- c('Other', 'DACCS', 'BECCS')

co2_seq_data <- DAC_co2_seq_RCP19 %>% 
  rbind(BECCS_co2_seq_RCP19) %>% 
  rbind(total_co2_seq_RCP19) %>%
  rbind(DAC_co2_seq_RCP19_DAC) %>% 
  rbind(BECCS_co2_seq_RCP19_DAC) %>%
  rbind(total_co2_seq_RCP19_DAC) %>% 
  spread(key = 'tech', value = 'co2_seq_Gt') %>%
  mutate(Other = total - BECCS - DACCS) %>%
  gather(key = 'tech', value = 'co2_seq_Gt', 4:7) %>%
  filter(year %in% year_10 & tech %in% tech_name)

co2_seq_data$tech <- factor(co2_seq_data$tech, levels = tech_name)  
co2_seq_data$year <- as.numeric(co2_seq_data$year) 



## Figure function
co2_seq_figure_fun  <- function(region_name, lower_l, upper_l, lower_b, upper_b){
  
  
  #figure_data <- co2_seq_data %>% filter(region == 'US')
  figure_data <- co2_seq_data %>% filter(region == region_name)
  co2_seq_color_scheme = c('BECCS' = 'green', 
                           'DACCS'= 'skyblue2',
                           'Other' = 'gray80')
  
  co2_seq_figure <- 
    ggplot() + 
    #geom_bar(data = figure_data, aes(x = year, y = co2_seq_Gt, fill = tech), position="stack", stat="identity", width = 6) + 
    geom_bar(data = figure_data %>% filter(scenario == 'RCP19'),
             aes(x = year - 2, y = co2_seq_Gt, fill = tech), position = "stack", stat = "identity", width = 3) + 
    geom_bar(data = figure_data %>% filter(scenario == 'RCP19_DAC'),
             aes(x = year + 2, y = co2_seq_Gt, fill = tech), position = "stack", stat = "identity", width = 3) + 
    #facet_wrap(~ scenario , nrow = 1) + 
    theme(plot.margin = margin(0.5,0.5,1.5,2.0, "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 26),
          axis.text.y = element_text(size = 26),
          axis.line = element_line(colour = "black", size = 0.5, linetype = "solid"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          strip.text = element_text(size = 16),
          strip.background = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.direction = "horizontal",
          legend.position = "none",
          legend.spacing.x = unit(0.5, 'cm'),
          legend.spacing.y = unit(0.8, 'cm'),
          legend.key.size = unit(1, "cm"),
          panel.spacing.x = unit(0.8, 'cm')) +
    scale_fill_manual(values = co2_seq_color_scheme) +
    scale_x_continuous(breaks = year_10) +
    scale_y_continuous(limits = c(lower_l, upper_l), breaks = c(seq(lower_b, upper_b, (upper_b - lower_b)/4)))
  
  co2_seq_figure 
}


## Make figures
US_co2_seq_figure <- co2_seq_figure_fun(region_name = 'US', lower_l = 0, upper_l = 4, lower_b = 0, upper_b = 4)
CN_co2_seq_figure <- co2_seq_figure_fun(region_name = 'China', lower_l = 0, upper_l = 4, lower_b = 0, upper_b = 4)
RU_co2_seq_figure <- co2_seq_figure_fun(region_name = 'Russia', lower_l = 0, upper_l = 4, lower_b = 0, upper_b = 4)
EU_co2_seq_figure <- co2_seq_figure_fun(region_name = 'Western_EU', lower_l = 0, upper_l = 4, lower_b = 0, upper_b = 4)
GL_co2_seq_figure <- co2_seq_figure_fun(region_name = 'World', lower_l = 0, upper_l = 41, lower_b = 0, upper_b = 40)


plot_grid(US_co2_seq_figure, CN_co2_seq_figure, ncol=1, align="v")  
plot_grid(RU_co2_seq_figure, EU_co2_seq_figure, ncol=1, align="v")
plot_grid(GL_co2_seq_figure, GL_co2_seq_figure, ncol=1, align="v")  



co2_seq_export <- co2_seq_data %>%
  spread(key = 'scenario', value = 'co2_seq_Gt') %>%
  set_colnames(c('year', 'region', 'tech', 'SSP2_RCP19_noDAC', 'SSP2_RCP19_DAC')) %>%
  gather(key = 'Climate_scenario', value = 'co2_seq_Gt', 4:5) 

writeWorksheetToFile("C:/Users/yqiu/Box/DAC LCA-IAM/YQ_work/result/Electricity_mix/SI_Figure_8_data.xlsx", data = co2_seq_export, sheet = "co2_seq")  

  
# calculate the difference between with and without DAC scenario  
co2_seq_data_for_writing <- co2_seq_data %>% filter(tech == 'Other') %>%
  spread(key = scenario, value = co2_seq_Gt) %>%
  mutate(diff = RCP19_DAC - RCP19)
###############################################
## End of SI-Figure_8



