#Code for the article Kazlou T., Cherp A., Jewell J.
#Feasible deployment of carbon capture and storage and the requirements of climate targets

#Vetting IPCC AR6 1.5°C- and 2°C-compatible scenarios with feasibility constraints (replication of Figure 4)
#Working directory should be set to the directory containing input files
#Necessary packages (should be installed)

library(tidyverse)
library(ggpmisc)
library(zoo)
library(patchwork)
library(lemon)
library(ggpubr)
`%nin%` = Negate(`%in%`)

all <- read_csv("CCS_data.csv") %>% 
  select(-...1) %>% filter(Year %in% c(2030:2100)) %>% 
  group_by(model_scenario) %>% 
  mutate(Value = Value/1000) %>% 
  group_by(model_scenario, temp) %>% 
  group_modify(~ add_row(., .before=0, Year = c(2031:2039, 2041:2049, 2051:2059, 2061:2069, 2071:2079, 2081:2089, 2091:2099), Value = NA)) %>% 
  arrange(model_scenario, Year) %>%
  mutate(Value=na.approx(Value), cumul = cumsum(Value)) %>%
  filter(Year %in% c(2030,2040,2100) & temp != "2.5°C") %>% 
  pivot_longer(cols = -c("model_scenario", "temp", "Year", "Category"), names_to = "Variable") %>% 
  filter((Variable == "cumul" & Year == 2100) | (Variable == "Value" & Year %in% c(2030,2040))) %>% select(-Variable) %>% 
  pivot_wider(names_from = "Year", values_from = "value") %>% 
  mutate(`2100` = ifelse(model_scenario == "MESSAGEix-GLOBIOM 1.0_LowEnergyDemand_1.3_IPCC", 0, `2100`))
#in this dataset, 2030 and 2040 indicate annual capacity, whereas 2100 - cumulative capcity (2030-2100)

#Formative phase
filter2030 <- all %>% filter(`2030` <= 0.37) %>% select(model_scenario, temp, Category, `2100`) %>% mutate(set = "Feasible by 2030")

#Acceleration phase
feasible2040 <- read.csv("acceleration_frontier.csv") %>% filter(group != "above")
filter2040 <- all %>% filter(model_scenario %in% feasible2040$model_scenario)  %>% select(model_scenario, temp, Category, `2100`) %>% mutate(set = "Feasible by 2040")

#Stable growth phase
normalisation <- read.csv("normalisation.csv") %>% 
  filter(temp != "2.5°C") %>% select(-X, -temp) %>% rename(model_scenario = Country) 
feasible_longterm_curmar <-  filter2040 %>% left_join(., normalisation, by = "model_scenario")  %>% filter(G_2022 <= 1.45)  %>% #CHOOSE BETWEEN G_2020_scenario & G_edgar
  select(model_scenario, temp, Category, `2100`)  %>% ungroup() %>% 
  mutate(set = "Feasible with G_2020")
feasible_longterm_futmar <-  filter2040 %>% left_join(., normalisation, by = "model_scenario")  %>% filter(G_TMax <= 1.45)  %>% 
  select(model_scenario, temp, Category, `2100`) %>% ungroup() %>% 
  add_row(model_scenario = "MESSAGEix-GLOBIOM 1.0_LowEnergyDemand_1.3_IPCC", temp="1.5°C", Category = "C1", `2100` = 0) %>% 
  mutate(set = "Feasible with G_TMax")

all_2100 <- all %>% select(model_scenario, temp, Category, `2100`) %>% mutate(set = "All scenarios")

filters_2100 <- rbind(all_2100, filter2030, filter2040, feasible_longterm_curmar, feasible_longterm_futmar) %>%
  mutate(set = ifelse(set == "Feasible with G_2020", "Feasible with G2022", ifelse(set == "Feasible with G_TMax", "Feasible with GTMax", set)))

filters_2100$set <- factor(filters_2100$set, levels = c("All scenarios", "Feasible by 2030", "Feasible by 2040", "Feasible with G2022", "Feasible with GTMax"),
                           ordered = TRUE) 

summary <- filters_2100 %>% group_by(set, temp) %>% summarise(N=n(), m = median(`2100`), q1 = quantile(`2100`, probs = 0.25), q3 = quantile(`2100`, probs = 0.75), 
                                                              min = min(`2100`), max = max(`2100`))

count <- filters_2100 %>% group_by(set, temp) %>% summarise(N = n()) %>% mutate(share=ifelse(temp == "1.5°C", N/218*100, N/423*100)) %>% ungroup() %>%
  slice(rep(1:n(), each = 2)) %>% arrange(temp) %>%
  mutate(x = rep(c(0.5, 1.5, 1.5, 2.5, 2.5, 3.5, 3.5, 4.5, 4.5, 5.5), 2)) 

#Plotting
filters_2100_plot <-  ggplot(filters_2100, aes(set, `2100`))+
  geom_area(data=count, aes(x=x, y=share*22.5, group = 1, alpha = 0.4), fill = "lightgrey", inherit.aes = FALSE)+
  geom_violin(data = subset(filters_2100, !(set %in% c("Feasible with G2022", "Feasible with GTMax") & temp == "1.5°C")), aes(fill = temp),  scale = "width", size = 0.2, color = NA, alpha = 0.5) +
  geom_boxplot(data = subset(filters_2100, !(set %in% c("Feasible with G2022", "Feasible with GTMax") & temp == "1.5°C")), aes(colour=factor(temp)), outlier.alpha = 0.5, size = 0.2, outlier.size = 0.3,width=0.1) +
  geom_boxplot(data = subset(filters_2100, !(set %in% c("Feasible with G2022", "Feasible with GTMax") & temp == "1.5°C")), aes(fill = temp), width=0.1, outlier.color = NA, position = position_dodge(width =0.9), size = 0.2, outlier.size = 0.1)+
  geom_point(data = subset(filters_2100, set %in% c("Feasible with G2022", "Feasible with GTMax") & temp == "1.5°C"), aes(colour=factor(temp)), size = 0.3, alpha = 0.5)+
  facet_grid(cols = vars(temp))+ 
  labs(alpha = "Share of scenarios\n(right y-axis)")+
  scale_fill_manual("Cumulative capture and storage in IPCC\nAR6 scenarios by 2100 (left y-axis)", labels = c("1.5°C", "2°C"),  values = c("1.5°C" = "#a6cee3", "2.0°C" = "#7fc97f")) +
  scale_color_manual("Cumulative capture and storage in IPCC\nAR6 scenarios by 2100 (left y-axis)", labels = c("1.5°C", "2°C"),  values = c("1.5°C" = "#a6cee3", "2.0°C" = "#7fc97f")) +
  scale_y_continuous("Cumulative capture and storage by 2100, GtCO2", expand = c(0.002,0.1), breaks = seq(500,2000,500),
                     sec.axis = sec_axis(~./22.5, name = "Share of scenarios for each temperature group, %", breaks = seq(20,80,20)))+
  scale_x_discrete("", expand = c(0,0))+
  guides(fill=guide_legend(order = 1), alpha = guide_legend(order = 2), color = "none")+
  theme_bw(base_size = 7) +
  theme(legend.position = "bottom", panel.grid = element_blank(), axis.text.x=element_text(angle = 45, hjust = 1, size = 5),  strip.background = element_blank(), #
        strip.text.x = element_blank()) 
ggsave("f4.png", dpi = 500, width = 150, height = 100, units = "mm", path = "./")

#saves a PNG file of Figure 4 in the working directory

